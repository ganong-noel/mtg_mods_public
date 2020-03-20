ptm <- proc.time()

## ---------------------------------##
## MAKE ACOUNT LEVEL MORTGAGE FILE ##
## ---------------------------------##

save_mortgage_and_index <- function(data, ind, year) {
  saveRDS(data,
            str_c(working_path, "mortgage_file_", ind, year, ".rds"))



  data %>%
    select(act_id, th_gen_rmk_cd) %>%
    filter(th_gen_rmk_cd == "LMD") %>%
    select(act_id) %>%
    distinct() %>%
    saveRDS(str_c(working_path, "LMD_code_ids_", ind, year, ".rds"))
}


make_year <- function(year){
  trade_file <- readRDS(str_c(working_path, "tradefile", year, ".rds"))

  header_file <- readRDS(str_c(working_path, "tu_r_files/DDVHDALL", year, ".rds")) %>%
    select(sq_nm, bcs_seqnum, zip, bir_dt, filetag)

  map(1:4, function(ind)
    readRDS(str_c(working_path, "tu_r_files/DDVTH", ind, year, ".rds")) %>%
      left_join(trade_file, by = c("act_id", "sq_nm")) %>%
      filter(port_tp_cd == "M" |
               act_tp_cd == "CV" |
               act_tp_cd == "FL" |
               act_tp_cd == "FR" |
               act_tp_cd == "RE" |
               act_tp_cd == "RM" |
               act_tp_cd == "VM") %>%
      filter(
        act_tp_cd != "CY" &
          act_tp_cd != "HE" &
          act_tp_cd != "HI" &
          act_tp_cd != "LN" &
          act_tp_cd != "RL" &
          act_tp_cd != "SM") %>%
      left_join(header_file, by = "sq_nm") %>%
    filter(filetag == "F1") %T>%
    save_mortgage_and_index(., ind, year)) %>%
    bind_rows()
}


combined_mortgage_file <- map(c("_2012", "_2014"), make_year) %>%
  bind_rows() %T>%
  saveRDS(file = paste0(working_path, "combined_mortgage_file_raw.rds"))

print("debug test")
##
## MAKE ACCOUNT LEVEL ONLY WITH LMDs ####
##

## -------------------------------------------------------------##
## Add Modification Date Indicator And Drop Those Without Mods ##
## -------------------------------------------------------------##

combined_mortgage_file <-
  combined_mortgage_file %>%
  arrange(bcs_seqnum, act_id, ver_dt)

first_LMD_date_file <-
  combined_mortgage_file %>%
  filter(th_gen_rmk_cd == "LMD") %>%
  group_by(bcs_seqnum, act_id) %>%
  summarise(first_LMD_date = min(ver_dt))

combined_mortgage_file <-
  left_join(combined_mortgage_file,
    first_LMD_date_file,
    by = c("bcs_seqnum", "act_id")
  )

mnths_observed_pre_LMD <-
  combined_mortgage_file %>%
  group_by(bcs_seqnum, act_id) %>%
  filter(ver_dt <= first_LMD_date) %>%
  summarise(
    months_pre_LMD = n(),
    first_LMD_date = mean(first_LMD_date)
  )

mnths_observed_pre_LMD_by_bcs <-
  mnths_observed_pre_LMD %>%
  group_by(bcs_seqnum) %>%
  summarise(months_pre_LMD_by_bcs = min(months_pre_LMD))

mnths_observed_pre_LMD_only <-
  mnths_observed_pre_LMD %>%
  select(-first_LMD_date)

combined_mortgage_file <-
  left_join(combined_mortgage_file,
    mnths_observed_pre_LMD_only,
    by = c("bcs_seqnum", "act_id")
  )

combined_mortgage_file <-
  left_join(combined_mortgage_file,
    mnths_observed_pre_LMD_by_bcs,
    by = "bcs_seqnum"
  )

# Drop those without any LMD
combined_mortgage_file_LMDs <-
  combined_mortgage_file %>%
  filter(!is.na(first_LMD_date))
rm(combined_mortgage_file)
n_distinct(combined_mortgage_file_LMDs$act_id)

# Drop those with no prior observations pre-LMD
combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  filter(months_pre_LMD > 1)
n_distinct(combined_mortgage_file_LMDs$act_id)

# Drop those with modifcation date before August 2010
combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  filter(first_LMD_date > 201007)
n_distinct(combined_mortgage_file_LMDs$act_id)

saveRDS(combined_mortgage_file_LMDs,
     file = paste0(working_path, "combined_mortgage_file_LMDs.rds"))
print(proc.time() - ptm)

rm(combined_mortgage_file_LMDs)
