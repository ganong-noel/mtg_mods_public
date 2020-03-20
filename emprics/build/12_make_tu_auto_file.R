## ---------------------------------
## MAKE ACOUNT LEVEL AUTO FILE ####
## ---------------------------------

make_year <- function(year){
need to match for 2012 only

  trade_file <- readRDS(str_c(working_path, "tradefile", year, ".rds"))

  header_file <- readRDS(str_c(working_path, "tu_r_files/DDVHDALL", year, ".rds")) %>%
    select(sq_nm, bcs_seqnum, zip, bir_dt, filetag)

  ids <- readRDS(paste0(working_path, "tu_match_file.rds")) %>%
    ungroup() %>%
    mutate(opn_dt = floor(opn_dt / 10000)) %>%
    filter(opn_dt < 2009) %>%
    select(bcs_seqnum) %>%
    distinct()

  map_dfr(1:4, function(ind)
    readRDS(str_c(working_path, "tu_r_files/DDVTH", ind, year, ".rds")) %>%
      left_join(trade_file, by = c("act_id", "sq_nm")) %>%
      filter(act_tp_cd == "AU") %>%
      select(
        -th_act_rtng_prof_cd, -th_cred_limit_am,
        -th_comp_rmk_cd, -th_gen_rmk_cd, -th_rat_rmk_cd
      ) %>%
      left_join(header_file, by = "sq_nm") %>%
      filter(filetag == "F1") %>%
      select(-port_tp_cd, -ind_cd, -zip, -bir_dt) %T>%
      saveRDS(str_c(working_path, "auto_file_", ind, year, "rds")) %>%
      select(-rcd_tp, -filetag) %>%
      inner_join(ids, by = "bcs_seqnum"))
}


combined_auto_file <- map_dfr(c("_2012", "_2014"),
                          make_year)

system.time(combined_auto_file <- combined_auto_file %>% arrange(bcs_seqnum, act_id, ver_dt))

saveRDS(combined_auto_file, file = paste0(working_path, "combined_auto_file_raw.rds"))

## ---------------------------#
## Add Auto Purchase Info ####
## ---------------------------#

# How many unique accounts?

combined_auto_file <- as.data.frame(combined_auto_file)

new_auto_loans <-
  combined_auto_file %>%
  group_by(bcs_seqnum, act_id) %>%
  mutate(first_obs_date = min(ver_dt)) %>%
  filter(ver_dt == first_obs_date)

# Drop those where opn_dt is NA
new_auto_loans <- new_auto_loans %>% filter(!is.na(opn_dt))

rm(combined_auto_file)

new_auto_loans <-
  new_auto_loans %>%
  mutate(
    opn_mnth = floor(opn_dt / 100),
    auto_pur_ind = 1
  )

new_auto_loans <- as.data.frame(new_auto_loans)

new_auto_loans <-
  new_auto_loans %>%
  ungroup() %>%
  mutate(auto_pur_am = ifelse(is.na(th_high_cdt_am),
                              mean(th_high_cdt_am, na.rm = TRUE),
                              th_high_cdt_am))


## -----------------------------#
## Roll up to customer level####
## -----------------------------#

new_auto_loans <-
  new_auto_loans %>%
  ungroup() %>%
  group_by(bcs_seqnum, opn_mnth) %>%
  summarise(
    auto_pur_ind = sum(auto_pur_ind),
    auto_pur_am = sum(auto_pur_am)
  )

new_auto_loans <- as.data.frame(new_auto_loans)

auto_purchase_by_month <-
  new_auto_loans %>%
  group_by(opn_mnth) %>%
  summarise(
    prob_auto_pur = (sum(auto_pur_ind) / 1208092) * 100,
    mean_auto_pur_am = sum(auto_pur_am / 1208092)
  )


# Create ver_dt_monnb so we can merge onto base dataset
new_auto_loans <-
  new_auto_loans %>%
  mutate(
    ver_dt_N = as.Date(ymd(as.character(opn_mnth * 100 + 1))),
    ver_dt_monnb = monnb(as.Date(ver_dt_N))
  )

## ----------------------------#
## Save and clear workspace ####
## ----------------------------#

system.time(saveRDS(new_auto_loans, file = paste0(working_path, "new_auto_loans.rds")))
system.time(saveRDS(auto_purchase_by_month, file = paste0(working_path, "auto_purchase_by_month.rds")))

rm(auto_purchase_by_month)
rm(new_auto_loans)
