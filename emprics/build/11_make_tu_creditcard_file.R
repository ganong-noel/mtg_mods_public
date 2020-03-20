## --------------------------------------#
## MAKE ACOUNT LEVEL CREDIT CARD FILE ####
## --------------------------------------#

start <- proc.time()

## Create 4 account-level credit card files in 2012

make_year <- function(year){

  #need to match for 2012 only

  trade_file <- readRDS(str_c(working_path, "tradefile", year, ".rds"))

  header_file <- readRDS(str_c(working_path, "tu_r_files/DDVHDALL", year, ".rds")) %>%
    select(sq_nm, bcs_seqnum, zip, bir_dt, filetag)

  year_df <- map_dfr(1:4, function(ind)
    readRDS(str_c(working_path, "tu_r_files/DDVTH", ind, year, ".rds")) %>%
      left_join(trade_file, by = c("act_id", "sq_nm")) %>%
      filter(
        act_tp_cd == "CC" |
          act_tp_cd == "FX" |
          act_tp_cd == "SC" |
          act_tp_cd == "CH"
      ) %>%
      select(
        -th_act_rtng_prof_cd, -th_high_cdt_am, -th_past_due_am,
        -th_comp_rmk_cd, -th_gen_rmk_cd, -th_rat_rmk_cd
      ) %>%
      left_join(header_file, by = "sq_nm") %>%
      filter(filetag == "F1") %>%
      select(-port_tp_cd, -ind_cd, -zip, -bir_dt) %T>%
      saveRDS(str_c(working_path, "cc_file_", ind, year, ".rds")) %>%
      select(-rcd_tp, -filetag))

  year_df <-
    readRDS(paste0(working_path, "tu_match_file.rds")) %>%
    ungroup() %>%
    mutate(opn_dt = floor(opn_dt / 10000)) %>%
    filter(opn_dt < 2009) %>%
    select(bcs_seqnum) %>%
    distinct() %>%
    inner_join(year_df)

  return(year_df)
}


combined_cc_file <- map_dfr(c("_2012", "_2014"), make_year)

## --------------------------------------------------------------------------------------
## Add Delinquency Indicator and Save Account-Level Credit Card File with All Cards ####
## --------------------------------------------------------------------------------------

combined_cc_file <-
  combined_cc_file %>%
  mutate(delinquent_ind = ifelse(th_mop %in% c("03", "04", "05", "9B"), 1, 0))


saveRDS(combined_cc_file,
        file = paste0(working_path, "combined_cc_file_act_all.rds"))


## -----------------------------------------------------
## Make bcs_seqnum Credit Card File With All Cards ####
## -----------------------------------------------------

combined_cc_file %>%
  ungroup() %>%
  group_by(bcs_seqnum, ver_dt) %>%
  summarise(
    th_cur_bal_am = sum(th_cur_bal_am, na.rm = TRUE),
    th_cred_limit_am = sum(th_cred_limit_am, na.rm = TRUE),
    tot_delin_ct = sum(delinquent_ind, na.rm = TRUE)
  ) %>%
  saveRDS(file = paste0(working_path,
                        "combined_cc_file_bcs_seqnum_all.rds"))

rm(combined_cc_file_bcs_seqnum_all)

#--------------------------------------------------
## Add Monthly Expenditure Calculations (head) ####
#--------------------------------------------------

mem_used()

combined_cc_file <- combined_cc_file %>%
  arrange(bcs_seqnum, act_id, ver_dt)


## -------------------------------------------------------------
## Keep only credit cards where payment amount is recorded ####
## -------------------------------------------------------------

obs_by_servicer <-
  combined_cc_file %>%
  group_by(rpt_sub_cd) %>%
  summarise(tot_obs = n())

na_obs_by_servicer <-
  combined_cc_file %>%
  filter(is.na(th_pay_am)) %>%
  group_by(rpt_sub_cd) %>%
  summarise(na_obs = n())
both_by_servicer <- left_join(obs_by_servicer, na_obs_by_servicer, by = "rpt_sub_cd")
both_by_servicer <- both_by_servicer %>% mutate(share_na = na_obs / tot_obs)
both_by_servicer <- both_by_servicer %>% arrange(share_na)


zero_obs_by_servicer <-
  combined_cc_file %>%
  filter(th_pay_am == 0) %>%
  group_by(rpt_sub_cd) %>%
  summarise(zero_obs = n())
both_by_servicer <- left_join(both_by_servicer, zero_obs_by_servicer, by = "rpt_sub_cd")
both_by_servicer <- both_by_servicer %>% mutate(share_zero = zero_obs / tot_obs)
both_by_servicer <- both_by_servicer %>% arrange(share_zero)

drop_list <- both_by_servicer %>% filter(share_na == 1 | share_zero > 0.9)
drop_list_char <- as.vector(drop_list$rpt_sub_cd)

n_bef_drop <-
  combined_cc_file %>%
  distinct(bcs_seqnum, act_id) %>%
  summarise(n())

# Filter these out
system.time(combined_cc_file <- combined_cc_file %>% filter(!(rpt_sub_cd %in% drop_list_char)))

n_aft_drop <-
  combined_cc_file %>%
  distinct(bcs_seqnum, act_id) %>%
  summarise(n())

print(as.numeric(n_aft_drop / n_bef_drop))
#test_that("Fraction of obs that we can construct credit card expenditure measure", {
#  expect_equal(as.numeric(n_aft_drop / n_bef_drop), 0.83, tolerance = 1e-2)
#})

combined_cc_file <- combined_cc_file %>% mutate(th_pay_am = ifelse(is.na(th_pay_am), 0, th_pay_am))


combined_cc_file <- as.data.frame(combined_cc_file)
combined_cc_file <-
  combined_cc_file %>%
  group_by(bcs_seqnum, act_id) %>%
  arrange(ver_dt) %>%
  mutate(
    lag_balance = lag(th_cur_bal_am, 1),
    balance_change = ifelse(!is.na(th_cur_bal_am) & !is.na(lag_balance), th_cur_bal_am - lag_balance, 0),
    cc_exp = balance_change + th_pay_am,
    cc_exp = ifelse(cc_exp < 0, 0, cc_exp)
  )

## ------------------------------
## Roll up to customer level####
## ------------------------------

# Make version of cc spending without charge cards
combined_cc_file_nocharge <-
  combined_cc_file %>%
  ungroup() %>%
  filter(act_tp_cd != "CH") %>%
  group_by(bcs_seqnum, ver_dt) %>%
  summarise(tot_cc_exp_nocharge = sum(cc_exp, na.rm = TRUE))

# Include charge cards
combined_cc_file <-
  combined_cc_file %>%
  ungroup() %>%
  group_by(bcs_seqnum, ver_dt) %>%
  summarise(
    tot_bal = sum(th_cur_bal_am, na.rm = TRUE),
    tot_bal_change = sum(balance_change, na.rm = TRUE),
    tot_cc_exp = sum(cc_exp, na.rm = TRUE),
    tot_delin_ct = sum(delinquent_ind, na.rm = TRUE)
  )

combined_cc_file <- left_join(combined_cc_file, combined_cc_file_nocharge, by = c("bcs_seqnum", "ver_dt"))



system.time(saveRDS(combined_cc_file, file = paste0(working_path, "cc_exp_file.rds")))

test_that("Average credit card spending is", {
  expect_equal(
    combined_cc_file %>%
      ungroup() %>%
      select(tot_cc_exp) %>%
      summarise(mean(tot_cc_exp)) %>%
      as.numeric() %>%
      round(.),
    452
  )
})

rm(combined_cc_file)
rm(drop_list)
rm(na_obs_by_servicer)
rm(zero_obs_by_servicer)
rm(drop_list_char)
rm(both_by_servicer)
rm(obs_by_servicer)
rm(combined_cc_file_nocharge)
