## ---------------------------------------------------#
## Load Base File and Filter to Matching Threshold ####
## ---------------------------------------------------#

df_base <-
  readRDS(paste0(working_path, "matched_hamp_yearblock_unc_sub.rds")) %>%
  select(
    fncl_ast_id, bcs_seqnum, hh_id, dist, act_id,
    mod_mnth_hamp, mod_mnth_tu, msa_state, ln_orig_note_dt,
    bir_decade_tu
  )
## Filter 1: Match threshold
match_dist_cutoff <- 0.2

df_base <- df_base %>% filter(dist < match_dist_cutoff)

##
## Load and add additional hamp variables
##
mrgdHamp <-
  readRDS(paste0(working_path, "mrgdHamp_R.rds")) %>%
  mutate(
    fracForgivePos = ifelse(!is.na(ln_upb_frgv_amt.x), ln_upb_frgv_amt.x > 0,
                            ifelse(!is.na(ln_upb_frgv_amt.y),
                                   ln_upb_frgv_amt.y > 0, FALSE)),
    praSampCore = ivsr_grpNum == "Non-GSE" & abs(x) > 0.005,
    praSamp = abs(x) < 0.9 & praSampCore,
    anyDelin = ln_st_cd == "Disqualified",
    pra = ifelse(is.na(pra), FALSE, pra),
    forgive_amount = ifelse(!is.na(ln_upb_frgv_amt.x), ln_upb_frgv_amt.x,
                            ifelse(!is.na(ln_upb_frgv_amt.y), ln_upb_frgv_amt.y, 0)),
    pi_pay_red_dol = ln_bef_mdfc_pi_pmt_amt.x - ln_aft_mdfc_pi_pmt_amt,
    pi_pay_red_pct = pi_pay_red_dol / ln_bef_mdfc_pi_pmt_amt.x,
    ln_bef_mdfc_int_rt_hamp = ifelse(!is.na(ln_bef_mdfc_int_rt.x), ln_bef_mdfc_int_rt.x / 100, ln_bef_mdfc_int_rt.y / 100),
    pre_mod_dti = ln_bef_mdfc_pi_pmt_amt.x / brwr_mthy_grs_incm_amt.x,
    pre_mod_dti_full = brwr_bmdfc_mthy_hsng_exp_amt / brwr_mthy_grs_incm_amt.x,
    post_mod_dti = ln_aft_mdfc_pi_pmt_amt / brwr_mthy_grs_incm_amt.x,
    post_mod_dti_full = brwr_amdfc_mthy_hsng_exp_amt / brwr_mthy_grs_incm_amt.x
  )

# Chose variables to add, and add them on
mrgdHamp_toadd <-
  mrgdHamp %>%
  select(
    fncl_ast_id, praSampCore, praSamp, anyDelin, x, brwr_curr_crdt_scr_val,
    brwr_mthy_grs_incm_amt.x, pos, x, xpos, x2, xpos2, fracForgivePos,
    ln_bef_mdfc_prdc_lbl_typ_cd, prop_typ_cd, ln_bef_mdfc_int_rt_hamp,
    ln_mtm_ltv_pct, pi_pay_red_dol, pi_pay_red_pct, ln_bef_mdfc_upb_amt.x,
    prop_valu_as_is_val_amt, pre_mod_dti, pre_mod_dti_full, post_mod_dti,
    post_mod_dti_full, ln_bef_mdfc_pi_pmt_amt.x, ln_orgnn_ltv_pct,
    brwr_mthy_dpmt_xcldg_piti_amt, forgive_amount, pra, ivsr_grpNum,
    ln_pral_upb_frgv_amt, ln_fst_trl_pmt_pstd_dtN
  )

system.time(df_base <- left_join(df_base, mrgdHamp_toadd, by = "fncl_ast_id"))
df_base <- df_base %>% mutate(dum = pos)

## Filter 2: Subset just to Evaluated for PRA and non-GSE
df_base <- df_base %>% filter(!is.na(x) & ivsr_grpNum == "Non-GSE")


## ----------------------------------------#
## Add Credit Card Consumption Estimate ####
## ----------------------------------------#
combined_cc_file <-
  readRDS(paste0(working_path, "cc_exp_file.rds")) %>%
  select(bcs_seqnum, ver_dt, tot_cc_exp, tot_bal, tot_cc_exp_nocharge, tot_delin_ct)

##
## Winsorize All outcome variables
##

var_list <- c("tot_cc_exp", "tot_bal", "tot_cc_exp_nocharge")
for (var in var_list) {
  combined_cc_file[[var]] <-
    winsor(combined_cc_file[[var]],
      fractionLow = 0, fractionHigh = 0.99,
      verbose = TRUE, varname = var, pos_only = TRUE
    )
}

test_that("Average credit card spending is", {
  expect_equal(
    combined_cc_file %>%
      ungroup() %>%
      select(tot_cc_exp) %>%
      summarise(mean(tot_cc_exp)) %>%
      as.numeric() %>%
      round(.), 417
  )
})


# Join household ID
combined_cc_file <- readRDS(paste0(working_path, "tu_match_file_hh_id.rds")) %>%
  right_join(combined_cc_file, by = "bcs_seqnum")

# Collapse by household ID
combined_cc_file <-
  combined_cc_file %>%
  ungroup() %>%
  group_by(hh_id, ver_dt) %>%
  summarise(
    tot_cc_exp = sum(tot_cc_exp, na.rm = TRUE),
    tot_bal = sum(tot_bal, na.rm = TRUE),
    tot_cc_exp_nocharge = sum(tot_cc_exp_nocharge, na.rm = TRUE),
    tot_delin_ct = sum(tot_delin_ct, na.rm = TRUE)
  )

system.time(df_cc_event_base <- inner_join(combined_cc_file, df_base, by = c("hh_id" = "hh_id")))

df_cc_event_base <-
  df_cc_event_base %>%
  mutate(
    ver_dt_N = ymd(as.character(ver_dt * 100 + 1)),
    ver_dt_monnb = monnb(as.Date(ver_dt_N)),
    ln_fst_trl_pmt_pstd_dt_monnb = monnb(as.Date(ln_fst_trl_pmt_pstd_dtN)),
    mos_since_mod_old = ver_dt_monnb - mod_mnth_hamp + 3,
    mos_since_mod = ver_dt_monnb - ln_fst_trl_pmt_pstd_dt_monnb,
    mod_mnth_diff = mos_since_mod - mos_since_mod_old,
    mod_mnth_diff_abs = abs(mos_since_mod - mos_since_mod_old),
    mod_mnth_diff_act = ln_fst_trl_pmt_pstd_dt_monnb - (mod_mnth_hamp - 3)
  ) %>%
  select(-mod_mnth_diff, -mod_mnth_diff_abs, -mod_mnth_diff_act)

## Filter 3: Balanced panel 12 months pre and post
df_cc_event_base <-
  df_cc_event_base %>%
  ungroup() %>%
  group_by(hh_id, fncl_ast_id) %>%
  mutate(
    min_period = min(mos_since_mod),
    max_period = max(mos_since_mod)
  ) %>%
  filter(min_period <= (-12) & max_period >= 12)

## Filter 5: Positive Spending during this period (taking out the zeros)
mean_spend_in_window <-
  df_cc_event_base %>%
  filter(abs(mos_since_mod) <= 12) %>%
  group_by(hh_id, fncl_ast_id) %>%
  summarise(mean_spend_window = mean(tot_cc_exp)) %>%
  select(hh_id, fncl_ast_id, mean_spend_window)
df_cc_event_base <- left_join(df_cc_event_base, mean_spend_in_window, by = c("hh_id", "fncl_ast_id"))
df_cc_event_base <- df_cc_event_base %>% filter(mean_spend_window > 0)
n_distinct(df_cc_event_base$fncl_ast_id)

## ---------------------#
## Export Data Files ####
## ---------------------#

# File for event study
system.time(saveRDS(df_cc_event_base,
                    file = paste0(working_path, "df_cc_event_base.rds")))

# Financial_ast_id File for calcModNpv
ids_event_study_base <- df_cc_event_base %>%
  ungroup() %>%
  select(fncl_ast_id) %>%
  distinct()

system.time(saveRDS(ids_event_study_base, file = paste0(working_path, "ids_event_study_base.rds")))

## -------------------#
## Clean workspace ####
## -------------------#
rm(combined_cc_file)
rm(df_base)
rm(df_cc_event_base)
rm(ids_event_study_base)
rm(tu_match_file_hh_id)
rm(mean_spend_in_window)
rm(mrgdHamp)
rm(mrgdHamp_toadd)
rm(match_dist_cutoff)
