## ----------------------------------------#
# Load Files and create analysis sample ####
## ----------------------------------------
df_cc_event_base <- readRDS(paste0(working_path, "df_cc_event_base.rds"))
dNPV_out <- readRDS(paste0(working_path, "dNPV_out_es_base.rds"))
mrgdHamp <- readRDS(paste0(working_path, "mrgdHamp_R.rds"))
new_auto_loans <- readRDS(paste0(working_path, "new_auto_loans.rds"))


# Merge
df <- inner_join(df_cc_event_base, dNPV_out, by = "fncl_ast_id")

rm(dNPV_out)
rm(df_cc_event_base)


mrgdHamp <-
  mrgdHamp %>%
  mutate(
    ln_post_mod_ltv_pct = ifelse(
      prop_valu_as_is_val_amt > 0,
      100 * (ln_aft_mdfc_upb_amt + ln_prin_frbrn_amt) / prop_valu_as_is_val_amt,
      NA
    ),
    ln_post_mod_ltv_pct = ifelse(ln_post_mod_ltv_pct > 200, 200, ln_post_mod_ltv_pct)
  )
mrgdHamp_toadd <- mrgdHamp %>% select(fncl_ast_id, ln_post_mod_ltv_pct)
system.time(df <- left_join(df, mrgdHamp_toadd, by = "fncl_ast_id"))
df <-
  as.data.frame(df) %>%
  mutate(ln_mtm_ltv_pct = ifelse(ln_mtm_ltv_pct < 2, ln_mtm_ltv_pct * 100, ln_mtm_ltv_pct))
# Create dNPV in positive (so bigger number means bigger reduction)
df <- df %>% mutate(dNPV_cashflow_pos_val = dNPV_cashflow * -1)

rm(mrgdHamp)
rm(mrgdHamp_toadd)

df <-
  df %>%
  select(
    -mod_mnth_tu, -ln_bef_mdfc_int_rt_hamp, -ivsr_grpNum,
    -ln_fst_trl_pmt_pstd_dtN, -ln_fst_trl_pmt_pstd_dt_monnb,
    -mean_spend_window, -NPV_bef_mod_cashflow, -NPV_aft_mod_cashflow
  ) %>%
  ungroup()

##
# Add auto consumption variables and winsorize
##

var_list <- c("auto_pur_ind", "auto_pur_am")
new_auto_loans %>%
  ungroup() %>%
  select(one_of(var_list)) %>%
  summary()
for (var in var_list) {
  new_auto_loans[[var]] <-
    winsor(new_auto_loans[[var]],
      fractionLow = 0, fractionHigh = 0.99,
      verbose = TRUE, varname = var, pos_only = TRUE
    )
}
new_auto_loans %>%
  ungroup() %>%
  select(one_of(var_list)) %>%
  summary()
new_auto_loans <-
  new_auto_loans %>%
  select(bcs_seqnum, ver_dt_monnb, auto_pur_ind, auto_pur_am)

# Join household ID
new_auto_loans <- left_join(new_auto_loans,
                            readRDS(paste0(working_path, "tu_match_file_hh_id.rds")),
                            by = "bcs_seqnum")

# Collapse by household ID
new_auto_loans <-
  new_auto_loans %>%
  ungroup() %>%
  group_by(hh_id, ver_dt_monnb) %>%
  summarise(
    auto_pur_ind = sum(auto_pur_ind, na.rm = TRUE),
    auto_pur_am = sum(auto_pur_am, na.rm = TRUE)
  )

# Left join by hh_id
df <- left_join(df, new_auto_loans, by = c("hh_id", "ver_dt_monnb"))

# Fill in NA as zero (months with no auto purchase)
df <-
  df %>%
  mutate(
    auto_pur_ind = ifelse(is.na(auto_pur_ind), 0, auto_pur_ind),
    auto_pur_am = ifelse(is.na(auto_pur_am), 0, auto_pur_am)
  )

system.time(saveRDS(df, file = paste0(working_path, "df_analysis_event_study_with_auto.rds")))
rm(df)
rm(tu_match_file_hh_id)
