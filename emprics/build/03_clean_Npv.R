npvAll <- readRDS(paste0(working_path, "npvAll_expanded_R.rds"))

npvAll <-
  npvAll %>%
  select(
    ln_npv_test_rslt_cd,
    ln_aft_mdfc_npv_mdl_rslt_amt,
    ln_bef_mdfc_npv_mdl_rslt_amt,
    ln_pral_npv_test_rslt_cd,
    ln_amdfc_pral_npv_mdl_rslt_amt,
    ln_bmdfc_pral_npv_mdl_rslt_amt,
    ln_npv_mdl_typ_cd,
    fncl_ast_id,
    ivsr_grp,
    data_clctn_dt,
    brwr_mthy_assc_dues_fees_amt,
    brwr_mthy_hi_and_fld_amt,
    brwr_mthy_re_tx_amt,
    brwr_mthy_grs_incm_amt,
    brwr_tot_mthy_oblg_amt,
    brwr_curr_crdt_scr_val,
    ln_npv_run_dt,
    ln_ocpy_elig_typ_cd,
    ln_mtm_ltv_pct,
    ln_bef_mdfc_pi_pmt_amt,
    ln_bef_mdfc_upb_amt,
    ln_bef_mdfc_int_rt,
    prop_valu_as_is_val_amt,
    ln_orgnn_ltv_pct,
    ln_orgnn_amrt_term,
    ln_orgnn_fst_pmt_dt,
    ln_orgnn_int_rt,
    ln_rmng_term,
    ln_aft_mdfc_pral_int_rt,
    ln_aft_mdfc_pral_amrt_term,
    ln_pral_prin_frbrn_amt,
    ln_pral_upb_frgv_amt,
    ln_aft_mdfc_pral_upb_amt,
    ln_aft_mdfc_pral_pi_pmt_amt,
    ln_aft_mdfc_amrt_term,
    ln_upb_frgv_amt,
    ln_pst_due_prd_cnt
  )


npvAll <-
  npvAll %>%
  mutate(
    ln_npv_mdl_typ_cd = factor(ln_npv_mdl_typ_cd,
      levels = c(1, 2),
      labels = c("Treasury", "Servicer")
    ),
    ln_npv_test_rslt_cd = factor(ln_npv_test_rslt_cd,
      levels = c(1, 2),
      labels = c("Positive", "Negative")
    ),
    ln_pral_npv_test_rslt_cd = factor(ln_pral_npv_test_rslt_cd,
      levels = c(1, 2),
      labels = c("Positive", "Negative")
    ),
    ln_orgnn_ltv_pct = ifelse(ln_orgnn_ltv_pct > 100 & !is.na(ln_orgnn_ltv_pct), 100, ln_orgnn_ltv_pct),
    dnpv_npvFile = ln_aft_mdfc_npv_mdl_rslt_amt - ln_bef_mdfc_npv_mdl_rslt_amt,
    dnpvPra_npvFile = ln_amdfc_pral_npv_mdl_rslt_amt - ln_bmdfc_pral_npv_mdl_rslt_amt,
    dnpvRelative_npvFile = dnpvPra_npvFile - dnpv_npvFile,
    brwr_mthy_assc_dues_fees_amt = ifelse(is.na(brwr_mthy_assc_dues_fees_amt), 0, brwr_mthy_assc_dues_fees_amt),
    brwr_mthy_hi_and_fld_amt = ifelse(is.na(brwr_mthy_hi_and_fld_amt), 0, brwr_mthy_hi_and_fld_amt),
    brwr_mthy_re_tx_amt = ifelse(is.na(brwr_mthy_re_tx_amt), 0, brwr_mthy_re_tx_amt),
    ratioBefore_npvFile = (brwr_mthy_assc_dues_fees_amt + brwr_mthy_hi_and_fld_amt +
      brwr_mthy_re_tx_amt + ln_bef_mdfc_pi_pmt_amt) / brwr_mthy_grs_incm_amt,
    data_clctn_dtNum = ymd(as.character(data_clctn_dt)),
    ivsr = ifelse(ivsr_grp == "GSE", 1, ifelse(ivsr_grp == "Non-GSE", 2, 3))
  ) %>%
  select(-data_clctn_dt, -ivsr_grp)

saveRDS(npvAll, file = paste0(working_path, "npv_R.rds"))
rm(npvAll)
