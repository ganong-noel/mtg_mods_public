mods1LienAll <- readRDS(paste0(working_path, "mods1LienAll_R.rds"))

mods1LienAll <-
  mods1LienAll %>%
  mutate(
    ln_smss_stat_cd = factor(mods1LienAll$ln_smss_stat_cd,
      levels = c(1, 3, 5, 6, 7),
      labels = c("Trial", "Official", "Trial Cancel", "Official Corrected", "Official Cancel")
    ),
    ln_st_cd = factor(mods1LienAll$ln_st_cd,
      levels = c(1, 2, 4, 5, 7, 10),
      labels = c("Active Trial", "Active Payment", "Trial Cancelled", "Disqualified", "Paid Off", "Rejected")
    ),
    ln_bef_mdfc_prdc_lbl_typ_cd = factor(mods1LienAll$ln_bef_mdfc_prdc_lbl_typ_cd,
      levels = c(1, 2, 3),
      labels = c("ARM", "Fixed Rate", "Step Rate")
    ),
    brwr_sex_typ_cd = factor(mods1LienAll$brwr_sex_typ_cd,
      levels = c(1, 2, 3, 4),
      labels = c("Male", "Female", "Unknown", "Not Applicable")
    ),
    ln_prin_rdcn_altv_cd = factor(mods1LienAll$ln_prin_rdcn_altv_cd,
      levels = c(1, 2, 3, 4),
      labels = c("No PRA", "PRA -- incented", "PRA -- not incented", "No Value")
    )
  )

# Merge NPV file
npvAll <- readRDS(paste0(working_path, "npv_R.rds"))
mrgdHamp <- merge(mods1LienAll, npvAll, by = "fncl_ast_id", all = TRUE)
rm(npvAll)
rm(mods1LienAll)

proportion_high_inc <- sum(mrgdHamp$brwr_mthy_grs_incm_amt.x > 8500, na.rm = TRUE) /
  sum(!is.na(mrgdHamp$brwr_mthy_grs_incm_amt.x), na.rm = TRUE)

testthat::expect_equal(proportion_high_inc, 0.05258, tolerance = 0.0001)

# Top code and construct program participation
mrgdHamp <-
  mrgdHamp %>% mutate(
    ln_mtm_ltv_pct = ifelse(ln_mtm_ltv_pct > 200 & !is.na(ln_mtm_ltv_pct), 200, ln_mtm_ltv_pct),
    brwr_mthy_grs_incm_amt.x = ifelse(brwr_mthy_grs_incm_amt.x > 8500 & !is.na(brwr_mthy_grs_incm_amt.x), 8500, brwr_mthy_grs_incm_amt.x),
    ageDum = as.factor(brwr_bir_dtNum),
    mtmBin = 10 * floor(ln_mtm_ltv_pct / 10),
    dnpv_HampFile = ln_aft_mdfc_npv_mdl_rslt_amt.x - ln_bef_mdfc_npv_mdl_rslt_amt.x,
    dnpvPra_HampFile = ln_amdfc_pral_npv_mdl_rslt_amt.x - ln_bmdfc_pral_npv_mdl_rslt_amt.x,
    ratioBefore_HampFile = ln_bef_mdfc_pi_pmt_amt.x / brwr_mthy_grs_incm_amt.x,
    permMod = ln_smss_stat_cd == "Official" | ln_smss_stat_cd == "Official Corrected",
    ivsr_HampFile = ifelse(ivsr_grpNum == "GSE", 1, ifelse(ivsr_grpNum == "Non-GSE", 2, 3)),
    pra = ifelse(ln_prin_rdcn_altv_cd == "PRA -- incented" | ln_prin_rdcn_altv_cd == "PRA -- not incented", TRUE,
      ifelse(ln_prin_rdcn_altv_cd == "No PRA", FALSE, NA)
    ),
    fracForgivePos = ln_upb_frgv_amt.x > 0 & !is.na(ln_upb_frgv_amt.x)
  )

# Construct delinquency rates
mrgdHamp <-
  mrgdHamp %>%
  mutate(
    mdfcCohort = monnb(as.Date(ln_mdfc_eff_dtN)),
    disqualDate = as.POSIXct(ifelse(ln_st_cd == "Disqualified", last_rptd_mthy_acvy_dtN, NA), origin = "1970-01-01", tz = "GMT"),
    mosToDisqual = mondf(as.Date(ln_mdfc_eff_dtN), as.Date(disqualDate)),
    delinDate = as.POSIXct(ifelse(ln_st_cd == "Disqualified", ln_lpi_dtN, NA), origin = "1970-01-01", tz = "GMT"),
    mosToDelin = mondf(as.Date(ln_mdfc_eff_dtN), as.Date(delinDate)),
    disqualNxtYr = ifelse(is.na(mosToDisqual) | mosToDisqual > 12, FALSE, TRUE),
    disqual2 = ifelse(is.na(mosToDisqual) | mosToDisqual > 24, FALSE, TRUE),
    delinNxtYr = ifelse(is.na(mosToDelin) | mosToDelin > 12, FALSE, TRUE),
    delin2 = ifelse(is.na(mosToDelin) | mosToDelin > 24, FALSE, TRUE)
  )

# Construct RD Variables
mrgdHamp <-
  mrgdHamp %>%
  mutate(
    dnpvRelPct_npvFile_stata2_paper = ifelse(
      !is.na(dnpvRelative_npvFile) & !is.na(ln_aft_mdfc_npv_mdl_rslt_amt.x) & ln_aft_mdfc_npv_mdl_rslt_amt.x != 0,
      100 * dnpvRelative_npvFile / ln_aft_mdfc_npv_mdl_rslt_amt.x,
      ifelse(!is.na(dnpvRelative_npvFile) & !is.na(ln_aft_mdfc_npv_mdl_rslt_amt.y) & ln_aft_mdfc_npv_mdl_rslt_amt.y != 0,
        100 * dnpvRelative_npvFile / ln_aft_mdfc_npv_mdl_rslt_amt.y, NA
      )
    ),
    pos = dnpvRelPct_npvFile_stata2_paper >= 0,
    x = dnpvRelPct_npvFile_stata2_paper,
    xpos = dnpvRelPct_npvFile_stata2_paper * pos,
    x2 = dnpvRelPct_npvFile_stata2_paper^2,
    xpos2 = x2 * pos
  )

saveRDS(mrgdHamp, file = paste0(working_path, "mrgdHamp_R.rds"))
rm(mrgdHamp)
