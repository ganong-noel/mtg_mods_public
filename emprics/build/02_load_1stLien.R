# This file reads in the raw .csv files from the HAMP Modification public file and creates an Rdata set

# Read in data
mods1LienAll <-
  read.csv(
    file = paste0(data_path, "HAMP_raw_files/HMP_Public_User_Mod_Data_All_20150317.csv"),
    head = TRUE, sep = ","
  )


test_that("Public mods1Lie file unchanged",
          expect_equivalent(mods1LienAll %>%
                              as.data.frame() %>%
                              transmute(one = 1,
                                        fncl_ast_id,
                                        mha_ln_id,
                                        brwr_excn_dt) %>%
                              summarise_each(funs(sum(as.numeric(.), na.rm = TRUE),
                                                  mean(is.na(.)))) %>%
                              as.vector(),
                            c(22764376324597, 5.944831e+16, 1.673374e+14, 1844772444,
                              0,              0,              0 ,                0),
                            tolerance = 1e-5))


# Keep variables we care about
mods1LienAll <-
  mods1LienAll %>%
  select(
    fncl_ast_id,
    mha_ln_id,
    brwr_excn_dt,
    brwr_mthy_dpmt_xcldg_piti_amt,
    brwr_bmdfc_mthy_hsng_exp_amt,
    last_rptd_mthy_acvy_dt,
    ln_mdfc_eff_dt,
    ln_smss_stat_cd,
    ln_st_cd,
    ln_mdfc_mode_cd,
    ln_t1_not_ofrd_rsn_cd,
    ln_trl_mdfc_dnal_rsn_cd,
    ln_orig_note_dt,
    perm_mdfc_cncln_rsn_cd,

    brwr_bir_dt,
    brwr_sex_typ_cd,
    brwr_mthy_grs_incm_amt,
    brwr_amdfc_mthy_hsng_exp_amt,
    ln_hshp_rsn_cd,
    ln_upb_frgv_amt,
    ivsr_grp,
    ln_acvy_actn_cd,
    ln_acvy_actn_dt,
    ln_lpi_dt,
    last_rptd_mthy_acvy_dt,
    ln_fst_lien_ind,

    prop_stdz_st_cd,
    prop_rgn_cd,
    prop_geoc_cnsus_msa_cd,
    prop_typ_cd,
    prop_unt_cnt,
    prop_usg_typ_cd,

    ln_aft_mdfc_int_rt,
    ln_aft_mdfc_pi_pmt_amt,
    ln_aft_mdfc_prdc_lbl_typ_cd,
    ln_aft_mdfc_rmng_term,
    ln_aft_mdfc_lpi_dt,
    ln_aft_mdfc_upb_amt,
    ln_aft_mdfc_amrt_term,

    ln_bef_mdfc_int_rt,
    ln_bef_mdfc_pi_pmt_amt,
    ln_bef_mdfc_prdc_lbl_typ_cd,
    ln_bef_mdfc_rmng_term,
    ln_bef_mdfc_upb_amt,
    ln_bef_mdfc_amrt_term,

    ln_npv_calc_dt,
    ln_bef_mdfc_npv_mdl_rslt_amt,
    ln_aft_mdfc_npv_mdl_rslt_amt,

    ln_bmdfc_pral_npv_mdl_rslt_amt,
    ln_amdfc_pral_npv_mdl_rslt_amt,
    ln_pral_wflrstrn_typ_cd,
    ln_prin_frbrn_amt,
    ln_prin_rdcn_altv_amt,
    ln_prin_rdcn_altv_cd,
    ln_prin_remd_amt,
    ln_mdfc_int_rt_lock_dt,
    ln_aft_mdfc_max_int_rt,

    ln_bef_mdfc_escr_pmt_amt,
    ln_aft_mdfc_escr_pmt_amt,

    ln_fst_trl_pmt_pstd_dt,
    ln_bef_mdfc_frnt_rto_pct,
    trial_mdfc_fout_rsn_nme
  )

mods1LienAll <-
  mods1LienAll %>%
  mutate(
    brwr_bir_dtNum = as.factor(brwr_bir_dt),
    ivsr_grpNum = as.factor(ivsr_grp),
    ln_orig_note_dtNum = as.factor(ln_orig_note_dt),
    brwr_excn_dtN = ymd(as.character(brwr_excn_dt)),
    last_rptd_mthy_acvy_dtN = ymd(as.character(last_rptd_mthy_acvy_dt)),
    ln_mdfc_eff_dtN = ymd(as.character(ln_mdfc_eff_dt)),
    ln_acvy_actn_dtN = ymd(as.character(ln_acvy_actn_dt)),
    ln_lpi_dtN = ymd(as.character(ln_lpi_dt)),
    ln_aft_mdfc_lpi_dtN = ymd(as.character(ln_aft_mdfc_lpi_dt)),
    ln_npv_calc_dtN = ymd(as.character(ln_npv_calc_dt)),
    ln_mdfc_int_rt_lock_dtN = ymd(as.character(ln_mdfc_int_rt_lock_dt)),
    ln_fst_trl_pmt_pstd_dtN = ymd(as.character(ln_fst_trl_pmt_pstd_dt))
  ) %>%
  select(
    -(brwr_excn_dt),
    -(last_rptd_mthy_acvy_dt),
    -(ln_mdfc_eff_dt),
    -(ln_acvy_actn_dt),
    -(ln_lpi_dt),
    -(last_rptd_mthy_acvy_dt),
    -(ln_aft_mdfc_lpi_dt),
    -(ln_npv_calc_dt),
    -(ln_mdfc_int_rt_lock_dt),
    -(ln_fst_trl_pmt_pstd_dt)
  )


## Assign labels ####
mods1LienAll$ln_t1_not_ofrd_rsn_cd <- factor(mods1LienAll$ln_t1_not_ofrd_rsn_cd,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29),
  labels = c(
    "Ineligible Mortgage", "Current DTI Less than 31%",
    "Property Not Owner Occupied",
    "Other Ineligible Property - Property Condemned, Property > 4 units",
    "Investor Guarantor Not Participating",
    "Court/Public Official Declined",
    "Negative NPV",
    "Offer Not Accepted by Borrower / Request Withdrawn",
    "Default Not Imminent-Default Status Not Eligible",
    "Property and/or Borrower Exceeds Allowable Number of HAMP Modifications",
    "Loan Paid Off",
    "Excessive Forbearance",
    "Request Incomplete",
    "Trial Plan Default",
    "Data Correction",
    "Payor Request",
    "Compliance Request",
    "Submission Error Correction (Incorrect transaction type)",
    "Unemployment Forbearance Plan ",
    "Federally Declared Disaster",
    "Application Discrepancy",
    "Dodd Frank Certification Non-Compliance",
    "Ineligible Borrower",
    "Ineligible Rental Property",
    "Insufficient Monthly Payment Reduction",
    "Post-Mod DTI Outside Acceptable Range",
    "No Change in Circumstance"
  )
)

mods1LienAll$ln_trl_mdfc_dnal_rsn_cd <- factor(mods1LienAll$ln_trl_mdfc_dnal_rsn_cd,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29),
  labels = c(
    "Ineligible Mortgage", "Current DTI Less than 31%",
    "Property Not Owner Occupied",
    "Other Ineligible Property - Property Condemned, Property > 4 units",
    "Investor Guarantor Not Participating",
    "Court/Public Official Declined",
    "Negative NPV",
    "Offer Not Accepted by Borrower / Request Withdrawn",
    "Default Not Imminent-Default Status Not Eligible",
    "Property and/or Borrower Exceeds Allowable Number of HAMP Modifications",
    "Loan Paid Off",
    "Excessive Forbearance",
    "Request Incomplete",
    "Trial Plan Default",
    "Data Correction",
    "Payor Request",
    "Compliance Request",
    "Submission Error Correction (Incorrect transaction type)",
    "Unemployment Forbearance Plan ",
    "Federally Declared Disaster",
    "Application Discrepancy",
    "Dodd Frank Certification Non-Compliance",
    "Ineligible Borrower",
    "Ineligible Rental Property",
    "Insufficient Monthly Payment Reduction",
    "Post-Mod DTI Outside Acceptable Range",
    "No Change in Circumstance"
  )
)

mods1LienAll$perm_mdfc_cncln_rsn_cd <- factor(mods1LienAll$perm_mdfc_cncln_rsn_cd,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 25, 26, 27, 28, 29),
  labels = c(
    "Ineligible Mortgage", "Current DTI Less than 31%",
    "Property Not Owner Occupied",
    "Other Ineligible Property - Property Condemned, Property > 4 units",
    "Investor Guarantor Not Participating",
    "Court/Public Official Declined",
    "Negative NPV",
    "Offer Not Accepted by Borrower / Request Withdrawn",
    "Default Not Imminent-Default Status Not Eligible",
    "Property and/or Borrower Exceeds Allowable Number of HAMP Modifications",
    "Loan Paid Off",
    "Excessive Forbearance",
    "Request Incomplete",
    "Trial Plan Default",
    "Data Correction",
    "Payor Request",
    "Compliance Request",
    "Submission Error Correction (Incorrect transaction type)",
    "Unemployment Forbearance Plan ",
    "Federally Declared Disaster",
    "Application Discrepancy",
    "Dodd Frank Certification Non-Compliance",
    "Ineligible Borrower",
    "Ineligible Rental Property",
    "Insufficient Monthly Payment Reduction",
    "Post-Mod DTI Outside Acceptable Range",
    "No Change in Circumstance"
  )
)

mods1LienAll$ln_hshp_rsn_cd <- factor(mods1LienAll$ln_hshp_rsn_cd,
  levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25),
  labels = c(
    "death of borrower",
    "Illness of principal borrower",
    "Illness of borrower family member",
    "Death of borrower family member",
    "Martial difficulties",
    "Curtailment of income",
    "Excessive obligation",
    "Abandonment of property",
    "Distant employment transfer",
    "Property problem",
    "Inability to sell property",
    "Inability to rent property",
    "Military service",
    "Other",
    "Unemployment",
    "Business failure",
    "Casualty Loss",
    "Energy environment costs",
    "Servicing problems",
    "Payment adjustment",
    "Payment dispute",
    "Transfer of ownership pending",
    "Fraud",
    "Unable to contact borrower",
    "Incarceration"
  )
)

saveRDS(mods1LienAll, file = paste0(working_path, "mods1LienAll_R.rds"))
print(paste0(working_path, "mods1LienAll_R.rds"))
list.files(paste0(working_path, "mods1LienAll_R.rds"))
rm(mods1LienAll)
