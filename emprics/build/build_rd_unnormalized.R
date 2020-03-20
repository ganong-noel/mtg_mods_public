cutoff <- 0
gran <- 40
n_cut <- 15

source("func/binscatter_rd.R")
source("func/loc_lin.R")


if (matched == "Yes") {

 matched_hamp <- readRDS(paste0(working_path, "matched_hamp_yearblock_unc_sub.rds"))

  df_base <-
    matched_hamp %>%
    select(
      fncl_ast_id, bcs_seqnum, dist, act_id,
      mod_mnth_hamp, mod_mnth_tu, any_disqual_tu
    )


  tu_match_file <- readRDS(paste0(working_path, "tu_match_file_clean.rds"))

    df_base <-
    df_base %>%
    left_join(tu_match_file %>%
                select(act_id, bcs_seqnum, rpt_sub_cd, th_mop, any_disqual_tu),
              by = c("act_id", "bcs_seqnum")
    )
  rm(tu_match_file)

  dNPV_out <- readRDS(paste0(working_path, "dNPV_out_nonmissing_dnpv.rds"))
  df_base <- df_base %>% left_join(dNPV_out, by = c("fncl_ast_id"))
  rm(dNPV_out)
} else if (matched == "No") {
  matched_hamp <- readRDS(paste0(working_path, "matched_hamp_yearblock_unc_sub_um.rds"))

  df_base <- matched_hamp %>% select(fncl_ast_id, mod_mnth_hamp)
}
rm(matched_hamp)

##
# Load and add additional hamp variables
##
mrgdHamp <- readRDS(paste0(working_path, "mrgdHamp_R.rds"))

mrgdHamp <-
  mrgdHamp %>%
  mutate(
    takeup_status = ifelse(
      (ln_smss_stat_cd == "Official" | ln_smss_stat_cd == "Official Corrected") & ln_st_cd != "Disqualified", "PermMod Active", ifelse(
        (ln_smss_stat_cd == "Official" | ln_smss_stat_cd == "Official Corrected") & ln_st_cd == "Disqualified", "PermMod Disqual", ifelse(
          ln_st_cd != "Rejected" & ln_smss_stat_cd == "Trial" & ln_st_cd == "Active Trial", "Trial Active", ifelse(
            (ln_smss_stat_cd == "Trial" | ln_smss_stat_cd == "Trial Cancel") & ln_st_cd == "Disqualified", "Trial Disqual", ifelse(
              ln_st_cd == "Trial Cancelled" & trial_mdfc_fout_rsn_nme == "Offer Not Accepted by Borrower / Request Withdrawn", "Offer Rejected", ifelse(
                ln_st_cd == "Rejected", "Rejected", ifelse(
                  ln_st_cd == "Trial Cancelled" & trial_mdfc_fout_rsn_nme != "Offer Not Accepted by Borrower / Request Withdrawn", "Rejected", NA
                )
              )
            )
          )
        )
      )
    ),
    offered_mod = ifelse(takeup_status %in% c("PermMod Active", "PermMod Disqual", "Trial Active", "Trial Disqual", "Offer Rejected"), TRUE, FALSE),
    takeup_mod = ifelse(takeup_status %in% c("PermMod Active", "PermMod Disqual", "Trial Active", "Trial Disqual"), TRUE, FALSE),
    fracForgivePos = ifelse(!is.na(ln_upb_frgv_amt.x), ln_upb_frgv_amt.x > 0, ifelse(
      !is.na(ln_upb_frgv_amt.y), ln_upb_frgv_amt.y > 0, FALSE
    )),
    post_mod_dti_full = ifelse(brwr_mthy_grs_incm_amt.x > 0,
                               100 * brwr_amdfc_mthy_hsng_exp_amt / brwr_mthy_grs_incm_amt.x, NA
    ),
    praSampCore = ivsr_grpNum == "Non-GSE" & abs(x) > 0.005,
    praSamp = abs(x) < 0.9 & praSampCore,
    anyDelin = ln_st_cd == "Disqualified",
    anyDelin_incl_tmp = ln_st_cd == "Disqualified" | permMod == FALSE,
    ln_aft_mdfc_npv_mdl_rslt_amt = ifelse(!is.na(ln_aft_mdfc_npv_mdl_rslt_amt.x),
                                          ln_aft_mdfc_npv_mdl_rslt_amt.x, ln_aft_mdfc_npv_mdl_rslt_amt.y
    ),
    ln_bef_mdfc_npv_mdl_rslt_amt = ifelse(!is.na(ln_bef_mdfc_npv_mdl_rslt_amt.x),
                                          ln_bef_mdfc_npv_mdl_rslt_amt.x, ln_bef_mdfc_npv_mdl_rslt_amt.y
    ),
    ln_amdfc_pral_npv_mdl_rslt_amt = ifelse(!is.na(ln_amdfc_pral_npv_mdl_rslt_amt.x),
                                            ln_amdfc_pral_npv_mdl_rslt_amt.x, ln_amdfc_pral_npv_mdl_rslt_amt.y
    ),
    ln_bmdfc_pral_npv_mdl_rslt_amt = ifelse(!is.na(ln_bmdfc_pral_npv_mdl_rslt_amt.x),
                                            ln_bmdfc_pral_npv_mdl_rslt_amt.x, ln_bmdfc_pral_npv_mdl_rslt_amt.y
    )
  ) %>%
  filter(#abs(x) < 6,
    ivsr_grpNum == "Non-GSE")

mrgdHamp_toadd <-
  mrgdHamp %>%
  select(
    fncl_ast_id, praSampCore, praSamp, anyDelin, anyDelin_incl_tmp, delin2,
    brwr_curr_crdt_scr_val, brwr_mthy_grs_incm_amt.x, prop_valu_as_is_val_amt,
    ln_pst_due_prd_cnt, pos, x, xpos, x2, xpos2, fracForgivePos,
    ln_upb_frgv_amt.x, ln_aft_mdfc_upb_amt,
    ln_bef_mdfc_pi_pmt_amt.x, ln_aft_mdfc_pi_pmt_amt,
    ln_aft_mdfc_npv_mdl_rslt_amt, ln_bef_mdfc_npv_mdl_rslt_amt,
    ln_amdfc_pral_npv_mdl_rslt_amt, ln_bmdfc_pral_npv_mdl_rslt_amt,
    ln_npv_calc_dtN,
    ln_prin_rdcn_altv_amt, ln_prin_rdcn_altv_cd, ln_prin_frbrn_amt,
    ln_mtm_ltv_pct,
    ln_bef_mdfc_frnt_rto_pct, post_mod_dti_full,
    mdfcCohort, permMod,
    takeup_status, offered_mod, takeup_mod,
    ln_bef_mdfc_upb_amt.x, ln_bef_mdfc_upb_amt.y,
    ln_orgnn_ltv_pct, brwr_mthy_dpmt_xcldg_piti_amt, brwr_sex_typ_cd,
    brwr_bir_dtNum, ln_orig_note_dtNum,
    ln_orgnn_amrt_term, ln_orgnn_int_rt, ln_bef_mdfc_int_rt.x,
    ln_bef_mdfc_prdc_lbl_typ_cd, ln_bef_mdfc_rmng_term,
    brwr_tot_mthy_oblg_amt
  )

if (matched == "No") {
  suffix <- "_um"
} else if (matched == "Yes") {
  suffix <- ""
}

df_base_um_long <- mrgdHamp_toadd
saveRDS(df_base_um_long, file = paste0(working_path, "rd_samp_unmatched_long", suffix, ".rds"))

df_base_um <-
  mrgdHamp_toadd %>%
  filter(praSampCore == TRUE & #abs(x) < 4 &
           mdfcCohort >= 1331)

saveRDS(df_base_um, file = paste0(working_path, "rd_samp_unmatched", suffix, ".rds"))

df_base <- left_join(df_base, mrgdHamp_toadd, by = "fncl_ast_id")
df_base_long <- df_base # %>% filter(abs(x) < 6)
saveRDS(df_base_long, file = paste0(working_path, "rd_samp_long", suffix, ".rds"))

if (matched == "Yes") {
  bw <- 2
  samp_tit <- " Matched "

  df <-
    df_base %>%
    filter(
     # abs(x) < 4 &
        mdfcCohort >= 1331 &
        dist < 0.2 &
        rpt_sub_cd != "x42g551" &
        !(is.na(ln_mtm_ltv_pct) |
            is.na(ln_upb_frgv_amt.x) |
            is.na(prop_valu_as_is_val_amt) |
            is.na(brwr_curr_crdt_scr_val) |
            is.na(ln_bef_mdfc_frnt_rto_pct) |
            is.na(ln_pst_due_prd_cnt) |
            is.na(post_mod_dti_full)) #&
        #abs(x) < bw
    )

  df_summary <-
    df %>%
    count(bin_hist = floor(x * 10) / 10) %>%
    mutate(bin_hist = bin_hist + 0.05)
  gg_hist <- ggplot(
    df_summary,
    aes(x = bin_hist, y = n)
  ) +
    geom_bar(stat = "identity", width = 0.1) +
    fte_theme() +
    scale_colour_manual(values = cbPalette) +
    labs(
      x = "Delta NPV from Principal Reduction over Payment Reduction Mod (in %)",
      y = "Number of Observations"
    ) +
    geom_vline(xintercept = 0, color = "red", linetype = "longdash")
  ggsave(file = paste0(out_path, "rd_matched_all_obs_hist_pn_v2.png"), gg_hist, width = wd, height = ht)


  df_summary <-
    df %>%
    filter(abs(x) > 0.005) %>%
    count(bin_hist = floor(x * 10) / 10) %>%
    mutate(bin_hist = bin_hist + 0.05)
  gg_hist <- ggplot(
    df_summary,
    aes(x = bin_hist, y = n)
  ) +
    geom_bar(stat = "identity", width = 0.1) +
    fte_theme() +
    scale_colour_manual(values = cbPalette) +
    labs(
      x = "Delta NPV from Principal Reduction over Payment Reduction Mod (in %)",
      y = "Number of Observations"
    ) +
    geom_vline(xintercept = 0, color = "red", linetype = "longdash")
  ggsave(file = paste0(out_path, "rd_matched_drop_close_hist_pn_v2.png"), gg_hist, width = wd, height = ht)
}

df_base <-
  df_base %>%
  filter(praSampCore == TRUE & #abs(x) < 4 &
           mdfcCohort >= 1331)
saveRDS(df_base, file = paste0(working_path, "rd_samp", suffix, ".rds"))


rm(list = ls(pattern = "^df"))
rm(mrgdHamp, mrgdHamp_toadd)

if (matched == "Yes") {
  df_base <- readRDS(paste0(working_path, "rd_samp.rds"))
} else if (matched == "No") {
  df_base <- readRDS(paste0(working_path, "rd_samp_um.rds"))
}

df <- df_base

if (matched == "Yes") {
  match_dist_cutoff <- 0.2
  df <- df %>% filter(dist < match_dist_cutoff)
  df <- df %>% filter(rpt_sub_cd != "x42g551")
}


samp_file_name <- "Matched"
if (matched == "No") {
  samp_file_name <- "Unmatched"
}
samp_tit <- ""
