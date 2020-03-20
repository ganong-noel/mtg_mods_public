df_match_graph <-
  readRDS(paste0(working_path, "df_match_graph.rds")) %>%
  filter(abs(x) < bw_window) %>%
  mutate(
    dum = x > 0,
    xpos = x * dum,
    w = ifelse(abs(x) > bw_preferred, 0, abs((bw_preferred - abs(x))) / bw_preferred)
  )

lin_model <- lm(post_match_rd_sample_indicator ~ dum + x + xpos,
  data = df_match_graph, weights = w
)
coef <- round(lin_model$coefficients[2], 3)
se <- round(summary(lin_model)$coefficients[8], 3)
caption_text <- paste0("RD Estimate: \n", coef, " (", se, ")")
gg_match_rate <-
  gg_bin(
    df = df_match_graph,
    dvs = c("post_match_rd_sample_indicator"),
    caption = caption_text,
    y_range = c(0.43, 0.5), y_size = 11,
    scales = "percent",
    xlab = lab_x, ylab = "Share of Treasury Records Matched to Credit Bureau",
    file_suffix = "_match_rate.png"
  )

# takup rate ####
praSampCore <-
  readRDS(paste0(working_path, "df_rd_takeup.rds")) %>%
  mutate(
    perm_mod_alt = ifelse(!is.na(permMod), permMod, FALSE),
    w = ifelse(abs(x) > bw_preferred, 0, abs((bw_preferred - abs(x))) / bw_preferred)
  )

df <-
  praSampCore %>%
  mutate(
    post_mod_dti_full = ifelse(brwr_mthy_grs_incm_amt.x > 0,
      100 * brwr_amdfc_mthy_hsng_exp_amt / brwr_mthy_grs_incm_amt.x, NA
    ),
    msa_state = ifelse(prop_geoc_cnsus_msa_cd > 0,
      as.character(prop_geoc_cnsus_msa_cd), as.character(prop_stdz_st_cd)
    ),
    ln_bef_mdfc_tot_pmt = ln_bef_mdfc_pi_pmt_amt.x + ln_bef_mdfc_escr_pmt_amt,
    ln_bef_mdfc_upb_amt = ifelse(ln_bef_mdfc_upb_amt.x != 0,
      ln_bef_mdfc_upb_amt.x, ln_bef_mdfc_upb_amt.y
    )
  ) %>%
  filter(
    mdfcCohort >= 1331 & mdfcCohort < 1380,
    ln_orig_note_dt < 2009,
    !is.na(msa_state),
    !is.na(ln_bef_mdfc_tot_pmt),
    !is.na(ln_bef_mdfc_upb_amt),
    ln_bef_mdfc_tot_pmt < 25000,
    ln_bef_mdfc_upb_amt > 0,
    !is.na(x),
    abs(x) > 0.005,
    abs(x) < 2,
    !(is.na(ln_mtm_ltv_pct) | is.na(ln_upb_frgv_amt.x) | is.na(prop_valu_as_is_val_amt) |
      is.na(brwr_curr_crdt_scr_val) | is.na(ln_bef_mdfc_frnt_rto_pct) |
      is.na(ln_pst_due_prd_cnt) | is.na(post_mod_dti_full))
  )

lin_model <- lm(takeup_mod ~ pos + x + xpos,
  data = df %>% filter(x < bw_preferred & offered_mod == TRUE),
  weights = w
)
coef <- round(lin_model$coefficients[2], 3)
se <- round(summary(lin_model)$coefficients[8], 3)
caption_text <- paste0("RD Estimate: \n", coef, " (", se, ")")
gg_takeup_rate_offer_to_trial <-
  gg_bin(
    df = df %>% filter(offered_mod == TRUE),
    dvs = c("takeup_mod"), caption = caption_text,
    loc = "lr", y_range = c(0.9, 1),
    xlab = lab_x, ylab = "Take-Up Rate", coef_size = 5.63,
    samp_name = "",
    file_suffix = "takeup_rate_offer_to_trial.png"
  )

rm(df, df_match_graph, praSampCore)
