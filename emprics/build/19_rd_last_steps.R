# restrict sample to obs w/non-missing RHS values ####
df <-
  df %>%
  filter(
    !(is.na(ln_mtm_ltv_pct) |
      is.na(ln_upb_frgv_amt.x) |
      is.na(prop_valu_as_is_val_amt) |
      is.na(brwr_curr_crdt_scr_val) |
      is.na(ln_bef_mdfc_frnt_rto_pct) |
      is.na(ln_pst_due_prd_cnt) |
      is.na(post_mod_dti_full))
  ) %>%
  mutate(
    pra_specific = ln_prin_rdcn_altv_cd == "PRA -- incented",
    dpay = ln_aft_mdfc_pi_pmt_amt - ln_bef_mdfc_pi_pmt_amt.x,
    dpay_pct = dpay / ln_bef_mdfc_pi_pmt_amt.x
  )

# winsorize
first_ptile_of_negative <- function(x){
  quantile(x[x<0], probs = 0.01, na.rm = T)
}


df <-
  df %>%
  ungroup() %>%
  mutate(
    ln_pst_due_prd_cnt = winsor(ln_pst_due_prd_cnt, 0, 0.95, verbose = TRUE),
    ln_prin_rdcn_altv_amt = winsor(ln_prin_rdcn_altv_amt, 0, 0.99, pos_only = TRUE, verbose = TRUE),
    ln_upb_frgv_amt.x = winsor(ln_upb_frgv_amt.x, 0, 0.99, pos_only = TRUE, verbose = TRUE),
    ln_bef_mdfc_pi_pmt_amt.x = winsor(ln_bef_mdfc_pi_pmt_amt.x, 0, 0.99, pos_only = TRUE, verbose = TRUE),
    ln_aft_mdfc_pi_pmt_amt = winsor(ln_aft_mdfc_pi_pmt_amt, 0, 0.99, pos_only = TRUE, verbose = TRUE),
    brwr_mthy_dpmt_xcldg_piti_amt = winsor(brwr_mthy_dpmt_xcldg_piti_amt, 0, 0.99, pos_only = TRUE, verbose = TRUE),
    ln_mtm_ltv_pct = ifelse(ln_mtm_ltv_pct < 2, ln_mtm_ltv_pct * 100, ln_mtm_ltv_pct),
    d_ltv_pct = ifelse(prop_valu_as_is_val_amt != 0, -100 * (ln_upb_frgv_amt.x) / prop_valu_as_is_val_amt, NA),
    d_ltv_pct = ifelse(d_ltv_pct <= first_ptile_of_negative(d_ltv_pct),
                        first_ptile_of_negative(d_ltv_pct),
                        d_ltv_pct)
  )

df <-
  df %>%
  mutate(
    mtm_ltv_post = ln_mtm_ltv_pct + d_ltv_pct,
    mtm_ltv_post_upb = ifelse(prop_valu_as_is_val_amt != 0,
      100 * (ln_aft_mdfc_upb_amt) / prop_valu_as_is_val_amt, NA
    ),
    mtm_ltv_post_upb_frgv = ifelse(prop_valu_as_is_val_amt != 0,
      100 * (ln_aft_mdfc_upb_amt - ln_prin_frbrn_amt - ln_upb_frgv_amt.x) / prop_valu_as_is_val_amt, NA
    ),
    past_due_decile = cut(ln_pst_due_prd_cnt,
      breaks = unique(quantile(df$ln_pst_due_prd_cnt, probs = seq(0, 1, by = 0.10))),
      include.lowest = TRUE
    )
  )

df_nonmissing <-
  df %>%
  filter(
    !(is.na(brwr_curr_crdt_scr_val) |
      is.na(ln_mtm_ltv_pct) |
      is.na(ln_bef_mdfc_frnt_rto_pct) |
      is.na(ln_pst_due_prd_cnt) |
      is.na(anyDelin_incl_tmp) |
      is.na(brwr_mthy_grs_incm_amt.x) |
      is.na(anyDelin))
  )
df_nonmissing_rich <-
  df_nonmissing %>%
  filter(
    !(is.na(ln_orgnn_ltv_pct) |
      ln_orgnn_ltv_pct == 0
    | brwr_mthy_dpmt_xcldg_piti_amt == 0)
  )

ctrl <- "poly(brwr_curr_crdt_scr_val,2) + poly(ln_mtm_ltv_pct,2) +
poly(brwr_mthy_grs_incm_amt.x,2) + poly(ln_bef_mdfc_frnt_rto_pct,3) + as.factor(past_due_decile)"

ctrl_rich <- paste(ctrl, "+ poly(prop_valu_as_is_val_amt,3)+ poly(ln_bef_mdfc_upb_amt.x,3)
                   + poly(ln_bef_mdfc_pi_pmt_amt.x,2) + poly(ln_orgnn_ltv_pct,3) +
                   poly(brwr_mthy_dpmt_xcldg_piti_amt,2)")

lm <- lm(paste0("anyDelin ~ ", ctrl), data = df_nonmissing)
df_nonmissing <-
  df_nonmissing %>%
  mutate(delin_pred = fitted.values(lm))

lm_rich <- lm(paste0("anyDelin ~ ", ctrl_rich), data = df_nonmissing_rich)
df_nonmissing_rich <-
  df_nonmissing_rich %>%
  mutate(delin_pred_rich = fitted.values(lm_rich))

lm_with_frgv_dum <- lm(paste0("anyDelin ~ fracForgivePos + ", ctrl), data = df_nonmissing)
df_nonmissing <-
  df_nonmissing %>%
  mutate(delin_pred_fracForgivePos = fitted.values(lm_with_frgv_dum))

df <-
  df %>%
  left_join(df_nonmissing %>%
    select(delin_pred, delin_pred_fracForgivePos, fncl_ast_id),
  by = "fncl_ast_id"
  ) %>%
  left_join(df_nonmissing_rich %>%
    select(delin_pred_rich, fncl_ast_id),
  by = "fncl_ast_id"
  )

summary(lm_with_frgv_dum)$r.squared

df %>%
  select(mtm_ltv_post, brwr_curr_crdt_scr_val, ln_bef_mdfc_frnt_rto_pct) %>%
  summary()

# create un-normalized running variable ####
df <-
  df %>%
  mutate(
    x_unnormed = (ln_amdfc_pral_npv_mdl_rslt_amt - ln_bmdfc_pral_npv_mdl_rslt_amt) -
      (ln_aft_mdfc_npv_mdl_rslt_amt - ln_bef_mdfc_npv_mdl_rslt_amt)
  )
df %>%
  select(x_unnormed) %>%
  summary()
df %>%
  group_by(x_unnormed == 0) %>%
  summarise(n())
rm(df_nonmissing)

# calculate average number of months observed after mods
df <-
  df %>%
  mutate(
    AsOfDate = monnb(as.Date("2015-02-01")),
    mosToToday = AsOfDate - mdfcCohort
  )

# Merge in foreclosre outcome ####
if (matched == "Yes") {
  df <- df %>% left_join(
    readRDS(paste0(working_path, "foreclosure_df.rds")),
    by = c("act_id", "bcs_seqnum"))

}

