## --------------------------------------------#
## Identify fncl_ast_id of various samples ####
## --------------------------------------------#
##
# (1) Matching sample (i.e. sample pre-match)
##

# Implement last pre-match screen
hamp_match_file <-
  readRDS(paste0(working_path, "hamp_match_file_clean.rds")) %>%
  filter(
    !is.na(msa_state),
    !is.na(ln_bef_mdfc_tot_pmt),
    !is.na(ln_bef_mdfc_upb_amt),
    ln_bef_mdfc_tot_pmt < 25000,
    ln_bef_mdfc_upb_amt > 0
  )

pre_match_ids <-
  hamp_match_file %>%
  mutate(pre_match_sample = TRUE) %>%
  select(fncl_ast_id, pre_match_sample)

rm(hamp_match_file)

##
# (2) Post match full sample
##

df_base <-
  readRDS(paste0(working_path, "matched_hamp_yearblock_unc_sub.rds")) %>%
  select(
    fncl_ast_id, bcs_seqnum, dist, act_id, mod_mnth_hamp, mod_mnth_tu,
    msa_state, ln_orig_note_dt, bir_decade_tu
  )
rm(matched_hamp)

## Filter 1: Match threshold
match_dist_cutoff <- 0.2
df_base <- df_base %>% filter(dist < match_dist_cutoff)

matched_sample_all_ids <-
  df_base %>%
  mutate(matched_sample_all = TRUE) %>%
  select(fncl_ast_id, act_id, bcs_seqnum, matched_sample_all)
rm(df_base)

# add rpt_sub_cd from tu_file
matched_sample_all_ids <-
  left_join(
    matched_sample_all_ids,
    readRDS(paste0(working_path, "tu_match_file_clean.rds")) %>%
      select(act_id, bcs_seqnum, rpt_sub_cd),
    by = c("act_id", "bcs_seqnum")
  )

##
# (3) Consumption DID sample
##
DID_sample_ids <-
  readRDS(paste0(working_path, "dNPV_out_es_base.rds")) %>%
  ungroup() %>%
  mutate(DID_sample = TRUE) %>%
  select(fncl_ast_id, DID_sample)

##
# (4) RD sample
##

##
# Load and add additional hamp variables.
##
mrgdHamp <-
  readRDS(paste0(working_path, "mrgdHamp_R.rds")) %>%
  as.data.frame() %>%
  mutate(
    fracForgivePos = ifelse(!is.na(ln_upb_frgv_amt.x), ln_upb_frgv_amt.x > 0, ifelse(
      !is.na(ln_upb_frgv_amt.y), ln_upb_frgv_amt.y > 0, FALSE
    )),
    praSampCore = ivsr_grpNum == "Non-GSE" & abs(x) > 0.005,
    praSamp = abs(x) < 0.9 & praSampCore,
    anyDelin = ln_st_cd == "Disqualified",
    pra = ifelse(is.na(pra), FALSE, pra),
    forgive_amount = ifelse(!is.na(ln_upb_frgv_amt.x), ln_upb_frgv_amt.x, ifelse(
      !is.na(ln_upb_frgv_amt.y), ln_upb_frgv_amt.y, 0
    )),
    pi_pay_red_dol = ln_bef_mdfc_pi_pmt_amt.x - ln_aft_mdfc_pi_pmt_amt,
    pi_pay_red_pct = pi_pay_red_dol / ln_bef_mdfc_pi_pmt_amt.x,
    ln_bef_mdfc_int_rt_hamp = ifelse(!is.na(ln_bef_mdfc_int_rt.x),
      ln_bef_mdfc_int_rt.x / 100, ln_bef_mdfc_int_rt.y / 100
    ),
    pre_mod_dti = ln_bef_mdfc_pi_pmt_amt.x / brwr_mthy_grs_incm_amt.x,
    pre_mod_dti_full = brwr_bmdfc_mthy_hsng_exp_amt / brwr_mthy_grs_incm_amt.x,
    post_mod_dti = ln_aft_mdfc_pi_pmt_amt / brwr_mthy_grs_incm_amt.x,
    post_mod_dti_full = brwr_amdfc_mthy_hsng_exp_amt / brwr_mthy_grs_incm_amt.x,
    ln_post_mod_ltv_pct = ifelse(prop_valu_as_is_val_amt > 0,
      100 * (ln_aft_mdfc_upb_amt + ln_prin_frbrn_amt) / prop_valu_as_is_val_amt, NA
    ),
    ln_post_mod_ltv_pct = ifelse(ln_post_mod_ltv_pct > 200, 200, ln_post_mod_ltv_pct),
    ln_mtm_ltv_pct = ifelse(ln_mtm_ltv_pct < 2, ln_mtm_ltv_pct * 100, ln_mtm_ltv_pct),
    ln_mtm_ltv_pct = ifelse(ln_mtm_ltv_pct > 200, 200, ln_mtm_ltv_pct),
    brwr_bir_dtNum = as.numeric(substr(as.character(brwr_bir_dtNum), 1, 4)),
    Age = 2011 - (brwr_bir_dtNum + 5)
  )


## Add sample identifier variables and subset to pre_match_analysis_sample ####

df <-
  pre_match_ids %>%
  left_join(mrgdHamp, by = "fncl_ast_id") %>%
  left_join(matched_sample_all_ids, by = "fncl_ast_id") %>%
  left_join(DID_sample_ids, by = "fncl_ast_id")

rm(mrgdHamp)
rm(pre_match_ids)
rm(matched_sample_all_ids)
rm(DID_sample_ids)

##
# Make Analysis pre and post match samples, RD sample
##
df <-
  df %>%
  mutate(
    pre_match_analysis_sample = ivsr_grpNum == "Non-GSE" & !is.na(x) & pre_match_sample == TRUE,
    matched_analysis_sample = pre_match_analysis_sample == TRUE & matched_sample_all == TRUE,
    rd_sample = matched_analysis_sample == TRUE &
      rpt_sub_cd != "x42g551" &
      mdfcCohort >= 1331 &
      abs(x) > 0.005 &
      abs(x) < bw_preferred &
      !(is.na(ln_mtm_ltv_pct) |
        is.na(ln_upb_frgv_amt.x) |
        is.na(prop_valu_as_is_val_amt) |
        is.na(brwr_curr_crdt_scr_val) |
        is.na(ln_bef_mdfc_frnt_rto_pct) |
        is.na(ln_pst_due_prd_cnt) |
        is.na(post_mod_dti_full))
  )

# Subset to pre_match
df <- df %>% filter(pre_match_analysis_sample == TRUE)

test_that("The number of years between mod date and March 2015, when our dataset ends", {
  expect_equal(
    df %>%
      filter(matched_analysis_sample == TRUE) %>%
      summarise(round(mean(monnb(as.Date("2015-03-17")) - mdfcCohort) / 12)) %>%
      as.numeric(),
    3
  )
})
test_that("The first delinquencies happened in", {
  expect_equivalent(
    df %>%
      filter(matched_analysis_sample == TRUE) %>%
      summarise(min(delinDate, na.rm = TRUE)) %>%
      .[[1]] %>%
      as.character(),
    "2010-11-01"
  )
})

test_that("Modification cohort in RD sample starts from", {
  expect_equivalent(
    df %>%
      filter(rd_sample == TRUE) %>%
      summarise(min(mdfcCohort)) %>%
      as.numeric(),
    monnb(as.Date("2010-12-1"))
  )
})

test_that("Modification cohort in RD sample ends in", {
  expect_equivalent(
    df %>%
      filter(rd_sample == TRUE) %>%
      summarise(max(mdfcCohort)) %>%
      as.numeric(),
    monnb(as.Date("2014-12-1"))
  )
})

test_that("Modification cohort in RD sample starts from", {
  expect_equal(
    df %>%
      filter(rd_sample == TRUE) %>%
      summarise(max(ln_orig_note_dt)) %>%
      as.numeric(),
    2008
  )
})

## ----------------------------------------#
## Make summary variables and winsorize ####
## ----------------------------------------#

df <-
  df %>%
  mutate(
    Income = brwr_mthy_grs_incm_amt.x * 12,
    `Home Value` = prop_valu_as_is_val_amt,
    `Mortgage Principal Remaining` = ln_aft_mdfc_upb_amt,
    `Loan to Value Ratio` = ln_mtm_ltv_pct,
    `Monthly Mortgage Payment` = ln_bef_mdfc_pi_pmt_amt.x,
    `Monthly Payment to Income Ratio` = pre_mod_dti_full,
    `Mortgage Interest Rate` = ln_bef_mdfc_int_rt_hamp,
    `Mortgage Term Remaining (Years)` = ln_bef_mdfc_rmng_term / 12,
    `ARM (d)` = ln_bef_mdfc_prdc_lbl_typ_cd == "ARM",
    `Months Past Due` = ln_pst_due_prd_cnt,
    `Credit Score` = brwr_curr_crdt_scr_val,
    `Male (d)` = ifelse(brwr_sex_typ_cd == "Male", TRUE, ifelse(brwr_sex_typ_cd == "Female", FALSE, NA)),
    Age = Age,
    `Monthly Payment Reduction ($)` = pi_pay_red_dol,
    `Monthly Payment Reduction (%)` = pi_pay_red_pct * 100,
    `Principal Forgiveness Amount` = forgive_amount,
    `Received Principal Forgiveness (d)` = fracForgivePos,
    `Post Modification LTV` = ln_post_mod_ltv_pct,
    `Post Modification DTI` = post_mod_dti_full,
    `Post Modification Default (d)` = anyDelin
  )

var_list <- c("Loan to Value Ratio", "Post Modification LTV")
for (var in var_list) {
  df[[var]] <- winsor(df[[var]],
    fractionLow = 0.01, fractionHigh = 1.0,
    verbose = TRUE, varname = var, pos_only = TRUE
  )
}

var_list <- c("Principal Forgiveness Amount")
for (var in var_list) {
  df[[var]] <- winsor(df[[var]],
    fractionLow = 0, fractionHigh = 0.99,
    verbose = TRUE, varname = var, pos_only = TRUE
  )
}

var_list <-
  c(
    "Home Value", "Mortgage Principal Remaining", "Monthly Mortgage Payment",
    "Monthly Payment to Income Ratio", "Mortgage Interest Rate",
    "Mortgage Term Remaining (Years)", "Credit Score", "Monthly Payment Reduction ($)",
    "Monthly Payment Reduction (%)", "Post Modification DTI"
  )
for (var in var_list) {
  df[[var]] <- winsor(df[[var]],
    fractionLow = 0.01, fractionHigh = 0.99,
    verbose = TRUE, varname = var, pos_only = TRUE
  )
}

df <- df %>% mutate(`Months Past Due` = winsor(`Months Past Due`,
                                         fractionLow = 0,
                                         fractionHigh = 0.95))

## -------------------------------------------------------#
## Make summary table 2: Match Quality, Select Columns ####
## -------------------------------------------------------#

##
# Pre-match analysis sample
##
df_long <-
  df %>%
  ungroup() %>%
  filter(pre_match_analysis_sample == TRUE) %>%
  mutate(N = n()) %>%
  select(
    fncl_ast_id,
    Income,
    `Home Value`,
    `Loan to Value Ratio`,
    `Monthly Mortgage Payment`,
    `Monthly Payment to Income Ratio`,
    `Mortgage Interest Rate`,
    `Mortgage Term Remaining (Years)`,
    `ARM (d)`,
    `Months Past Due`,
    `Credit Score`,
    `Male (d)`,
    Age,
    `Monthly Payment Reduction ($)`,
    `Monthly Payment Reduction (%)`,
    `Principal Forgiveness Amount`,
    `Received Principal Forgiveness (d)`,
    `Post Modification LTV`,
    `Post Modification DTI`,
    `Post Modification Default (d)`, N
  ) %>%
  gather(Variable, value, -fncl_ast_id)



pre_match_analysis_sample_summary <-
  df_long %>%
  group_by(Variable) %>%
  summarise(
    mean_pre_match = mean(value, na.rm = TRUE),
    sd_pre_match = sd(value, na.rm = TRUE),
    p10_pre_match = quantile(value, probs = 0.10, na.rm = TRUE),
    p50_pre_match = quantile(value, probs = 0.50, na.rm = TRUE),
    p90_pre_match = quantile(value, probs = 0.90, na.rm = TRUE)
  )
pre_match_analysis_sample_summary <- as.data.frame(pre_match_analysis_sample_summary)

pre_match_analysis_sample_summary <-
  pre_match_analysis_sample_summary %>%
  mutate(Variable = factor(Variable, levels = c(
    "Income",
    "Home Value",
    # "Mortgage Principal Remaining",
    "Loan to Value Ratio",
    "Monthly Mortgage Payment",
    "Monthly Payment to Income Ratio",
    "Mortgage Interest Rate",
    "Mortgage Term Remaining (Years)",
    "ARM (d)",
    "Months Past Due",
    "Credit Score",
    "Male (d)",
    "Age",
    "Monthly Payment Reduction ($)",
    "Monthly Payment Reduction (%)",
    "Principal Forgiveness Amount",
    "Received Principal Forgiveness (d)",
    "Post Modification LTV",
    "Post Modification DTI",
    "Post Modification Default (d)",
    "N"
  ))) %>%
  arrange(Variable)

##
# matched_analysis_sample
##
df_long <-
  df %>%
  ungroup() %>%
  filter(matched_analysis_sample == TRUE) %>%
  mutate(N = n()) %>%
  select(
    fncl_ast_id,
    Income,
    `Home Value`,
    `Loan to Value Ratio`,
    `Monthly Mortgage Payment`,
    `Monthly Payment to Income Ratio`,
    `Mortgage Interest Rate`,
    `Mortgage Term Remaining (Years)`,
    `ARM (d)`,
    `Months Past Due`,
    `Credit Score`,
    `Male (d)`,
    Age,
    `Monthly Payment Reduction ($)`,
    `Monthly Payment Reduction (%)`,
    `Principal Forgiveness Amount`,
    `Received Principal Forgiveness (d)`,
    `Post Modification LTV`,
    `Post Modification DTI`,
    `Post Modification Default (d)`, N
  ) %>%
  gather(Variable, value, -fncl_ast_id)


matched_analysis_sample_summary <-
  df_long %>%
  group_by(Variable) %>%
  summarise(
    mean_post_match = mean(value, na.rm = TRUE),
    sd_post_match = sd(value, na.rm = TRUE),
    p10_post_match = quantile(value, probs = 0.10, na.rm = TRUE),
    p50_post_match = quantile(value, probs = 0.50, na.rm = TRUE),
    p90_post_match = quantile(value, probs = 0.90, na.rm = TRUE)
  )
matched_analysis_sample_summary <- as.data.frame(matched_analysis_sample_summary)

##
# Combined pre-post match and calculate differences
##

# Add blank column to differentiate in the table
pre_match_analysis_sample_summary <-
  pre_match_analysis_sample_summary %>%
  mutate(blank1 = "", blank2 = "")

match_qual_table <-
  left_join(pre_match_analysis_sample_summary,
    matched_analysis_sample_summary,
    by = "Variable"
  )
match_qual_table <-
  match_qual_table %>%
  mutate(
    diff = mean_post_match - mean_pre_match,
    diff_as_sd = diff / sd_pre_match
  )
# Make "N" blank other than mean
match_qual_table <-
  match_qual_table %>%
  mutate(
    sd_pre_match = ifelse(Variable == "N", NA, sd_pre_match),
    p10_pre_match = ifelse(Variable == "N", NA, p10_pre_match),
    p50_pre_match = ifelse(Variable == "N", NA, p50_pre_match),
    p90_pre_match = ifelse(Variable == "N", NA, p90_pre_match),
    sd_post_match = ifelse(Variable == "N", NA, sd_post_match),
    p10_post_match = ifelse(Variable == "N", NA, p10_post_match),
    p50_post_match = ifelse(Variable == "N", NA, p50_post_match),
    p90_post_match = ifelse(Variable == "N", NA, p90_post_match),
    diff = ifelse(Variable == "N", NA, diff),
    diff_as_sd = ifelse(Variable == "N", NA, diff_as_sd)
  ) %>%
  select(
    Variable, mean_pre_match, sd_pre_match, blank1, blank2,
    mean_post_match, sd_post_match, diff_as_sd
  )
##
# Output to table
##
x <- xtable(match_qual_table, auto = TRUE)
digit_matrix <- matrix(c(
  rep(0, 8), 2,
  rep(0, 8), 2,
  rep(0, 8), 2,
  rep(0, 8), 2,
  rep(2, 8), 2,
  rep(3, 8), 2,
  rep(1, 8), 2,
  rep(2, 8), 2,
  rep(1, 8), 2,
  rep(0, 8), 2,
  rep(2, 8), 2,
  rep(1, 8), 2,
  rep(0, 8), 2,
  rep(0, 8), 2,
  rep(0, 8), 2,
  rep(2, 8), 2,
  rep(0, 8), 2,
  rep(2, 8), 2,
  rep(3, 8), 2,
  rep(0, 8), 2
),
nrow = 20, ncol = 9, byrow = TRUE
)
digits(x) <- digit_matrix

# Add column headers and output
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <-
  paste0('& \\multicolumn{2}{c}{Pre-Match} & & &\\multicolumn{2}{c}{Post-Match} & Normalized',
                           '\\\\',
                           '& Mean & SD &&& Mean & SD & Difference','\\\\')
print(x,
  add.to.row = addtorow, format.args = list(big.mark = ","),
  include.colnames = F, include.rownames = FALSE, floating = FALSE, auto = TRUE,
  hline.after = c(-1, -1, 0, nrow(x) - 1, nrow(x), nrow(x)),
  file = str_c(out_path, "match_quality_select_cols.tex"),
  booktabs = TRUE
)

## ----------------------------------------------------------#
## Make summary table 4: Representativeness, Just Pre-mod ####
## ----------------------------------------------------------#

##
# RD Sample
##
df_long <-
  df %>%
  ungroup() %>%
  filter(rd_sample == TRUE) %>%
  mutate(N = n()) %>%
  select(
    fncl_ast_id,
    Income,
    `Home Value`,
    `Loan to Value Ratio`,
    `Monthly Mortgage Payment`,
    `Mortgage Interest Rate`,
    `Mortgage Term Remaining (Years)`,
    `Months Past Due`,
    `Male (d)`,
    Age,
    N
  ) %>%
  gather(Variable, value, -fncl_ast_id)

rd_sample_summary <-
  df_long %>%
  group_by(Variable) %>%
  summarise(
    mean_rd = mean(value, na.rm = TRUE),
    sd_rd = sd(value, na.rm = TRUE),
    p10_rd = quantile(value, probs = 0.10, na.rm = TRUE),
    p50_rd = quantile(value, probs = 0.50, na.rm = TRUE),
    p90_rd = quantile(value, probs = 0.90, na.rm = TRUE)
  )
rd_sample_summary <- as.data.frame(rd_sample_summary)

# Add blank column to differentiate in the table
rd_sample_summary <- rd_sample_summary %>% mutate(blank1 = "", blank2 = "")


##
# Combine with PSID, calculate diffs
##
PSID_data <- read.csv(paste0(data_path, "PSID/PSID_input_representativeness.csv"))
PSID_data <-
  PSID_data %>%
  mutate(
    Variable = as.character(Variable),
    Variable = ifelse(Variable == "Mortgage Term Remaining",
      "Mortgage Term Remaining (Years)", Variable
    ),
    p10_PSID = ifelse(Variable == "Mortgage Interest Rate",
      sprintf("%.3f", p10_PSID),
      ifelse(Variable == "Male (d)",
        sprintf("%.2f", p10_PSID),
        ifelse(Variable %in% c("Mortgage Term Remaining (Years)", "Months Past Due", "Age"),
          sprintf("%.1f", p10_PSID), p10_PSID
        )
      )
    )
  ) %>%
  filter(
    Variable != "ARM (d)",
    !(Variable %in% c(
      "Mortgage Principal Remaining", "Value of Stocks",
      "Value of Bonds", "Value of IRA"
    ))
  ) %>%
  mutate(p10_PSID = as.numeric(p10_PSID) + 0.0001)
rep_table <- full_join(rd_sample_summary, PSID_data, by = "Variable")

# Reorder using factors
rep_table <-
  rep_table %>%
  mutate(Variable = factor(Variable, levels = c(
    "Income",
    "Home Value",
    "Loan to Value Ratio",
    "Monthly Mortgage Payment",
    "Mortgage Interest Rate",
    "Mortgage Term Remaining (Years)",
    "Months Past Due",
    "Male (d)",
    "Age",
    "Value of Liquid Assets",
    "N"
  ))) %>%
  arrange(Variable)
rep_table <-
  rep_table %>%
  mutate(
    sd_rd = ifelse(Variable == "N", NA, sd_rd),
    p10_rd = ifelse(Variable == "N", NA, p10_rd),
    p50_rd = ifelse(Variable == "N", NA, p50_rd),
    p90_rd = ifelse(Variable == "N", NA, p90_rd)
  )
##
# Make table we output
##

# Take out standard deviation for this table
rep_table <- rep_table %>% select(-sd_rd)

x <- xtable(rep_table, auto = TRUE)
digit_matrix <- matrix(c(
  rep(0, 12),
  rep(0, 12),
  rep(0, 12),
  rep(0, 12),
  rep(3, 12),
  rep(1, 12),
  rep(1, 12),
  rep(2, 12),
  rep(1, 12),
  rep(0, 12),
  rep(0, 12)
),
nrow = 11, ncol = 12, byrow = TRUE
)
digits(x) <- digit_matrix

# Add column headers and output
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <-
  paste0('& \\multicolumn{4}{c}{RD Analysis Sample} & & &\\multicolumn{4}{c}{PSID Delinquent Households}',
                           '\\\\',
                           '& Mean & p10 & p50 & p90 &&  & Mean & p10 & p50 & p90','\\\\')
print(x,
  add.to.row = addtorow, format.args = list(big.mark = ","),
  include.colnames = F, include.rownames = FALSE, floating = FALSE, auto = TRUE,
  hline.after = c(-1, -1, 0, nrow(x) - 1, nrow(x), nrow(x)),
  file = str_c(out_path, "representativeness_rd_pre.tex"), booktabs = TRUE
)

##
# Combine with Chase data, calculate diffs
##
Chase_data <- read.xls("./data/mtg_rd_ac2019-03-22.xls", sheet = "tbl_sum_stats") %>%
  select(Variable, Mean) %>%
  left_join(read.xls("./data/mtg_delin_03_19_20.xls")) %>%
  mutate(blank1 = NA,
         blank2 = NA)


rep_table <- full_join(Chase_data, PSID_data, by = "Variable")

# Reorder using factors
rep_table <-
  rep_table %>%
  mutate(Variable = factor(Variable, levels = c(
    "Income",
    "Home Value",
    "Loan to Value Ratio",
    "Monthly Mortgage Payment",
    "Mortgage Interest Rate",
    "Mortgage Term Remaining (Years)",
    "Months Past Due",
    "N"
  ))) %>%
  filter(!is.na(Variable)) %>%
  arrange(Variable)

##
# Make table we output
##

x <- xtable(rep_table, auto = TRUE)
digit_matrix <- matrix(c(
  rep(0, 12),
  rep(0, 12),
  rep(0, 12),
  rep(0, 12),
  rep(3, 12),
  rep(1, 12),
  rep(1, 12),
  rep(0, 12)
),
nrow = 8, ncol = 12, byrow = TRUE
)
digits(x) <- digit_matrix

# Add column headers and output
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <-
  paste0('& \\multicolumn{4}{c}{RD Analysis Sample} & & &\\multicolumn{4}{c}{PSID Delinquent Households}',
                           '\\\\',
                           '& Mean & p10 & p50 & p90 &&  & Mean & p10 & p50 & p90','\\\\')
print(x,
  add.to.row = addtorow, format.args = list(big.mark = ","), include.colnames = F,
  include.rownames = FALSE, floating = FALSE, auto = TRUE,
  hline.after = c(-1, -1, 0, nrow(x) - 1, nrow(x), nrow(x)),
  file = str_c(out_path, "representativeness_chase_psid.tex"), booktabs = TRUE
)

## -----------------------------------#
## Table 6: DID Sample HAMP vs PRA ####
## -----------------------------------#

dNPV_out <-
  readRDS(paste0(working_path, "dNPV_out_es_base.rds")) %>%
  select(fncl_ast_id, dNPV_cashflow) %>%
  mutate(dNPV_cashflow = dNPV_cashflow * -1)

df <- left_join(df, dNPV_out, by = "fncl_ast_id")

var_list <- c("dNPV_cashflow")
for (var in var_list) {
  df[[var]] <- winsor(df[[var]],
    fractionLow = 0.01, fractionHigh = 0.99,
    verbose = TRUE, varname = var, pos_only = TRUE
  )
}
df <- df %>% mutate(`NPV Payment Reduction` = dNPV_cashflow)


##
# DID HAMP
##
df_long <-
  df %>%
  ungroup() %>%
  filter(DID_sample == TRUE & pra == FALSE) %>%
  mutate(N = n()) %>%
  select(
    fncl_ast_id,
    Income,
    `Home Value`,
    `Loan to Value Ratio`,
    `Monthly Mortgage Payment`,
    `Monthly Payment to Income Ratio`,
    `Mortgage Interest Rate`,
    `Mortgage Term Remaining (Years)`,
    `Credit Score`,
    `Male (d)`,
    Age,
    `Monthly Payment Reduction ($)`,
    `Monthly Payment Reduction (%)`,
    `Principal Forgiveness Amount`,
    `Post Modification LTV`,
    `Post Modification DTI`,
    `NPV Payment Reduction`, N
  ) %>%
  gather(Variable, value, -fncl_ast_id)

DID_hamp_summary <-
  df_long %>%
  group_by(Variable) %>%
  summarise(
    mean_hamp = mean(value, na.rm = TRUE),
    sd_hamp = sd(value, na.rm = TRUE)
  )
DID_hamp_summary <- as.data.frame(DID_hamp_summary)

# Add blank column to differentiate in the table
DID_hamp_summary <- DID_hamp_summary %>% mutate(blank1 = "", blank2 = "")

##
# DID PRA
##
df_long <-
  df %>%
  ungroup() %>%
  filter(DID_sample == TRUE & pra == TRUE) %>%
  mutate(N = n()) %>%
  select(
    fncl_ast_id,
    Income,
    `Home Value`,
    `Loan to Value Ratio`,
    `Monthly Mortgage Payment`,
    `Monthly Payment to Income Ratio`,
    `Mortgage Interest Rate`,
    `Mortgage Term Remaining (Years)`,
    `Credit Score`,
    `Male (d)`,
    Age,
    `Monthly Payment Reduction ($)`,
    `Monthly Payment Reduction (%)`,
    `Principal Forgiveness Amount`,
    `Post Modification LTV`,
    `Post Modification DTI`,
    `NPV Payment Reduction`, N
  ) %>%
  gather(Variable, value, -fncl_ast_id)

test_that("The median post-mod LTV for the principal reduction sample is ", {
  expect_equal(
    df_long %>%
      filter(Variable == "Post Modification LTV") %>%
      summarise(median_post_mod_LTV = round(median(value, na.rm = TRUE))) %>%
      as.numeric(),
    114
  )
})

DID_pra_summary <-
  df_long %>%
  group_by(Variable) %>%
  summarise(
    mean_pra = mean(value, na.rm = TRUE),
    sd_pra = sd(value, na.rm = TRUE)
  )
DID_pra_summary <- as.data.frame(DID_pra_summary)

##
# Combine
##
DID_table <- full_join(DID_hamp_summary, DID_pra_summary, by = "Variable")

# Reorder using factors,
DID_table <-
  DID_table %>%
  mutate(Variable = factor(Variable, levels = c(
    "Principal Forgiveness Amount",
    "NPV Payment Reduction",
    "Monthly Payment Reduction ($)",
    "Monthly Payment Reduction (%)",
    "Loan to Value Ratio",
    "Post Modification LTV",
    "Monthly Payment to Income Ratio",
    "Post Modification DTI",
    "Income",
    "Credit Score",
    "Home Value",
    "Monthly Mortgage Payment",
    "Mortgage Interest Rate",
    "Mortgage Term Remaining (Years)",
    "Male (d)",
    "Age",
    "N"
  ))) %>%
  arrange(Variable)

##
# Make table we output
##
DID_table <-
  DID_table %>%
  mutate(
    sd_hamp = ifelse(Variable == "N", NA, sd_hamp),
    sd_pra = ifelse(Variable == "N", NA, sd_pra)
  )

x <- xtable(DID_table, auto = TRUE)
digit_matrix <- matrix(c(
  rep(0, 8),
  rep(0, 8),
  rep(0, 8),
  rep(1, 8),
  rep(0, 8),
  rep(0, 8),
  rep(2, 8),
  rep(2, 8),
  rep(0, 8),
  rep(0, 8),
  rep(0, 8),
  rep(0, 8),
  rep(3, 8),
  rep(1, 8),
  rep(2, 8),
  rep(1, 8),
  rep(0, 8)
),
nrow = 17, ncol = 8, byrow = TRUE
)
digits(x) <- digit_matrix


# Add column headers and output
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <-
  paste0('& \\multicolumn{2}{c}{Payment Reduction} & & &\\multicolumn{2}{c}{Payment and Principal}',
                           '\\\\',
                           '& \\multicolumn{2}{c}{} & & &\\multicolumn{2}{c}{Reduction}',
                           '\\\\',
                           '& Mean & SD &&  & Mean & SD','\\\\')
print(x,
  add.to.row = addtorow, format.args = list(big.mark = ","),
  include.colnames = F, include.rownames = FALSE, floating = FALSE, auto = TRUE,
  hline.after = c(-1, -1, 0, nrow(x) - 1, nrow(x), nrow(x)),
  file = str_c(out_path, "DID_table_select.tex"), booktabs = TRUE
)

## --------------------------------------------------#
## Data For Match Rate Around Discontinuity Graph ####
## --------------------------------------------------#

df <-
  df %>%
  mutate(
    pre_match_rd_sample = ivsr_grpNum == "Non-GSE" & !is.na(x) &
      pre_match_sample == TRUE & abs(x) > 0.005,
    post_match_rd_sample = pre_match_rd_sample == TRUE & matched_analysis_sample == TRUE,
    post_match_rd_sample_indicator = ifelse(is.na(post_match_rd_sample) | post_match_rd_sample == FALSE, 0, 1)
  )

# Make data frame for match rate graph
df_match_graph <-
  df %>%
  filter(pre_match_rd_sample == TRUE) %>%
  select(
    fncl_ast_id, pre_match_rd_sample, post_match_rd_sample,
    post_match_rd_sample_indicator, x
  ) %>%
  saveRDS(file = paste0(working_path, "df_match_graph.rds"))

rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^DID"))

remove <- ls(pattern = "^match")
remove <- remove[remove != "matched"]
rm(list = remove)
rm(remove)
rm(PSID_data)
rm(dNPV_out)
rm(digit_matrix)
rm(pre_match_analysis_sample_summary)
rm(rd_sample_summary)
rm(rep_table)
rm(x)
rm(Chase_data)
