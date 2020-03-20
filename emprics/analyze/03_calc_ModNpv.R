##
# Setup
##
ptm <- proc.time()
avg_int_rt <- 0.0411 # from notes/interest_rate_30-yr_historics
discount_rt <- 0.0411
min_int_rt <- 0.0331 # from notes/interest_rate_30-yr_historics.  Used when interest rate cap is implausibly low.

##
# Clean Data
##

# Eliminate rejected loans
df <-
  df %>%
  filter(
    ln_mdfc_mode_cd != 4,
    mdfcCohort > 1326
  )


# Narrrow to key vars
df <-
  df %>%
  select(
    fncl_ast_id,
    ln_bef_mdfc_prdc_lbl_typ_cd,
    ln_mtm_ltv_pct,
    ln_bef_mdfc_int_rt.x,
    ln_bef_mdfc_int_rt.y,
    ln_aft_mdfc_int_rt,
    ln_aft_mdfc_max_int_rt,
    ln_aft_mdfc_rmng_term,
    ln_bef_mdfc_rmng_term,
    ln_prin_frbrn_amt,
    ln_prin_rdcn_altv_amt,
    ln_upb_frgv_amt.x,
    ln_upb_frgv_amt.y,
    ln_aft_mdfc_upb_amt,
    ln_bef_mdfc_upb_amt.x,
    ln_bef_mdfc_upb_amt.y,
    ln_bef_mdfc_pi_pmt_amt.x,
    ln_bef_mdfc_pi_pmt_amt.y,
    ln_aft_mdfc_pi_pmt_amt,
    ln_prin_rdcn_altv_cd,
    pra,
    x
  )

# Combine those coming from first lien and NPV data set
df <-
  df %>%
  mutate(
    ln_bef_mdfc_int_rt_hamp = ifelse(!is.na(ln_bef_mdfc_int_rt.x), ln_bef_mdfc_int_rt.x / 100, ln_bef_mdfc_int_rt.y / 100),
    ln_upb_frgv_amt = ifelse(!is.na(ln_upb_frgv_amt.x), ln_upb_frgv_amt.x, ln_upb_frgv_amt.y),
    ln_bef_mdfc_upb_amt = ifelse(!is.na(ln_bef_mdfc_upb_amt.x), ln_bef_mdfc_upb_amt.x, ln_bef_mdfc_upb_amt.y),
    ln_bef_mdfc_pi_pmt_amt = ifelse(!is.na(ln_bef_mdfc_pi_pmt_amt.x), ln_bef_mdfc_pi_pmt_amt.x, ln_bef_mdfc_pi_pmt_amt.y),
    dnpv_pos = ifelse(x > 0, TRUE, FALSE),
    pra = ifelse(is.na(pra), FALSE, pra),
    forgive_amount = ifelse(!is.na(ln_upb_frgv_amt), ln_upb_frgv_amt, 0)
  ) %>%
  select(
    -ln_bef_mdfc_int_rt.x, -ln_bef_mdfc_int_rt.y, -ln_upb_frgv_amt.x, -ln_upb_frgv_amt.y,
    -ln_bef_mdfc_upb_amt.x, -ln_bef_mdfc_upb_amt.y, -ln_bef_mdfc_pi_pmt_amt.x, -ln_bef_mdfc_pi_pmt_amt.y,
    -x
  )


# Begin drops. Two phases.
# Phase 1: Drops for characteristics.
df <-
  df %>%
  filter(
    !is.na(ln_aft_mdfc_rmng_term),
    ln_aft_mdfc_rmng_term > 12,
    ln_aft_mdfc_rmng_term < 600,
    !is.na(ln_bef_mdfc_rmng_term),
    ln_bef_mdfc_rmng_term > 12,
    ln_bef_mdfc_rmng_term < 600,
    ln_bef_mdfc_pi_pmt_amt <= 5000,
    ln_aft_mdfc_pi_pmt_amt <= 5000,
    ln_bef_mdfc_upb_amt > 1000
  )

# Phase 2: split into Fixed rate DF and ARM df (everything else)
if (sample_name == "es_base") {
  df_arm <- df %>% filter(ln_bef_mdfc_prdc_lbl_typ_cd != "Fixed Rate" | is.na(ln_bef_mdfc_prdc_lbl_typ_cd))
} else {
  df_arm <- df %>% filter(ln_bef_mdfc_prdc_lbl_typ_cd != "Fixed Rate")
  count(df_arm)
}

if (sample_name != "es_rd") {
  df <- df %>% filter(ln_bef_mdfc_prdc_lbl_typ_cd == "Fixed Rate")
  count(df)
}

# Find years remaining for unmodified and modified loan
df <-
  df %>%
  mutate(
    aft_mdfc_rmng_yrs = round(ln_aft_mdfc_rmng_term / 12),
    bef_mdfc_rmng_yrs = round(ln_bef_mdfc_rmng_term / 12)
  )

# Figure out implied annual interest rate directly, as the rate required such that the monthly payment results in full loan payoff by final year.
annuity_difference <- function(i, m, p, n) {
  diff <- m - p * (i * (1 + i)^n) / ((1 + i)^n - 1)
}

df <-
  df %>%
  rowwise() %>%
  mutate(
    ln_bef_mdfc_int_rt_calc_r = uniroot(annuity_difference, c(-1, 1),
      extendInt = "no", m = ln_bef_mdfc_pi_pmt_amt * 12, p = ln_bef_mdfc_upb_amt,
      n = bef_mdfc_rmng_yrs
    )$root
  ) %>%
  ungroup()

# Drop those with calculated interest rates below zero
count(df)
df <- df %>% filter(ln_bef_mdfc_int_rt_calc_r > 0)
count(df)

# Expand by 40 years (COULD expand by max remaining years)
df <- df[rep(seq_len(nrow(df)), 40), ]
df <- df %>% arrange(fncl_ast_id)

## CREATE ANNUAL VARIABLES ####
df <-
  df %>%
  group_by(fncl_ast_id) %>%
  mutate(
    yrsPaid = row_number(),
    aft_mdfc_yrsLeft = ifelse(aft_mdfc_rmng_yrs - yrsPaid + 1 > 0, aft_mdfc_rmng_yrs - yrsPaid + 1, 0), # last year has a 1
    bef_mdfc_yrsLeft = ifelse(bef_mdfc_rmng_yrs - yrsPaid + 1 > 0, bef_mdfc_rmng_yrs - yrsPaid + 1, 0),
    anl_aft_mdfc_pi_amt = ln_aft_mdfc_pi_pmt_amt * 12,
    anl_bef_mdfc_pi_amt = ln_bef_mdfc_pi_pmt_amt * 12,
    ln_aft_mdfc_int_rt_dec = ln_aft_mdfc_int_rt / 100,
    ln_aft_mdfc_max_int_rt_dec = ifelse(!is.na(ln_aft_mdfc_max_int_rt) & ln_aft_mdfc_max_int_rt / 100 >= min_int_rt,
      ln_aft_mdfc_max_int_rt / 100, avg_int_rt
    ),
    ln_aft_mdfc_int_rate_dec_cal = ln_aft_mdfc_int_rt_dec
  )

# I nterest rate rises 1% per year to rate cap after year five
df <-
  df %>%
  mutate(
    ln_aft_mdfc_int_rate_dec_cal = ifelse(yrsPaid > 5 & ln_aft_mdfc_int_rt_dec + (yrsPaid - 5) / 100 < ln_aft_mdfc_max_int_rt_dec, ln_aft_mdfc_int_rt_dec + (yrsPaid - 5) / 100,
      ifelse(yrsPaid > 5 & ln_aft_mdfc_int_rt_dec + (yrsPaid - 5) / 100 >= ln_aft_mdfc_max_int_rt_dec, ln_aft_mdfc_max_int_rt_dec,
        ifelse(yrsPaid <= 5, ln_aft_mdfc_int_rt_dec, NA)
      )
    )
  )
summary(df$ln_aft_mdfc_int_rate_dec_cal)

## --------------------------------------------------------------##
## CALCULATE STATUS-QUO INTEREST, PRINCIPAL, UPB, AND CASH FLOW ##
## --------------------------------------------------------------##

# Set it up
df <-
  df %>%
  mutate(
    ln_bef_mdfc_upb_amt_beg = ifelse(yrsPaid == 1, ln_bef_mdfc_upb_amt, 0), # will fill in for beggining of period
    ln_bef_mdfc_upb_amt_end = 0, # to find implied balloon payment at yrsLeft==1
    anl_bef_mdfc_int_pmt_amt = 0,
    anl_bef_mdfc_princ_pmt_amt = 0
  )

# Fill it in
year_list <- c(1:40)
for (year in year_list) {
  df <-
    df %>%
    mutate(
      anl_bef_mdfc_int_pmt_amt = ifelse(yrsPaid == year & bef_mdfc_yrsLeft != 0, ln_bef_mdfc_upb_amt_beg * ln_bef_mdfc_int_rt_calc_r, anl_bef_mdfc_int_pmt_amt),
      anl_bef_mdfc_princ_pmt_amt = ifelse(yrsPaid == year & bef_mdfc_yrsLeft != 0, anl_bef_mdfc_pi_amt - anl_bef_mdfc_int_pmt_amt, anl_bef_mdfc_princ_pmt_amt),
      ln_bef_mdfc_upb_amt_beg = ifelse(yrsPaid == year + 1 & bef_mdfc_yrsLeft != 0, lag(ln_bef_mdfc_upb_amt_beg, 1) - lag(anl_bef_mdfc_princ_pmt_amt, 1), ln_bef_mdfc_upb_amt_beg),
      ln_bef_mdfc_upb_amt_end = ifelse(yrsPaid == year & bef_mdfc_yrsLeft != 0, ln_bef_mdfc_upb_amt_beg - anl_bef_mdfc_princ_pmt_amt, ln_bef_mdfc_upb_amt_end)
    )
}

# Calculate annual discounted cashflows, then sum up by loan
df <-
  df %>%
  mutate(
    disct_fact = 1 / (1 + discount_rt)^yrsPaid,
    bef_mdfc_disc_cashflw = ifelse(bef_mdfc_yrsLeft != 0, anl_bef_mdfc_pi_amt * disct_fact, 0)
  ) %>%
  group_by(fncl_ast_id) %>%
  mutate(NPV_bef_mod_cashflow = sum(bef_mdfc_disc_cashflw))

## ------------------------------------------------------------##
## CALCULATE POST-MOD INTEREST, PRINCIPAL, UPB, AND CASH FLOW ##
## ------------------------------------------------------------##

# Set it up
df <-
  df %>%
  mutate(
    ln_aft_mdfc_upb_amt_beg = ifelse(yrsPaid == 1, ln_aft_mdfc_upb_amt, 0),
    ln_aft_mdfc_upb_amt_end = 0,
    anl_aft_mdfc_int_pmt_amt = 0,
    anl_aft_mdfc_princ_pmt_amt = 0,
    ln_prin_frbrn_amt = ifelse(is.na(ln_prin_frbrn_amt), 0, ln_prin_frbrn_amt)
  )

# start with the first five years, int rate and monthly payment are set
year_list <- c(1:5)
df <- df %>% arrange(fncl_ast_id, yrsPaid)
for (year in year_list) {
  df <-
    df %>%
    mutate(
      anl_aft_mdfc_int_pmt_amt = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, ln_aft_mdfc_upb_amt_beg * ln_aft_mdfc_int_rate_dec_cal, anl_aft_mdfc_int_pmt_amt),
      anl_aft_mdfc_princ_pmt_amt = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, anl_aft_mdfc_pi_amt - anl_aft_mdfc_int_pmt_amt, anl_aft_mdfc_princ_pmt_amt),
      ln_aft_mdfc_upb_amt_beg = ifelse(yrsPaid == year + 1 & aft_mdfc_yrsLeft != 0, lag(ln_aft_mdfc_upb_amt_beg, 1) - lag(anl_aft_mdfc_princ_pmt_amt, 1), ln_aft_mdfc_upb_amt_beg),
      ln_aft_mdfc_upb_amt_end = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, ln_aft_mdfc_upb_amt_beg - anl_aft_mdfc_princ_pmt_amt, ln_aft_mdfc_upb_amt_end)
    )
}

# For years after 5, recalculate annual payment each year given the new interest rate
year_list <- c(6:40)
df <- df %>% arrange(fncl_ast_id, yrsPaid)
for (year in year_list) {
  df <-
    df %>%
    mutate(
      anl_aft_mdfc_int_pmt_amt = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, ln_aft_mdfc_upb_amt_beg * ln_aft_mdfc_int_rate_dec_cal, anl_aft_mdfc_int_pmt_amt),
      anl_aft_mdfc_pi_amt = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0,
        ln_aft_mdfc_upb_amt_beg * (ln_aft_mdfc_int_rate_dec_cal * (1 + ln_aft_mdfc_int_rate_dec_cal)^aft_mdfc_yrsLeft) /
          ((1 + ln_aft_mdfc_int_rate_dec_cal)^aft_mdfc_yrsLeft - 1), anl_aft_mdfc_pi_amt
      ),
      anl_aft_mdfc_princ_pmt_amt = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, anl_aft_mdfc_pi_amt - anl_aft_mdfc_int_pmt_amt, anl_aft_mdfc_princ_pmt_amt),
      ln_aft_mdfc_upb_amt_beg = ifelse(yrsPaid == year + 1 & aft_mdfc_yrsLeft != 0, lag(ln_aft_mdfc_upb_amt_beg, 1) - lag(anl_aft_mdfc_princ_pmt_amt, 1),
        ln_aft_mdfc_upb_amt_beg
      ),
      ln_aft_mdfc_upb_amt_end = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, ln_aft_mdfc_upb_amt_beg - anl_aft_mdfc_princ_pmt_amt, ln_aft_mdfc_upb_amt_end)
    )
}

# Go back and add in the balloon payment for forbearance amount while paying, due at end of term, and calculate discounted cashflows.
# Forbearance not included in upb amount by itself.
df <-
  df %>%
  mutate(
    ln_aft_mdfc_upb_amt_beg = ifelse(aft_mdfc_yrsLeft > 0, ln_aft_mdfc_upb_amt_beg + ln_prin_frbrn_amt, ln_aft_mdfc_upb_amt_beg),
    ln_aft_mdfc_upb_amt_end = ifelse(aft_mdfc_yrsLeft > 1, ln_aft_mdfc_upb_amt_end + ln_prin_frbrn_amt, ln_aft_mdfc_upb_amt_end),
    anl_aft_mdfc_pi_amt = ifelse(aft_mdfc_yrsLeft == 1, anl_aft_mdfc_pi_amt + ln_prin_frbrn_amt, anl_aft_mdfc_pi_amt),
    aft_mdfc_disc_cashflw = ifelse(aft_mdfc_yrsLeft != 0, anl_aft_mdfc_pi_amt * disct_fact, 0)
  )

# Sum up NPV cashflow
df <-
  df %>%
  group_by(fncl_ast_id) %>%
  mutate(NPV_aft_mod_cashflow = sum(aft_mdfc_disc_cashflw))

# Drop outliers. Figure out which fncl_ast_ids ever have a negative payment, and drop all rows with these loans.
df <-
  df %>%
  group_by(fncl_ast_id) %>%
  mutate(
    min_anl_aft_mdfc_princ_pmt_amt = min(anl_aft_mdfc_princ_pmt_amt),
    min_anl_bef_mdfc_princ_pmt_amt = min(anl_bef_mdfc_princ_pmt_amt)
  ) %>%
  filter(min_anl_aft_mdfc_princ_pmt_amt >= 0 & min_anl_bef_mdfc_princ_pmt_amt >= 0) %>%
  select(-min_anl_aft_mdfc_princ_pmt_amt, -min_anl_bef_mdfc_princ_pmt_amt)

# Calculate actual principal and interest payments for each month, dollar reductions, and npv of these dollar reductions
df <-
  df %>%
  mutate(
    anl_bef_pi = anl_bef_mdfc_int_pmt_amt + anl_bef_mdfc_princ_pmt_amt,
    anl_aft_pi = ifelse(aft_mdfc_yrsLeft == 1, anl_aft_mdfc_int_pmt_amt + anl_aft_mdfc_princ_pmt_amt + ln_prin_frbrn_amt,
      anl_aft_mdfc_int_pmt_amt + anl_aft_mdfc_princ_pmt_amt
    ),
    pi_diff_amt = anl_aft_pi - anl_bef_pi,
    npv_pi_diff_amt = pi_diff_amt * disct_fact,
    dNPV_cashflow = NPV_aft_mod_cashflow - NPV_bef_mod_cashflow
  )

# Calculate cummulative NPV of cashflow savings
df <- df %>% mutate(npv_pi_diff_amt_cum = ifelse(yrsPaid == 1, npv_pi_diff_amt, 0))
year_list <- c(2:40)
df <- df %>% arrange(fncl_ast_id, yrsPaid)
for (year in year_list) {
  df <-
    df %>%
    mutate(npv_pi_diff_amt_cum = ifelse(yrsPaid == year, lag(npv_pi_diff_amt_cum, 1) + npv_pi_diff_amt, npv_pi_diff_amt_cum))
}

## ------------------------------------##
## Calculate reductions for ARM group ##
## ------------------------------------##
# Find years remaining for unmodified and modified loan
df_arm <-
  df_arm %>%
  mutate(
    aft_mdfc_rmng_yrs = round(ln_aft_mdfc_rmng_term / 12),
    bef_mdfc_rmng_yrs = round(ln_bef_mdfc_rmng_term / 12)
  )

# Figure out implied annual interest rate
df_arm <-
  df_arm %>%
  rowwise() %>%
  mutate(ln_bef_mdfc_int_rt_calc_r = uniroot(annuity_difference, c(-1, 1),
    extendInt = "no",
    m = ln_bef_mdfc_pi_pmt_amt * 12, p = ln_bef_mdfc_upb_amt,
    n = bef_mdfc_rmng_yrs
  )$root) %>%
  ungroup() %>%
  filter(ln_bef_mdfc_int_rt_calc_r > 0)


# Expand by 40 years (COULD expand by max remaining years)
df_arm <- df_arm[rep(seq_len(nrow(df_arm)), 40), ]
df_arm <- df_arm %>% arrange(fncl_ast_id)

## CREATE ANNUAL VARIABLES ##
df_arm <-
  df_arm %>%
  group_by(fncl_ast_id) %>%
  mutate(
    yrsPaid = row_number(),
    aft_mdfc_yrsLeft = ifelse(aft_mdfc_rmng_yrs - yrsPaid + 1 > 0, aft_mdfc_rmng_yrs - yrsPaid + 1, 0), # last year has a 1
    bef_mdfc_yrsLeft = ifelse(bef_mdfc_rmng_yrs - yrsPaid + 1 > 0, bef_mdfc_rmng_yrs - yrsPaid + 1, 0),
    anl_aft_mdfc_pi_amt = ln_aft_mdfc_pi_pmt_amt * 12,
    anl_bef_mdfc_pi_amt = ln_bef_mdfc_pi_pmt_amt * 12,
    ln_aft_mdfc_int_rt_dec = ln_aft_mdfc_int_rt / 100,
    ln_aft_mdfc_max_int_rt_dec = ifelse(!is.na(ln_aft_mdfc_max_int_rt) & ln_aft_mdfc_max_int_rt / 100 >= min_int_rt,
      ln_aft_mdfc_max_int_rt / 100, avg_int_rt
    ),
    ln_aft_mdfc_int_rate_dec_cal = ln_aft_mdfc_int_rt_dec
  )

# Interest rate rises 1% per year to rate cap after year five
df_arm <-
  df_arm %>%
  mutate(ln_aft_mdfc_int_rate_dec_cal = ifelse(
    yrsPaid > 5 & ln_aft_mdfc_int_rt_dec + (yrsPaid - 5) / 100 < ln_aft_mdfc_max_int_rt_dec, ln_aft_mdfc_int_rt_dec + (yrsPaid - 5) / 100,
    ifelse(
      yrsPaid > 5 & ln_aft_mdfc_int_rt_dec + (yrsPaid - 5) / 100 >= ln_aft_mdfc_max_int_rt_dec, ln_aft_mdfc_max_int_rt_dec,
      ifelse(
        yrsPaid <= 5, ln_aft_mdfc_int_rt_dec, NA
      )
    )
  ))

## ---------------------##
## Status-Quo for ARMS ##
## ---------------------##

if (sample_name == "es_base") {
  df_arm <-
    df_arm %>%
    mutate(
      ln_bef_mdfc_upb_amt_beg = ifelse(yrsPaid == 1, ln_bef_mdfc_upb_amt, 0),
      ln_bef_mdfc_upb_amt_end = 0,
      anl_bef_mdfc_int_pmt_amt = 0,
      anl_bef_mdfc_princ_pmt_amt = 0
    )
  # Fill it in
  year_list <- c(1:40)
  for (year in year_list) {
    df_arm <- df_arm %>% mutate(
      anl_bef_mdfc_int_pmt_amt = ifelse(yrsPaid == year & bef_mdfc_yrsLeft != 0, ln_bef_mdfc_upb_amt_beg * ln_bef_mdfc_int_rt_calc_r, anl_bef_mdfc_int_pmt_amt),
      anl_bef_mdfc_princ_pmt_amt = ifelse(yrsPaid == year & bef_mdfc_yrsLeft != 0, anl_bef_mdfc_pi_amt - anl_bef_mdfc_int_pmt_amt, anl_bef_mdfc_princ_pmt_amt),
      ln_bef_mdfc_upb_amt_beg = ifelse(yrsPaid == year + 1 & bef_mdfc_yrsLeft != 0, lag(ln_bef_mdfc_upb_amt_beg, 1) - lag(anl_bef_mdfc_princ_pmt_amt, 1), ln_bef_mdfc_upb_amt_beg),
      ln_bef_mdfc_upb_amt_end = ifelse(yrsPaid == year & bef_mdfc_yrsLeft != 0, ln_bef_mdfc_upb_amt_beg - anl_bef_mdfc_princ_pmt_amt, ln_bef_mdfc_upb_amt_end)
    )
  }

  # Calculate annual discounted cashflows, then sum up by loan
  df_arm <-
    df_arm %>%
    mutate(
      disct_fact = 1 / (1 + discount_rt)^yrsPaid,
      bef_mdfc_disc_cashflw = ifelse(bef_mdfc_yrsLeft != 0, anl_bef_mdfc_pi_amt * disct_fact, 0)
    ) %>%
    group_by(fncl_ast_id) %>%
    mutate(NPV_bef_mod_cashflow = sum(bef_mdfc_disc_cashflw))
} else {
  df_arm <- df_arm %>% mutate(NPV_bef_mod_cashflow = as.numeric(ln_bef_mdfc_upb_amt))
}

## -------------------##
## Post-Mod for ARMS ##
## -------------------##
# Set it up
df_arm <-
  df_arm %>%
  mutate(
    ln_aft_mdfc_upb_amt_beg = ifelse(yrsPaid == 1, ln_aft_mdfc_upb_amt, 0),
    ln_aft_mdfc_upb_amt_end = 0,
    anl_aft_mdfc_int_pmt_amt = 0,
    anl_aft_mdfc_princ_pmt_amt = 0,
    ln_prin_frbrn_amt = ifelse(is.na(ln_prin_frbrn_amt), 0, ln_prin_frbrn_amt)
  )
# start with the first five years, int rate and monthly payment are set
year_list <- c(1:5)
df_arm <- df_arm %>% arrange(fncl_ast_id, yrsPaid)
for (year in year_list) {
  df_arm <-
    df_arm %>%
    mutate(
      anl_aft_mdfc_int_pmt_amt = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, ln_aft_mdfc_upb_amt_beg * ln_aft_mdfc_int_rate_dec_cal, anl_aft_mdfc_int_pmt_amt),
      anl_aft_mdfc_princ_pmt_amt = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, anl_aft_mdfc_pi_amt - anl_aft_mdfc_int_pmt_amt, anl_aft_mdfc_princ_pmt_amt),
      ln_aft_mdfc_upb_amt_beg = ifelse(yrsPaid == year + 1 & aft_mdfc_yrsLeft != 0, lag(ln_aft_mdfc_upb_amt_beg, 1) - lag(anl_aft_mdfc_princ_pmt_amt, 1), ln_aft_mdfc_upb_amt_beg),
      ln_aft_mdfc_upb_amt_end = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, ln_aft_mdfc_upb_amt_beg - anl_aft_mdfc_princ_pmt_amt, ln_aft_mdfc_upb_amt_end)
    )
}
# For years after 5, recalculate annual payment each year given the new interest rate
year_list <- c(6:40)
df_arm <- df_arm %>% arrange(fncl_ast_id, yrsPaid)
for (year in year_list) {
  df_arm <-
    df_arm %>%
    mutate(
      anl_aft_mdfc_int_pmt_amt = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, ln_aft_mdfc_upb_amt_beg * ln_aft_mdfc_int_rate_dec_cal, anl_aft_mdfc_int_pmt_amt),
      anl_aft_mdfc_pi_amt = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0,
        ln_aft_mdfc_upb_amt_beg * (ln_aft_mdfc_int_rate_dec_cal * (1 + ln_aft_mdfc_int_rate_dec_cal)^aft_mdfc_yrsLeft) /
          ((1 + ln_aft_mdfc_int_rate_dec_cal)^aft_mdfc_yrsLeft - 1), anl_aft_mdfc_pi_amt
      ),
      anl_aft_mdfc_princ_pmt_amt = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, anl_aft_mdfc_pi_amt - anl_aft_mdfc_int_pmt_amt, anl_aft_mdfc_princ_pmt_amt),
      ln_aft_mdfc_upb_amt_beg = ifelse(yrsPaid == year + 1 & aft_mdfc_yrsLeft != 0, lag(ln_aft_mdfc_upb_amt_beg, 1) - lag(anl_aft_mdfc_princ_pmt_amt, 1),
        ln_aft_mdfc_upb_amt_beg
      ),
      ln_aft_mdfc_upb_amt_end = ifelse(yrsPaid == year & aft_mdfc_yrsLeft != 0, ln_aft_mdfc_upb_amt_beg - anl_aft_mdfc_princ_pmt_amt, ln_aft_mdfc_upb_amt_end)
    )
}

# Confirm that the balance at the end is zero
summary(df_arm[df_arm$aft_mdfc_yrsLeft == 1, "ln_aft_mdfc_upb_amt_end"])

# Go back and add in the balloon payment for forbearance amount, due at end of term, and calculate discounted cashflows
df_arm <-
  df_arm %>%
  mutate(
    disct_fact = 1 / (1 + discount_rt)^yrsPaid,
    ln_aft_mdfc_upb_amt_beg = ifelse(aft_mdfc_yrsLeft > 0, ln_aft_mdfc_upb_amt_beg + ln_prin_frbrn_amt, ln_aft_mdfc_upb_amt_beg),
    ln_aft_mdfc_upb_amt_end = ifelse(aft_mdfc_yrsLeft > 1, ln_aft_mdfc_upb_amt_end + ln_prin_frbrn_amt, ln_aft_mdfc_upb_amt_end),
    anl_aft_mdfc_pi_amt = ifelse(aft_mdfc_yrsLeft == 1, anl_aft_mdfc_pi_amt + ln_prin_frbrn_amt, anl_aft_mdfc_pi_amt),
    aft_mdfc_disc_cashflw = ifelse(aft_mdfc_yrsLeft != 0, anl_aft_mdfc_pi_amt * disct_fact, 0)
  )
# Sum up NPV cashflow
df_arm <-
  df_arm %>%
  group_by(fncl_ast_id) %>%
  mutate(NPV_aft_mod_cashflow = sum(aft_mdfc_disc_cashflw))

# Drop outliers. Figure out which fncl_ast_ids ever have a negative payment, and drop all rows with these loans.
if (sample_name == "es-base") {
  df_arm <-
    df_arm %>%
    group_by(fncl_ast_id) %>%
    mutate(
      min_anl_aft_mdfc_princ_pmt_amt = min(anl_aft_mdfc_princ_pmt_amt),
      min_anl_bef_mdfc_princ_pmt_amt = min(anl_bef_mdfc_princ_pmt_amt)
    ) %>%
    filter(min_anl_aft_mdfc_princ_pmt_amt >= 0 & min_anl_bef_mdfc_princ_pmt_amt >= 0) %>%
    select(-min_anl_aft_mdfc_princ_pmt_amt, -min_anl_bef_mdfc_princ_pmt_amt)
} else {
  df_arm <-
    df_arm %>%
    group_by(fncl_ast_id) %>%
    mutate(min_anl_aft_mdfc_princ_pmt_amt = min(anl_aft_mdfc_princ_pmt_amt)) %>%
    filter(min_anl_aft_mdfc_princ_pmt_amt >= 0) %>%
    select(-min_anl_aft_mdfc_princ_pmt_amt)
}

# Calculate actual principal and interest payments for each month, dollar reductions, and npv of these dollar reductions
df_arm <-
  df_arm %>%
  mutate(dNPV_cashflow = NPV_aft_mod_cashflow - NPV_bef_mod_cashflow)

## -------------------------------------------------------##
## Deal with Incomplete payoff post-mod with short terms ##
## ------------------------------------------------------##

# Issue: With post-mod terms less than five years, we assume the payment reported is accurate.
# But sometimes this is not enough (or is too much) to pay off the UPB, and sometimes we are way off.
# So drop these problem loans with implausbile values
df_upb_end <-
  df %>%
  filter(aft_mdfc_yrsLeft == 1) %>%
  mutate(final_ln_aft_mdfc_upb_amt_end = ln_aft_mdfc_upb_amt_end) %>%
  select(fncl_ast_id, final_ln_aft_mdfc_upb_amt_end)
df <- left_join(df, df_upb_end, by = "fncl_ast_id")
df <- df %>% filter(abs(final_ln_aft_mdfc_upb_amt_end) < 1000)
rm(df_upb_end)

df_arm_upb_end <-
  df_arm %>%
  filter(aft_mdfc_yrsLeft == 1) %>%
  mutate(final_ln_aft_mdfc_upb_amt_end = ln_aft_mdfc_upb_amt_end) %>%
  select(fncl_ast_id, final_ln_aft_mdfc_upb_amt_end)
df_arm <- left_join(df_arm, df_arm_upb_end, by = "fncl_ast_id")
df_arm <- df_arm %>% filter(abs(final_ln_aft_mdfc_upb_amt_end) < 1000)
rm(df_arm_upb_end)

## ------------------------------------------------##
## Calulate Summary Statistics Between the Groups ##
## ------------------------------------------------##

# Generate modification type indicators. modGroup assumes exact waterfall, so anybody with forbearance is in forbearance group, implictly also has first two steps.
# For now, applying to rateRed group based on calculated rate rather than HAMP reported rate.
df <-
  df %>%
  mutate(
    rateRed = ln_aft_mdfc_int_rate_dec_cal < ln_bef_mdfc_int_rt_calc_r,
    rateRed_hamp = ln_aft_mdfc_int_rate_dec_cal < ln_bef_mdfc_int_rt_hamp,
    termExtend = ln_aft_mdfc_rmng_term > ln_bef_mdfc_rmng_term,
    anyForborn = ln_prin_frbrn_amt > 0 & !is.na(ln_prin_frbrn_amt),
    anyForgiven = ln_prin_rdcn_altv_amt > 0 & !is.na(ln_prin_rdcn_altv_amt),
    modGroup = ifelse(anyForborn == TRUE, "anyForborn",
      ifelse(termExtend == TRUE, "termExtend", "rateRed")
    )
  )

# Make df with one row per mod
df_sum_frm <- df %>% filter(yrsPaid == 1)

# Calculate share receiving each type of intervention (not cumulative necesarilly), by PRA status
hamp_share <-
  df_sum_frm %>%
  ungroup() %>%
  filter(pra == FALSE) %>%
  summarise(
    rateRed = round(mean(rateRed) * 100, 0),
    rateRed_hamp = round(mean(rateRed_hamp) * 100, 0),
    termExtend = round(mean(termExtend) * 100, 0),
    anyForborn = round(mean(anyForborn) * 100, 0)
  )
hamp_share

# Summary Table
summary_dnpv_frm <-
  df_sum_frm %>%
  group_by(pra) %>%
  summarise(
    count = n(),
    mean_NPV_bef_cashflow = mean(NPV_bef_mod_cashflow),
    mean_NPV_aft_cashflow = mean(NPV_aft_mod_cashflow),
    median_NPV_bef_cashflow = median(NPV_bef_mod_cashflow),
    median_NPV_aft_cashflow = median(NPV_aft_mod_cashflow),
    mean_dNPV_cashflow = mean(dNPV_cashflow),
    median_dNPV_cashflow = median(dNPV_cashflow),
    mean_forgive_amount = mean(forgive_amount)
  )
summary_dnpv_frm

# Make summary data frame for fixed-rate-mortgages
df_sum_frm <-
  df_sum_frm %>%
  select(
    fncl_ast_id, pra, dnpv_pos, ln_bef_mdfc_upb_amt, ln_bef_mdfc_int_rt_hamp,
    NPV_bef_mod_cashflow, NPV_aft_mod_cashflow, dNPV_cashflow, forgive_amount,
    anl_bef_mdfc_pi_amt, anl_aft_mdfc_pi_amt
  ) %>%
  mutate(one_year_cashflow_change = anl_aft_mdfc_pi_amt - anl_bef_mdfc_pi_amt)
# Make summary data frame for ARMS
df_sum_arm <-
  df_arm %>%
  filter(yrsPaid == 1) %>%
  select(
    fncl_ast_id, pra, dnpv_pos, ln_bef_mdfc_upb_amt, ln_bef_mdfc_int_rt_hamp,
    NPV_bef_mod_cashflow, NPV_aft_mod_cashflow, dNPV_cashflow, forgive_amount,
    anl_bef_mdfc_pi_amt, anl_aft_mdfc_pi_amt
  ) %>%
  mutate(one_year_cashflow_change = anl_aft_mdfc_pi_amt - anl_bef_mdfc_pi_amt)
df_sum_all <- rbind(df_sum_frm, df_sum_arm)

# Make summary table with all loans
summary_dnpv_all <-
  df_sum_all %>%
  group_by(pra) %>%
  summarise(
    count = n(),
    mean_NPV_bef_cashflow = mean(NPV_bef_mod_cashflow),
    mean_NPV_aft_cashflow = mean(NPV_aft_mod_cashflow),
    median_NPV_bef_cashflow = median(NPV_bef_mod_cashflow),
    median_NPV_aft_cashflow = median(NPV_aft_mod_cashflow),
    mean_dNPV_cashflow = mean(dNPV_cashflow),
    median_dNPV_cashflow = median(dNPV_cashflow),
    mean_forgive_amount = mean(forgive_amount),
    mean_one_year_cashflow_change = mean(one_year_cashflow_change)
  )

if (sample_name == "es_base") {
  fit <- lm(df_sum_all$one_year_cashflow_change ~ df_sum_all$pra)
  se_one_year_cashflow_change <- summary(fit)$coefficients[2, 2]
  fit <- lm(df_sum_all$forgive_amount ~ df_sum_all$pra)
  se_forgive_amount <- summary(fit)$coefficients[2, 2]
  fit <- lm(df_sum_all$dNPV_cashflow ~ df_sum_all$pra)
  se_dNPV_cashflow <- summary(fit)$coefficients[2, 2]
}

# Make summary table just ARMS for comparison
summary_dnpv_arm <-
  df_sum_arm %>%
  group_by(pra) %>%
  summarise(
    count = n(),
    mean_NPV_bef_cashflow = mean(NPV_bef_mod_cashflow),
    mean_NPV_aft_cashflow = mean(NPV_aft_mod_cashflow),
    median_NPV_bef_cashflow = median(NPV_bef_mod_cashflow),
    median_NPV_aft_cashflow = median(NPV_aft_mod_cashflow),
    mean_dNPV_cashflow = mean(dNPV_cashflow),
    median_dNPV_cashflow = median(dNPV_cashflow),
    mean_forgive_amount = mean(forgive_amount)
  )

## ----------------------------------------------##
## Save data set with dNPV for each fncl_ast_id ##
## ----------------------------------------------##
dNPV_out <- df_sum_all %>% select(fncl_ast_id, NPV_bef_mod_cashflow, NPV_aft_mod_cashflow, dNPV_cashflow)
count(dNPV_out)
n_distinct(dNPV_out$fncl_ast_id)
saveRDS(dNPV_out, file = paste0(working_path, "dNPV_out_", sample_name, ".rds"))

## ------------------------------------------##
## GRAPH ANNUAL PAYMENT AND BALANCE CHANGES ##
## ------------------------------------------##

df <-
  df %>%
  ungroup() %>%
  mutate(
    pi_diff_amt_bef_yr40 = ifelse(yrsPaid < 40, pi_diff_amt, NA),
    pi_diff_amt_at_yr40 = ifelse(yrsPaid == 40, pi_diff_amt, NA),
    pi_diff_amt_winsor_95 = ifelse(yrsPaid < 40,
      winsor(pi_diff_amt_bef_yr40, 0, 0.95, pos_only = TRUE),
      winsor(pi_diff_amt_at_yr40, 0, 0.95, pos_only = TRUE)
    )
  )

if (sample_name %in% c("es_base", "es_base_delin_v3")) {
  df_anl <-
    df %>%
    ungroup() %>%
    select(
      yrsPaid, pra, anl_bef_pi, anl_aft_pi, ln_bef_mdfc_upb_amt_end,
      ln_aft_mdfc_upb_amt_end, pi_diff_amt, pi_diff_amt_winsor_95, npv_pi_diff_amt_cum
    ) %>%
    group_by(yrsPaid, pra) %>%
    summarise_each(funs(mean)) %>%
    mutate(
      ln_bef_mdfc_upb_amt_end = ln_bef_mdfc_upb_amt_end / 1000,
      ln_aft_mdfc_upb_amt_end = ln_aft_mdfc_upb_amt_end / 1000
    )
}

if (sample_name == "es_rd") {
  df_anl <-
    df %>%
    ungroup() %>%
    select(
      yrsPaid, dnpv_pos, anl_bef_pi, anl_aft_pi, ln_bef_mdfc_upb_amt_end,
      ln_aft_mdfc_upb_amt_end, pi_diff_amt, npv_pi_diff_amt_cum
    ) %>%
    group_by(yrsPaid, dnpv_pos) %>%
    summarise_each(funs(mean)) %>%
    mutate(
      ln_bef_mdfc_upb_amt_end = ln_bef_mdfc_upb_amt_end / 1000,
      ln_aft_mdfc_upb_amt_end = ln_aft_mdfc_upb_amt_end / 1000
    )
}

if ("piHamp5" %in% graph_name) {
  df_long <-
    df_anl %>%
    filter(pra == 0) %>%
    select(yrsPaid, anl_bef_pi, anl_aft_pi) %>%
    gather(Group, value, -yrsPaid)
  df_long <-
    df_long %>%
    mutate(Group = ifelse(Group == "anl_bef_pi", "Status Quo",
      ifelse(Group == "anl_aft_pi", "After Modification", "Error")
    ))

  gg <-
    ggplot(df_long, aes(x = yrsPaid, y = value, group = Group, colour = Group, shape = Group)) +
    geom_point() + geom_line() +
    fte_theme() +
    xlab("Years Since Modification") + ylab("Mean Annual Payment") +
    theme(legend.justification = c(0, 0), legend.position = c(0, 0)) +
    scale_colour_manual(values = cbPalette_drop1) +
    scale_y_continuous(labels = scales::dollar) +
    annotate("text", x = 4, y = 9000, label = paste("Lower Interest \n Rate"), family = "serif", size = 4) + # (", hamp_share$rateRed,"%)"), size=4)+
    annotate("text", x = 33, y = 7000, label = paste("Loan Term \n Extension"), family = "serif", size = 4) + # (", hamp_share$termExtend,"%)"), size=4)+
    annotate("text", x = 35, y = 12000, label = paste("Balloon \n Payment"), family = "serif", size = 4) # (", hamp_share$anyForborn,"%)"), size=4)
  ggsave(file = paste0(out_path_diagnosis, "piHamp5_", sample_name, ".png"), gg, width = 6, height = 4)
}

if ("piHamp5_no_shares" %in% graph_name) {
  df_long <-
    df_anl %>%
    filter(pra == 0) %>%
    select(yrsPaid, anl_bef_pi, anl_aft_pi) %>%
    gather(Group, value, -yrsPaid)
  df_long <-
    df_long %>%
    mutate(Group = ifelse(Group == "anl_bef_pi", "Status Quo",
      ifelse(Group == "anl_aft_pi", "After Modification", "Error")
    ))
  gg <-
    ggplot(df_long, aes(x = yrsPaid, y = value, group = Group, colour = Group, shape = Group)) +
    geom_point() + geom_line() +
    fte_theme() +
    xlab("Years Since Modification") + ylab("Mean Annual Payment") +
    theme(legend.justification = c(0, 0), legend.position = c(0, 0)) +
    scale_colour_manual(values = cbPalette_drop1) +
    scale_y_continuous(labels = scales::dollar) +
    annotate("text", x = 4, y = 9000, label = paste("Lower Interest \n Rate"), size = 4, family = "serif") +
    annotate("text", x = 33, y = 7000, label = paste("Loan Term \n Extension"), size = 4, family = "serif") +
    annotate("text", x = 35, y = 12000, label = paste("Balloon \n Payment"), size = 4, family = "serif")
  ggsave(file = paste0(out_path_diagnosis, "piHamp5_no_shares_", sample_name, ".png"), gg, width = 6, height = 4)
}

if ("dollar_red" %in% graph_name) {
  if (sample_name == "es_base") {
    # Dollar Reduction in Annual Payment
    df_long <-
      df_anl %>%
      select(yrsPaid, pra, pi_diff_amt, pi_diff_amt_winsor_95) %>%
      gather(Group, value, -yrsPaid, -pra)
    df_long <-
      df_long %>%
      mutate(
        mod = ifelse(pra == TRUE, "Treatment: Payment & Principal Reduction",
          ifelse(pra == FALSE, "Control: Payment Reduction Only", "Error")
        ),
        "Modification Type" = factor(mod, levels = c(
          "Treatment: Payment & Principal Reduction", "Control: Payment Reduction Only",
          "Error"
        ))
      )

    gg <-
      ggplot(df_long %>% filter(Group == "pi_diff_amt"), aes(
        x = yrsPaid, y = value,
        group = `Modification Type`,
        colour = `Modification Type`,
        shape = `Modification Type`
      )) +
      geom_point() + geom_line() +
      fte_theme() +
      xlab("Years Since Modification") + ylab("Change in Annual Payment") +
      theme(legend.title = element_blank(), legend.justification = c(0, 1), legend.position = c(0, 1)) +
      scale_colour_manual(values = c(cbPalette_blue[1], cbPalette_blue[2])) +
      scale_shape_manual(values = c(16, 17)) +
      scale_y_continuous(labels = scales::dollar) + theme(axis.title.y = element_text(size = 14)) +
      geom_hline(yintercept = 0, linetype = 2) +
      theme(legend.key = element_rect(colour = "white", linetype = "solid"))
    ggsave(file = paste0(out_path, "dollar_red_", sample_name, ".png"), gg, width = 6, height = 4)

    gg_winsor <-
      ggplot(df_long %>% filter(Group == "pi_diff_amt_winsor_95"), aes(
        x = yrsPaid, y = value,
        group = `Modification Type`,
        colour = `Modification Type`,
        shape = `Modification Type`
      )) +
      geom_point() + geom_line() +
      fte_theme() +
      xlab("Years Since Modification") + ylab("Change in Annual Payment") +
      theme(legend.title = element_blank(), legend.justification = c(0, 1), legend.position = c(0, 1)) +
      scale_colour_manual(values = c(cbPalette_blue[1], cbPalette_blue[2])) +
      scale_shape_manual(values = c(16, 17)) +
      scale_y_continuous(labels = scales::dollar) + theme(axis.title.y = element_text(size = 14)) +
      geom_hline(yintercept = 0, linetype = 2) +
      theme(legend.key = element_rect(colour = "white", linetype = "solid"))
    ggsave(file = paste0(out_path, "dollar_red_winsor_", sample_name, ".png"), gg_winsor, width = 6, height = 4)

    gg2 <-
      ggplot(
        df_long %>% filter(pra == FALSE, Group == "pi_diff_amt"),
        aes(x = yrsPaid, y = value, group = `Modification Type`, colour = `Modification Type`, shape = `Modification Type`)
      ) +
      geom_point() + geom_line() +
      fte_theme() +
      xlab("Years Since Modification") + ylab("Change in Annual Payment") +
      theme(legend.title = element_blank(), legend.justification = c(0, 1), legend.position = c(0, 1)) +
      scale_colour_manual(values = c(cbPalette_blue[2], cbPalette_blue[1])) +
      scale_shape_manual(values = c(17, 16)) +
      scale_y_continuous(labels = scales::dollar) + theme(axis.title.y = element_text(size = 14)) +
      geom_hline(yintercept = 0, linetype = 2) +
      theme(legend.key = element_rect(colour = "white", linetype = "solid"))
    ggsave(file = paste0(out_path, "slide/", "dollar_red_control_", sample_name, ".png"), gg2, width = 6, height = 4)

    gg2_winsor <-
      ggplot(
        df_long %>% filter(pra == FALSE, Group == "pi_diff_amt_winsor_95"),
        aes(x = yrsPaid, y = value, group = `Modification Type`, colour = `Modification Type`, shape = `Modification Type`)
      ) +
      geom_point() + geom_line() +
      fte_theme() +
      xlab("Years Since Modification") + ylab("Change in Annual Payment") +
      theme(legend.title = element_blank(), legend.justification = c(0, 1), legend.position = c(0, 1)) +
      scale_colour_manual(values = c(cbPalette_blue[2], cbPalette_blue[1])) +
      scale_shape_manual(values = c(17, 16)) +
      scale_y_continuous(labels = scales::dollar) + theme(axis.title.y = element_text(size = 14)) +
      geom_hline(yintercept = 0, linetype = 2) +
      theme(legend.key = element_rect(colour = "white", linetype = "solid"))
    ggsave(file = paste0(out_path, "slide/", "dollar_red_control_winsor_", sample_name, ".png"), gg2_winsor, width = 6, height = 4)

    labels <- c("Lower Temp \nInterest Rate", "Extend \nMaturity", "Forbear \nPrincipal")
    x_pos <- c(4, 32, 36)
    y_pos <- c(-2500, -1000, 6000)

    gg_i <- gg2
    for (i in 1:3) {
      gg_i <- gg_i + annotate("text",
        y = y_pos[i], x = x_pos[i], label = labels[i], size = 4.5, family = "serif",
        color = brewer.pal("Greys", n = 9)[7]
      )
      file_name <- paste0(out_path, "slide/", "dollar_red_control_", sample_name, "_", i, "_of_", 3, ".png")
      ggsave(file_name, gg_i, width = 6, height = 4)
    }

    gg_i_winsor <- gg2_winsor
    for (i in 1:3) {
      gg_i_winsor <- gg_i_winsor + annotate("text",
        y = y_pos[i], x = x_pos[i], label = labels[i], size = 4.5, family = "serif",
        color = brewer.pal("Greys", n = 9)[7]
      )
      file_name <- paste0(out_path, "slide/", "dollar_red_control_winsor_", sample_name, "_", i, "_of_", 3, ".png")
      ggsave(file_name, gg_i_winsor, width = 6, height = 4)
    }
  }

  if (sample_name == "es_rd") {
    # Dollar Reduction in Annual Payment
    df_long <-
      df_anl %>%
      select(yrsPaid, dnpv_pos, pi_diff_amt) %>%
      gather(Group, value, -yrsPaid, -dnpv_pos) %>%
      mutate(
        pos = ifelse(dnpv_pos == TRUE, "Above Discontinuity",
          ifelse(dnpv_pos == FALSE, "Below Discontinuity", "Error")
        ),
        "Delta NPV from Principal Reduction over Payment Reduction Mod" =
          factor(pos, levels = c("Above Discontinuity", "Below Discontinuity"))
      )

    gg <-
      ggplot(df_long, aes(
        x = yrsPaid, y = value,
        group = `Delta NPV from Principal Reduction over Payment Reduction Mod`,
        colour = `Delta NPV from Principal Reduction over Payment Reduction Mod`,
        shape = `Delta NPV from Principal Reduction over Payment Reduction Mod`
      )) +
      geom_point() + geom_line() +
      fte_theme() +
      xlab("Years Since Modification") + ylab("Change in Annual Payment") +
      theme(legend.title = element_blank(), legend.justification = c(0, 1), legend.position = c(0, 1)) +
      scale_colour_manual(values = cbPalette_blue) +
      scale_y_continuous(labels = scales::dollar) +
      geom_hline(yintercept = 0, linetype = 2) +
      theme(legend.key = element_rect(colour = "gray", size = 0, linetype = "solid"))
    ggsave(file = paste0(out_path_diagnosis, "dollar_red_", sample_name, ".png"), gg, width = 6, height = 4)
  }
}

if ("fin_impacts_all" %in% graph_name) {
  # Cumulative NPV of Dollar Reductions selected years and change in Face Value, with ARMS
  summary_dnpv_all <-
    summary_dnpv_all %>%
    mutate(
      mean_one_year_cashflow_change = mean_one_year_cashflow_change * -1,
      mean_dNPV_cashflow = mean_dNPV_cashflow * -1
    )

  if (sample_name == "es_base") {
    summary_dnpv_all_long <-
      summary_dnpv_all %>%
      select(pra, mean_one_year_cashflow_change, mean_forgive_amount, mean_dNPV_cashflow) %>%
      gather(value, amount, -pra) %>%
      mutate(
        value2 = ifelse(value == "mean_one_year_cashflow_change", "1-year Payment Reduction",
          ifelse(value == "mean_forgive_amount", "Balance Due Reduction",
            "Reduction in NPV of payments owed at 4% discount rate"
          )
        ),
        mod = ifelse(pra == TRUE, "Treatment: Payment & Principal Reduction",
          ifelse(pra == FALSE, "Control: Payment Reduction Only", "Error")
        ),
        "Modification Type" = factor(mod, levels = c(
          "Treatment: Payment & Principal Reduction",
          "Control: Payment Reduction Only"
        )),
        amount_se = ifelse(value == "mean_one_year_cashflow_change" & pra == TRUE, se_one_year_cashflow_change,
          ifelse(value == "mean_forgive_amount" & pra == TRUE, se_forgive_amount,
            ifelse(value == "mean_dNPV_cashflow" & pra == TRUE, se_dNPV_cashflow, NA)
          )
        )
      )

    gg <-
      ggplot(
        data = summary_dnpv_all_long %>%
          mutate(value3 = factor(value2, levels = c(
            "1-year Payment Reduction", "Balance Due Reduction",
            "Reduction in NPV of payments owed at 4% discount rate"
          ))),
        aes(x = value3, y = amount, ymin = amount - 1.96 * amount_se, ymax = amount + 1.96 * amount_se, fill = `Modification Type`)
      ) +
      geom_bar(stat = "identity", position = position_dodge()) +
      xlab(" ") +
      ylab(" ") +
      fte_theme() +
      theme(
        legend.title = element_blank(), legend.justification = c(0, 1), legend.position = c(0, 1),
        legend.key.size = unit(0.2, "in")
      ) +
      scale_fill_manual(values = cbPalette_blue) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
      scale_y_continuous(labels = dollar, breaks = c(0, 50000, 100000), limits = c(0, 120000))
    ggsave(file = paste0(out_path, "fin_impacts_all", sample_name, ".png"), gg, width = 6, height = 4)

    gg_slide <- ggplot(
      data = summary_dnpv_all_long %>%
        mutate(value3 = factor(value2, levels = c(
          "Reduction in NPV of payments owed at 4% discount rate", "Balance Due Reduction",
          "1-year Payment Reduction"
        ))),
      aes(x = value3, y = amount, ymin = amount - 1.96 * amount_se, ymax = amount + 1.96 * amount_se, fill = `Modification Type`, 40)
    ) +
      geom_bar(stat = "identity", position = position_dodge(-0.9)) +
      xlab(" ") +
      ylab(" ") +
      fte_theme() +
      theme(legend.title = element_blank(), legend.justification = c(0, 1), legend.position = c(0, 1)) +
      scale_fill_manual(values = cbPalette_blue) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      scale_y_continuous(labels = dollar, limits = c(0, 105000), oob = rescale_none, breaks = seq(0, 100000, by = 50000)) +
      theme(
        legend.key = element_rect(colour = "white", linetype = "solid"),
        legend.position = "bottom", panel.grid.major = element_blank(), panel.grid.minor = element_blank()
      ) +
      coord_flip() + guides(fill = guide_legend(nrow = 2, byrow = TRUE))
    ggsave(file = paste0(out_path, "slide/", "fin_impacts_all", sample_name, "_slide.png"), gg_slide, width = 6, height = 4)


    summary_dnpv_all_long <-
      summary_dnpv_all %>%
      select(pra, mean_one_year_cashflow_change, mean_dNPV_cashflow) %>%
      gather(value, amount, -pra) %>%
      mutate(
        value2 = ifelse(value == "mean_one_year_cashflow_change", "% Reduction in 1-year Payment", "% Reduction in NPV of payments owed at 4% discount rate"),
        value3 = factor(value2, levels = c("% Reduction in NPV of payments owed at 4% discount rate", "% Reduction in 1-year Payment")),
        mod = ifelse(pra == TRUE, "Treatment: Payment & Principal Reduction",
          ifelse(pra == FALSE, "Control: Payment Reduction Only", "Error")
        ),
        "Modification Type" = factor(mod, levels = c(
          "Treatment: Payment & Principal Reduction",
          "Control: Payment Reduction Only",
          "Error"
        )),
        amount_se = ifelse(value == "mean_one_year_cashflow_change" & pra == TRUE, se_one_year_cashflow_change,
          ifelse(value == "mean_dNPV_cashflow" & pra == TRUE, se_dNPV_cashflow, NA)
        ),
        amount = ifelse(value == "mean_one_year_cashflow_change" & pra == TRUE, (amount) / (1594 * 12), amount),
        amount = ifelse(value == "mean_one_year_cashflow_change" & pra == FALSE, (amount) / (1727 * 12), amount)
      )

    NPV_pre_mod <- 1594 * 12 * ((1 - 0.96^26) / (1 - 0.96))
    summary_dnpv_all_long <-
      summary_dnpv_all_long %>%
      mutate(amount = ifelse(value == "mean_dNPV_cashflow" & pra == TRUE, (amount) / NPV_pre_mod, amount))
    NPV_pre_mod <- 1727 * 12 * ((1 - 0.96^26) / (1 - 0.96))
    summary_dnpv_all_long <-
      summary_dnpv_all_long %>%
      mutate(amount = ifelse(value == "mean_dNPV_cashflow" & pra == FALSE, (amount) / NPV_pre_mod, amount))


    gg2 <- ggplot(data = summary_dnpv_all_long,
                  aes(x = value3, y = amount,
                      ymin = amount - 1.96 * amount_se, ymax = amount + 1.96 * amount_se,
                      fill = `Modification Type`, 40)) +
      geom_bar(stat = "identity", position = position_dodge(-0.9)) +
      xlab(" ") +
      ylab(" ") +
      fte_theme() +
      scale_fill_manual(values = cbPalette_blue) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
      scale_y_continuous(labels = percent, limits = c(0, 0.5)) +
      theme(
        legend.key = element_rect(colour = "white", linetype = "solid"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom"
      ) +
      coord_flip() + guides(fill = guide_legend(nrow = 2, byrow = TRUE))
    ggsave(file = paste0(out_path_diagnosis, "fin_impacts_all", sample_name, "_horizontal.png"),
           gg2, width = 6, height = 4)
  }

  if (sample_name == "es_rd") {
    summary_dnpv_all_long <-
      output_fin_impacts %>%
      mutate(
        pos = ifelse(pos == "above", "Treatment: Above Discontinuity",
          ifelse(pos == "below", "Control: Below Discontinuity", "Error")
        ),
        value2 = ifelse(dv == "dpay", "1-year Payment Reduction",
          ifelse(dv == "ln_upb_frgv_amt.x", "Balance Due Reduction",
            "NPV Total Payment Reduction at 4% Discount Rate"
          )
        ),
        `Delta NPV from Principal Reduction \n over Payment Reduction Mod` =
          factor(pos, levels = c("Treatment: Above Discontinuity", "Control: Below Discontinuity")),
        value3 = factor(value2, levels = c(
          "1-year Payment Reduction",
          "Balance Due Reduction",
          "NPV Total Payment Reduction at 4% Discount Rate"
        ))
      )

    gg <- ggplot(data = summary_dnpv_all_long, aes(
      x = value3, y = amount, ymin = amount - 1.96 * amount_se, ymax = amount + 1.96 * amount_se,
      fill = `Delta NPV from Principal Reduction \n over Payment Reduction Mod`
    )) +
      geom_bar(stat = "identity", position = position_dodge()) +
      xlab(" ") +
      ylab(" ") +
      fte_theme() +
      theme(
        legend.title = element_blank(), legend.justification = c(0, 1), legend.position = c(0, 1),
        legend.text = element_text(size = 7)
      ) +
      scale_fill_manual(values = cbPalette_blue) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
      scale_y_continuous(labels = scales::dollar) +
      theme(legend.key = element_rect(colour = "gray", size = 0, linetype = "solid"))
    ggsave(file = paste0(out_path_diagnosis, "fin_impacts_all", sample_name, ".png"), gg, width = 6, height = 4)
  }
}

if (sample_name == "es_base") {
  # Get average payment stream
  df_summary <-
    df %>%
    mutate(
      anl_bef_mdfc_pi_amt_net = ifelse(bef_mdfc_yrsLeft != 0, anl_bef_mdfc_pi_amt, 0),
      anl_aft_mdfc_pi_amt_net = ifelse(aft_mdfc_yrsLeft != 0, anl_aft_mdfc_pi_amt, 0)
    ) %>%
    group_by(yrsPaid, pra) %>%
    summarise(
      pmt_bef = mean(anl_bef_mdfc_pi_amt_net),
      pmt_aft = mean(anl_aft_mdfc_pi_amt_net),
      upb_bef = mean(ln_bef_mdfc_upb_amt_end),
      upb_aft = mean(ln_aft_mdfc_upb_amt_end)
    )

  dnpv_411 <-
    df_summary %>%
    mutate(
      disct_fact = 1 / (1 + 0.0411)^yrsPaid,
      dnpv = (pmt_bef - pmt_aft) * disct_fact
    ) %>%
    group_by(pra) %>%
    summarise(sum = sum(dnpv))

  dnpv_822 <-
    df_summary %>%
    mutate(
      disct_fact = 1 / (1 + 0.0822)^yrsPaid,
      dnpv = (pmt_bef - pmt_aft) * disct_fact
    ) %>%
    group_by(pra) %>%
    summarise(sum = sum(dnpv))

  df_summary_treat <- df_summary %>% filter(pra == TRUE)
  df_summary_ctrl <- df_summary %>% filter(pra == FALSE)

  disct_rate_1000 <- uniroot(function(x) {
    sum(((df_summary_treat$pmt_bef - df_summary_treat$pmt_aft) -
      (df_summary_ctrl$pmt_bef - df_summary_ctrl$pmt_aft)) *
      (1 / (1 + x)^c(1:40))) - 1000
  },
  c(0.01, 0.99),
  extendInt = "no"
  )$root

  source("./func/npv_calc_funcs.R")
  assumps <- list(
    prepay_vec = rep(0, 40),
    disc_vec = 1 / (1 + 0.0411)^c(1:40),
    mtmval = 160000,
    p_dflt = 0.46,
    recovery = "baseline"
  )

  calc_npv_frm_terms <- function(pmt, upb, assumps) {
    pays <- pmt
    upbs <- upb

    prepay_vec <- assumps$prepay_vec
    disc_vec <- assumps$disc_vec
    mtmval <- assumps$mtmval
    p_dflt <- assumps$p_dflt
    recovery <- assumps$recovery

    npv_if_cure <- calc_npv_cure(pays, upbs, prepay_vec, disc_vec)
    npv_if_dflt <- calc_npv_dflt(pays, upbs, prepay_vec, disc_vec, mtmval, recovery = recovery)
    npv <- calc_npv_invest(npv_if_cure, npv_if_dflt, p_dflt)

    return(npv)
  }

  dnpv_dflt <- (calc_npv_frm_terms(df_summary_treat$pmt_bef, df_summary_treat$upb_bef, assumps) -
    calc_npv_frm_terms(df_summary_treat$pmt_aft, df_summary_treat$upb_aft, assumps)) -
    (calc_npv_frm_terms(df_summary_ctrl$pmt_bef, df_summary_ctrl$upb_bef, assumps) -
      calc_npv_frm_terms(df_summary_ctrl$pmt_aft, df_summary_ctrl$upb_aft, assumps))

  summary_dnpv <-
    data.frame(row = "Discount payments owed by 4.11%", num = diff(dnpv_411$sum)) %>%
    add_row(row = "Discount payments owed by 8.22% (double)", num = diff(dnpv_822$sum)) %>%
    add_row(row = "Discount rate needed to make the dNPV be $1000", num = disct_rate_1000) %>%
    add_row(row = "Five year 90-day default rates of 46% and self-cure rates of 31%", num = dnpv_dflt)

  x <- xtable(summary_dnpv, auto = TRUE)
  digit_matrix <- matrix(c(
    rep(0, 3),
    rep(0, 3),
    0, 0, 4,
    rep(0, 3)
  ), nrow = 4, ncol = 3, byrow = TRUE)
  digits(x) <- digit_matrix

  # Add column headers and output
  addtorow <- list()
  addtorow$pos <- list(0)
  addtorow$command <- paste0("Row & Number", "\\")
  print(x,
    add.to.row = addtorow, format.args = list(big.mark = ","), include.colnames = F, include.rownames = FALSE, floating = FALSE, auto = TRUE,
    hline.after = c(-1, -1, 0, nrow(x) - 1, nrow(x), nrow(x)),
    file = str_c(out_path_diagnosis, "summary_dnpv.tex"), booktabs = TRUE
  )
}

print(proc.time() - ptm)
rm(list = ls(pattern = "^summary"))
rm(list = ls(pattern = "^share"))
rm(dNPV_out, hamp_share)
