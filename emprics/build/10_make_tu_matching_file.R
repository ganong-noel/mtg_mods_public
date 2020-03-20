## ---------------------------------------#
## Load in combined_mortgage_file_LMDs ####
## ---------------------------------------#
combined_mortgage_file_LMDs <- readRDS(paste0(working_path, "combined_mortgage_file_LMDs.rds"))

## ---------------------------------------------------------------------------------#
## Add Monthly Mortgage Payment and Unpaid Principal Balance Before Modification ####
## ---------------------------------------------------------------------------------#

combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  group_by(bcs_seqnum, act_id) %>%
  arrange(ver_dt) %>%
  mutate(pay_due_am_bef_mdfc = lag(th_pay_due_am, 4))

# Go back and fill in NAs
for (ind in c(5, 6, 7, 3, 2, 1)) {
  combined_mortgage_file_LMDs <-
    combined_mortgage_file_LMDs %>%
    ungroup() %>%
    group_by(bcs_seqnum, act_id) %>%
    arrange(ver_dt) %>%
    mutate(pay_due_am_bef_mdfc = ifelse(is.na(pay_due_am_bef_mdfc),
                                        lag(th_pay_due_am, ind),
                                        pay_due_am_bef_mdfc
    ))
}

## ------------------------------------------#
## Add Unpaid Balance Before Modification ####
## ------------------------------------------#

combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  group_by(bcs_seqnum, act_id) %>%
  arrange(ver_dt) %>%
  mutate(cur_bal_am_bef_mdfc = lag(th_cur_bal_am, 4))

# Go back and fill in NAs
for (ind in c(5, 6, 7, 3, 2, 1)) {
  combined_mortgage_file_LMDs <-
    combined_mortgage_file_LMDs %>%
    ungroup() %>%
    group_by(bcs_seqnum, act_id) %>%
    arrange(ver_dt) %>%
    mutate(cur_bal_am_bef_mdfc = ifelse(is.na(cur_bal_am_bef_mdfc),
                                        lag(th_cur_bal_am, ind),
                                        cur_bal_am_bef_mdfc
    ))
}



## ----------------------------------------------------------------#
## Add Monthly Payment and Principal Balance After Modification ####
## ----------------------------------------------------------------#

no_pay_change_file <-
  combined_mortgage_file_LMDs %>%
  filter(ver_dt == first_LMD_date) %>%
  mutate(no_pay_change = th_pay_due_am == pay_due_am_bef_mdfc) %>%
  select(bcs_seqnum, act_id, no_pay_change)

combined_mortgage_file_LMDs <-
  left_join(combined_mortgage_file_LMDs,
    no_pay_change_file,
    by = c("bcs_seqnum", "act_id")
  )

# PART 1: Replace payment due before mod as max of prior.
alt_prior_pay <-
  combined_mortgage_file_LMDs %>%
  filter(no_pay_change == TRUE,
         ver_dt <= first_LMD_date) %>%
  group_by(bcs_seqnum, act_id) %>%
  mutate(pay_due_am_bef_mdfc_alt = max(th_pay_due_am, na.rm = TRUE)) %>%
  select(bcs_seqnum, act_id, ver_dt, pay_due_am_bef_mdfc_alt)


combined_mortgage_file_LMDs <-
  left_join(combined_mortgage_file_LMDs,
    alt_prior_pay,
    by = c("bcs_seqnum", "act_id", "ver_dt")
  ) %>%
  as.data.frame() %>%
  mutate(pay_due_am_bef_mdfc_alt = ifelse(no_pay_change == FALSE,
                                          pay_due_am_bef_mdfc,
                                          pay_due_am_bef_mdfc_alt))



no_pay_change2_file <-
  combined_mortgage_file_LMDs %>%
  filter(ver_dt == first_LMD_date) %>%
  mutate(no_pay_change2 = th_pay_due_am == pay_due_am_bef_mdfc_alt) %>%
  select(bcs_seqnum, act_id, no_pay_change2)

combined_mortgage_file_LMDs <-
  left_join(combined_mortgage_file_LMDs,
    no_pay_change2_file,
    by = c("bcs_seqnum", "act_id")
  )

# PART2: Replace payment after modification with next month, or following month
combined_mortgage_file_LMDs <- as.data.frame(combined_mortgage_file_LMDs)

alt_post_pay <-
  combined_mortgage_file_LMDs %>%
  ungroup() %>%
  filter(no_pay_change2 == TRUE) %>%
  arrange(bcs_seqnum, act_id, ver_dt) %>%
  mutate(pay_due_am_aft_mdfc_alt = ifelse(act_id != lead(act_id, 1) | act_id != lead(act_id, 2), NA,
    ifelse(th_pay_due_am == lead(th_pay_due_am, 1), lead(th_pay_due_am, 2),
      lead(th_pay_due_am, 1)
    )
  )) %>%
  select(bcs_seqnum, act_id, ver_dt, pay_due_am_aft_mdfc_alt)

combined_mortgage_file_LMDs <-
  left_join(combined_mortgage_file_LMDs %>% ungroup(),
    alt_post_pay,
    by = c("bcs_seqnum", "act_id", "ver_dt")
  ) %>%
  mutate(pay_due_am_aft_mdfc_alt = ifelse(no_pay_change2 == FALSE, th_pay_due_am, pay_due_am_aft_mdfc_alt))

no_pay_change3_file <-
  combined_mortgage_file_LMDs %>%
  filter(ver_dt == first_LMD_date) %>%
  mutate(no_pay_change3 = pay_due_am_aft_mdfc_alt == pay_due_am_bef_mdfc_alt) %>%
  select(bcs_seqnum, act_id, no_pay_change3)

rm(no_pay_change2_file)
rm(no_pay_change3_file)
rm(alt_prior_pay)
rm(alt_post_pay)

## -------------------------------------------------------#
## Add Delinquency Status and Delinquency Date pre LMD ####
## -------------------------------------------------------#

delin_at_mod_file <-
  combined_mortgage_file_LMDs %>%
  mutate(delin_at_mod = ifelse(months_pre_LMD <= 4 | ver_dt != first_LMD_date, NA, ifelse(
    lag(th_mop, 1) == "01" & lag(th_mop, 2) == "01" & lag(th_mop, 3) == "01", FALSE, TRUE
  ))) %>%
  filter(ver_dt == first_LMD_date) %>%
  select(bcs_seqnum, act_id, delin_at_mod)

combined_mortgage_file_LMDs <-
  left_join(combined_mortgage_file_LMDs,
    delin_at_mod_file,
    by = c("bcs_seqnum", "act_id")
  )

# For last_curr_date, want to lag three months, need these in terms of Monnbs
combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  mutate(
    ver_dt_N = ymd(as.character(ver_dt * 100 + 1)),
    first_LMD_date_N = ymd(as.character(first_LMD_date * 100 + 1)),
    ver_dt_monnb = monnb(as.Date(ver_dt_N)),
    first_LMD_date_monnb = monnb(as.Date(first_LMD_date_N))
  )

last_curr_date_file <-
  combined_mortgage_file_LMDs %>%
  filter(delin_at_mod == TRUE & ver_dt_monnb < first_LMD_date_monnb - 3 & th_mop == "01") %>%
  group_by(bcs_seqnum, act_id) %>%
  summarise(last_curr_date = max(ver_dt))

combined_mortgage_file_LMDs <-
  left_join(combined_mortgage_file_LMDs,
    last_curr_date_file,
    by = c("bcs_seqnum", "act_id")
  )

## ----------------------------------------------#
## Add Information about Re-default After Mod ####
## ----------------------------------------------#
delin_samp <-
  combined_mortgage_file_LMDs %>%
  filter(th_mop %in% c("04", "05", "09", "9B", "9P", "UR"))

first_90dpd <-
  delin_samp %>%
  filter(th_mop == "04") %>%
  group_by(bcs_seqnum, act_id) %>%
  summarise(first_90dpd = min(ver_dt_monnb))

first_other_delin <-
  delin_samp %>%
  filter(th_mop != "04") %>%
  group_by(bcs_seqnum, act_id) %>%
  summarise(first_other_delin = min(ver_dt_monnb))

delin_samp <- left_join(delin_samp, first_90dpd, by = c("bcs_seqnum", "act_id")) %>%
  left_join(first_other_delin, by = c("bcs_seqnum", "act_id")) %>%
  mutate(other_minus_90dpd = first_other_delin - first_90dpd)


rm(delin_samp, first_90dpd, first_other_delin)

post_LMD_delin <-
  combined_mortgage_file_LMDs %>%
  filter(ver_dt > first_LMD_date & th_mop %in% c("04", "05")) %>%
  group_by(bcs_seqnum, act_id) %>%
  summarise(disqual_date_tu_monnb = min(ver_dt_monnb))


combined_mortgage_file_LMDs <-
  left_join(combined_mortgage_file_LMDs,
    post_LMD_delin,
    by = c("bcs_seqnum", "act_id")
  )

rm(post_LMD_delin)

combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  mutate(
    any_disqual_tu = ifelse(is.na(disqual_date_tu_monnb), FALSE, TRUE),
    mos_to_disqual_tu = disqual_date_tu_monnb - first_LMD_date_monnb,
    disqual_nxt_yr_tu = ifelse(is.na(mos_to_disqual_tu) | mos_to_disqual_tu > 12, FALSE, TRUE),
    disqual_two_yr_tu = ifelse(is.na(mos_to_disqual_tu) | mos_to_disqual_tu > 24, FALSE, TRUE)
  ) %>%
  group_by(bcs_seqnum, act_id) %>%
  mutate(months_post_LMD = max(ver_dt_monnb - first_LMD_date_monnb))


## -----------------#
## Find PRA Mods ####
## -----------------#

combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  mutate(mos_since_LMD = ver_dt_monnb - first_LMD_date_monnb) %>%
  ungroup()

# (1) Calculate balance change as monthly change if observed in subsequent months
combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  mutate(cur_bal_change = ifelse(ver_dt_monnb - lag(ver_dt_monnb, 1) == 1,
                                 th_cur_bal_am - lag(th_cur_bal_am, 1),
                                 NA))

# (2) Calculate baseline balance change as mean of months 3-5 after mod
combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  ungroup() %>%
  mutate(base_bal_change_tmp = ifelse(mos_since_LMD %in% c(3, 4, 5), cur_bal_change, NA)) %>%
  group_by(bcs_seqnum, act_id) %>%
  mutate(base_bal_change = mean(base_bal_change_tmp, na.rm = TRUE))

# (3) Calculate excess balance change as highest of months 8-12 relative to baseline
combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  ungroup() %>%
  mutate(excess_bal_change_tmp = ifelse(mos_since_LMD %in% c(8, 9, 10, 11, 12), cur_bal_change, 0)) %>%
  group_by(bcs_seqnum, act_id) %>%
  mutate(excess_bal_change = min(excess_bal_change_tmp - base_bal_change))

summary(combined_mortgage_file_LMDs$excess_bal_change)

# (4) define as PRA any excess balance paydown more than $3,000.
combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  mutate(pra_tu = excess_bal_change < (-3000))

combined_mortgage_file_LMDs <- combined_mortgage_file_LMDs %>%
  select(-base_bal_change_tmp, -excess_bal_change_tmp)
mem_used()
rm(delin_at_mod_file)
rm(last_curr_date_file)


## ----------------------------------------------#
## Collapse to One Row Per Mortgage and Clean ####
## ----------------------------------------------#
tu_match_file <- combined_mortgage_file_LMDs %>% filter(ver_dt == first_LMD_date)
rm(combined_mortgage_file_LMDs)

# Taking out people where pre-mod payment same as post-mod payment
tu_match_file <- tu_match_file %>%
  filter(pay_due_am_aft_mdfc_alt != pay_due_am_bef_mdfc_alt)


# Now drop any remaining NAs, end up with totally clean file for matching at least on this side.
# Not dropping bir_dt bc not using for matching in the end since missing in HAMP.
tu_match_file <-
  tu_match_file %>%
  filter(!is.na(th_cur_bal_am) &
    !is.na(th_pay_due_am) &
    !is.na(pay_due_am_bef_mdfc) &
    !is.na(cur_bal_am_bef_mdfc))
## --------------------------------#
## Add MSA and State Identifier ####
## --------------------------------#

zip_to_cbsa <- read.csv(paste0(data_path, "MSA/geocorr2k_zip_to_cbsa_with_ind.csv"))
zip_to_cbsa <- zip_to_cbsa[-1, ]
zip_to_cbsa <-
  zip_to_cbsa %>%
  mutate(
    zip = zcta5,
    cbsa = as.numeric(as.character(cbsa)),
    zip = as.numeric(as.character(zip)),
    afact = as.numeric(as.character(afact))
  ) %>%
  select(-zcta5, -pop2k) %>%
  filter(!is.na(zip))

# Keep only Metro MSAs
zip_to_cbsa <- zip_to_cbsa %>%
  filter(cbsatype == "Metro")

rm(obs_by_zip)

# For zips with two(or more) MSAs, keep the one with higher pop_share
zip_to_cbsa <-
  zip_to_cbsa %>%
  group_by(zip) %>%
  mutate(max_pop_share = max(afact)) %>%
  filter(afact == max_pop_share)

tu_match_file <- left_join(tu_match_file, zip_to_cbsa, by = "zip")


# Add State ID
zip_to_state <- read.csv(paste0(data_path, "MSA/geocorr2k_zip_to_state.csv"))
zip_to_state <-
  zip_to_state[-1, ] %>%
  mutate(
    zip = zcta5,
    state = stab,
    zip = as.numeric(as.character(zip)),
    afact = as.numeric(as.character(afact))
  ) %>%
  select(-zcta5, -stab, -pop2k) %>%
  filter(!is.na(zip)) %>%
  group_by(zip) %>%
  mutate(max_pop_share = max(afact)) %>%
  filter(afact == max_pop_share)

tu_match_file <-
  left_join(
    tu_match_file,
    zip_to_state %>% select(zip, state),
    by = "zip"
  )

# Are there any zips in TU where state is missing?
missing_state <- tu_match_file %>%
  filter(is.na(state)) %>%
  select(zip, cbsa, state)

# Bring in 2012 Geocorr (with 2010 zips) to try and fill in missing values
zip_to_state <- read.csv(paste0(data_path, "MSA/geocorr12_zip_to_state.csv"))

zip_to_state <-
  zip_to_state[-1, ] %>%
  mutate(
    zip = zcta5,
    state = stab,
    zip = as.numeric(as.character(zip)),
    afact = as.numeric(as.character(afact))
  ) %>%
  select(-zcta5, -stab, -pop10) %>%
  filter(!is.na(zip)) %>%
  group_by(zip) %>%
  mutate(max_pop_share = max(afact)) %>%
  filter(afact == max_pop_share) %>%
  mutate(state10 = state) %>%
  select(zip, state10)

tu_match_file <- left_join(tu_match_file, zip_to_state, by = "zip")

str(tu_match_file$state10)

# Now rename state as state10 if missing.
tu_match_file <-
  tu_match_file %>%
  mutate(
    state = as.character(state),
    state10 = as.character(state10),
    state = ifelse(is.na(state), state10, state)
  ) %>%
  select(-state10)

system.time(saveRDS(tu_match_file, file = paste0(working_path, "tu_match_file.rds")))

rm(tu_match_file)
rm(zip_to_cbsa)
rm(zip_to_state)
