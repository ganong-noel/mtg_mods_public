if (matched == "Yes") {
  hamp_match_file <- readRDS(paste0(working_path, "hamp_match_file_clean.rds"))
  tu_match_file <- readRDS(paste0(working_path, "tu_match_file_clean.rds"))
} else if (matched == "No") {
  hamp_match_file <- readRDS(paste0(working_path, "hamp_match_file_clean_um.rds"))
}

## ----------------------------------------------------------------------#
## Summarize matching variables in each file, check for remaining NAs ####
## ----------------------------------------------------------------------#

##
# TU
##
if (matched == "Yes") {
  tu_match_file <-
    tu_match_file %>%
    filter(
      pay_due_am_bef_mdfc < 25000,
      th_pay_due_am < 25000,
      cur_bal_am_bef_mdfc < 1500000,
      pay_due_am_bef_mdfc > 0,
      th_pay_due_am > 0,
      cur_bal_am_bef_mdfc > 0
    )
}

if (matched == "Yes") {
  hamp_match_file <- hamp_match_file %>% filter(!is.na(msa_state))
}
hamp_match_file <-
  hamp_match_file %>%
  filter(
    !is.na(ln_bef_mdfc_tot_pmt),
    !is.na(ln_bef_mdfc_upb_amt),
    ln_bef_mdfc_tot_pmt < 25000,
    ln_bef_mdfc_upb_amt > 0
  )

## -------------------------------------------------------------#
## Calculate sd, create normalizd vars, and consistent names ####
## -------------------------------------------------------------#

##
## Calculate standard deviationof matching vars
##

if (matched == "Yes") {
  tu_sd_file <-
    tu_match_file %>%
    summarise(
      opn_dt_sd = sd(opn_dt),
      LMD_cohort_sd = sd(LMD_cohort),
      pay_due_am_bef_mdfc_sd = sd(pay_due_am_bef_mdfc),
      th_pay_due_am_sd = sd(th_pay_due_am),
      cur_bal_am_bef_mdfc_sd = sd(cur_bal_am_bef_mdfc)
    )
}
hamp_sd_file <-
  hamp_match_file %>%
  summarise(
    ln_orig_note_dt_sd = sd(ln_orig_note_dt),
    mdfcCohort_sd = sd(mdfcCohort),
    ln_bef_mdfc_tot_pmt_sd = sd(ln_bef_mdfc_tot_pmt),
    ln_aft_mdfc_tot_pmt_sd = sd(ln_aft_mdfc_tot_pmt),
    ln_bef_mdfc_upb_amt_sd = sd(ln_bef_mdfc_upb_amt)
  )
##
## Create consistently named variable in each file, with same name but with file subscript
##

if (matched == "Yes") {
  tu_match_file <-
    tu_match_file %>%
    mutate(
      orig_year_tu = opn_dt,
      mod_mnth_tu = LMD_cohort,
      pre_mod_pay_tu = pay_due_am_bef_mdfc,
      post_mod_pay_tu = th_pay_due_am,
      pre_mod_upb_tu = cur_bal_am_bef_mdfc_round,
      bir_decade_tu = bir_dt,
      msa_state_tu = msa_state
    )
}
hamp_match_file <-
  hamp_match_file %>%
  mutate(
    orig_year_hamp = ln_orig_note_dt,
    mod_mnth_hamp = mdfcCohort,
    pre_mod_pay_hamp = ln_bef_mdfc_tot_pmt,
    post_mod_pay_hamp = ln_aft_mdfc_tot_pmt,
    pre_mod_upb_hamp = ln_bef_mdfc_upb_amt,
    bir_decade_hamp = brwr_bir_dtNum,
    msa_state_hamp = msa_state
  )
##
## Create normalized variable in each file, with same name
##

if (matched == "Yes") {
  tu_match_file <-
    tu_match_file %>%
    mutate(
      orig_year_norm = opn_dt / hamp_sd_file$ln_orig_note_dt_sd,
      mod_mnth_norm = LMD_cohort / hamp_sd_file$mdfcCohort_sd,
      pre_mod_pay_norm = pay_due_am_bef_mdfc_round / hamp_sd_file$ln_bef_mdfc_tot_pmt_sd,
      post_mod_pay_norm = th_pay_due_am / hamp_sd_file$ln_aft_mdfc_tot_pmt_sd,
      pre_mod_upb_norm = cur_bal_am_bef_mdfc_round / hamp_sd_file$ln_bef_mdfc_upb_amt_sd
    )
}
hamp_match_file <-
  hamp_match_file %>%
  mutate(
    orig_year_norm = ln_orig_note_dt / hamp_sd_file$ln_orig_note_dt_sd,
    mod_mnth_norm = mdfcCohort / hamp_sd_file$mdfcCohort_sd,
    pre_mod_pay_norm = ln_bef_mdfc_tot_pmt / hamp_sd_file$ln_bef_mdfc_tot_pmt_sd,
    post_mod_pay_norm = ln_aft_mdfc_tot_pmt / hamp_sd_file$ln_aft_mdfc_tot_pmt_sd,
    pre_mod_upb_norm = ln_bef_mdfc_upb_amt / hamp_sd_file$ln_bef_mdfc_upb_amt_sd
  )

if (matched == "No") {
  saveRDS(hamp_match_file, file = paste0(working_path, "matched_hamp_yearblock_unc_sub_um.rds"))
}

## ------------------------------------------------------------------------------------#
## RUN FOR ALL MSAs, ADDING ORIGINATION DATE AS BLOCKING VARIABLE, WITH REPLACEMENT ####
## ------------------------------------------------------------------------------------#
if (matched == "Yes") {
  tu_match_file_samp <- tu_match_file
  hamp_match_file_samp <- hamp_match_file

  # Only include loans in HAMP with origination dates that also show up in TU for given msa_state
  tu_orig_year <- tu_match_file_samp %>%
    group_by(msa_state, orig_year_tu) %>%
    summarise(count_tu = n())
  hamp_orig_year <- hamp_match_file_samp %>%
    group_by(msa_state, orig_year_hamp) %>%
    summarise(count_hamp = n())
  comb_orig_year <- inner_join(tu_orig_year, hamp_orig_year, by = c("msa_state", "orig_year_tu" = "orig_year_hamp"))
  # Sizes before join
  tu_match_file_samp %>% summarise(n())
  hamp_match_file_samp %>% summarise(n())
  # And inner join this with the samples to keep only observations where there are non-empty cells in both
  tu_match_file_samp <- inner_join(tu_match_file_samp, comb_orig_year, by = c("msa_state", "orig_year_tu"))
  hamp_match_file_samp <- inner_join(hamp_match_file_samp, comb_orig_year, by = c("msa_state", "orig_year_hamp" = "orig_year_tu"))
  # Sizes after join
  tu_match_file_samp %>% summarise(n())
  hamp_match_file_samp %>% summarise(n())

  # Remove duplicates
  drop_list_tu <-
    tu_match_file_samp %>%
    group_by(
      mod_mnth_norm,
      pre_mod_pay_norm,
      post_mod_pay_norm,
      pre_mod_upb_norm,
      msa_state,
      orig_year_norm
    ) %>%
    summarise(count = n()) %>%
    filter(count > 3)

  tu_match_file_samp <-
    left_join(tu_match_file_samp,
      drop_list_tu,
      by = c(
        "mod_mnth_norm",
        "pre_mod_pay_norm",
        "post_mod_pay_norm",
        "pre_mod_upb_norm",
        "msa_state",
        "orig_year_norm"
      )
    )

  # Drop cases in HAMP where TU has no donor
  drop_list_hamp <-
    tu_match_file_samp %>%
    mutate(dup = !is.na(count)) %>%
    group_by(msa_state, orig_year_norm) %>%
    mutate(has_dup = max(dup)) %>%
    summarise(
      sum_dup = sum(dup, na.rm = TRUE),
      sum_has_dup = sum(has_dup, na.rm = TRUE)
    ) %>%
    filter(sum_dup == sum_has_dup & sum_dup > 0) %>%
    select(-sum_has_dup)

  hamp_match_file_samp <-
    left_join(hamp_match_file_samp,
      drop_list_hamp,
      by = c("msa_state", "orig_year_norm")
    )

  hamp_match_file_samp <- hamp_match_file_samp %>% filter(is.na(sum_dup))

  tu_match_file_samp <-
    tu_match_file_samp %>%
    filter(is.na(count)) %>%
    select(-count)

  # Create household ID
  tu_match_file_samp$hh_id <-
    tu_match_file_samp %>%
    group_indices(
      msa_state,
      orig_year_norm,
      mod_mnth_norm,
      pre_mod_pay_norm,
      post_mod_pay_norm,
      pre_mod_upb_norm
    )

  tu_match_file_hh_id <- tu_match_file_samp %>% select(bcs_seqnum, hh_id)
  saveRDS(tu_match_file_hh_id, file = paste0(working_path, "tu_match_file_hh_id.rds"))

  # Remove duplicates in TU
  tu_match_file_samp <-
    tu_match_file_samp %>%
    group_by(msa_state, orig_year_norm) %>%
    distinct(
      mod_mnth_norm,
      pre_mod_pay_norm,
      post_mod_pay_norm,
      pre_mod_upb_norm
    ) %>%
    ungroup()

  # (0) Create row.names that can be used
  tu_match_file_samp$new_id <- c(1:nrow(tu_match_file_samp))

  rownames(tu_match_file_samp) <- c(1:nrow(tu_match_file_samp))

  head(hamp_match_file_samp)
  hamp_match_file_samp$new_id <- c(1:nrow(hamp_match_file_samp))

  # Now reassign row.name, see if aligns with new_id
  rownames(hamp_match_file_samp) <- c(1:nrow(hamp_match_file_samp))

  # (1) Define the blocking groups (donation class)
  group.v <- c("msa_state", "orig_year_norm")
  # (2) Define the matching variables
  x.mtc <- c("mod_mnth_norm", "pre_mod_pay_norm", "post_mod_pay_norm", "pre_mod_upb_norm")

  # (3) Run the matching, using Euclidean distance and NOT constraining so each donor only used once.
  tu_match_file_samp <- as.data.frame(tu_match_file_samp)
  hamp_match_file_samp <- as.data.frame(hamp_match_file_samp)
  system.time(out.nnd.c <- NND.hotdeck(
    data.rec = hamp_match_file_samp,
    data.don = tu_match_file_samp,
    match.vars = x.mtc,
    don.class = group.v,
    dist.fun = "Euclidean",
    keep.t = TRUE
  ))


  # Combine the matched ids and the distance (so I can later filter)
  match_and_dist <- cbind(out.nnd.c$mtc.ids, out.nnd.c$dist.rd, out.nnd.c$noad)
  match_and_dist <- as.data.frame(match_and_dist)
  match_and_dist$dist <- as.numeric(as.character(match_and_dist$V3))
  match_and_dist$noad <- as.numeric(as.character(match_and_dist$V4))
  match_and_dist$don.id <- as.character(match_and_dist$don.id)
  match_and_dist$rec.id <- as.character(match_and_dist$rec.id)

  # (4) Create the fused data set
  fused.c <- create.fused(
    data.rec = hamp_match_file_samp,
    data.don = tu_match_file_samp,
    mtc.ids = out.nnd.c$mtc.ids,
    z.vars = c(
      "bcs_seqnum", "act_id", "hh_id", "orig_year_tu", "mod_mnth_tu", "pre_mod_pay_tu", "post_mod_pay_tu",
      "pre_mod_upb_tu", "bir_decade_tu", "msa_state_tu", "delin_at_mod", "any_disqual_tu", "pra_tu"
    )
  )

  # merge on distance metric, sort by it, and see if the low ones better than high ones.
  match_and_dist <- match_and_dist %>% mutate(rec_id_int = as.numeric(as.character(rec.id)))

  fused.c_dist <- left_join(fused.c, match_and_dist, by = c("new_id" = "rec_id_int"))

  # Sort by distance
  fused.c_dist <- fused.c_dist %>% arrange(dist)

  # Remove any cases with coincidental equal distances
  fused.c_dist <- fused.c_dist %>% filter(noad == 1) %>%
    saveRDS(file = paste0(working_path, "matched_hamp_yearblock_unc.rds"))

  rm(matched_hamp_yearblock_unc)
  rm(comb_orig_year)
  rm(fused.c)
  rm(fused.c_dist)
  rm(hamp_match_file_samp)
  rm(hamp_orig_year)
  rm(match_and_dist)
  rm(tu_match_file)
  rm(tu_match_file_samp)
  rm(tu_match_file_hh_id)
  rm(tu_orig_year)
  rm(tu_sd_file)
  rm(out.nnd.c)
  rm(drop_list_hamp)
  rm(drop_list_tu)
}

rm(hamp_match_file)
rm(hamp_sd_file)
