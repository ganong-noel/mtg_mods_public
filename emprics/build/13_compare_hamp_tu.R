
if (matched == "Yes") {
  tu_match_file <- readRDS(paste0(working_path, "tu_match_file.rds")) %>%
    ungroup()
}


hamp_match_file <- readRDS(paste0(working_path, "hamp_match_file.rds")) %>%
                             ungroup()

## --------------------------------------------------------------------------------------#
## FILTER LOANS FROM HAMP TO MATCH SAME TIME PERIOD AND INSPECT MONTH OF MODIFICATION ####
## --------------------------------------------------------------------------------------#

hamp_match_file <- hamp_match_file %>%
  mutate(mdfcCohort = monnb(as.Date(ln_mdfc_eff_dtN)))

if (matched == "Yes") {
  tu_match_file <- tu_match_file %>%
    mutate(LMD_cohort = monnb(as.Date(first_LMD_date_N)))
}
##
# Drop earlier HAMP Mods
##
if (matched == "Yes") {
  monnb(as.Date("2010-07-01"))
  monnb(as.Date("2015-01-01"))
  hamp_match_file <- hamp_match_file %>%
    filter(mdfcCohort > 1326 & mdfcCohort < 1380)
}

## -----------------------#
## Year of Origination ####
## -----------------------#

##
# Make Sure Format Aligns
##
str(hamp_match_file$ln_orig_note_dt)
if (matched == "Yes") {
  tu_match_file <-
    tu_match_file %>%
    mutate(
      opn_dt_mnth = floor(opn_dt / 100),
      opn_dt = floor(opn_dt / 10000)
    )
}

if (matched == "Yes") {
  tu_match_file <- tu_match_file %>% filter(opn_dt < 2009)
}
hamp_match_file <- hamp_match_file %>% filter(ln_orig_note_dt < 2009)

## ---------------#
## MSA / STATE ####
## ---------------#

# Create new variable in each which is combination of these
if (matched == "Yes") {
  tu_match_file <-
    tu_match_file %>%
    mutate(msa_state = ifelse(!is.na(cbsa),
                              as.character(cbsa),
                              as.character(state)))
  }
hamp_match_file <-
  hamp_match_file %>%
  mutate(msa_state = ifelse(prop_geoc_cnsus_msa_cd > 0,
                            as.character(prop_geoc_cnsus_msa_cd),
                            as.character(prop_stdz_st_cd)))


# By inspection, found it must be 31080, which is validated by this piece: http://www.census.gov/econ/census/pdf/2012_geonotes/2012MetroNotes_CA.pdf
# 31080 is in HAMP but no obs in TU. So assign all obs for 31080 in HAMP toi 31100
hamp_match_file <- hamp_match_file %>% mutate(msa_state = ifelse(msa_state == "31080", "31100", msa_state))
# Look at others, using this resource for changes: http://mcdc.missouri.edu/data/georef/Tools/cbsa_changes.lst
# 41780, more in TU than HAMP, its in OH. And overall Ohio has a few hundred more HAMp than TU. So reassign these to OH for BOTH Files
# 42060 has more in TU than HAMP. But changed to 42200, which has none in TU. So reassign the HAMP with 42200 to 42060.
# Arizona: couldn't figure out what was going on here. More in TU than HAMP, but no indication of 40940 which is only MSA that changed.
# 26180 more in HAMP than TU. Moved to 46520 in new definition, reclassify
# 29140, more HAMP than TU. Moved to 29200, reclassify
# 37380 to 19960, BOTH
# 26100 to 24340, BOTH
# 14060 to 14010
# Doing it in parts bc getting unwieldy
hamp_match_file <-
  hamp_match_file %>%
  mutate(msa_state = ifelse(msa_state == "41780", "OH", ifelse(
    msa_state == "42200", "42060", ifelse(
      msa_state == "46520", "26180", ifelse(
        msa_state == "29200", "29140", ifelse(
          msa_state == "19960", "37380", ifelse(
            msa_state == "24340", "26100", ifelse(
              msa_state == "14010", "14060", msa_state
            )
          )
        )
      )
    )
  )))
# 41540 to DE
# 39100 to 35620
# IL to 14010
# 1L to 1460 (not actually but this simplifies)
# FL to 18880
# 11300 to 26900
# 11340 to 24860
# Don't know why we have more CA in TU than HAMP
# MS to 37700 and 25060
# TX to 47020, 19100,
# 26180 to 46520
# HI to 27980
# 11340 to 24860
# MD to 15680, 41540
hamp_match_file <-
  hamp_match_file %>%
  mutate(msa_state = ifelse(msa_state == "41540", "DE", ifelse(
    msa_state == "35620", "39100", ifelse(
      msa_state == "14010", "IL", ifelse(
        msa_state == "14060", "IL", ifelse(
          msa_state == "18880", "FL", ifelse(
            msa_state == "26900", "11300", ifelse(
              msa_state == "24860", "11340", ifelse(
                msa_state == "37700" | msa_state == "25060", "MS", ifelse(
                  msa_state == "47020" | msa_state == "19100", "TX", ifelse(
                    msa_state == "46520", "26180", ifelse(
                      msa_state == "27980", "HI", ifelse(
                        msa_state == "24860", "11340", ifelse(
                          msa_state == "15680" | msa_state == "41540", "MD", msa_state
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )))
# Now the ones where TU is low
# OH to 48260 and 44600
# 29140 to 29200
# 29140 to 29200
# MS to 32820
# Now where HAMP missing
# 14600 to 35840 https://www.ffiec.gov/geocode/help1.aspx
# 23020 to 18880
hamp_match_file <-
  hamp_match_file %>%
  mutate(msa_state = ifelse(msa_state == "15680" | msa_state == "48260", "44600", ifelse(
    msa_state == "29200", "29140", ifelse(
      msa_state == "29200", "29140", ifelse(
        msa_state == "32820", "MS", ifelse(
          msa_state == "35840", "14600", ifelse(
            msa_state == "18880", "23020", msa_state
          )
        )
      )
    )
  )))

if (matched == "Yes") {

  # Make the same changes to tu_match_file
  tu_match_file <-
    tu_match_file %>%
    mutate(msa_state = ifelse(msa_state == "41780", "OH", ifelse(
      msa_state == "42200", "42060", ifelse(
        msa_state == "46520", "26180", ifelse(
          msa_state == "29200", "29140", ifelse(
            msa_state == "19960", "37380", ifelse(
              msa_state == "24340", "26100", ifelse(
                msa_state == "14010", "14060", msa_state
              )
            )
          )
        )
      )
    )))

  tu_match_file <-
    tu_match_file %>%
    mutate(msa_state = ifelse(msa_state == "41540", "DE", ifelse(
      msa_state == "35620", "39100", ifelse(
        msa_state == "14010", "IL", ifelse(
          msa_state == "14060", "IL", ifelse(
            msa_state == "18880", "FL", ifelse(
              msa_state == "26900", "11300", ifelse(
                msa_state == "24860", "11340", ifelse(
                  msa_state == "37700" | msa_state == "25060", "MS", ifelse(
                    msa_state == "47020" | msa_state == "19100", "TX", ifelse(
                      msa_state == "46520", "26180", ifelse(
                        msa_state == "27980", "HI", ifelse(
                          msa_state == "24860", "11340", ifelse(
                            msa_state == "15680" | msa_state == "41540", "MD", msa_state
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )))

  tu_match_file <-
    tu_match_file %>%
    mutate(msa_state = ifelse(msa_state == "15680" | msa_state == "48260", "44600", ifelse(
      msa_state == "29200", "29140", ifelse(
        msa_state == "29200", "29140", ifelse(
          msa_state == "32820", "MS", ifelse(
            msa_state == "35840", "14600", ifelse(
              msa_state == "18880", "23020", msa_state
            )
          )
        )
      )
    )))
}


## -------------------------#
## Borrower Birth Decade ####
## -------------------------#
hamp_match_file <-
  hamp_match_file %>%
  mutate(brwr_bir_dtNum = as.numeric(substr(as.character(brwr_bir_dtNum), 1, 4)))

if (matched == "Yes") {
  tu_match_file <- tu_match_file %>% mutate(bir_dt = floor(bir_dt / 100))
  tu_match_file <-
    tu_match_file %>%
    mutate(
      bir_dt_yr = bir_dt,
      bir_dt = floor(bir_dt / 10) * 10
    )
  # HAMP reports no lower than 1940 and no higher than 1990, so replace accordingly
  tu_match_file <-
    tu_match_file %>%
    mutate(bir_dt = ifelse(bir_dt < 1940, 1940, ifelse(bir_dt > 1990, 1990, bir_dt)))
  }

## ---------------------------------------#
## Monthly Payment Before Modification ####
## ---------------------------------------#

##
# Make Sure Format Aligns
##
if (matched == "Yes") {
  tu_match_file <-
    tu_match_file %>%
    mutate(pay_due_am_bef_mdfc = pay_due_am_bef_mdfc_alt)
  }

# Make full pre-mod payment in HAMP, combining principal, interest, escrow
hamp_match_file <-
  hamp_match_file %>%
  mutate(ln_bef_mdfc_tot_pmt = ln_bef_mdfc_pi_pmt_amt.x + ln_bef_mdfc_escr_pmt_amt)
##
# Compare the distribution of the two
##

# Round both to nearest 100 for groupings
hamp_match_file <-
  hamp_match_file %>%
  mutate(ln_bef_mdfc_tot_pmt_round = round(ln_bef_mdfc_tot_pmt, -2))
if (matched == "Yes") {
  tu_match_file <-
    tu_match_file %>%
    mutate(pay_due_am_bef_mdfc_round = round(pay_due_am_bef_mdfc, -2))
}

## --------------------------------------#
## Monthly Payment After Modification ####
## --------------------------------------#

##
# Make Sure Format Aligns
##
if (matched == "Yes") {
  tu_match_file <- tu_match_file %>% mutate(th_pay_due_am = pay_due_am_aft_mdfc_alt)
}

# Make full post-mod payment in HAMP, combining principal, interest, escrow
hamp_match_file <-
  hamp_match_file %>%
  mutate(ln_aft_mdfc_tot_pmt = ln_aft_mdfc_pi_pmt_amt + ln_aft_mdfc_escr_pmt_amt)

# Round both to nearest 100 for groupings
hamp_match_file <-
  hamp_match_file %>%
  mutate(ln_aft_mdfc_tot_pmt_round = round(ln_aft_mdfc_tot_pmt, -2))

if (matched == "Yes") {
  tu_match_file <- tu_match_file %>% mutate(th_pay_due_am_round = round(th_pay_due_am, -2))
}

## --------------------------------------#
## Unpaid Balance Before Modification ####
## --------------------------------------#
if (matched == "Yes") {
  tu_match_file <-
    tu_match_file %>%
    mutate(cur_bal_am_bef_mdfc_round = round(cur_bal_am_bef_mdfc, -4))
}

## --------#
## Save ####
## --------#
if (matched == "Yes") {
  saveRDS(hamp_match_file,
       file = paste0(working_path, "hamp_match_file_clean.rds"))
  saveRDS(tu_match_file,
       file = paste0(working_path, "tu_match_file_clean.rds"))
  rm(hamp_match_file, tu_match_file)
} else if (matched == "No") {
  saveRDS(hamp_match_file, file = paste0(working_path, "hamp_match_file_clean_um.rds"))
  rm(hamp_match_file)
}
