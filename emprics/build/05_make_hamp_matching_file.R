mrgdHamp <- readRDS(paste0(working_path, "mrgdHamp_R.rds"))

# Filter to permanent obs file
hamp_match_file <- mrgdHamp %>% filter(permMod == TRUE)
rm(mrgdHamp)

# How many records uniquely identified by our nine fields?
distinct_ids <-
  hamp_match_file %>%
  group_by(
    ln_orig_note_dt,
    prop_geoc_cnsus_msa_cd,
    prop_stdz_st_cd,
    brwr_bir_dtNum,
    ln_mdfc_eff_dtN,
    ln_bef_mdfc_pi_pmt_amt.x,
    ln_aft_mdfc_pi_pmt_amt,
    ln_bef_mdfc_upb_amt.x,
    ln_aft_mdfc_upb_amt
  ) %>%
  summarise(count = n())
table(distinct_ids$count)

summary(hamp_match_file[, c(
  "ln_orig_note_dt",
  "prop_geoc_cnsus_msa_cd",
  "prop_stdz_st_cd",
  "brwr_bir_dtNum",
  "ln_mdfc_eff_dtN",
  "ln_bef_mdfc_pi_pmt_amt.x",
  "ln_aft_mdfc_pi_pmt_amt",
  "ln_bef_mdfc_upb_amt.x",
  "ln_bef_mdfc_escr_pmt_amt",
  "ln_aft_mdfc_escr_pmt_amt",
  "ln_aft_mdfc_upb_amt",
  "ln_prin_frbrn_amt",
  "ln_prin_rdcn_altv_amt"
)])

hamp_match_file <-
  hamp_match_file %>%
  select(
    fncl_ast_id,
    mha_ln_id,
    ln_orig_note_dt,
    prop_geoc_cnsus_msa_cd,
    prop_stdz_st_cd,
    brwr_bir_dtNum,
    ln_mdfc_eff_dtN,
    ln_bef_mdfc_pi_pmt_amt.x,
    ln_bef_mdfc_pi_pmt_amt.y,
    ln_aft_mdfc_pi_pmt_amt,
    ln_bef_mdfc_upb_amt.x,
    ln_bef_mdfc_upb_amt.y,
    ln_bef_mdfc_escr_pmt_amt,
    ln_aft_mdfc_escr_pmt_amt,
    ln_aft_mdfc_upb_amt,
    ln_prin_frbrn_amt,
    ln_prin_rdcn_altv_amt
  )

# Check NA / zero value
test_that("Alternate information available for missing prop_geoc_cnsus_mcs_cd",
          {hamp_match_file %>%
                filter(is.na(prop_geoc_cnsus_msa_cd) | prop_geoc_cnsus_msa_cd == 0) %>%
                mutate(state_missing = prop_stdz_st_cd == " " | is.na(prop_stdz_st_cd)) %>%
                group_by(state_missing) %>%
                summarise(count = n()) %>%
                `$`(count) %>%
                expect_equal(85059, tol = 1)})

# Really true that BEFORE modification payment has no NAs?

test_that("No missing payment before modification",
          {hamp_match_file %>%
              filter(is.na(ln_bef_mdfc_pi_pmt_amt.x)) %>%
              nrow() %>%
              expect_equal(0)})

test_that("4 values of 0 in ln_bef_mdfc_pi_pmt_amt.x",
          {hamp_match_file %>%
              filter(ln_bef_mdfc_pi_pmt_amt.x == 0) %>%
              nrow() %>%
              expect_equal(4)})

# Really true that AFTER modification payment has no NAs?
test_that("No missing paymeynt after modification",
          {hamp_match_file %>%
              filter(is.na(ln_aft_mdfc_pi_pmt_amt)) %>%
              nrow() %>%
              expect_equal(0)})

test_that("One 0 value of payment after modification",
          {hamp_match_file %>%
              filter(ln_aft_mdfc_pi_pmt_amt == 0) %>%
              nrow() %>%
              expect_equal(1)})

# Really true that Before modification upb has no NAs?
test_that("Before modification upb has no NAs",
          hamp_match_file %>%
            filter(is.na(ln_bef_mdfc_upb_amt.x)) %>%
            nrow() %>%
            expect_equal(0))


test_that("Before mdoficiation ubp has 93 0 values",
          hamp_match_file %>%
            filter(ln_bef_mdfc_upb_amt.x == 0) %>%
            nrow() %>%
            expect_equal(93))

hamp_match_file <-
  hamp_match_file %>%
  mutate(ln_bef_mdfc_upb_amt = ifelse(ln_bef_mdfc_upb_amt.x != 0, ln_bef_mdfc_upb_amt.x, ln_bef_mdfc_upb_amt.y))

test_that("4629 0 pre-mod escrow",
          hamp_match_file %>%
            filter(ln_bef_mdfc_escr_pmt_amt == 0) %>%
            nrow() %>%
            expect_equal(4629))

test_that("1306 post mod escrow",
         hamp_match_file %>%
           filter(ln_aft_mdfc_escr_pmt_amt == 0) %>%
           nrow() %>%
           expect_equal(1306))


saveRDS(hamp_match_file, file = paste0(working_path, "hamp_match_file.rds"))
rm(hamp_match_file)
rm(distinct_ids)
