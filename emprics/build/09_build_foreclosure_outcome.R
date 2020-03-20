combined_mortgage_file_LMDs <- readRDS(paste0(working_path,
                                              "combined_mortgage_file_LMDs.rds"))


combined_mortgage_file_LMDs <-
  combined_mortgage_file_LMDs %>%
  select(act_id, bcs_seqnum, ver_dt, th_mop, th_gen_rmk_cd, first_LMD_date) %>%
  as.data.frame(combined_mortgage_file_LMDs) %>%
  filter(ver_dt >= first_LMD_date) %>%
  mutate(
    ver_dt_N = ymd(as.character(ver_dt * 100 + 1)),
    first_LMD_date_N = ymd(as.character(first_LMD_date * 100 + 1)),
    ver_dt_monnb = monnb(as.Date(ver_dt_N)),
    first_LMD_date_monnb = monnb(as.Date(first_LMD_date_N))
  )
##
# Build same indicators as with 90dpd
##

# Calculate date of first FPI (foreclosure initiated)
post_LMD_fpi <-
  combined_mortgage_file_LMDs %>%
  filter(ver_dt > first_LMD_date & th_gen_rmk_cd %in% c("FPI")) %>%
  group_by(bcs_seqnum, act_id) %>%
  summarise(fpi_date_tu_monnb = min(ver_dt_monnb))


n_distinct(combined_mortgage_file_LMDs$act_id)
count(post_LMD_fpi %>% ungroup())

combined_mortgage_file_LMDs <-
  left_join(
    combined_mortgage_file_LMDs,
    post_LMD_fpi,
    by = c("bcs_seqnum", "act_id")
  )


rm(post_LMD_fpi)

#build the foreclosure outcome
combined_mortgage_file_LMDs %>%
  mutate(any_fpi_tu = ifelse(is.na(fpi_date_tu_monnb), FALSE, TRUE),# Create indicator variable for disqualification
         mos_to_fpi_tu = fpi_date_tu_monnb - first_LMD_date_monnb, # Calculate months until disqualification
         fpi_nxt_yr_tu = ifelse(is.na(mos_to_fpi_tu) | mos_to_fpi_tu > 12, FALSE, TRUE),# Create indicator variables for fpi within 1 or 2 years
         fpi_two_yr_tu = ifelse(is.na(mos_to_fpi_tu) | mos_to_fpi_tu > 24, FALSE, TRUE)
  ) %>%
  group_by(act_id, bcs_seqnum) %>%
  filter(row_number() == 1) %>%
  select(
    act_id, bcs_seqnum, fpi_date_tu_monnb, any_fpi_tu,
    mos_to_fpi_tu, fpi_nxt_yr_tu, fpi_two_yr_tu
  ) %>%
  saveRDS(str_c(working_path, "foreclosure_df.rds"))

rm(combined_mortgage_file_LMDs)
