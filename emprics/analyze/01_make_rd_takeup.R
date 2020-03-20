mrgdHamp <-
  readRDS(paste0(working_path, "mrgdHamp_R.rds")) %>%
  mutate(
    fracForgivePos = ifelse(!is.na(ln_upb_frgv_amt.x), ln_upb_frgv_amt.x > 0,
      ifelse(!is.na(ln_upb_frgv_amt.y), ln_upb_frgv_amt.y > 0, FALSE)
    ),
    praSampCore = ivsr_grpNum == "Non-GSE" & abs(x) > 0.005,
    praSamp = abs(x) < 0.9 & praSampCore,
    anyDelin = ln_st_cd == "Disqualified"
  )

praSampCore <-
  mrgdHamp %>%
  filter(praSampCore == TRUE) %>%
  mutate(
    offered_mod = ifelse(
      ln_st_cd != "Rejected" &
        (ln_st_cd != "Trial Cancelled" |
          trial_mdfc_fout_rsn_nme == "Offer Not Accepted by Borrower / Request Withdrawn"),
      TRUE, FALSE
    ),
    takeup_mod = ifelse(
      ln_st_cd == "Rejected" |
        (ln_st_cd == "Trial Cancelled" &
          trial_mdfc_fout_rsn_nme == "Offer Not Accepted by Borrower / Request Withdrawn"),
      FALSE, TRUE
    ),
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
    takeup_status = ifelse(is.na(takeup_status) & ln_st_cd == "Rejected", "Rejected", takeup_status),
    offered_mod = ifelse(takeup_status %in% c("PermMod Active", "PermMod Disqual", "Trial Active", "Trial Disqual", "Offer Rejected"), TRUE, FALSE),
    takeup_mod = ifelse(takeup_status %in% c("PermMod Active", "PermMod Disqual", "Trial Active", "Trial Disqual"), TRUE, FALSE)
  )

saveRDS(praSampCore, file = paste0(working_path, "df_rd_takeup.rds"))
rm(mrgdHamp)
rm(praSampCore)
