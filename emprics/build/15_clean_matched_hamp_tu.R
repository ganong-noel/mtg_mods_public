##
## Blocking on MSA and Year
##

# Load results from blocking on MSA and year
matched_hamp_yearblock_unc <- readRDS(paste0(working_path, "matched_hamp_yearblock_unc.rds"))

test_that("Matched file is the same",
          expect_equivalent(
            matched_hamp_yearblock_unc %>%
              transmute(one = 1,
                        fncl_ast_id,
                        ln_bef_mdfc_pi_pmt_amt.x,
                        dist) %>%
              summarise_each(funs(sum(., na.rm = T),
                                  mean(is.na(.)))) %>%
              as.vector(),
            c(1007947, 1.08094e+16,
              1488367200, 180109.7,
              rep(0, 4)),
            tolerance = 1e-06))

matched_hamp_yearblock_unc_sub <-
  matched_hamp_yearblock_unc %>%
  group_by(bcs_seqnum, act_id) %>%
  mutate(tu_obs = n(), min_dist = min(dist)) %>%
  filter(dist == min_dist) %>%
  group_by(bcs_seqnum, act_id) %>%
  mutate(tu_obs_post = n()) %>%
  filter(tu_obs_post == 1) %>%
  select(-tu_obs_post)

matched_hamp_yearblock_unc_sub %>%
  ungroup() %>%
  as.data.frame() %>%
  saveRDS(file = paste0(working_path, "matched_hamp_yearblock_unc_sub.rds"))


remove <- ls(pattern = "^matched")
remove <- remove[remove != "matched"]
rm(list = remove)
rm(remove)
