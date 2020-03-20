# This file reads in the raw .csv files from the HAMP NPV public file and creates an RDS file

# Takes about 3 mins
npvAll <- read.csv(
  file = paste0(data_path, "HAMP_raw_files/HMP_Public_User_NPV_Data_All_20150317.csv"),
  head = TRUE, sep = ","
)

test_that("Public NPV file unchanged",
          expect_equivalent(npvAll %>%
                              as.data.frame() %>%
                              transmute(one = 1,
                                        fncl_ast_id,
                                        ln_aft_mdfc_amrt_term,
                                        ln_orgnn_amrt_term) %>%
                              summarise_each(funs(sum(., na.rm = TRUE),
                                                  mean(is.na(.)))) %>%
                              as.vector(),
                            c(2276437, 1.064597e+16, 833257998,
                              841300701,  0, 0, 0.04149862, 0.00379540),
                            tolerance = 1e-5))

saveRDS(npvAll, file = paste0(working_path, "npvAll_expanded_R.rds"))
rm(npvAll)
