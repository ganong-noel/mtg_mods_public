##
# Setup
##
sample_name <- c("nonmissing_dnpv")
graph_name <- c()
mrgdHamp <-
  readRDS(paste0(working_path, "mrgdHamp_R.rds"))
##
# Take sample
##
df <-
  mrgdHamp %>%
  filter(ivsr_grpNum == "Non-GSE", !is.na(x), abs(x) <= 4)
