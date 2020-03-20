##
# Setup
##
sample_name <- c("es_base")
graph_name <- c("piHamp5", "piHamp5_no_shares", "dollar_red", "fin_impacts_all")
mrgdHamp <- readRDS(paste0(working_path, "mrgdHamp_R.rds"))

##
# Take sample
##
ids_event_study_base <-  readRDS(paste0(working_path, "ids_event_study_base.rds"))
df <- left_join(ids_event_study_base, mrgdHamp, by="fncl_ast_id")
rm(ids_event_study_base, mrgdHamp)
