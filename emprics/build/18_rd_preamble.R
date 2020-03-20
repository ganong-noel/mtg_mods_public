bw_window <- 2
cutoff <- 0
gran <- 40
n_cut <- 15

source("func/binscatter_rd.R")
source("func/loc_lin.R")

if (matched == "Yes") {
  df <- readRDS(paste0(working_path, "rd_samp.rds"))
} else if (matched == "No") {
  df <- readRDS(paste0(working_path, "rd_samp_um.rds"))
}

df <- df %>% filter(abs(x) < 4)

if (matched == "Yes") {
  match_dist_cutoff <- 0.2
  df <- df %>%
    filter(dist < match_dist_cutoff) %>%
    filter(rpt_sub_cd != "x42g551")
}


samp_file_name <- "Matched"
if (matched == "No") {
  samp_file_name <- "Unmatched"
}
samp_tit <- ""
