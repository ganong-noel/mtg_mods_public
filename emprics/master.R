# Setup ####

environment <- "Odyssey" # Options: 'Odyssey', 'Local'
user <- "Pete" # Options: 'Chanwool', 'Guillermo'
snapshot_pkgs_local <- TRUE

if (environment == "Odyssey") {
  if (user == "Chanwool") {
    setwd("/n/home07/chanwoolkim/Desktop/hamp-emp/")
  } else if (user == "Pete") {
    setwd("/n/home12/probertson/repo/hamp-emp/")
  } else if (user == "Guillermo") {
    setwd("/n/home01/gcarranzajordan2/Desktop/hamp-emp/")
  } else if (user == "Lei") {
    setwd("/n/home07/lma/repo/hamp-emp/")
  }
  data_path <- "/n/boslfs/LABS/chodorow_reich_lab/hampra_v4/data/"
  working_path <- "../../hamp_emp_working/"
  data_path_local <- "data/"
} else if (environment == "Local") {
  if (user == "Chanwool") {
    setwd("C:/Users/Chanwoolkim/repo/hamp-emp/")
  } else if (user == "Guillermo") {
    setwd("/Users/gcarranza/Documents/repo/hamp-emp")
  } else if (user == "Lei") {
    setwd("/Users/leima/Documents/hamp-emp")
  } else if (user == "Pete") {
    setwd("~/repo/hamp-emp/")
  }
  data_path <- "data/"
  data_path_local <- "data/"
}

out_path <- "out/"
out_path_diagnosis <- "out/diagnosis/"

matched <- "Yes" # Options: 'Yes', 'No'
if (matched == "No") {
  suffix <- "_um"
}

# General
if (environment == "Odyssey") {
  source("func/binscatter_MultiControl.R")
  source("func/winsor.R")
  source("func/prelim.R")
}


# Run part that requires microdata in Odyssey ####

if (environment == "Odyssey") {

  # HAMP ONLY ####

  library(lubridate)
  library(dplyr)
  library(multiwayvcov)
  library(ggplot2)
  library(AER)
  library(haven)
  library(RColorBrewer)
  library(scales)
  library(tidyr)
  library(grid)
  require(rdrobust)
  require(KernSmooth)
  require(rdd)
  require(testthat)
  require(purrr)
  if (matched == "Yes") {
    # Load and Clean HAMP files
    source("build/01_load_Npv.R")
    source("build/02_load_1stLien.R")
    source("build/03_clean_Npv.R")
    source("build/04_clean_1stLien.R")

    # Make matching file
    source("build/05_make_hamp_matching_file.R")

    # HAMP-Only Analysis
    source("analyze/01_make_rd_takeup.R")
    source("analyze/02_set_sample_nonmissing_dnpv.R")
    source("analyze/03_calc_ModNpv.R")
  }

  # Clean workspace
  detach("package:lubridate", unload = TRUE)
  detach("package:dplyr", unload = TRUE)
  detach("package:multiwayvcov", unload = TRUE)
  detach("package:ggplot2", unload = TRUE)

  # TRANSUNION ONLY ####

  library(dplyr)
  library(readr)
  library(pryr)
  library(lubridate)
  library(ggplot2)
  library(RColorBrewer)
  library(scales)
  library(grid)
  library(testthat)
  library(magrittr)

  summarise <- dplyr::summarise
  filter <- dplyr::filter

  if (matched == "Yes") {

    # Read in TU data and Make Analysis and Matching Files
    source("build/06_load_tu.R")
    source("build/07_make_tu_tradefiles.R")
    source("build/08_make_tu_mortgage_file.R")
    source("build/09_build_foreclosure_outcome.R")
    source("build/10_make_tu_matching_file.R")

    source("build/11_make_tu_creditcard_file.R")
    source("build/12_make_tu_auto_file.R")
  }

  # Clean workspace
  detach("package:readr", unload = TRUE)
  detach("package:dplyr", unload = TRUE)
  detach("package:pryr", unload = TRUE)

  # MATCHING HAMP AND TU ####
  library(purrr)
  library(ggplot2)
  library(reshape)
  library(lubridate)
  library(StatMatch)
  library(multiwayvcov)
  library(AER)
  library(pryr)
  library(RColorBrewer)
  library(scales)
  library(tidyr)
  library(grid)
  library(haven)
  library(stargazer)
  library(lfe)
  library(readxl)
  library(xtable)
  library(gdata)
  library(tibble)
  library(testthat)
  library(dplyr)

  source("build/13_compare_hamp_tu.R")
  source("build/14_match_hamp_tu.R")
  if (matched == "Yes") {
  source("build/15_clean_matched_hamp_tu.R")
  }

#  ANALYSIS ####

  if (matched == "Yes") {

    # Event Study, Consumption
   source("build/16_build_event_study_dataset.R")
   source("analyze/04_set_sample_es_base.R")
   source("analyze/03_calc_ModNpv.R")
   source("analyze/05_analyze_event_study_cc.R")
   source("analyze/06_analyze_event_study_auto_part1.R")
   source("analyze/07_analyze_event_study_auto_part2.R")
  }
  # RD Analysis
  #unnormalized running variable
  source("build/build_rd_unnormalized.R")
  source("build/19_rd_last_steps.R") #file is used twice
  source("analyze/rd_unnormalized_running_var.R")

  #normalized running variable
  source("build/17_build_rd_dataset.R")
  source("build/18_rd_preamble.R")
  source("build/19_rd_last_steps.R") #file is used twice
  source("build/20_npv_model.R")

  source("analyze/08_rd_bw.R")
  source("analyze/09_rd_delin.R")

  if (matched == "Yes") {

    # Matched sample consumption analysis
    source("build/21_rd_cons_last_steps.R")
    source("analyze/10_rd_cons.R")

    # Representativeness and Summary Stats
    source("analyze/11_representativeness.R")
    source("analyze/12_rd_data_quality.R")

    # Manual Inputs ####
    source("analyze/13_plot_external_data_figures.R")
  }
}

if (environment != "Odyssey") {
  if (snapshot_pkgs_local == TRUE) {
    # Installs packages from a certain date using the MRAN server
    # By default installs into the first element of .lib
    # Need to add a path for the packages
    require(versions)
    install.dates(
      c("dplyr", "ggplot2", "readr", "RColorBrewer", "scales", "tibble",
        "tidyr", "rprintf", "readxl", "knitr", "rmarkdown", "testthat", "ssanv",
        "magrittr", "purrr"),
      "2019-01-01", lib = "~/rmd_packages/"
    )
  }

  # Run maturity extension part
  library("knitr")
  library("rmarkdown")



  rmarkdown::render("analyze/14_maturity_extension_npv.Rmd")
  rmarkdown::render("analyze/15_npv_cost_benefit.Rmd")

  # Move markdown output to /out/ and cleanup
  file.copy("14_maturity_extension_npv.md", "out/diagnosis/")
  file.copy("15_npv_cost_benefit.md", "out/diagnosis/")

  file.remove("14_maturity_extension_npv.md")
  file.remove("15_npv_cost_benefit.md")
  unlink("figure", recursive = TRUE)

  source("func/prelim_local.R")
  source("analyze/16_maturity_extension_default.R")
}
