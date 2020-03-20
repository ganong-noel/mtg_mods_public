#install_packages.R
#This file removes all previously installed packages, and install packages used for HAMP
#Note: this file currently only runs in Odyssey

#Remove all user-installed packages ####
#Refer to http://www.r-bloggers.com/how-to-remove-all-user-installed-packages-in-r/

user <- "Pete"

if (user == "Lei") {
  lib_path <- "/n/home07/lma/apps/R/"
} else if (user == "Guillermo") {
  lib_path <- "/n/home01/gcarranzajordan2/apps/R/"
} else if (user == "Pete") {
  lib_path <- "/n/home12/probertson/apps/R/"
}
# specify the order of libraries
.libPaths(c(lib_path, .libPaths()))
.libPaths()

#Create a list of all installed packages
ip <- as.data.frame(installed.packages(lib.loc = lib_path))
#If you use MRO, make sure that no packages in this library will be removed
ip <- subset(ip, !grepl("MRO", ip$LibPath))
#We do not want to remove base packages
ip <- ip[!(ip[,"Priority"] %in% c("base")),]
#Determine the library where the packages are installed
path_lib <- unique(ip$LibPath)
#Create a vector with all the names of the packages you want to remove
pkgs_to_remove <- ip[,1]
#Remove the packages
sapply(pkgs_to_remove, remove.packages, lib = path_lib)
rm(ip, path_lib, pkgs_to_remove)

# double check all packages installed on local path are moved
if (length(list.files(path = lib_path)) > 0) {
  stop("Not all packages are removed in the local library path. Delete packages manually first.")
}

#Note on R info ####
#This uses R version R/3.3.3-fasrc01 Core, which comes with the following base packages:
rownames(installed.packages(priority="base"))

#Install packages ####

#Packages under packages
install.packages("http://cran.r-project.org/src/contrib/Archive/Formula/Formula_1.2-1.tar.gz", repos=NULL, type="source") #1.2.1
install.packages("http://cran.r-project.org/src/contrib/Archive/survival/survival_2.38-3.tar.gz", repos=NULL, type="source") #2.38-3
install.packages("http://cran.r-project.org/src/contrib/Archive/zoo/zoo_1.7-12.tar.gz", repos=NULL, type="source") #1.7-12
install.packages("http://cran.r-project.org/src/contrib/Archive/lmtest/lmtest_0.9-34.tar.gz", repos=NULL, type="source") #0.9-34
install.packages("http://cran.r-project.org/src/contrib/Archive/sandwich/sandwich_2.3-3.tar.gz", repos=NULL, type="source") #2.3-3
install.packages("http://cran.r-project.org/src/contrib/Archive/lattice/lattice_0.20-33.tar.gz", repos=NULL, type="source") #0.20-33
install.packages("http://cran.r-project.org/src/contrib/Archive/lpSolve/lpSolve_5.6.11.tar.gz", repos=NULL, type="source") #5.6.11
install.packages("http://cran.r-project.org/src/contrib/Archive/RANN/RANN_2.5.tar.gz", repos=NULL, type="source") #2.5
install.packages("http://cran.r-project.org/src/contrib/Archive/survey/survey_3.30-3.tar.gz", repos=NULL, type="source") #3.30-3
install.packages("http://cran.r-project.org/src/contrib/Archive/clue/clue_0.3-50.tar.gz", repos=NULL, type="source") #0.3-50
install.packages("http://cran.r-project.org/src/contrib/Archive/proxy/proxy_0.4-15.tar.gz", repos=NULL, type="source") #0.4.15

#Packages that run behind (loaded via a namespace (and not attached))
install.packages("https://cran.r-project.org/src/contrib/crayon_1.3.4.tar.gz", repos=NULL, type="source") #1.69.0-1
install.packages("https://cran.r-project.org/src/contrib/BH_1.69.0-1.tar.gz", repos=NULL, type="source") #1.69.0-1
install.packages("https://cran.r-project.org/src/contrib/RcppEigen_0.3.3.5.0.tar.gz", repos=NULL, type="source") #0.3.3.5.0
install.packages("https://cran.r-project.org/src/contrib/dichromat_2.0-0.tar.gz", repos=NULL, type="source") #2.0.0
install.packages("https://cran.r-project.org/src/contrib/labeling_0.3.tar.gz", repos=NULL, type="source") #0.3
install.packages("https://cran.r-project.org/src/contrib/Archive/clipr/clipr_0.5.0.tar.gz", repos=NULL, type="source") #0.5.0
install.packages("http://cran.r-project.org/src/contrib/readr_1.3.1.tar.gz", repos=NULL, type="source") #1.3.1; this is not mentioned in sessioninfo, requires BH
install.packages("http://cran.r-project.org/src/contrib/nloptr_1.2.1.tar.gz", repos=NULL, type="source") #1.2.1; need module load nlopt/2.4.2-fasrc01
install.packages("http://cran.r-project.org/src/contrib/Archive/Rcpp/Rcpp_0.12.12.tar.gz", repos=NULL, type="source", INSTALL_opts = c('--no-lock'))
install.packages("http://cran.r-project.org/src/contrib/Archive/plyr/plyr_1.8.3.tar.gz", repos=NULL, type="source") #1.8.3
install.packages("http://cran.r-project.org/src/contrib/Archive/boot/boot_1.3-17.tar.gz", repos=NULL, type="source") #1.3.17
install.packages("http://cran.r-project.org/src/contrib/Archive/digest/digest_0.6.8.tar.gz", repos=NULL, type="source") #0.6.8
install.packages("http://cran.r-project.org/src/contrib/minqa_1.2.4.tar.gz", repos=NULL, type="source") #1.2.4
install.packages("http://cran.r-project.org/src/contrib/Archive/lme4/lme4_1.1-8.tar.gz", repos=NULL, type="source") #1.1-8
install.packages("http://cran.r-project.org/src/contrib/Archive/memoise/memoise_0.2.1.tar.gz", repos=NULL, type="source") #0.2.1
install.packages("http://cran.r-project.org/src/contrib/Archive/nlme/nlme_3.1-121.tar.gz", repos=NULL, type="source") #3.1-121
install.packages("http://cran.rstudio.com/src/contrib/Archive/gtable/gtable_0.1.2.tar.gz", repos=NULL, type="source") #0.1.2
install.packages("http://cran.r-project.org/src/contrib/Archive/mgcv/mgcv_1.8-7.tar.gz", repos=NULL, type="source") #1.8-7
install.packages("http://cran.r-project.org/src/contrib/Archive/DBI/DBI_0.3.1.tar.gz", repos=NULL, type="source") #0.3.1
install.packages("http://cran.r-project.org/src/contrib/Archive/SparseM/SparseM_1.6.tar.gz", repos=NULL, type="source") #1.6
install.packages("http://cran.r-project.org/src/contrib/Archive/proto/proto_0.3-10.tar.gz", repos=NULL, type="source") #0.3-10
install.packages("http://cran.r-project.org/src/contrib/Archive/nnet/nnet_7.3-10.tar.gz", repos=NULL, type="source") #7.3-10
install.packages("http://cran.r-project.org/src/contrib/Archive/R6/R6_2.2.0.tar.gz", repos=NULL, type="source")
install.packages("http://cran.r-project.org/src/contrib/Archive/stringr/stringr_1.0.0.tar.gz", repos=NULL, type="source") #1.0.0
install.packages("http://cran.r-project.org/src/contrib/Archive/reshape2/reshape2_1.4.1.tar.gz", repos=NULL, type="source") #1.4.1
install.packages("http://cran.r-project.org/src/contrib/magrittr_1.5.tar.gz", repos=NULL, type="source") #1.5
install.packages("http://cran.r-project.org/src/contrib/Archive/MASS/MASS_7.3-43.tar.gz", repos=NULL, type="source") #7.3-43
install.packages("http://cran.r-project.org/src/contrib/Archive/assertthat/assertthat_0.1.tar.gz", repos=NULL, type="source") #0.1
install.packages("http://cran.r-project.org/src/contrib/Archive/pbkrtest/pbkrtest_0.4-2.tar.gz", repos=NULL, type="source") #0.4-2
install.packages("http://cran.r-project.org/src/contrib/Archive/quantreg/quantreg_5.11.tar.gz", repos=NULL, type="source") #5.11
install.packages("http://cran.r-project.org/src/contrib/Archive/car/car_2.0-26.tar.gz", repos=NULL, type="source") #2.0-26
install.packages("http://cran.r-project.org/src/contrib/Archive/colorspace/colorspace_1.2-6.tar.gz", repos=NULL, type="source") #1.2-6
install.packages("http://cran.r-project.org/src/contrib/Archive/stringi/stringi_0.5-5.tar.gz", repos=NULL, type="source") #0.5-5
install.packages("http://cran.r-project.org/src/contrib/Archive/munsell/munsell_0.4.2.tar.gz", repos=NULL, type="source") #0.4.2
install.packages("http://cran.r-project.org/src/contrib/Archive/codetools/codetools_0.2-14.tar.gz", repos=NULL, type="source") #0.2-14
install.packages("http://cran.r-project.org/src/contrib/Archive/cluster/cluster_2.0.3.tar.gz", repos=NULL, type="source") #2.0.3

#Loaded packages
install.packages("http://cran.r-project.org/src/contrib/Archive/rdrobust/rdrobust_0.80.tar.gz", repos=NULL, type="source") #0.80
install.packages("http://cran.r-project.org/src/contrib/KernSmooth_2.23-15.tar.gz", repos=NULL, type="source")  #2.23-15
install.packages("http://cran.r-project.org/src/contrib/Archive/AER/AER_1.2-4.tar.gz", repos=NULL, type="source") #1.2-4
install.packages("http://cran.r-project.org/src/contrib/rdd_0.57.tar.gz", repos=NULL, type="source") #0.57
install.packages("http://cran.r-project.org/src/contrib/RColorBrewer_1.1-2.tar.gz", repos=NULL, type="source") #1.1-2
install.packages("http://cran.r-project.org/src/contrib/Archive/scales/scales_0.4.1.tar.gz", repos=NULL, type="source") #0.2.5
install.packages("http://cran.r-project.org/src/contrib/Archive/ggplot2/ggplot2_2.2.0.tar.gz", repos=NULL, type="source") #1.0.1
install.packages("http://cran.r-project.org/src/contrib/Archive/lazyeval/lazyeval_0.2.0.tar.gz", repos=NULL, type="source",dependencies=TRUE, INSTALL_opts = c('--no-lock'))  #0.2.0
install.packages("http://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_0.4.3.tar.gz", repos=NULL, type="source",dependencies=TRUE, INSTALL_opts = c('--no-lock'))  #0.4.3
install.packages("http://cran.r-project.org/src/contrib/Archive/tidyr/tidyr_0.3.1.tar.gz", repos=NULL, type="source",dependencies=TRUE, INSTALL_opts = c('--no-lock'))   #0.3.1
install.packages("http://cran.r-project.org/src/contrib/Archive/lubridate/lubridate_1.3.3.tar.gz", repos=NULL, type="source") #1.3.3
install.packages("http://cran.r-project.org/src/contrib/Archive/multiwayvcov/multiwayvcov_1.2.1.tar.gz", repos=NULL, type="source") #1.2.1
install.packages("http://cran.r-project.org/src/contrib/Archive/haven/haven_0.2.0.tar.gz", repos=NULL, type="source") #0.2.0
install.packages("http://cran.r-project.org/src/contrib/Archive/pryr/pryr_0.1.2.tar.gz", repos=NULL, type="source") #0.1.2
install.packages("http://cran.r-project.org/src/contrib/Archive/reshape/reshape_0.8.5.tar.gz", repos=NULL, type="source") #0.8.5
install.packages("http://cran.r-project.org/src/contrib/Archive/StatMatch/StatMatch_1.2.3.tar.gz", repos=NULL, type="source") #1.2.3
install.packages("http://cran.r-project.org/src/contrib/Archive/stargazer/stargazer_5.2.tar.gz", repos=NULL, type="source") #5.2
install.packages("http://cran.r-project.org/src/contrib/Archive/xtable/xtable_1.8-2.tar.gz", repos=NULL, type="source") #1.8.2
install.packages("http://cran.r-project.org/src/contrib/Archive/lfe/lfe_2.4-1788.tar.gz", repos=NULL, type="source") #2.4-1788
install.packages("http://cran.r-project.org/src/contrib/Archive/gdata/gdata_2.17.0.tar.gz", repos=NULL, type="source")
install.packages("http://cran.r-project.org/src/contrib/praise_1.0.0.tar.gz", repos=NULL, type="source") #2.0.1; this is not mentioned in sessioninfo
install.packages("https://cran.r-project.org/src/contrib/Archive/testthat/testthat_2.0.1.tar.gz", repos=NULL, type="source") #2.0.1; this is not mentioned in sessioninfo
install.packages("http://cran.r-project.org/src/contrib/fansi_0.4.0.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/rlang/rlang_0.3.1.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/pillar/pillar_1.3.1.tar.gz", repos=NULL, type="source")
install.packages("http://cran.r-project.org/src/contrib/pkgconfig_2.0.2.tar.gz", repos=NULL, type="source")
install.packages("https://cran.r-project.org/src/contrib/Archive/tibble/tibble_2.0.1.tar.gz", repos=NULL, type="source")
install.packages("http://cran.r-project.org/src/contrib/Archive/purrr/purrr_0.2.5.tar.gz", repos=NULL, type="source")
install.packages("http://cran.r-project.org/src/contrib/Archive/purrr/purrr_0.2.5.tar.gz", repos=NULL, type="source")
install.packages("http://cran.r-project.org/src/contrib/Archive/purrr/purrr_0.2.5.tar.gz", repos=NULL, type="source")
install.packages("http://cran.r-project.org/src/contrib/generics_0.0.2.tar.gz", repos=NULL, type="source")

install.packages("http://cran.r-project.org/src/contrib/Archive/broom/broom_0.5.1.tar.gz", repos=NULL, type="source")

# check all packages are installed
# note: some links might become obsolete when there is a new version is released and the old one is moved to Archive
ip <- as.data.frame(installed.packages(lib.loc = lib_path))
ip_names <- levels(ip$Package)
pkg <- c("Formula", "survival", "zoo", "lmtest", "sandwich", "lattice", "lpSolve", "RANN", "survey", "clue", "proxy",
         "crayon", "BH", "RcppEigen", "dichromat", "labeling", "clipr", "readr", "nloptr", "Rcpp", "plyr", "boot",
         "digest", "minqa", "lme4", "memoise", "nlme", "gtable", "mgcv", "DBI", "SparseM", "proto", "nnet", "R6",
         "stringr", "reshape2", "magrittr", "MASS", "assertthat", "pbkrtest", "quantreg", "car", "colorspace",
         "stringi", "munsell", "codetools", "cluster", "rdrobust", "KernSmooth", "AER", "rdd", "RColorBrewer",
         "scales", "ggplot2", "lazyeval", "dplyr", "tidyr", "lubridate", "multiwayvcov", "haven", "pryr", "reshape",
         "StatMatch", "stargazer", "xtable", "lfe", "gdata", "praise", "testthat", "fansi", "rlang", "pillar",
         "pkgconfig", "tibble", "purrr")
pkg_not_installed <- setdiff(pkg, ip_names)
if (length(pkg_not_installed) > 0) {
  stop("Some packages are not installed: ", pkg_not_installed)
}
rm(ip, ip_names, pkg, pkg_not_installed, user)
