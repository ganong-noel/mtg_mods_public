## ---------------------#
## LOAD HEADER FILES ####
## ---------------------#

##
## Read in first header file
##
col_name_vector <- c(
  "rcd_tp",
  "sq_nm",
  "in_file_sin_dt",
  "dcd_dt",
  "state",
  "zip",
  "bir_dt",
  "aka_ctr",
  "aka_mn_ctr",
  "filetag",
  "bcs_seqnum"
)
widths <- c(2, 12, 8, 8, 2, 5, 8, 2, 2, 2, 7)
col_type_vector <- "cdiiciiiici"

system.time(DDVHDALL_2012 <- read_fwf(paste0(data_path, "tu_raw_download/PRM/INHOUSE/L112683/P232867/DDVHDALL"),
  fwf_widths(widths,
    col_names = col_name_vector
  ),
  col_types = col_type_vector,
  n_max = -1
))
DDVHDALL_2012 <- DDVHDALL_2012 %>% filter(rcd_tp != "\032")
saveRDS(DDVHDALL_2012, file = paste0(working_path, "tu_r_files/DDVHDALL_2012.rds"))

test_that('Trade file header unchanged (2012)',
          expect_equivalent(
            DDVHDALL_2012 %>%
            as.data.frame() %>%
              transmute(one = 1,
                        sq_nm,
                        aka_ctr,
                        bcs_seqnum) %>%
              summarise_each(funs(sum(as.numeric(.)),
                                  mean(is.na(.)))) %>%
              as.vector(),
            c(2682660, 4.695109e+16,
              2335688, 3.619013e+12,
              0, 0, 0, 0),
            tolerance = 1e-6))

rm(DDVHDALL_2012)

##
## Load 2014 header file
##

read_lines(paste0(data_path, "tu_raw_download/PRM/INHOUSE/L112683/P232868/DDVHDALL"), n_max = 5)
system.time(DDVHDALL_2014 <- read_fwf(paste0(data_path, "tu_raw_download/PRM/INHOUSE/L112683/P232868/DDVHDALL"),
  fwf_widths(widths,
    col_names = col_name_vector
  ),
  col_types = col_type_vector,
  n_max = -1
))
DDVHDALL_2014 <- DDVHDALL_2014 %>% filter(rcd_tp != "\032")
system.time(saveRDS(DDVHDALL_2014, file = paste0(working_path, "tu_r_files/DDVHDALL_2014.rds")))


test_that('Trade file header unchanged (2014)',
          expect_equivalent(
            DDVHDALL_2014 %>%
              as.data.frame() %>%
              transmute(one = 1,
                        sq_nm,
                        aka_ctr,
                        bcs_seqnum) %>%
              summarise_each(funs(sum(as.numeric(.)),
                                  mean(is.na(.)))) %>%
              as.vector(),
            c(2703129, 4.958854e+16,
              2781362, 3.653455e+12,
              0, 0, 0, 0),
            tolerance = 1e-6))


rm(DDVHDALL_2014)



#Define function for reading trade and trade history FWF

write_all_trade_files_rds <- function(col_name_vector,
                                 widths,
                                 col_type_vector,
                                 file_locations){
  map(c(2012, 2014), function(year)
    map(1:4, function(index)
      read_fwf(str_c(data_path,
                     "tu_raw_download/PRM/INHOUSE/L112683/",
                     ifelse(year == 2012, "P232867","P232868"),
                     "/",
                     file_locations,
                     ifelse(index == 1, "", as.character(index))),
               fwf_widths(widths,
                          col_names = col_name_vector),
               col_types = col_type_vector,
               n_max = -1) %>%
        saveRDS(str_c(working_path, "tu_r_files/",
                      file_locations,
                      index, "_",
                      year, ".rds", sep = ""))))
}

## -------------------##
## LOAD TRADE FILES ####
## -------------------##



read_lines(paste0(data_path, "tu_raw_download/PRM/INHOUSE/L112683/P232867/DDVTR"), n_max = 5)
col_name_vector <- c(
  "rcd_tp",
  "sq_nm",
  "act_id",
  "ind_cd",
  "rpt_sub_cd",
  "act_nm",
  "port_tp_cd",
  "act_tp_cd",
  "eco_act_cd",
  "opn_dt",
  "end_dt",
  "eff_dt",
  "paid_out_dt",
  "close_dt",
  "last_pymt_dt",
  "cur_bal_am",
  "high_crdt_am",
  "crdt_lmt_am",
  "or_chg_off_am",
  "act_pay_am",
  "past_due_am",
  "pay_due_am",
  "pay_sched_am",
  "freq_of_pymt",
  "aff_rmk_cd",
  "aff_rmk_cd_dt",
  "com_rmk_cd",
  "com_rmk_cd_dt",
  "gen_rmk_cd",
  "gen_rmk_cd_dt",
  "rat_rmk_cd",
  "rat_rmk_cd_dt",
  "mop",
  "pay_pattrn",
  "max_delin_mop",
  "max_delin_dt",
  "max_delin_pst_due_am",
  "most_rct_max_delin_dt",
  "Most_rct_max_delin_am",
  "pymt_typ_cd",
  "bal_pmt_due_am",
  "bal_pmt_due_dt",
  "def_pmt_st_dt",
  "port_sale_ind_cd",
  "crdtr_class_cd",
  "leg_proh_cd",
  "updt_ind",
  "rptng_sub_nam",
  "org_credt_nam",
  "port_sale_nam"
)

widths <- c(
  2,
  12,
  11,
  2,
  7,
  30,
  1,
  2,
  1,
  8,
  8,
  8,
  8,
  8,
  8,
  9,
  9,
  9,
  9,
  9,
  9,
  9,
  6,
  1,
  3,
  8,
  3,
  8,
  3,
  8,
  3,
  8,
  2,
  82,
  2,
  8,
  9,
  8,
  9,
  2,
  9,
  8,
  8,
  2,
  2,
  2,
  1,
  12,
  30,
  30
)


col_type_vector <- "cddcc_ccci_iiiiiiiiiiiiccicicicicc_____ciiiii_c___"

#Across the two years, and four files per year save the fwf as RDS files.

write_all_trade_files_rds(col_name_vector,
                          widths,
                          col_type_vector,
                          "DDVTR")

## ----------------------------#
## LOAD TRADE HISTORY FILES ####
## ----------------------------#
read_lines(paste0(data_path, "tu_raw_download/PRM/INHOUSE/L112683/P232867/DDVTH"), n_max = 5)

col_name_vector <- c(
  "rcd_tp",
  "sq_nm",
  "act_id",
  "ver_dt",
  "th_mop",
  "th_act_rtng_prof_cd",
  "th_cur_bal_am",
  "th_pay_due_am",
  "th_pay_am",
  "th_past_due_am",
  "th_high_cdt_am",
  "th_cred_limit_am",
  "th_comp_rmk_cd",
  "th_gen_rmk_cd",
  "th_rat_rmk_cd"
)

widths <- c(
  2,
  12,
  11,
  6,
  2,
  1,
  9,
  9,
  9,
  9,
  9,
  9,
  3,
  3,
  3
)
col_type_vector <- "cddicciiiiiiccc"

write_all_trade_files_rds(col_name_vector,
                          widths,
                          col_type_vector,
                          "DDVTH")
