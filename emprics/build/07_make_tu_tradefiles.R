map(c(2012, 2014), function(year)
  map(1:4, function(index)
    str_c(working_path, "tu_r_files/DDVTR", index, "_", year, ".rds", sep = "") %>%
      readRDS() %>%
      select(sq_nm,
             act_id,
             port_tp_cd,
             act_tp_cd,
             ind_cd,
             opn_dt,
             rpt_sub_cd,
             eco_act_cd)) %>%
    bind_rows() %>%
    saveRDS(file = paste0(working_path, "tradefile_", year, ".rds")))
