## Check if bw_preferred has been set

if (matched == "Yes") {
  if (exists("bw_preferred")) {
    test_that("Bandwith is", {
      expect_equal(bw_preferred, 0.601)
    })
  } else {
    bw_preferred <- 0.601
  }
} else if (matched == "No") {
  if (exists("bw_preferred")) {
    test_that("Bandwith is", {
      expect_equal(bw_preferred, 0.525)
    })
  } else {
    bw_preferred <- 0.525
  }
}

write_output <- function(output, i, bw, dv, lm, polynomial = "linear") {
  output[i, "dv"] <- dv
  output[i, "bw"] <- bw
  output[i, "polynomial"] <- polynomial
  output[i, "intercept"] <- round(lm$coefficients[1], 4)
  output[i, "coef"] <- round(lm$coefficients[2], 4)
  output[i, "se"] <- round(summary(lm)$coefficients[2, 2], 4)
  output[i, "coef_min"] <- output[i, "coef"] - 1.96 * round(summary(lm)$coefficients[2, 2], 4)
  output[i, "coef_max"] <- output[i, "coef"] + 1.96 * round(summary(lm)$coefficients[2, 2], 4)
  return(output)
}


output <- data.frame(bw = NULL, coef = NULL, se = NULL, depvar = NULL)
i <- 1
dv_list_every_bw <-
  c(
    "anyDelin", "delin_pred", "delin_pred_rich",
    "brwr_curr_crdt_scr_val", "delin_pred_fracForgivePos"
  )
bw_list <- seq(bw_preferred - 0.3, bw_preferred + 1.4, 0.1)


for (dv in dv_list_every_bw) {
  for (bw in bw_list) {
    df <-
      df %>%
      mutate_(dv = dv) %>%
      mutate(w = ifelse(abs(x) > bw, 0, abs((bw - abs(x))) / bw))
    if (dv == "anyDelin") {
      ctrl <- "+ brwr_curr_crdt_scr_val"
      lm <- ivreg(paste0("dv ~ fracForgivePos + x + xpos  ", ctrl, " | pos + x + xpos", ctrl),
        data = df %>% filter(abs(x) < bw),
        weights = w
      )
      output <- write_output(
        output = output, i = i, bw = bw,
        dv = paste0(dv, ctrl), lm = lm, polynomial = "linear"
      )
      lm_q <- ivreg(paste0(
        "dv ~ fracForgivePos + x + xpos + x2 + xpos2  ",
        ctrl, " | pos + x + xpos + x2 + xpos2", ctrl
      ),
      data = df %>% filter(abs(x) < bw), weights = w
      )
      output <- write_output(
        output = output, i = i + 1, bw = bw,
        dv = paste0(dv, ctrl), lm = lm_q, polynomial = "quadratic"
      )
      i <- i + 2
      ctrl <- "+ delin_pred"
      lm <- ivreg(paste0(
        "dv ~ fracForgivePos + x + xpos  ", ctrl,
        " | pos + x + xpos", ctrl
      ),
      data = df %>% filter(abs(x) < bw),
      weights = w
      )
      output <- write_output(
        output = output, i = i, bw = bw,
        dv = paste0(dv, ctrl), lm = lm, polynomial = "linear"
      )
      lm_q <- ivreg(paste0(
        "dv ~ fracForgivePos + x + xpos + x2 + xpos2  ",
        ctrl, " | pos + x + xpos + x2 + xpos2", ctrl
      ),
      data = df %>% filter(abs(x) < bw),
      weights = w
      )
      output <- write_output(
        output = output, i = i + 1, bw = bw,
        dv = paste0(dv, ctrl), lm = lm_q, polynomial = "quadratic"
      )
      i <- i + 2
      lm <- ivreg(paste0("dv ~ fracForgivePos + x + xpos | pos + x + xpos"),
        data = df %>% filter(abs(x) < bw),
        weights = w
      )
      lm_q <- ivreg(paste0("dv ~ fracForgivePos + x + xpos + x2 + xpos2 | pos + x + xpos + x2 + xpos2"),
        data = df %>% filter(abs(x) < bw),
        weights = w
      )
    }
    else {
      lm <- lm(dv ~ pos + x + xpos, data = df %>% filter(abs(x) < bw), weights = w)
      lm_q <- lm(dv ~ pos + x + xpos + x2 + xpos2, data = df %>% filter(abs(x) < bw), weights = w)
    }
    output <- write_output(output = output, i = i, bw = bw, dv = dv, lm = lm, polynomial = "linear")
    output <- write_output(output = output, i = i + 1, bw = bw, dv = dv, lm = lm_q, polynomial = "quadratic")
    i <- i + 2
  }
}
output <- output %>% filter(!is.na(dv))

# rd coefficients at preferred bw ####
if (matched == "Yes") {
  dv_list_single_bw <-
    c(
      "anyDelin", "delin_pred", "delin_pred_rich", "fracForgivePos",
      "brwr_mthy_grs_incm_amt.x", "ln_bef_mdfc_frnt_rto_pct", "ln_upb_frgv_amt.x",
      "d_ltv_pct", "mtm_ltv_post", "ln_mtm_ltv_pct", "mtm_ltv_post_upb",
      "ln_bef_mdfc_upb_amt.x", "prop_valu_as_is_val_amt",
      "ln_pst_due_prd_cnt", "ln_orgnn_ltv_pct", "brwr_mthy_dpmt_xcldg_piti_amt",
      "ln_bef_mdfc_pi_pmt_amt.x", "ln_aft_mdfc_pi_pmt_amt", "dpay", "dpay_pct",
      "delin_v5", "delin_v4", "delin_v5_ltv", "delin_v4_ltv", "pra_specific",
      "any_disqual_tu.x", "any_fpi_tu", "dNPV_cashflow"
    )
} else if (matched == "No") {
  dv_list_single_bw <- c(
    "anyDelin", "delin_pred", "fracForgivePos", "brwr_mthy_grs_incm_amt.x",
    "ln_bef_mdfc_frnt_rto_pct", "ln_upb_frgv_amt.x",
    "d_ltv_pct", "mtm_ltv_post", "ln_mtm_ltv_pct", "ln_bef_mdfc_upb_amt.x",
    "ln_pst_due_prd_cnt", "dpay"
  )
}
output_pref <- data.frame(bw = NULL, coef = NULL, se = NULL, depvar = NULL)
i <- 1
for (dv in dv_list_single_bw) {
  df <-
    df %>%
    mutate_(dv = dv) %>%
    mutate(w = ifelse(abs(x) > bw_preferred, 0, abs((bw_preferred - abs(x))) / bw_preferred))
  lm <- lm(dv ~ pos + x + xpos,
    data = df %>% filter(abs(x) < bw_preferred),
    weights = w
  )
  output_pref <- write_output(
    output = output_pref, i = i, bw = bw_preferred,
    dv = dv, lm = lm, polynomial = "linear"
  )
  i <- i + 1
}
output_pref <- output_pref %>% filter(!is.na(dv))

df <-
  df %>%
  mutate(
    balance_due_at_sale = ln_aft_mdfc_upb_amt + ln_prin_frbrn_amt,
    balance_pre_mod_tess = ln_aft_mdfc_upb_amt + ln_upb_frgv_amt.x + ln_prin_frbrn_amt,
    PR_shr_UPB = ln_upb_frgv_amt.x / balance_pre_mod_tess,
    PR_shr_UPB_email_first = ln_prin_rdcn_altv_amt / balance_pre_mod_tess,
    ln_prin_rdcn_altv_amt_0 = ifelse(is.na(ln_prin_rdcn_altv_amt), 0, ln_prin_rdcn_altv_amt),
    PR_shr_UPB_email_first_0 = ln_prin_rdcn_altv_amt_0 / balance_pre_mod_tess
  )

# IV coefficients at preferred bw ####
output_pref_iv <- data.frame(bw = NULL, coef = NULL, se = NULL, depvar = NULL)
i <- 1
for (dv in dv_list_single_bw) {
  df <-
    df %>%
    mutate_(dv = dv) %>%
    mutate(w = ifelse(abs(x) > bw_preferred, 0, abs((bw_preferred - abs(x))) / bw_preferred))
  lm <- ivreg(dv ~ fracForgivePos + x + xpos | pos + x + xpos,
    data = df %>% filter(abs(x) < bw_preferred),
    weights = w
  )
  output_pref_iv <- write_output(
    output = output_pref_iv, i = i, bw = bw_preferred,
    dv = dv, lm = lm, polynomial = "linear"
  )
  i <- i + 1
}
output_pref_iv <- output_pref_iv %>% filter(!is.na(dv))


# set universal parameters
lab_x <- "Delta NPV from Principal Reduction over Payment Reduction Mod (in %)"
lab_x_dollar <- "Delta NPV from Principal Reduction over Payment Reduction Mod (in $)"

bw_window <- 2
first_stage <-
  output_pref %>%
  filter(polynomial == "linear" & dv == "fracForgivePos") %>%
  select(coef) %>%
  as.numeric()
d_delin_ust_coef <-
  output_pref %>%
  filter(polynomial == "linear" & dv == "delin_v4") %>%
  select(coef) %>%
  as.numeric()
d_delin_ust <- d_delin_ust_coef / first_stage

if (matched == "Yes") {

  # graphs of RD estimates by bandwidth ####
  gg_anyDelin_est_by_bw <-
    gg_coef(
      df = output,
      dvs = "anyDelin",
      ylab = "IV Estimate and 95% CI",
      file_suffix = "_delin_cut_by_bw.png",
      legend_pos = c(1, 1.1), y_range = c(-0.1, 0.14)
    )

  gg_anyDelin_est_by_bw_lin <-
    gg_coef(
      df = output %>%
        filter(polynomial == "linear") %>%
        mutate(group = bw == bw_preferred),
      dvs = "anyDelin",
      group_var = "group",
      legend_labels = c("Other", "IK Optimal"),
      legend_title = "Bandwidth",
      out_path_f = out_path_diagnosis,
      file_suffix = "_delin_cut_by_bw_lin.png",
      legend_pos = c(1, 0.08)
    )

  gg_delin_pred_est_by_bw <-
    ggplot(output %>% filter(dv == "delin_pred") %>% mutate_(group = "polynomial")) +
    geom_point(aes(x = bw, y = coef, shape = group, colour = group, ymax = coef),
      position = position_dodge(width = 0.05)
    ) +
    geom_errorbar(aes(
      x = bw, y = coef, colour = group,
      ymax = coef_max, ymin = coef_min, width = 0.05
    ),
    position = position_dodge(width = 0.05)
    ) +
    fte_theme() +
    labs(x = "Bandwidth", y = "RD Estimate and 95% CI") +
    scale_colour_manual("polynomial", values = cbPalette[2:8], labels = c("linear", "quadratic")) +
    scale_shape_discrete("polynomial", labels = c("linear", "quadratic")) +
    theme(legend.position = c(1, 0), legend.justification = c(1, 0))
  ggsave(
    file = paste0(out_path_diagnosis, "rd_", samp_file_name, "_delin_pred_cut_by_bw.png"),
    gg_delin_pred_est_by_bw, width = wd, height = ht
  )

  gg_delin_pred_est_by_bw_lin <-
    ggplot(output %>%
      filter(dv == "delin_pred" & polynomial == "linear") %>%
      mutate(group = bw == bw_preferred) %>%
      mutate_(group = "group")) +
    geom_point(aes(x = bw, y = coef, shape = group, colour = group, ymax = coef),
      position = position_dodge(width = 0.05)
    ) +
    geom_errorbar(aes(
      x = bw, y = coef, colour = group,
      ymax = coef_max, ymin = coef_min, width = 0.05
    ),
    position = position_dodge(width = 0.05)
    ) +
    fte_theme() +
    labs(x = "Bandwidth", y = "RD Estimate and 95% CI") +
    scale_colour_manual("Bandwidth", values = cbPalette[2:8], labels = c("Other", "IK Optimal")) +
    scale_shape_discrete("Bandwidth", labels = c("Other", "IK Optimal")) +
    theme(legend.position = c(1, 0), legend.justification = c(1, 0))
  ggsave(
    file = paste0(out_path_diagnosis, "rd_", samp_file_name, "_delin_pred_cut_by_bw_lin.png"),
    gg_delin_pred_est_by_bw_lin, width = wd, height = ht
  )
}

# binscatters: Default ####
lab_y_anyDelin <- "Default Rate"
reg <-
  round(output %>%
    filter(round(bw, 3) == bw_preferred & polynomial == "linear" & dv == "anyDelin") %>%
    select(coef, se), 4) %>%
  format(scientific = FALSE, nsmall = 4)
if (matched == "No") {
  reg <-
    round(output %>%
            filter(bw == as.character(bw_preferred) & polynomial == "linear" & dv == "anyDelin") %>%
            select(coef, se), 4) %>%
    format(scientific = FALSE, nsmall = 4)
}
base <-
  df %>%
  summarise(mean(anyDelin)) %>%
  as.numeric()
impact_ub <- as.numeric(reg["coef"]) - 1.96 * as.numeric(reg["se"])
or_ub <- round(base / (1 - base) / ((base + impact_ub) / (1 - (base + impact_ub))), 2)
caption_text <- paste0("IV Effect of \nPrincipal Reduction: \n", reg["coef"], " (", reg["se"], ")")
gg_anyDelin <-
  gg_bin(
    df = df,
    dvs = c("anyDelin"),
    y_div = seq(0.12, 0.24, 0.04),
    y_range = c(0.10, 0.25),
    caption = caption_text,
    loc = "lr",
    xlab = lab_x, ylab = lab_y_anyDelin,
    file_suffix = "_delin.png",
    scales = "percent"
  )

if (matched == "Yes") {
  gg_anyDelin <-
    gg_bin(
      df = df,
      dvs = c("anyDelin"),
      y_range = c(0.10, 0.25),
      caption = caption_text,
      loc = "lr", grid = FALSE,
      xlab = lab_x, ylab = lab_y_anyDelin,
      out_path_f = out_path_diagnosis,
      file_suffix = "_delin_no_grid.png",
      scales = "percent"
    )


  gg_anyDelin_no_yrange <-
    gg_bin(
      df = df,
      dvs = c("anyDelin"),
      caption = caption_text,
      loc = "lr",
      xlab = lab_x, ylab = lab_y_anyDelin,
      out_path_f = out_path_diagnosis,
      file_suffix = "_delin_no_yrange.png"
    )

  caption_text <- ""
  gg_anyDelin <-
    gg_bin(
      df = df,
      dvs = c("anyDelin"),
      y_range = c(0.10, 0.25),
      caption = caption_text,
      loc = "lr",
      save_temp = TRUE,
      xlab = lab_x, ylab = lab_y_anyDelin,
      file_suffix = "_delin_media.png",
      scales = "percent",
      title = "Default Unchanged at Principal Reduction Eligibility Cutoff"
    )

  tmp <- readRDS("out/rd_data_temp.RDS")
  point_media_pra_1 <- tmp %>%
    ungroup() %>%
    select(x, y, -cut) %>%
    dplyr::rename(dnpv_bin = x, delin = y)

  line <- readRDS("out/rd_data_line.RDS")
  line_media_pra_1 <- line %>%
    select(x, y) %>%
    dplyr::rename(dnpv_bin = x, delin_ll = y)
  file.remove("out/rd_data_temp.RDS")
  file.remove("out/rd_data_line.RDS")
}

reg <-
  round(output %>%
    filter(round(bw, 3) == bw_preferred & polynomial == "linear" & dv == "delin_pred") %>%
    select(coef, se), 4)
caption_text <-
  paste0("RD \"Effect\" of \nPrincipal Reduction: \n", reg["coef"], " (", reg["se"], ")")
gg_rd_delin_pred <-
  gg_bin(
    df = df,
    dvs = "delin_pred",
    caption = caption_text,
    loc = "lr", y_range = c(0.10, 0.25),
    xlab = lab_x, ylab = "Predicted Default Rate",
    coef_size = 5.23, scales = "percent",
    file_suffix = "_delin_pred.png"
  )

if (matched == "Yes") {
  # TU foreclosure start
  lab_y_any_fpi <- "Foreclosure Initiation Rate"
  reg <-
    round(output_pref_iv %>%
      filter(bw == bw_preferred & polynomial == "linear" & dv == "any_fpi_tu") %>%
      select(coef, se), 4) %>%
    format(scientific = FALSE)
  caption_text <- paste0("IV Effect of \nPrincipal Reduction: \n", reg["coef"], " (", reg["se"], ")")
  gg_rd_any_fpi <-
    gg_bin(
      df = df,
      dvs = c("any_fpi_tu"),
      y_range = c(0, 0.06),
      caption = caption_text, loc = "lr",
      xlab = lab_x, ylab = lab_y_any_fpi,
      scales = "percent",
      file_suffix = "_any_fpi.png"
    )
}

# binscatters: first stage measures ####
reg <-
  round(output_pref %>%
    filter(polynomial == "linear" & dv == "fracForgivePos") %>%
    select(coef, se), 2) %>%
  format(scientific = FALSE)
caption_text <- paste0("RD Estimate: \n", reg["coef"], " (", reg["se"], ")")
gg_rd_any_frgv <-
  gg_bin(
    df = df,
    dvs = c("fracForgivePos"),
    caption = caption_text, loc = "lr",
    xlab = lab_x, ylab = "Share Getting Principal Reduction",
    y_div = seq(0.12, 0.6, 0.12),
    file_suffix = "_pra.png",
    scales = "percent"
  )

if (matched == "Yes") {
  gg_rd_any_frgv <-
    gg_bin(
      df = df,
      dvs = c("fracForgivePos"),
      caption = caption_text, loc = "lr",
      xlab = lab_x, ylab = "Share Getting Principal Reduction",
      y_div = seq(0.12, 0.6, 0.12), grid = FALSE,
      out_path_f = out_path_diagnosis,
      file_suffix = "_pra_no_grid.png",
      scales = "percent"
    )


  caption_text <- ""
  gg_rd_any_frgv <-
    gg_bin(
      df = df,
      dvs = c("fracForgivePos"),
      caption = caption_text, loc = "lr",
      xlab = lab_x, ylab = "Share Getting Principal Reduction",
      save_temp = TRUE,
      file_suffix = "_pra_media.png",
      scales = "percent",
      title = "41% More Borrowers Receive Principal Reduction \n Above Eligibility Cutoff"
    )

  tmp <- readRDS("out/rd_data_temp.RDS")
  point_media_pra_2 <- tmp %>%
    ungroup() %>%
    select(x, y, -cut) %>%
    dplyr::rename(dnpv_bin = x, pra_share = y)

  line <- readRDS("out/rd_data_line.RDS")
  line_media_pra_2 <- line %>%
    select(x, y) %>%
    dplyr::rename(dnpv_bin = x, pra_share_ll = y)

  point_media_pra <- point_media_pra_1 %>%
    left_join(point_media_pra_2)
  line_media_pra <- line_media_pra_1 %>%
    left_join(line_media_pra_2)

  # library(xlsx)
  # path <- "out/not_for_rep_kit/media_kit/data_underlying_plots.xlsx"
  # wb <- loadWorkbook(path)
  # sheets <- getSheets(wb)
  # if (length(sheets) > 2) {
  #   removeSheet(wb, sheetName = names(sheets[2]))
  #   removeSheet(wb, sheetName = names(sheets[3]))
  # }
  # saveWorkbook(wb, path)
  #
  # write.xlsx(point_media_pra,
  #            file = "out/not_for_rep_kit/media_kit/data_underlying_plots.xlsx",
  #            sheetName = "tbl_recreate_rd_dots_pra",
  #            append = TRUE)
  # write.xlsx(line_media_pra,
  #            file = "out/not_for_rep_kit/media_kit/data_underlying_plots.xlsx",
  #            sheetName = "tbl_recreate_rd_line_pra",
  #            append = TRUE)

  file.remove("out/rd_data_temp.RDS")
  file.remove("out/rd_data_line.RDS")
  # rm(path)
  # rm(wb)
  # rm(sheets)

  file.copy("out/rd_Matched_pra_media.png",
            "out/not_for_rep_kit/media_kit/principal_reduction_first.png",
            overwrite = TRUE)
  file.copy("out/rd_Matched_delin_media.png",
            "out/not_for_rep_kit/media_kit/principal_reduction_results.png",
            overwrite = TRUE)
  file.remove("out/rd_Matched_pra_media.png")
  file.remove("out/rd_Matched_delin_media.png")


  reg <-
    round(output_pref %>%
      filter(polynomial == "linear" & dv == "d_ltv_pct") %>%
      select(coef, se), 1) %>%
    format(scientific = FALSE)
  caption_text <-
    paste0(
      "RD Estimate: \n", reg["coef"], " (", reg["se"], ")\n",
      "IV Estimate: \n", round(as.numeric(reg["coef"]) / first_stage, 1),
      " (", specify_decimal(round(as.numeric(reg["se"]) / first_stage, 1), 1), ")"
    )
  gg_dltv <-
    gg_bin(
      df = df, dvs = c("d_ltv_pct"),
      caption = caption_text,
      xlab = lab_x, ylab = "dMTM LTV",
      file_suffix = "_dltv.png"
    )

  reg <-
    round(output_pref %>%
      filter(polynomial == "linear" & dv == "ln_upb_frgv_amt.x") %>%
      select(coef, se)) %>%
    format(scientific = FALSE)
  caption_text <- paste0(
    "RD Estimate: \n", reg["coef"], " (", reg["se"], ")\n",
    "IV Estimate: \n", round(as.numeric(reg["coef"]) / first_stage),
    " (", round(as.numeric(reg["se"]) / first_stage), ")"
  )
  gg_rd_forgive_nominal <-
    gg_bin(
      df = df, dvs = c("ln_upb_frgv_amt.x"),
      caption = caption_text, loc = "lr",
      xlab = lab_x, ylab = "Mean Amount of Mortgage Balance Reduced",
      file_suffix = "_forgive_nominal.png",
      scales = "dollar"
    )

  reg <-
    round(output_pref %>%
      filter(polynomial == "linear" & dv == "dNPV_cashflow") %>%
      select(coef, se)) %>%
    format(scientific = FALSE)
  caption_text <- paste0(
    "RD Estimate: \n", reg["coef"], " (", reg["se"], ")\n",
    "IV Estimate: \n", round(as.numeric(reg["coef"]) / first_stage),
    " (", round(as.numeric(reg["se"]) / first_stage), ")"
  )
  gg_rd_forgive_npv <-
    gg_bin(
      df = df, dvs = c("dNPV_cashflow"),
      caption = caption_text,
      xlab = lab_x, ylab = "Mean Change in NPV of Mortgage Payments",
      file_suffix = "_forgive_npv.png",
      scales = "dollar"
    )

  reg <-
    round(output_pref %>%
      filter(polynomial == "linear" & dv == "dpay") %>%
      select(coef, se)) %>%
    format(scientific = FALSE)
  caption_text <- paste0(
    "RD Estimate: \n", reg["coef"], " (", reg["se"], ")\n",
    "IV Estimate: \n", round(as.numeric(reg["coef"]) / first_stage),
    " (", round(as.numeric(reg["se"]) / first_stage), ")"
  )
  gg_rd_dpay <-
    gg_bin(
      df = df, dvs = c("dpay"),
      caption = caption_text, loc = "lr",
      xlab = lab_x, ylab = "Mean Change in Monthly Housing Payment",
      file_suffix = "_dpay.png",
      scales = "dollar"
    )

  reg <-
    round(output_pref %>%
      filter(polynomial == "linear" & dv == "dpay_pct") %>%
      select(coef, se)) %>%
    format(scientific = FALSE)
  caption_text <- paste0(
    "RD Estimate: \n", reg["coef"], " (", reg["se"], ")\n",
    "IV Estimate: \n", round(as.numeric(reg["coef"]) / first_stage),
    " (", round(as.numeric(reg["se"]) / first_stage), ")"
  )
  gg_rd_dpay_pct <-
    gg_bin(
      df = df, dvs = c("dpay_pct"),
      caption = caption_text, loc = "lr",
      xlab = lab_x, ylab = "Mean Change in Monthly Housing Payment (%)",
      out_path_f = out_path_diagnosis,
      file_suffix = "_dpaypct.png",
      scales = "percent"
    )

  reg <- round(output_pref %>% filter(polynomial == "linear" & dv == "ln_bef_mdfc_pi_pmt_amt.x") %>%
    select(coef, se)) %>% format(scientific = FALSE)
  gg_rd_pay <-
    gg_bin(
      df = df, dvs = c("ln_bef_mdfc_pi_pmt_amt.x"),
      caption = paste0("RD Estimate: \n", reg["coef"], "\n(", reg["se"], ")"),
      xlab = lab_x, ylab = "Mean Monthly Housing Payment",
      scales = "dollar",
      out_path_f = paste0(out_path, "slide/"), file_suffix = "_pay_pre.png"
    )
}

# binscatters: balance pre-mod ####
reg <-
  round(output_pref %>%
    filter(polynomial == "linear" & dv == "ln_mtm_ltv_pct") %>%
    select(coef, se), 2) %>%
  format(scientific = FALSE)
caption_text <- paste0("RD Estimate: \n", reg["coef"], " (", reg["se"], ")")
gg_mtm_ltv <-
  gg_bin(
    df = df, dvs = c("ln_mtm_ltv_pct"),
    caption = paste0("RD Estimate: \n", reg["coef"], " (", reg["se"], ")"),
    loc = "lr",
    xlab = lab_x, ylab = "Loan-to-Value Ratio (x 100)",
    coef_size = 5.63,
    file_suffix = "_mtmltv_pre.png"
  )

reg <-
  round(output_pref %>%
    filter(polynomial == "linear" & dv == "ln_pst_due_prd_cnt") %>%
    select(coef, se), 2) %>%
  format(scientific = FALSE)
caption_text <- paste0("RD Estimate: \n", reg["coef"], " (", reg["se"], ")")
gg_pastdue <-
  gg_bin(
    df = df, dvs = c("ln_pst_due_prd_cnt"),
    caption = caption_text,
    xlab = lab_x, ylab = "Months Past Due", coef_size = 5.63,
    file_suffix = "_pastdue.png"
  )

reg <- round(output_pref %>%
  filter(polynomial == "linear" & dv == "brwr_mthy_grs_incm_amt.x") %>%
  select(coef, se)) %>%
  format(scientific = FALSE)
caption_text <- paste0("RD Estimate: \n", reg["coef"], " (", reg["se"], ")")
gg_rd_incm <-
  gg_bin(
    df = df, dvs = c("brwr_mthy_grs_incm_amt.x"),
    caption = caption_text,
    xlab = lab_x, ylab = "Monthly Income",
    coef_size = 5.63, scales = "dollar",
    file_suffix = "_incm.png"
  )

reg <-
  round(output %>%
    filter(round(bw, 3) == bw_preferred & polynomial == "linear" & dv == "brwr_curr_crdt_scr_val") %>%
    select(coef, se), 2) %>%
  format(scientific = FALSE)
caption_text <- paste0("RD Estimate: \n", reg["coef"], " (", reg["se"], ")")
gg_rd_fico <-
  gg_bin(
    df = df, dvs = c("brwr_curr_crdt_scr_val"),
    caption = caption_text,
    xlab = lab_x, ylab = "FICO",
    coef_size = 5.63,
    file_suffix = "_fico.png"
  )


reg <-
  round(output_pref %>%
    filter(polynomial == "linear" & dv == "ln_bef_mdfc_frnt_rto_pct") %>%
    select(coef, se), 2) %>%
  format(scientific = FALSE)
caption_text <- paste0("RD Estimate: \n", reg["coef"], " (", reg["se"], ")")
gg_dti <- gg_bin(
  df = df, dvs = c("ln_bef_mdfc_frnt_rto_pct"),
  caption = caption_text,
  xlab = lab_x, ylab = "Payment-to-Income Ratio (x 100)",
  coef_size = 5.63,
  file_suffix = "_dti.png"
)

write.csv(output_pref, "out/rd_delin_coefs.csv")
write.csv(output_pref_iv, "out/rd_delin_iv_coefs.csv")

if (matched == "Yes") {

  # Default rate in HAMP
  tmp <- binscatter("anyDelin", "d_dti",
    df %>% filter(abs(x) < bw_window & d_dti < 50),
    n.cut = n_cut
  )$df_bin %>%
    mutate(group = "HAMP Cross-Sectional")

  # Default rate in Chase and GSE
  chase <- data.frame(
    x = c(0, 12.10471789, 29.5491547),
    y = c(0.41, 0.2851130151, 0.2383714457),
    group = "Chase Causal",
    cut = NA
  )
  summary(glm(y ~ x, family = "binomial", data = chase))

  gse <- data.frame(
    x = c(4.5, 27.5),
    y = c(0.275, 0.218),
    group = "GSE Causal",
    cut = NA
  )
  tmp_combined <- bind_rows(tmp, chase, gse)

  gg <- ggplot(
    tmp_combined,
    aes(x = x, y = y, group = group)
  ) +
    geom_point(aes(colour = group)) +
    labs(
      x = "Reduction in Monthly Payment (Measured as % of Income at Mod Date)",
      y = "Default Rate"
    ) +
    fte_theme() +
    stat_smooth(
      data = tmp_combined %>% filter(group == "Chase Causal"),
      method = "glm",
      method.args = list(family = "binomial"),
      formula = y ~ x,
      size = 0.5, colour = cbPalette_set2[1],
      se = FALSE, fullrange = TRUE
    ) +
    stat_smooth(
      data = tmp_combined %>% filter(group == "GSE Causal"),
      method = "glm",
      method.args = list(family = "binomial"),
      formula = y ~ x,
      size = 0.5, colour = cbPalette_set2[2],
      se = FALSE, fullrange = TRUE
    ) +
    stat_smooth(
      data = tmp_combined %>% filter(group == "HAMP Cross-Sectional"),
      method = "lm",
      method.args = list(family = "binomial"),
      formula = y ~ x,
      size = 0.5, colour = cbPalette_set2[3], se = FALSE
    ) +
    theme(
      legend.title = element_blank(),
      legend.justification = c(1, 1),
      legend.position = c(1, 1),
      legend.text = element_text(size = 7)
    ) +
    scale_colour_manual(values = c(cbPalette_set2[1], cbPalette_set2[2], cbPalette_set2[3]))
  ggsave(file = paste0(out_path_diagnosis, "d_dti_anyDelin_incl_tmp.png"), gg, width = wd, height = ht)

  rm(list = ls(pattern = "^lm"))
  rm(list = ls(pattern = "^m_v"))
  rm(list = ls(pattern = "^line"))
  rm(list = ls(pattern = "^tmp"))
  rm(chase, gse, reg)
  rm(df_base, df_nonmissing_rich, output_pref_iv, output)
}
