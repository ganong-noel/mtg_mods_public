df_cons_rd <-
  readRDS(paste0(working_path, "df_cons_rd.rds")) %>%
  mutate(
    w = ifelse(abs(x) > bw_preferred, 0, abs((bw_preferred - abs(x))) / bw_preferred),
    w_wide = ifelse(abs(x) > bw_preferred, 0, abs((bw_window - abs(x))) / bw_window)
  )

# binscatter mimicking delinquency plots ####
lab_y_cons12 <- "Change in Mean Monthly Card Spending (12 months)"
iv_model <- ivreg(dol_chg_mean_cc_12m ~ fracForgivePos + x + xpos | dum + x + xpos,
  data = df_cons_rd %>% filter(abs(x) < bw_preferred),
  weights = w
)
coefIv <- round(iv_model$coefficients[2])
seIv <- round(summary(iv_model)$coefficients[8])
caption_text <- paste0("IV Effect of \nPrincipal Reduction: \n", coefIv, " (", seIv, ")")
gg_cons_12m <-
  gg_bin(
    df = df_cons_rd %>% filter(abs(x) < bw_window),
    dvs = c("dol_chg_mean_cc_12m"),
    caption = caption_text, y_range = c(-50, 20), scales = "dollar",
    xlab = lab_x, ylab = lab_y_cons12,
    file_suffix = "_cons_12m.png"
  )

lab_y_auto12 <- "Change in Mean Monthly Auto Spending (12 months)"
iv_model <- ivreg(dol_chg_mean_auto_12m ~ fracForgivePos + x + xpos | dum + x + xpos,
  data = df_cons_rd %>% filter(abs(x) < bw_preferred),
  weights = w
)
coefIv <- round(iv_model$coefficients[2])
seIv <- round(summary(iv_model)$coefficients[8])
caption_text <- paste0("IV Effect of \nPrincipal Reduction: \n", coefIv, " (", seIv, ")")
gg_auto_12m <-
  gg_bin(
    df = df_cons_rd,
    dvs = c("dol_chg_mean_auto_12m"),
    caption = caption_text, loc = "lr",
    scales = "dollar",
    y_range = c(-50, 200),
    xlab = lab_x, ylab = lab_y_auto12,
    file_suffix = "_auto_12m.png"
  )

rm(df_cons_rd)
