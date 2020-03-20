#should have packages attaches at the RDD stage in master

df_tmp <- df %>%
  filter(abs(x_unnormed) > 1)

bw_lin <-
    rdbwselect(df_tmp$anyDelin, df_tmp$x_unnormed,
               bwselect = "IK", p = 1, q = 2)$bws[, c("h", "b")] %>%
    as.numeric() %>%
    round(3)

bw_preferred <- bw_lin[1]
bw_window <- 10000


df <-
  df %>%
  mutate(w = ifelse(abs(x_unnormed) > bw_preferred, 0,
                    abs((bw_preferred - abs(x_unnormed))) / bw_preferred))
make_plot <- function(variable, data, reduced_form, iv_reg){

  if (variable == "delin_pred"){
    variable_lab <- "Predicted Default Rate"

    caption_text <<- str_c("RD \"Effect\" of\nPrincipal Reduction:\n",
                           round(reduced_form$estimate, 3),
                           " (",
                           round(reduced_form$std.error, 3),
                           ")")
  } else if(variable == "anyDelin") {
    variable_lab <- "Default Rate"

    caption_text <<- str_c("IV Effect of\nPrincipal Reduction:\n",
                           round(iv_reg$estimate, 3),
                           " (",
                           round(iv_reg$std.error, 3),
                           ")")
  }



  gg_bin(
    data %>%
      mutate(x = x_unnormed) %>%
      as.data.frame(), #needed for subsetting within gg_bin
    dvs = c("dep_var"),
    x = "x",
    caption = caption_text, #don't want to alter the function for backwards compat.
    loc = "ul",
    xlab = "Delta NPV from Principal Reduction over Payment Reduction Mod (in $)",
    ylab = variable_lab,
    scales = "percent",
    file_suffix = str_c("_", variable, "_unnormed.png"))
}


for_iteration_df <- df %>%
  as.data.frame() %>%
  as_tibble() %>%
  filter(abs(x_unnormed) > 1) %>%
  select(pos, x_unnormed, fracForgivePos, w, anyDelin, delin_pred) %>%
  gather(variable, dep_var, anyDelin, delin_pred)

rm(df)

for(variable_it in c("anyDelin",
                  "delin_pred")){

  df <- for_iteration_df %>%
    filter(variable == variable_it)

  reduced_form <- lm(dep_var ~ pos*x_unnormed,
                     data = df %>%
                       filter(abs(x_unnormed) < bw_preferred),
                     weights = w) %>%
    broom::tidy() %>%
    filter(term == "posTRUE") %>%
    select(estimate, std.error)

  iv_reg <- ivreg(dep_var ~ fracForgivePos + x_unnormed +
                    + x_unnormed:pos |x_unnormed +
                    + x_unnormed:pos + pos,
                  data = df %>% filter(abs(x_unnormed) < bw_preferred),
                  weights = w) %>%
    broom::tidy() %>%
    filter(term == "fracForgivePosTRUE") %>%
    select(estimate, std.error)

  make_plot(variable_it, df, reduced_form, iv_reg)
}
