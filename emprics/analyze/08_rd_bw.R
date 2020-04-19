# optimal bandwidth calculation ####
optimal_bw <- data.frame(
  method = "Method", max_width = NA,
  bw_lin_h = 0, bw_lin_b = 0, bw_quad_h = 0, bw_quad_b = 0,
  runtime = 0, stringsAsFactors = FALSE
)
colnames(optimal_bw) <-
  c("method", "max_width", "bw_lin_h", "bw_lin_b", "bw_quad_h", "bw_quad_b", "runtime")
i <- 1

for (max_width in c(1, 2, 4)) {
  ptm <- proc.time()
  df_tmp <- df %>% filter(abs(x) < max_width)
  bw_lin <-
    rdbwselect(df_tmp$anyDelin, df_tmp$x, bwselect = "IK", p = 1, q = 2)$bws[, c("h", "b")] %>%
    as.numeric() %>%
    round(3)
  bw_quad <-
    rdbwselect(df_tmp$anyDelin, df_tmp$x, bwselect = "IK", p = 2, q = 3)$bws[, c("h", "b")] %>%
    as.numeric() %>%
    round(3)
  runtime <- (proc.time() - ptm) %>% round(3)
  optimal_bw[i, ] <- c("IK", max_width, bw_lin, bw_quad, runtime["elapsed"])
  i <- i + 1
}

# recompute for values residual on FICO
df <- df %>% filter(!is.na(anyDelin))
lm <- lm(anyDelin ~ brwr_curr_crdt_scr_val, data = df)
df <- df %>% mutate(delin_resid_fico = residuals(lm))
lm <- lm(x ~ brwr_curr_crdt_scr_val, data = df)
df <- df %>% mutate(x_resid_fico = residuals(lm))

for (max_width in c(1, 2, 4)) {
  ptm <- proc.time()
  df_tmp <- df %>% filter(abs(x) < max_width)
  bw_lin <-
    rdbwselect(df_tmp$delin_resid_fico, df_tmp$x_resid_fico, bwselect = "IK", p = 1, q = 2)$bws[, c("h", "b")] %>%
    as.numeric() %>%
    round(3)
  bw_quad <-
    rdbwselect(df_tmp$delin_resid_fico, df_tmp$x_resid_fico, bwselect = "IK", p = 2, q = 3)$bws[, c("h", "b")] %>%
    as.numeric() %>%
    round(3)
  runtime <- (proc.time() - ptm) %>% round(3)
  optimal_bw[i, ] <- c("IK FICO Resid", max_width, bw_lin, bw_quad, runtime["elapsed"])
  i <- i + 1
}

for (max_width in c(1, 2, 4)) {
  ptm <- proc.time()
  df_tmp <- df %>% filter(abs(x) < max_width)
  bw_lin <-
    rdbwselect(df_tmp$anyDelin, df_tmp$x, p = 1, q = 2)$bws[, c("h", "b")] %>%
    as.numeric() %>%
    round(3)
  bw_quad <-
    rdbwselect(df_tmp$anyDelin, df_tmp$x, p = 2, q = 3)$bws[, c("h", "b")] %>%
    as.numeric() %>%
    round(3)
  runtime <- (proc.time() - ptm) %>% round(3)
  optimal_bw[i, ] <- c("CCT_2016", max_width, bw_lin, bw_quad, runtime["elapsed"])
  print(runtime)
  i <- i + 1
}

bw_preferred <-
  optimal_bw %>%
  filter(method == "IK" & max_width == 2) %>%
  select(bw_lin_h) %>%
  as.numeric()

if (matched == "Yes") {
  test_that("Bandwith is", {
    expect_equal(bw_preferred, 0.601)
  })
} else if (matched == "No") {
  test_that("Bandwith is", {
    expect_equal(bw_preferred, 0.525)
  })
}

rm(optimal_bw)
