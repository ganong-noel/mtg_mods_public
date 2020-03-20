logit_f <- function(num) {
  return(1 / (1 + exp(-num)))
}

# NPV model 4.0 for owner-occupied properties ####
m_v4_c <- list(
  intrcpt = -2.95,
  mtmltv = c(0.0494, 0, 0, 0, 0),
  fico = -0.00568,
  dti_start = 0.010,
  d_dti = c(-0.275, 0, 0, 0, 0)
)
m_v4_d30 <- list(
  intrcpt = -2.25,
  mtmltv = c(0.0425, 0, 0, 0, 0),
  fico = -0.00495,
  dti_start = 0.015,
  d_dti = c(-0.365, 0, 0, 0, 0)
)
m_v4_d60 <- list(
  intrcpt = -1.98,
  mtmltv = c(0.0375, 0, -0.01084, -0.01448, 0),
  fico = -0.00332,
  dti_start = 0.025,
  d_dti = c(-0.555, 0, 0, 0, 0)
)
m_v4_d90 <- list(
  intrcpt = -1.15,
  mtmltv = c(0.0255, 0, 0, -0.01309, 0),
  fico = -0.00195,
  dti_start = 0.045,
  d_dti = c(-0.745, 0, 0, 0, 0)
)

# NPV model 5.0 for owner-occupied properties ####
m_v5_c_d30_d60 <- m_v4_d60
m_v5_c_d30_d60$d_dti <- c(0, -0.2178, 0.1712, 0.0217, 0)
m_v5_d90 <- m_v4_d90
m_v5_d90$d_dti <- c(0, -0.2927, 0.2303, 0.0174, 0)


# build right hand side variables ####
df <-
  df %>%
  mutate(
    intercept = 1,
    ltv_80 = ifelse(mtm_ltv_post - 80 < 0, 0, mtm_ltv_post - 80),
    ltv_100 = ifelse(mtm_ltv_post - 100 < 0, 0, mtm_ltv_post - 100),
    ltv_120 = ifelse(mtm_ltv_post - 120 < 0, 0, mtm_ltv_post - 120),
    ltv_150 = ifelse(mtm_ltv_post - 150 < 0, 0, mtm_ltv_post - 150),
    ln_bef_mdfc_frnt_rto_pct = bottom_code(top_code(ln_bef_mdfc_frnt_rto_pct, 100), 0),
    post_mod_dti_full = top_code(post_mod_dti_full, 31),
    d_dti = bottom_code(ln_bef_mdfc_frnt_rto_pct - post_mod_dti_full, 0),
    log_d_dti = log(1 + d_dti),
    d_dti5 = log(1 + ifelse(d_dti <= 5, 0, d_dti - 5)),
    d_dti15 = log(1 + ifelse(d_dti <= 15, 0, d_dti - 15)),
    d_dti30 = log(1 + ifelse(d_dti <= 30, 0, d_dti - 30))
  )

x_vars <- c(
  "intercept", "mtm_ltv_post", "ltv_80", "ltv_100", "ltv_120", "ltv_150",
  "brwr_curr_crdt_scr_val", "ln_bef_mdfc_frnt_rto_pct",
  "log_d_dti", "d_dti", "d_dti5", "d_dti15", "d_dti30"
)
x_mat <- as.matrix(df[, x_vars])
df <- cbind(
  df,
  data.frame(
    delin_v4_c = logit_f(x_mat %*% unlist(m_v4_c)),
    delin_v4_d30 = logit_f(x_mat %*% unlist(m_v4_d30)),
    delin_v4_d60 = logit_f(x_mat %*% unlist(m_v4_d60)),
    delin_v4_d90 = logit_f(x_mat %*% unlist(m_v4_d90)),
    delin_v5_c_d30_d60 = logit_f(x_mat %*% unlist(m_v5_c_d30_d60)),
    delin_v5_d90 = logit_f(x_mat %*% unlist(m_v5_d90))
  )
)


df <-
  df %>%
  mutate(
    delin_v4 = ifelse(ln_pst_due_prd_cnt == 0, delin_v4_c,
      ifelse(ln_pst_due_prd_cnt == 1, delin_v4_d30,
        ifelse(ln_pst_due_prd_cnt == 2, delin_v4_d60,
          ifelse(ln_pst_due_prd_cnt >= 3, delin_v4_d90, NA)
        )
      )
    ),
    delin_v5 = ifelse(ln_pst_due_prd_cnt <= 2, delin_v5_c_d30_d60,
      ifelse(ln_pst_due_prd_cnt >= 3, delin_v5_d90, NA)
    )
  )

x_mat_ltv <- as.matrix(cbind(
  df[, x_vars[1:6]],
  df[, x_vars[7:13]] %>% mutate_each(funs(median))
))
df <- cbind(
  df,
  data.frame(
    delin_v4_ltv_c = logit_f(x_mat_ltv %*% unlist(m_v4_c)),
    delin_v4_ltv_d30 = logit_f(x_mat_ltv %*% unlist(m_v4_d30)),
    delin_v4_ltv_d60 = logit_f(x_mat_ltv %*% unlist(m_v4_d60)),
    delin_v4_ltv_d90 = logit_f(x_mat_ltv %*% unlist(m_v4_d90)),
    delin_v5_ltv_c_d30_d60 = logit_f(x_mat_ltv %*% unlist(m_v5_c_d30_d60)),
    delin_v5_ltv_d90 = logit_f(x_mat_ltv %*% unlist(m_v5_d90))
  )
)
df <-
  df %>%
  mutate(
    delin_v4_ltv = ifelse(ln_pst_due_prd_cnt == 0, delin_v4_ltv_c,
      ifelse(ln_pst_due_prd_cnt == 1, delin_v4_ltv_d30,
        ifelse(ln_pst_due_prd_cnt == 2, delin_v4_ltv_d60,
          ifelse(ln_pst_due_prd_cnt >= 3, delin_v4_ltv_d90, NA)
        )
      )
    ),
    delin_v5_ltv = ifelse(ln_pst_due_prd_cnt <= 2, delin_v5_ltv_c_d30_d60,
      ifelse(ln_pst_due_prd_cnt >= 3, delin_v5_ltv_d90, NA)
    )
  )

rm(x_mat, x_mat_ltv)
