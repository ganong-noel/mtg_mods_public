## -------------#
## Load File ###
## --------------#
df <- readRDS(paste0(working_path, "df_analysis_event_study_with_auto.rds"))

## --------------------------------#
## Create consumption variables ####
## --------------------------------#

# Start with six months before and six months after mod
df_pre_6m <-
  df %>%
  filter(mos_since_mod >= (-6) & mos_since_mod < 0) %>%
  group_by(fncl_ast_id) %>%
  summarise(
    mean_cc_exp_pre_6m = mean(tot_cc_exp),
    mean_auto_pur_am_pre_6m = mean(auto_pur_am)
  )

df_post_6m <-
  df %>%
  filter(mos_since_mod >= 0 & mos_since_mod < 6) %>%
  group_by(fncl_ast_id) %>%
  summarise(
    mean_cc_exp_post_6m = mean(tot_cc_exp),
    mean_auto_pur_am_post_6m = mean(auto_pur_am)
  )

# One year before and after
df_pre_12m <-
  df %>%
  filter(mos_since_mod >= (-12) & mos_since_mod < 0) %>%
  group_by(fncl_ast_id) %>%
  summarise(
    mean_cc_exp_pre_12m = mean(tot_cc_exp),
    mean_auto_pur_am_pre_12m = mean(auto_pur_am)
  )

df_post_12m <-
  df %>%
  filter(mos_since_mod >= 0 & mos_since_mod < 12) %>%
  group_by(fncl_ast_id) %>%
  summarise(
    mean_cc_exp_post_12m = mean(tot_cc_exp),
    mean_auto_pur_am_post_12m = mean(auto_pur_am)
  )

# Group into one data set
df %>%
  distinct(fncl_ast_id) %>%
  summarise(n())
df_cons_rd <-
  df %>%
  select(fncl_ast_id, fracForgivePos, x, pos, xpos, x2, xpos2, dum) %>%
  distinct()
df_cons_rd <-
  df_cons_rd %>%
  left_join(df_pre_6m, by = "fncl_ast_id") %>%
  left_join(df_post_6m, by = "fncl_ast_id") %>%
  left_join(df_pre_12m, by = "fncl_ast_id") %>%
  left_join(df_post_12m, by = "fncl_ast_id")

# Make changes
df_cons_rd <-
  df_cons_rd %>%
  mutate(
    dol_chg_mean_cc_6m = mean_cc_exp_post_6m - mean_cc_exp_pre_6m,
    dol_chg_mean_cc_12m = mean_cc_exp_post_12m - mean_cc_exp_pre_12m,
    dol_chg_mean_auto_6m = mean_auto_pur_am_post_6m - mean_auto_pur_am_pre_6m,
    dol_chg_mean_auto_12m = mean_auto_pur_am_post_12m - mean_auto_pur_am_pre_12m
  )

# Take out zero dNPV group
df_cons_rd %>%
  filter(abs(x) > 0.005, abs(x) < bw_window) %>%
  saveRDS(file = paste0(working_path, "df_cons_rd.rds"))

rm(list = ls(pattern = "^df"))
