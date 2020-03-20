hamp_legend <- "Payment Reduction Only"
pra_legend <- "Payment & Principal Reduction"

## -----------------------------------------#
## Load Files and create analysis sample ####
## -----------------------------------------#

df <- inner_join(readRDS(paste0(working_path, "df_cc_event_base.rds")),
                 readRDS(paste0(working_path, "dNPV_out_es_base.rds")),
                 by = "fncl_ast_id")

# Add post-mod LTV and recode pre-mod LTV
mrgdHamp <-
  readRDS(paste0(working_path, "mrgdHamp_R.rds")) %>%
  mutate(
    ln_post_mod_ltv_pct = ifelse(prop_valu_as_is_val_amt > 0,
      100 * (ln_aft_mdfc_upb_amt + ln_prin_frbrn_amt) / prop_valu_as_is_val_amt, NA
    ),
    ln_post_mod_ltv_pct = ifelse(ln_post_mod_ltv_pct > 200, 200, ln_post_mod_ltv_pct)
  )
mrgdHamp_toadd <- mrgdHamp %>% select(fncl_ast_id, ln_post_mod_ltv_pct)
system.time(df <- left_join(df, mrgdHamp_toadd, by = "fncl_ast_id"))
# Recode pre-mod LTV
df <-
  as.data.frame(df) %>%
  mutate(ln_mtm_ltv_pct = ifelse(ln_mtm_ltv_pct < 2, ln_mtm_ltv_pct * 100, ln_mtm_ltv_pct))
df <- df %>% mutate(dNPV_cashflow_pos_val = dNPV_cashflow * -1)


## -------------------------------------------------#
## Create DID vars and factors for fixed effects ####
## -------------------------------------------------#
df <-
  df %>%
  ungroup() %>%
  mutate(post = mos_since_mod >= (0), treat = pra * post)

df <-
  df %>%
  mutate(
    mos_since_mod_fact = as.factor(mos_since_mod),
    fncl_ast_id_fact = as.factor(fncl_ast_id),
    ver_dt_monnb_fact = as.factor(ver_dt_monnb),
    msa_state_fact = as.factor(msa_state),
    mod_mnth_hamp_fact = as.factor(mod_mnth_hamp),
    pi_pay_red_decile_fact = as.factor(cut_number(pi_pay_red_dol, 10)),
    pi_pay_red_centile_fact = as.factor(cut_number(pi_pay_red_dol, 100)),
    ln_orig_note_dt_fact = as.factor(ln_orig_note_dt),
    ln_bef_mdfc_prdc_lbl_typ_cd_fact = as.factor(ln_bef_mdfc_prdc_lbl_typ_cd),
    bir_decade_tu_fact = as.factor(bir_decade_tu),
    prop_typ_cd_fact = as.factor(prop_typ_cd)
  )

## -----------------------#
## Baseline Regression ####
## -----------------------#

df_all <- df %>% filter(abs(mos_since_mod) <= 12)

##
# Baseline: Months_since_Mod fixed effects
##
felm_mossincemod_all <-
  felm(tot_cc_exp ~ pra + treat | mos_since_mod_fact | 0 | fncl_ast_id,
    data = df_all
  )


## ---------------------------------------------#
## Additional Group-Level Fixed Effects, ALL ####
## ---------------------------------------------#

# (b)mos_since_mod and MSA fixed effects
felm_mossincemod_msa_all <-
  felm(tot_cc_exp ~ pra + treat | mos_since_mod_fact + msa_state_fact | 0 | fncl_ast_id,
    data = df_all
  )
# (c)mos_since_mod and calendar month fixed effects
felm_mossincemod_date_all <-
  felm(tot_cc_exp ~ pra + treat | mos_since_mod_fact + ver_dt_monnb_fact | 0 | fncl_ast_id,
    data = df_all
  )

# (d) mos_since_mod, msa X calendar_month fixed effects
felm_mossincemod_msaXdate_all <-
  felm(tot_cc_exp ~ pra + treat | mos_since_mod_fact + ver_dt_monnb_fact:msa_state_fact | 0 | fncl_ast_id,
    data = df_all
  )

## ------------#
## Controls ####
## ------------#

# Make a sample for regressions with controls that gets rid of outliers
df_cont <-
  df %>%
  filter(
    !is.na(brwr_curr_crdt_scr_val),
    !is.na(ln_mtm_ltv_pct),
    ln_mtm_ltv_pct > 0,
    pi_pay_red_pct < .95,
    !is.na(ln_orgnn_ltv_pct),
    !is.na(brwr_mthy_dpmt_xcldg_piti_amt),
    prop_valu_as_is_val_amt > 10000,
    pre_mod_dti_full > .15,
    post_mod_dti_full < .60,
    brwr_mthy_dpmt_xcldg_piti_amt < 50000
  )

# Make new indicator variable for post-mod LTV below 100
df_cont <- df_cont %>% mutate(ln_post_mod_ltv_pct_less100 = ifelse(is.na(ln_post_mod_ltv_pct), FALSE, ln_post_mod_ltv_pct <= 100))

# Make subsets for analysis
df_all_cont <- df_cont %>% filter(abs(mos_since_mod) <= 12)

# Make control_pre and control_post variables
df_all_cont <-
  df_all_cont %>%
  ungroup() %>%
  mutate_each(
    funs(post = . * post, pra = . * pra, treat = . * treat),
    brwr_curr_crdt_scr_val, brwr_mthy_grs_incm_amt.x,
    ln_mtm_ltv_pct, pi_pay_red_dol, pi_pay_red_pct,
    ln_bef_mdfc_upb_amt.x, prop_valu_as_is_val_amt,
    pre_mod_dti_full, post_mod_dti_full, ln_bef_mdfc_pi_pmt_amt.x,
    ln_orgnn_ltv_pct, brwr_mthy_dpmt_xcldg_piti_amt,
    ln_post_mod_ltv_pct, ln_post_mod_ltv_pct_less100,
    x, xpos
  )


# (b.5) Baseline regression, with base continuous controls and payment reduction and dNPV
base_controls <- c(
  "brwr_curr_crdt_scr_val", "brwr_mthy_grs_incm_amt.x", "ln_mtm_ltv_pct",
  "prop_valu_as_is_val_amt", "ln_bef_mdfc_upb_amt.x", "pre_mod_dti_full",
  "ln_bef_mdfc_pi_pmt_amt.x", "ln_orgnn_ltv_pct", "brwr_mthy_dpmt_xcldg_piti_amt",
  "pi_pay_red_dol", "x", "xpos"
)
formula <- formula(str_c(
  "tot_cc_exp ~ pra + treat +", paste(base_controls, collapse = " +"),
  "|mos_since_mod_fact+ver_dt_monnb_fact:msa_state_fact|0| fncl_ast_id"
))
felm_mossincemod_msaXdate_cont_dol_dnpv_all <- felm(formula, data = df_all_cont)


# (b.6) Baseline regression, With Base Continuous controls and payment reduction and dnpv interacted with POST
base_controls_post <- c(
  "brwr_curr_crdt_scr_val_post", "brwr_mthy_grs_incm_amt.x_post", "ln_mtm_ltv_pct_post",
  "prop_valu_as_is_val_amt_post", "ln_bef_mdfc_upb_amt.x_post", "pre_mod_dti_full_post",
  "ln_bef_mdfc_pi_pmt_amt.x_post", "ln_orgnn_ltv_pct_post", "brwr_mthy_dpmt_xcldg_piti_amt_post",
  "pi_pay_red_dol_post", "x_post", "xpos_post"
)
formula <- formula(str_c(
  "tot_cc_exp ~ pra + treat +", paste(base_controls, collapse = " +"), "+",
  paste(base_controls_post, collapse = " +"),
  "|mos_since_mod_fact+ver_dt_monnb_fact:msa_state_fact|0| fncl_ast_id"
))
felm_mossincemod_msaXdate_cont_post_dol_dnpv_all <- felm(formula, data = df_all_cont)

# calculate dependent variable mean
mean_cc_exp_all <-
  df_all %>%
  filter(pra == TRUE & mos_since_mod < 0) %>%
  select(tot_cc_exp) %>%
  summarise(mean = round(mean(tot_cc_exp), 2))
mean_cc_exp_all_cont <-
  df_all_cont %>%
  filter(pra == TRUE & mos_since_mod < 0) %>%
  select(tot_cc_exp) %>%
  summarise(mean = round(mean(tot_cc_exp), 2))

# Make into value
mean_cc_exp_all_value <- mean_cc_exp_all$mean
mean_cc_exp_all_cont_value <- mean_cc_exp_all_cont$mean

# Table to .tex
stargazer(
  felm_mossincemod_all, felm_mossincemod_msa_all, felm_mossincemod_date_all,
  felm_mossincemod_msaXdate_all, felm_mossincemod_msaXdate_cont_dol_dnpv_all, felm_mossincemod_msaXdate_cont_post_dol_dnpv_all,
  out = paste0(out_path, "cons_table_paper_base_v2.tex"),
  no.space = TRUE, multicolumn = FALSE, float = FALSE, intercept.bottom = FALSE,
  keep = c("\\btreat"),
  covariate.labels = c("Treatment (Principal Reduction x Post)"),
  title = "Table for Paper with dNPV",
  dep.var.labels.include = FALSE,
  dep.var.caption = "",
  add.lines = list(
    c("MSA Fixed Effects", "", "Yes", "", "", "", ""),
    c("Calendar Month Fixed Effects", "", "", "Yes", "", "", ""),
    c("MSA by Calendar Month Fixed Effects", "", "", "", "Yes", "Yes", "Yes"),
    c("Controls", "", "", "", "", "Yes", "Yes"),
    c("Controls x Post Interactions", "", "", "", "", "", "Yes"),
    c(
      "Dependent Variable Mean", mean_cc_exp_all_value, mean_cc_exp_all_value, mean_cc_exp_all_value,
      mean_cc_exp_all_value, mean_cc_exp_all_cont_value, mean_cc_exp_all_cont_value
    )
  ),
  star.cutoffs = NA,
  omit.table.layout = "n",
  omit.stat = c("f", "ser", "rsq")
)

## ------------------------#
## Graphs (Full Sample) ####
## ------------------------#

df_all_grouped <-
  df_all %>%
  ungroup() %>%
  group_by(mos_since_mod, pra) %>%
  summarise(
    mean_cc_exp = mean(tot_cc_exp),
    mean_cc_bal = mean(tot_bal, na.rm = TRUE)
  )

# Change var name
df_all_grouped <-
  df_all_grouped %>%
  mutate(
    "Payment & Principal Reduction" = ifelse(pra == FALSE, "NO", "YES"),
    mod = ifelse(pra == FALSE, "Control: Payment Reduction Only", "Treatment: Payment & Principal Reduction"),
    "Modification Type" = factor(mod,
      levels = c(
        "Treatment: Payment & Principal Reduction",
        "Control: Payment Reduction Only"
      )
    )
  )

# Balanced Plot zoomed out, only pre-mod info
gg <- ggplot(
  df_all_grouped %>% filter(mos_since_mod < 0),
  aes(
    x = mos_since_mod, y = mean_cc_exp,
    group = `Modification Type`,
    colour = `Modification Type`,
    shape = `Modification Type`
  )
) +
  geom_point() + geom_line() +
  fte_theme() +
  xlab("Months Since Modification") + ylab("Mean Credit Card Spend") +
  theme(legend.justification = c(0, 0), legend.position = c(0, 0)) +
  scale_colour_manual(values = cbPalette_blue) +
  coord_cartesian(ylim = c(0, 750), xlim = c(-13, 13)) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(breaks = seq(-12, 12, 6)) +
  theme(legend.key = element_rect(colour = "white", linetype = "solid"))
ggsave(str_c(out_path, "slide/tu_cc_spend_all_wide2_pre.png"), gg, width = 6, height = 4)

# Balanced Plot zoomed out
gg <- ggplot(
  df_all_grouped %>% filter(abs(mos_since_mod) <= 12),
  aes(
    x = mos_since_mod, y = mean_cc_exp,
    group = `Modification Type`,
    colour = `Modification Type`,
    shape = `Modification Type`
  )
) +
  geom_point() + geom_line() +
  fte_theme() +
  xlab("Months Since Modification") + ylab("Mean Credit Card Spend") +
  theme(legend.justification = c(0, 0), legend.position = c(0, 0)) +
  scale_colour_manual(values = cbPalette_blue) +
  coord_cartesian(ylim = c(0, 750), xlim = c(-13, 13)) +
  geom_vline(xintercept = 0, linetype = 2) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(breaks = seq(-12, 12, 6)) +
  theme(legend.key = element_rect(colour = "white", linetype = "solid"))
ggsave(paste0(out_path, "tu_cc_spend_all_wide2.png"), gg, width = 6, height = 4)

##
# Look at differences between the two groups over time
##

# Create a series with the diference between the two groups, by month
pra_true <-
  df_all_grouped %>%
  filter(pra == "TRUE") %>%
  mutate(
    mean_cc_exp_pra_true = mean_cc_exp,
    mean_cc_bal_pra_true = mean_cc_bal
  ) %>%
  select(mos_since_mod, mean_cc_exp_pra_true, mean_cc_bal_pra_true)

pra_false <-
  df_all_grouped %>%
  filter(pra == "FALSE") %>%
  mutate(
    mean_cc_exp_pra_false = mean_cc_exp,
    mean_cc_bal_pra_false = mean_cc_bal
  ) %>%
  select(mos_since_mod, mean_cc_exp_pra_false, mean_cc_bal_pra_false)

df_all_grouped_diff <-
  left_join(pra_true, pra_false, by = "mos_since_mod") %>%
  mutate(
    cc_exp_diff = mean_cc_exp_pra_true - mean_cc_exp_pra_false,
    cc_bal_diff = mean_cc_bal_pra_true - mean_cc_bal_pra_false
  )
rm(pra_true, pra_false)


##
# Plot normalized series to see drop
##
df_all_grouped_diff <-
  df_all_grouped_diff %>%
  mutate(
    mean_cc_exp_pra_true_norm = mean_cc_exp_pra_true /
      df_all_grouped_diff$mean_cc_exp_pra_true[df_all_grouped_diff$mos_since_mod == -12],
    mean_cc_exp_pra_false_norm = mean_cc_exp_pra_false /
      df_all_grouped_diff$mean_cc_exp_pra_false[df_all_grouped_diff$mos_since_mod == -12]
  )

# Normalized spending to zero at modification date, include estimate on graph
df_all_grouped_diff <-
  df_all_grouped_diff %>%
  mutate(
    mean_cc_exp_pra_true_norm_dol = mean_cc_exp_pra_true -
      df_all_grouped_diff$mean_cc_exp_pra_true[df_all_grouped_diff$mos_since_mod == 0],
    mean_cc_exp_pra_false_norm_dol = mean_cc_exp_pra_false -
      df_all_grouped_diff$mean_cc_exp_pra_false[df_all_grouped_diff$mos_since_mod == 0]
  )

df_long <-
  df_all_grouped_diff %>%
  select(mos_since_mod, mean_cc_exp_pra_true_norm_dol, mean_cc_exp_pra_false_norm_dol) %>%
  gather(source, value, -mos_since_mod)
df_long <-
  df_long %>%
  mutate(
    mod = ifelse(source == "mean_cc_exp_pra_false_norm_dol", "Control: Payment Reduction Only", "Treatment: Payment & Principal Reduction"),
    "Modification Type" = factor(mod,
      levels = c(
        "Treatment: Payment & Principal Reduction",
        "Control: Payment Reduction Only"
      )
    )
  )

# Version without estimate on graph
gg <- ggplot(
  df_long %>% filter(abs(mos_since_mod) <= 12),
  aes(
    x = mos_since_mod, y = value,
    group = `Modification Type`,
    colour = `Modification Type`,
    shape = `Modification Type`
  )
) +
  geom_point() + geom_line() +
  fte_theme() +
  xlab("Months Since Modification") + ylab("Mean Credit Card Spend Relative to t=0 ($)") +
  theme(legend.justification = c(1, 1), legend.position = c(1, 1)) +
  scale_colour_manual(values = cbPalette_blue) +
  geom_vline(xintercept = 0, linetype = 2) +
  theme(legend.key = element_rect(colour = "white", linetype = "solid"))
ggsave(paste0(out_path, "tu_cc_spend_norm_dol_all_noest.png"), gg, width = 6, height = 4)

## -------------------#
## Clean Workspace ####
## -------------------#
rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^felm"))
rm(list = ls(pattern = "^lm"))
rm(list = ls(pattern = "^base_"))
rm(list = ls(pattern = "^tmp"))
rm(mean_cc_exp_all)
rm(mean_cc_exp_all_cont)
rm(mrgdHamp)
rm(mrgdHamp_toadd)
