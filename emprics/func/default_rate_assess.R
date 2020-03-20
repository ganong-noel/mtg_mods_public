# Assess impact of functional form on default rate projections

# Load relevant data ####
# https://www.treasury.gov/initiatives/financial-stability/reports/Documents/1Q17%20MHA%20Report%20Final.pdf
make_plots <- TRUE
# Default rates of 2010 HAMP Tier 1 Modifications (Page 8): See Months 24 and 60
dflt_2year <- 0.281
dflt_5year <- 0.456
dflt_factor_5year <- dflt_5year / dflt_2year

# Source: "hampra/notes/Prob foreclosure after default_HAMP.xlsx"
# No-Mod Default Rate (No-Mod Liquidation Rate / Share of Defaults Liquidated)
no_mod_jpmc <- 0.73

# Borrower Data (under various assumptions)
bdata <- read_xls("./data/mtg_rd_ac2019-03-22.xls", sheet = "tbl_local_linear")
bdata <-
  bdata %>%
  select(
    "Type" = pos, "inc" = cplt_loan_brw_incm_am,
    "mtmval" = curr_prop_aprs_val_am, "pay_mo_pre" = pri_loan_prin_int_am,
    "pay_reduc" = pi_due_chg_ratio, "dflt_post" = delin_ever90,
    "pay_pre" = pri_loan_prin_int_am
  )

bdata <-
  mutate(bdata, "Type" = bdata$Type %>%
    lapply((function(pos) {
      if (pos == "FALSE") {
        return("lhs")
      }
      if (pos == "TRUE") {
        return("rhs")
      }
    }))
    %>%
    unlist()) %>%
  data.frame()
bdata <- mutate(bdata, pay_post = pay_pre * (1 + pay_reduc))
bdata <- mutate(bdata,
  dti_pre = 100 * pay_pre / inc,
  dti_post = 100 * pay_post / inc
)

# Build data for main plot ####
dflt_jpmc <- data.frame(pay_red = 0, dflt_rate = no_mod_jpmc, type = "Chase No-Mod")
dflt_jpmc <- add_row(dflt_jpmc,
  pay_red = (-1) * bdata[, "pay_reduc"],
  dflt_rate = bdata[, "dflt_post"] * dflt_factor_5year,
  type = "Data: 31% PTI Discontinuity"
)


# Main plot ####
# Note that this exercise is for 5-year default rates
# Add estimate for 50% payment reduction and 12% 2-year default rate
dflt_jpmc <- add_row(dflt_jpmc,
  pay_red = 0.5, dflt_rate = 0.12 * dflt_factor_5year,
  type = "Data: HAMP Recipients"
)

gg <- ggplot() +
  geom_point(
    data = dflt_jpmc %>% filter(!(type == "Chase No-Mod")),
    aes(x = pay_red, y = dflt_rate, colour = type, shape = type), size = 4.23
  ) +
  stat_smooth(
    data = dflt_jpmc %>% filter(type %in% c(
      "Data: 31% PTI Discontinuity",
      "Data: HAMP Recipients"
    )) %>%
      mutate(type = "Chase All"),
    aes(x = pay_red, y = dflt_rate), colour = cbPalette_set2[3],
    method = "glm", method.args = list(family = "binomial"),
    formula = y ~ x,
    size = 0.5, se = FALSE, fullrange = TRUE
  ) +
  fte_theme() +
  labs(
    x = "Reduction in Monthly Payment (Measured as % of Income at Mod Date)",
    y = "Estimated Five-Year Default Rate"
  ) +
  theme(
    legend.justification = c(1, 1), legend.position = c(1, 1),
    legend.text = element_text(size = 12),
    legend.box = "horizontal"
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(0, 0.70)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L), limits = c(0, 0.80)) +
  scale_colour_manual(values = c(cbPalette_set2[1], cbPalette_set2[2])) +
  scale_shape_manual(values = c(17, 19))
gg

if (make_plots == TRUE) {
  ggsave(gg, file = "./out/default_assess.png", width = wd, height = ht)
}

# Get a new estimate for the functional form
fit <- summary(glm(dflt_rate ~ pay_red,
  data = dflt_jpmc %>% filter(type %in% c(
    "Data: 31% PTI Discontinuity",
    "Data: HAMP Recipients"
  )) %>%
    mutate(pay_red = pay_red * 100),
  family = "binomial"
))

test_that("Default prediction function coefficients match", {
  expect_equal(fit$coefficients[1], 0.654357, tolerance = 1e-5)
  expect_equal(fit$coefficients[2], -0.0387106, tolerance = 1e-5)
})

calc_mod_dflt <- function(d_pay){
  return(1/(1+exp(-(fit$coefficients[1]+fit$coefficients[2]*d_pay))))
}

