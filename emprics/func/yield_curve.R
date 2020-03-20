make_plots <- TRUE 
data_path <- "./data/calc_npv_data/"


# Use FRED to project the yield on a 40-year mortgage ####
mortgage <- read.table(paste0(data_path, "mortgage.csv"), header = TRUE, sep = ",") %>%
  transmute(DATE, "15" = MORTGAGE15US, "30" = MORTGAGE30US)

mortgage <- 
  mortgage %>%
  gather(key = "year", value = "rate", -DATE) %>%
  group_by(year) %>%
  summarise(mean_rate = mean(rate) / 100) %>%
  mutate(
    year = as.numeric(year),
    type = "Freddie Mac Mortgage"
  )

lm_mortgage <- lm(mean_rate ~ log(year), data = mortgage)
predict_year30 <- predict(lm_mortgage, newdata = data.frame(year = 30)) %>% as.numeric()
predict_year30
predict_year40 <- predict(lm_mortgage, newdata = data.frame(year = 40)) %>% as.numeric()
predict_year40

spread <- data.frame(
  spread = predict_year40 - predict_year30,
  id = "implied_gse",
  type = "Implied\nFreddie Mac\nMortgage"
)

mortgage <- add_row(mortgage,
  year = 40,
  mean_rate = predict_year40,
  type = "Extrapolate Using Shorter-Term Mortgage Spreads"
)

rm(predict_year30, predict_year40)


# Use corporate bond spot rates to compute a yield curve equation ####
corporate_bond <- read.table(paste0(data_path, "corporate_bond.csv"), header = TRUE, sep = ",") %>%
  transmute(DATE, "30" = HQMCB30YR, "40" = HQMCB40YR)
corporate_bond_year5 <- read.table(paste0(data_path, "HQMCB5YR.csv"), header = TRUE, sep = ",") %>%
  transmute(DATE, "5" = HQMCB5YR)
corporate_bond_year10 <- read.table(paste0(data_path, "HQMCB10YR.csv"), header = TRUE, sep = ",") %>%
  transmute(DATE, "10" = HQMCB10YR)
corporate_bond_year20 <- read.table(paste0(data_path, "HQMCB20YR.csv"), header = TRUE, sep = ",") %>%
  transmute(DATE, "20" = HQMCB20YR)

corporate_bond <- 
  full_join(corporate_bond, corporate_bond_year5, by = "DATE") %>%
  full_join(., corporate_bond_year10, by = "DATE") %>%
  full_join(., corporate_bond_year20, by = "DATE")

corporate_bond <- 
  corporate_bond %>%
  gather(key = "year", value = "rate", -DATE) %>%
  group_by(year) %>%
  summarise(mean_rate = mean(rate) / 100) %>%
  mutate(
    year = as.numeric(year),
    type = "Corporate Bond"
  )

lm_corporate_bond <- lm(mean_rate ~ log(year), data = corporate_bond)
predict_year30 <- predict(lm_corporate_bond, newdata = data.frame(year = 30)) %>% as.numeric()
predict_year30
predict_year40 <- predict(lm_corporate_bond, newdata = data.frame(year = 40)) %>% as.numeric()
predict_year40

diff <- corporate_bond %>%
  filter(year == 40) %>%
  select(mean_rate) %>%
  as.numeric() -
  corporate_bond %>%
  filter(year == 30) %>%
  select(mean_rate) %>%
  as.numeric()
diff

spread <- add_row(spread,
  spread = diff,
  id = "actual_corp_bond",
  type = "Actual\nCorporate Bond"
) %>%
  add_row(.,
    spread = predict_year40 - predict_year30,
    id = "implied_corp_bond",
    type = "Implied\nCorporate Bond"
  )

rm(
  diff,
  predict_year30, predict_year40
)


# Use treasuries to compute a yield curve equation ####
treasury_year5 <- read.table(paste0(data_path, "DGS5.csv"), header = TRUE, sep = ",") %>%
  filter(DGS5 != ".") %>%
  mutate("5" = as.numeric(as.character(DGS5))) %>%
  select(-DGS5)
treasury_year10 <- read.table(paste0(data_path, "DGS10.csv"), header = TRUE, sep = ",") %>%
  filter(DGS10 != ".") %>%
  mutate("10" = as.numeric(as.character(DGS10))) %>%
  select(-DGS10)
treasury_year20 <- read.table(paste0(data_path, "DGS20.csv"), header = TRUE, sep = ",") %>%
  filter(DGS20 != ".") %>%
  mutate("20" = as.numeric(as.character(DGS20))) %>%
  select(-DGS20)
treasury_year30 <- read.table(paste0(data_path, "DGS30.csv"), header = TRUE, sep = ",") %>%
  filter(DGS30 != ".") %>%
  mutate("30" = as.numeric(as.character(DGS30))) %>%
  select(-DGS30)

treasury <- 
  full_join(treasury_year5, treasury_year10, by = "DATE") %>%
  full_join(., treasury_year20, by = "DATE") %>%
  full_join(., treasury_year30, by = "DATE")

treasury <- 
  treasury %>%
  gather(key = "year", value = "rate", -DATE) %>%
  group_by(year) %>%
  summarise(mean_rate = mean(rate) / 100) %>%
  mutate(
    year = as.numeric(year),
    type = "Treasury"
  )

lm_treasury <- lm(mean_rate ~ log(year), data = treasury)
predict_year30 <- predict(lm_treasury, newdata = data.frame(year = 30)) %>% as.numeric()
predict_year30
predict_year40 <- predict(lm_treasury, newdata = data.frame(year = 40)) %>% as.numeric()
predict_year40

spread <- add_row(spread,
  spread = predict_year40 - predict_year30,
  id = "implied_treasury",
  type = "Implied\nTreasury"
)

treasury <- add_row(treasury,
  year = 40,
  mean_rate = predict_year40,
  type = "Extrapolate Using Shorter-Term Treasury Spreads"
)

rm(
  lm_treasury,
  treasury_year5, treasury_year10, treasury_year20, treasury_year30,
  predict_year30, predict_year40
)

# Estimates from fixed-for-floating swaps
library(readxl)
swap_data <- read_excel(paste0(data_path, "swap_rates.xlsx")) %>%
  as.data.frame() %>%
  filter(Date == "Average") %>%
  select(-Date)

swaps <- tibble(
  "year" = c(5, 10, 20, 30, 40),
  "mean_rate" = unlist(swap_data[1, ] / 100),
  "type" = "Swaps"
)

lm_swaps <- lm(mean_rate ~ log(year), data = swaps)
predict_year30 <- predict(lm_swaps, newdata = data.frame(year = 30)) %>% as.numeric()
predict_year30
predict_year40 <- predict(lm_swaps, newdata = data.frame(year = 40)) %>% as.numeric()
predict_year40

spread <- add_row(spread,
  spread = (swap_data[, "USSW40"] - swap_data[, "USSW30"]) / 100,
  id = "actual_swaps",
  type = "Actual\nSwaps"
) %>%
  add_row(.,
    spread = predict_year40 - predict_year30,
    id = "implied_swaps",
    type = "Implied\nSwaps"
  )
rm(predict_year30, predict_year40)



# Baseline 30-year rates
rate_30_base <- mortgage %>%
  filter(year == 30) %>%
  select(mean_rate) %>%
  pull()
rate_30_T <- treasury %>%
  filter(year == 30) %>%
  select(mean_rate) %>%
  pull()
rate_30_swaps <- swaps %>%
  filter(year == 30) %>%
  select(mean_rate) %>%
  pull()

# Save output for use in other files
spread <- mutate(spread,
  rate_30 = ifelse(grepl("Treasury", type),
    rate_30_T,
    ifelse(grepl("Swaps", type),
      rate_30_swaps,
      rate_30_base
    )
  )
)
spread <- mutate(spread, rate_40 = rate_30 + spread)
write.csv(spread, "./data/calc_npv_data/yield_curves.csv", row.names = FALSE)

# Testing
test_that("Yield curve spreads match", {
  expect_equal(spread %>% filter(id == "implied_gse") %>% select(spread) %>% pull(), 0.0032734753)
  expect_equal(spread %>% filter(id == "implied_treasury") %>% select(spread) %>% pull(), 0.0035265455)
  expect_equal(spread %>% filter(id == "actual_swaps") %>% select(spread) %>% pull(), 0.0001985000)
})



# Figure plotting all the estimates
df_combined <- rbind(mortgage, corporate_bond, treasury, swaps) %>%
  mutate(type = factor(type, levels = c(
    "Freddie Mac Mortgage",
    "Extrapolate Using Shorter-Term Mortgage Spreads",
    "Corporate Bond",
    "Treasury",
    "Extrapolate Using Shorter-Term Treasury Spreads",
    "Swaps"
  )))

yield_curve_combined <- ggplot(
  df_combined,
  aes(year, mean_rate, colour = type, shape = type)
) +
  geom_point(size = 5) +
  fte_theme() +
  stat_smooth(aes(year, mean_rate),
    data = mortgage,
    method = "lm", formula = y ~ log(x), se = FALSE,
    colour = cbPalette_set2[1], size = 0.5, fullrange = TRUE
  ) +
  stat_smooth(aes(year, mean_rate),
    data = corporate_bond,
    method = "lm", formula = y ~ log(x), se = FALSE,
    colour = cbPalette_set2[2], size = 0.5, fullrange = TRUE
  ) +
  stat_smooth(aes(year, mean_rate),
    data = treasury,
    method = "lm", formula = y ~ log(x), se = FALSE,
    colour = cbPalette_set2[3], size = 0.5, fullrange = TRUE
  ) +
  stat_smooth(aes(year, mean_rate),
    data = swaps,
    method = "lm", formula = y ~ log(x), se = FALSE,
    colour = cbPalette_set2[4], size = 0.5, fullrange = TRUE
  ) +
  theme(
    legend.title = element_blank(),
    legend.justification = c(0, 1),
    legend.position = c(0, 1)
  ) +
  scale_shape_manual(
    breaks = c(
      "Freddie Mac Mortgage",
      "Corporate Bond",
      "Treasury",
      "Swaps"
    ),
    values = c(19, 1, 15, 17, 2, 18)
  ) +
  scale_colour_manual(
    breaks = c(
      "Freddie Mac Mortgage",
      "Corporate Bond",
      "Treasury",
      "Swaps"
    ),
    values = c(
      rep(cbPalette_set2[1], 2),
      cbPalette_set2[2],
      rep(cbPalette_set2[3], 2),
      cbPalette_set2[4]
    )
  ) +
  coord_cartesian(xlim = c(0, 45), ylim = c(0, 0.07)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1L)) +
  labs(x = "Loan Term in Years", y = "Average Interest Rate")
# yield_curve_combined
if (make_plots == TRUE) {
  ggsave(file = "./out/yield_curve_combined_rates.png", yield_curve_combined, width = wd, height = ht)
}

spread <- as_data_frame(spread)
spread$type <- factor(spread$type, levels = spread$type[order(-spread$spread)])


spread_bar <- ggplot(
  data = spread,
  aes(x = type, y = spread, fill = type, colour = type)
) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  xlab(" ") +
  ylab(" ") +
  fte_theme() +
  scale_fill_manual(values = c(
    rep("white", 4),
    cbPalette_set2[2],
    cbPalette_set2[4]
  )) +

  scale_colour_manual(values = c(
    cbPalette_set2[2],
    cbPalette_set2[3],
    cbPalette_set2[1],
    cbPalette_set2[4],
    cbPalette_set2[2],
    cbPalette_set2[4]
  )) +
  coord_flip() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1))
# spread_bar
if (make_plots == TRUE) {
  ggsave(file = "./out/yield_curve_spread_bar.png", spread_bar, width = wd, height = ht)
}
