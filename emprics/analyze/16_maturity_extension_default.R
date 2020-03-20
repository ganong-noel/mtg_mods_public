source('./func/prelim.R')

library('readxl', lib.loc = lib.loc)
library('tibble', lib.loc = lib.loc)
library('magrittr', lib.loc = lib.loc)
library('testthat', lib.loc = lib.loc)
library('purrr', lib.loc = lib.loc)
library('lubridate')
library('purrr')

source('./func/npv_calc_funcs.R')

### Setup control/representative mortgage ###

#Assumptions about property value growth
periods_for_growth <- tibble(
  month = c(1, 1,
            12, 12,
            1, 1),
  year = c(1991, 2011, 2011, 2016, 2006, 2010),
  type = c("pre-period", "pre-period",
           "coincident", "coincident",
           "measure_worst", "measure_worst")
)


CS_20_city <- map(c("data/first_12_CS_price.csv",
                    "data/next_8_CS_price.csv"),
                  ~ read_csv(.x, na = ".")) %>%
  bind_cols() %>%
  select(-DATE1,
         date = DATE) %>%
  mutate(date = ymd(date),
         year = year(date),
         month = month(date)) %>%
  inner_join(periods_for_growth) %>%
  filter(type != "pre-period") %>%
  select(-date) %>%
  gather(city, HPI, -year, -month, -type)


all_HPI <- read_csv("data/FHFA_PO_monthly.csv", skip = 3) %>%
  select(month = Month,
         HPI = tidyselect::matches("^USA.*\\(NSA\\)$")) %>%
  filter(!is.na(month)) %>%
  mutate(date = mdy(month),
         year = year(date),
         month = month(date)) %>%
  select(-date) %>%
  inner_join(periods_for_growth) %>%
  bind_rows(CS_20_city) %>%
  arrange(type, year, month, city) %>%
  group_by(type, city) %>%
  transmute(annualised_growth = -1 + (HPI/lag(HPI))^(1/(year-lag(year)))) %>%
  filter(!is.na(annualised_growth)) %>%
  ungroup() %>%
  mutate(min_growth = min(annualised_growth)) %>%
  group_by(city) %>%
  filter(is.na(city) | min_growth == min(annualised_growth),
         type != "measure_worst") %>%
  mutate(type = if_else(!is.na(city),
                        "worst_city",
                        type)) %>%
  ungroup()


#City with slowest post period growth is New York
testthat::expect_equal(all_HPI %>%
                         filter(!is.na(city)) %>%
                         pull(city), "LVXRNSA")

HPI <- all_HPI %>%
  select(type, annualised_growth) %>%
  group_by(type) %>%
  nest(.key = "growth")

test_that("Check house rate growth in main period",
          expect_equal(HPI %>%
                         unnest() %>%
                         filter(type == "coincident") %>%
                         pull(annualised_growth),
                       0.0580,
                       tol = 0.0001))

#Unmodified mortgage terms, from source csv
df <- read_xls('./data/mtg_rd_ac2019-03-22.xls', sheet='tbl_sum_stats') %>%
  select(Variable, Mean) %>%
  as.data.frame()

rownames(df) <- df$Variable
df <- df %>% select('Mean') %>% t() %>% as.data.frame()

ltv <- df$`Loan to Value Ratio`
mtmval_0 <- df$`Home Value`
upb_0 <- (1/100) * ltv * mtmval_0
r_perm_0 <- df$`Mortgage Interest Rate`
term_0 <- round(df$`Mortgage Term Remaining (Years)`)
terms_nomod <-
  list(r_temp = r_perm_0, r_perm = r_perm_0, term = term_0, upb_0 = upb_0, forbear = 0.0)
pays_nomod <-
  pmt_stream(terms_nomod)
pay_nomod <- pays_nomod$pay[[1]]

#Discount rate assumptions
yield_curves <- read_csv('./data/calc_npv_data/yield_curves.csv')
disc_rate_30 <-
  yield_curves %>%
  filter(id == 'implied_gse') %>%
  select(rate_30) %>%
  pull() %>%
  round(4)
disc_vec_30 <- rep(1/(1+disc_rate_30), 40)

### Account for different prin. reduc on each side of RD before applying waterfall ###
df <-
  tribble(
    ~side, ~prin_red, ~pmt_target_total,
    #-----/-----------/----------------
    'RHS', 19500, 0.135,
    'LHS', 14000,0.31
  )

df <- df %>%
  mutate(upb_aft_forgive = upb_0 - prin_red)

df <- df %>%
  mutate(terms_prin_red_only = upb_aft_forgive %>%
           map(function(upb) list(upb_0 = upb,
                                  r_temp = r_perm_0,
                                  r_perm = r_perm_0,
                                  term = term_0,
                                  forbear = 0.0))
  )

df_terms <- df

#Stream of expected payments with principal reduction only
df_pays <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(df_pays) <- c('pay', 'upb', 'year', 'side')

for (x in c('LHS', 'RHS')){
  df <- data.frame(df_terms %>%
                     filter(side==x) %>%
                     select(terms_prin_red_only) %>%
                     extract2(1) %>%
                     extract2(1) %>%
                     pmt_stream())
  df <- df %>%
    mutate(side = x,
           year = row_number())
  df_pays <- df_pays %>% rbind(df)
}
df_pays <- df_pays %>%
  mutate(treat = 'prin_red_only')

#Pay reduc targets. Account for different prin. reduc on each side of RD
df <- data.frame(matrix(ncol = 2, nrow = 0))
for (x in c('LHS', 'RHS')){
  pay_year <- df_pays %>%
    filter(year == 1, side == x) %>%
    select(pay, side)
  df <- rbind(df, pay_year)
}

#Amount of payment reduction needed after principal forgiveness
df_terms <- df_terms %>%
  full_join(df, by='side') %>%
  rename(pay_prin_red_only = pay) %>%
  mutate(target_pay = (1-pmt_target_total) * pay_nomod,
         pay_red_percent = (pay_prin_red_only - target_pay)/pay_prin_red_only)

#Terms to get to target (total) payment reduction
get_terms <- function(x, data = df_terms){
  row <- data %>%
    filter(side==x)

  terms <- row %>%
    select(terms_prin_red_only) %>%
    extract2(1) %>%
    extract2(1)
  pmt_target <- row %>%
    select(pay_red_percent) %>%
    pull()

  if(x == 'LHS'){
    wfall <-  'Private Chase'
  } else if(x=='RHS'){
    wfall <-  "HAMP"
  }

  out <-
    wfall_mod_terms(
      pmt_target,
      wfall,
      terms,
      mtmval_0,
      min_r_perm = 0.02
    )
}

df_terms <- df_terms %>%
  mutate(terms = side %>% map(get_terms))

#Stream of expected payments (prin red + treatment)
for (x in c('LHS', 'RHS')){
  df <- data.frame(df_terms %>%
                     filter(side==x) %>%
                     select(terms) %>%
                     extract2(1) %>%
                     extract2(1) %>%
                     pmt_stream())
  df <- df %>%
    mutate(side = x,
           year = row_number(),
           treat = 'total')
  df_pays <- df_pays %>% rbind(df)
}

pmt_stream_nomod <- pmt_stream(terms_nomod)
df_pays <- rbind(df_pays,
                 data.frame(pmt_stream_nomod) %>%
                   mutate(
                     side = 'nomod',
                     year = row_number(),
                     treat = 'total'))




############################################
###### Calculate Time Underwater ###########
############################################
#Account for growth in property values
t_breakeven <- function(x, data){
  t_brkeven <- data %>%
    filter(side == x,
           treat =='total',
           abvwater == TRUE) %>%
    select(year) %>%
    min()
}

<<<<<<< HEAD
=======
t_breakeven_exact <- function(x, data){
  data %>%
    filter(side == x, treat == "total") %>%
    group_by(abvwater) %>%
    filter(abvwater & year == min(year) |
             !abvwater & year == max(year)) %>%
    mutate(equity = prop_val - upb) %>%
    lm(year ~ equity, .) %>%
    broom::tidy() %>%
    filter(str_detect(term, "Intercept")) %>%
    pull(estimate)
}

>>>>>>> master
prop_vals <-
  HPI %>%
  mutate(value_series =
           map(growth,
               ~ data.frame(
                 prop_val = cumprod(rep(1+ .x, 40)) * mtmval_0,
                 year = seq (1,40))),
         df_pays =
           map(value_series, ~ .x %>%
                 inner_join(df_pays) %>%
                 mutate(abvwater = upb < prop_val)),
         df_terms = map(df_pays, function(df_pays)
           df_terms %>%
             mutate(t_breakeven =
                    map_dbl(side, ~ t_breakeven(.x, df_pays)),
                    t_breakeven_exact = map_dbl(side, ~ t_breakeven_exact(.x, df_pays)))),
         df = map(df_pays, ~ .x %>%
                    filter(treat=='total',
                           side=='LHS' | side =='RHS') %>%
                    select(year, side, upb) %>%
                    rename(key = side, value = upb)),
         df = map2(value_series, df,
                   ~ .x %>%
                     transmute(value = prop_val,
                               key = rep('prop_val', 40),
                               year = year) %>%
                     rbind(.y) %>%
                     mutate(key = case_when(key == 'LHS' ~ 'Unpaid balance: treatment',
                                            key == 'RHS' ~ 'Unpaid balance: control',
                                            key == 'prop_val' ~ 'Property Value'))),
         plt = map(df,
                    ~ ggplot(.x, aes(year, value, color = key, linetype = key)) +
                      geom_line() +
                      scale_color_manual(values = cbPalette_set2[1:3]) +
                      scale_linetype_manual(values=c("solid", "twodash", "dotted")) +
                      scale_y_continuous(labels = scales::dollar, limits =c(150000,275000)) +
                      scale_x_continuous(breaks =c(2,4,6,8,10), limits =c(0,10)) +
                      fte_theme() +
                      theme(legend.title=element_blank(),
                            legend.justification=c(0,1),
                            legend.position=c(0,0.3)) +
                      xlab('Years Since Modification') +
                      ylab("Value")))


map2(prop_vals$type, prop_vals$plt,
     ~ ggsave(filename = str_c("./out/years_underwater",
                               if_else(.x == "coincident",
                                "",
                                str_c("_robustness_", .x)),
                               ".png"),
              plot = .y,
              width = 6,
              height = 4))

############################################
#### Calculate defaults when underwater#####
############################################
#https://www.treasury.gov/initiatives/financial-stability/reports/Documents/1Q17%20MHA%20Report%20Final.pdf
#Default rates of 2010 HAMP Tier 1 Modifications (Page 8)
hamp_perf_data <-
  tribble(
    ~year, ~dflts_perf,
    #------/------------
    0.25, 0.017,
    0.5,  0.067,
    1.0,  0.156,
    1.5,  0.227,
    2.0,  0.281,
    2.5,  0.326,
    3.0,  0.366,
    3.5,  0.394,
    4.0,  0.416,
    4.5,  0.436,
    5.0,  0.456
  )

#2- year default rates from 31% RD
df <- read_xls('./data/mtg_rd_ac2019-03-22.xls', sheet='tbl_local_linear') %>%
  select('Type' = pos,'dflt_post' = delin_ever90)

#use ratio of t-year/2-year default in perf. data to estimate 5 years of dflt
scale_fac_dflt <-
  hamp_perf_data$dflts_perf/
  (hamp_perf_data %>% filter(year==2) %>% pull(dflts_perf))

hamp_perf_data <-
  hamp_perf_data %>%
  mutate(dflts_RHS = (df %>% filter(Type=='TRUE') %>% pull(dflt_post)) *scale_fac_dflt,
         dflts_LHS = (df %>% filter(Type=='FALSE') %>% pull(dflt_post)) *scale_fac_dflt)


### Fit log model ###
df_predicts <- tibble(year = seq(0,40))

get_dflts_predict <- function(dflt_type, data = hamp_perf_data){
  model <- data %>%
    select(dflt_type, year) %>%
    rename(dflt = dflt_type) %>%

    lm(dflt ~ log(year), data = .)
  predicts <- predict(model ,newdata = df_predicts %>% select(year))
}


for (x in c('dflts_perf', 'dflts_RHS', 'dflts_LHS')){
  df_predicts <- df_predicts %>%
    mutate(!!x := get_dflts_predict(x))
  }

### Plot cumulative default probabilities ###
df_lines <- df_predicts %>%
  rename("HAMP performance data" = dflts_perf,
         "Defaults: treatment" = dflts_LHS,
         "Defaults: control" = dflts_RHS) %>%
  gather(key = type, value = dflt, -year)

df_pts <- hamp_perf_data %>%
  transmute(year,
            dflt = dflts_perf,
            type = 'HAMP performance data')


with_plots <- prop_vals %>%
  mutate(df_pts = map(df_terms, function(df_terms)
           df_terms %>%
             group_by(side) %>%
             nest() %>%
             mutate(output =
                      map2(side, data, function(side, data)
                        df_lines %>%
                          filter(year == data %>% pull(t_breakeven),
                                 type == if_else(side=='LHS',
                                                "Defaults: treatment",
                                                "Defaults: control")) %>%
                          select(year, type, dflt))) %>%
             pull(output) %>%
             bind_rows(df_pts)),
         plt = map(df_pts, function(df_pts)
           ggplot() +
             geom_line(data = df_lines, aes(x = year, y= dflt, group = type, color = type, linetype = type)) +
             geom_point(data = df_pts, aes(x = year, y= dflt, group = type, color = type, shape = type)) +
             scale_color_manual(values = c(cbPalette_set2[2], cbPalette_set2[3], cbPalette_set2[1])) +
             scale_linetype_manual(values=c("twodash", "dotted", "solid")) +
             scale_shape_manual(values = c(17, 15, 16))+
             fte_theme() +
             scale_x_continuous(breaks =c(2,4,6,8), limits =c(0,8)) +
             xlab('Years Since Modification') +
             ylab("Cumulative Default Rate") +
             theme(legend.title=element_blank(),
                   legend.justification=c(0,1),
                   legend.position=c(0.0,0.9))
           ))

map2(with_plots$type, with_plots$plt,
     ~ ggsave(filename = str_c("./out/default_by_years_underwater",
                               if_else(.x == "coincident",
                                       "",
                                       str_c("_robustness_", .x)),
                               ".png"),
              plot = .y,
              width = 6,
              height = 4))



with_plots %>%
  filter(type == "coincident") %>%
  pull(df_terms) %>%
  pluck(1) %>%
  select(side, t_breakeven) %>%
  testthat::expect_equal(tibble(side = c("RHS", "LHS"),
                                t_breakeven = c(3, 4)))

test_that("correct months extra undewater in Las Vegas",
          with_plots %>%
  filter(type == "worst_city") %>%
  pull(df_terms) %>%
  pluck(1) %>%
  select(side, t_breakeven_exact) %>%
  spread(side, t_breakeven_exact) %>%
  transmute(diff = round((LHS - RHS)*12)) %>%
  pull(diff) %>%
  expect_equal(5))

with_plots %>%
  filter(type == "coincident") %>%
  pull(df_pts) %>%
  pluck(1) %>%
  filter(str_detect(type, "Defaults")) %>%
  select(type, dflt) %>%
  mutate_if(is.numeric, ~ round(., digits = 5)) %>%
  testthat::expect_equal(tibble(type = c("Defaults: control",
                             "Defaults: treatment"),
                    dflt = c(0.41178,
                             0.35755)))
