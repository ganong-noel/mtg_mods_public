---
title: "NPV Cost-Benfit Analysis for Different Modifications"
author: "Xian Ng"
date: "Jan 24, 2018"
output: github_document
---


### Setup Initial Conditions & Parameters

```r
df <- read_xls('./data/calc_npv_data/disclosed_20180622.xls', sheet='tbl_sum_stats') %>% select(var, mean) %>% as.data.frame()
rownames(df) <- df$var
df <- df %>% select('mean') %>% t() %>% as.data.frame()

#Stats from JPMC
upb_0_jpmc <- (1/100) * df$ltv * df$prop_val
mtmval_jpmc <- df$prop_val
r_jpmc <- df$rate 
term_jpmc <- round(df$years_remain)

#Terms for representative mortgage
base_terms_rhs <- list(r_temp = r_jpmc, r_perm = r_jpmc, term = term_jpmc, upb_0 = upb_0_jpmc, forbear = 0.0)
pmt_stream_rhs <- pmt_stream(base_terms_rhs)
base_terms_5_pct <- list(r_temp = 0.05, r_perm = 0.05, term = term_jpmc, upb_0 = upb_0_jpmc, forbear = 0.0)
pmt_stream_5_pct <- pmt_stream(base_terms_5_pct)

#Different Discount Rates -
yield_curves <- read_csv('./data/calc_npv_data/yield_curves.csv')
```

```
## Parsed with column specification:
## cols(
##   spread = col_double(),
##   id = col_character(),
##   type = col_character(),
##   rate_30 = col_double(),
##   rate_40 = col_double()
## )
```

```r
disc_rate_30 <-  yield_curves %>% filter(id == 'implied_gse') %>% select(rate_30) %>% pull() %>% round(4)
disc_rate_40 <- yield_curves %>% filter(id == 'implied_gse') %>% select(rate_40) %>% pull() %>% round(4)
disc_vec_30 <- rep(1/(1+disc_rate_30), 40)
disc_vec_40 <- rep(1/(1+disc_rate_40), 40)
disc_rates <- round(sapply(seq(30, 40, 1),
                           function(x) -0.03297+1.13788*log(x))/100, 4) #interpolates from 30 to 40 year

#Prepayment and Default
prepay_vec_dynamic <- calc_prepayment(pmt_stream_rhs$upb, mtmval_jpmc)
prepay_vec_5_pct <- calc_prepayment(pmt_stream_5_pct$upb, mtmval_jpmc)

dflt_factor_5year <-45.6/ 28.1 #Conversion factor from 2 year to 5 year default rates
dflt_5_year_nomod <- calc_mod_dflt(0)
dflt_2_year_nomod <- dflt_5_year_nomod / dflt_factor_5year

#Different assumptions
assumps_dynamic_30 <- list(mtmval=mtmval_jpmc, prepay_vec=prepay_vec_dynamic, disc_vec=disc_vec_30,
                           p_dflt=dflt_5_year_nomod, recovery='baseline')
assumps_dynamic_30_pessimistic <- list(mtmval=mtmval_jpmc, prepay_vec=prepay_vec_dynamic, disc_vec=disc_vec_30,
                                       p_dflt=dflt_5_year_nomod, recovery='pessimistic')
assumps_dynamic_30_optimistic <- list(mtmval=mtmval_jpmc, prepay_vec=prepay_vec_dynamic, disc_vec=disc_vec_30,
                                      p_dflt=dflt_5_year_nomod, recovery='optimistic')
assumps_pay_on_schd_30 <- list(mtmval=mtmval_jpmc, prepay_vec=rep(0.0, 40), disc_vec=disc_vec_30, p_dflt=0, recovery='baseline')
assumps_5_pct <- list(mtmval=mtmval_jpmc, prepay_vec=prepay_vec_5_pct, disc_vec=disc_vec_30,
                      p_dflt=dflt_5_year_nomod, recovery='baseline')

#For saving output stats
df_stats <- data_frame('stat' = character(),
                       'value' = double(),
                       'unit' = character())
```




### What is the cost of achieving 10% payment reduction with the different methods?
Rate reductions are in fractions (multiply by 100 to get percentage points). Maturity extenion is in years. Forgiveness and forbearance are in dollars. Note that the discount rate used when calculating maturity extension is 4.16\% compared to3.84\% for the other modifications.

```r
#Setup for bar graphs
pay_red_goal <- 0.1

df_pay_owed <- cost_target_pay_red_df(pay_red_goal, base_terms_rhs, assumps_pay_on_schd_30, dynamic=FALSE)
df_pay_expect <- cost_target_pay_red_df(pay_red_goal, base_terms_rhs, assumps_dynamic_30, dynamic=TRUE)
df_recover_optimistic <- cost_target_pay_red_df(pay_red_goal, base_terms_rhs, assumps_dynamic_30_optimistic, dynamic=TRUE)
df_recover_pessimistic <- cost_target_pay_red_df(pay_red_goal, base_terms_rhs, assumps_dynamic_30_pessimistic, dynamic=TRUE)
df_5_pct <- cost_target_pay_red_df(pay_red_goal, base_terms_5_pct, assumps_5_pct, dynamic=TRUE)
```


|mod_type   |       amt_mod| d_pay_dollars| d_pay_percent|  cost_npv|
|:----------|-------------:|-------------:|-------------:|---------:|
|mat_extend |     7.0000000|       2055.91|      9.607209| -18059.60|
|r_temp     |     0.0117947|       2139.91|      9.999738|  13755.03|
|r_perm     |     0.0117947|       2139.91|      9.999738|  32301.98|
|forgive    | 24794.2823692|       2139.97|     10.000019|  32302.89|
|forbear    | 24794.2823692|       2139.97|     10.000019|  21880.58|



|mod_type   |       amt_mod| d_pay_dollars| d_pay_percent|   cost_npv|
|:----------|-------------:|-------------:|-------------:|----------:|
|mat_extend |     7.0000000|       2055.91|      9.607209| -22661.103|
|r_temp     |     0.0117947|       2139.91|      9.999738|  -4670.396|
|r_perm     |     0.0117947|       2139.91|      9.999738|   5369.736|
|forgive    | 24794.2823692|       2139.97|     10.000019|  11087.089|
|forbear    | 24794.2823692|       2139.97|     10.000019|  -1470.001|

![plot of chunk bar_graphs](figure/bar_graphs-1.png)

```
## Scale for 'y' is already present. Adding another scale for 'y', which
## will replace the existing scale.
```

![plot of chunk bar_graphs](figure/bar_graphs-2.png)
## Waterfall Comparisons
In this section we describe the cumulative effects of introducting the following modifications in sequence (efficient waterfall):

1. Extension of mortgage term to 40 years
2. Reduction of interest rate to 2\% for the first 5 years
3. Principal forbearance until 50\% payment reduction is reached


We compare the costs of achieving between 1-60\% payment reductions using the efficient waterfall and the other waterfalls/modifications offered.


###Find Inflection Points
For each of the waterfalls, we find the amount of payment reduction reached and thee npv cost at each inflection point where the waterfall switches to a different method of payment reduction. 

```r
df_compare <- data_frame('waterfall' = character(), 'terms_0' = character(),
                         'terms_exist' = character(), 'terms_mod' = character(),
                         'd_pay_step' = double(), 'cost_npv' = double(),
                         'd_pay_cum' = double(), 'pay_red_cum'=double())
df_compare_PO <- df_compare

for(waterfall in c("HAMP Principal Reduction Alternative",
                   "HAMP",
                   "Efficient Default-minimizing",
                   "Principal Forgiveness")){
  df_compare <- df_compare %>% rbind(find_inflec_pts(waterfall, base_terms_rhs, assumps_dynamic_30))
  df_compare_PO <- df_compare_PO %>% rbind(find_inflec_pts(waterfall, base_terms_rhs, assumps_pay_on_schd_30, dynamic=FALSE))
}  
df_compare_sparse <- df_compare %>% select(waterfall, pay_red_cum, cost_npv)
df_compare_PO_sparse <- df_compare_PO %>% select(waterfall, pay_red_cum, cost_npv)
```







### Waterfalls Grid
For each of the waterfalls, we compute the npv cost at a grid over a grid of payment reduction.

```r
df_waterfall <- data_frame('pay_red_cum' = c(seq(0.05, 0.7, 0.05),
                                             seq(0.05, 0.7, 0.05),
                                             seq(0.05, 0.7, 0.05),
                                             seq(0.05, 0.7, 0.05)),
                           'waterfall' = c(rep('Principal Forgiveness', 14),
                                           rep('HAMP', 14),
                                           rep('HAMP Principal Reduction Alternative', 14),
                                           rep('Efficient Default-minimizing', 14)))
df_waterfall_PO <- df_waterfall


#Compute cost for each of these
df_waterfall <- df_waterfall %>%
  rowwise() %>%
  mutate(cost_npv = npv_cost(pmt_target=pay_red_cum,
                             waterfall=waterfall,
                             base_terms=base_terms_rhs,
                             assumps=assumps_dynamic_30))


df_waterfall <- rbind(df_compare_sparse, df_waterfall)
df_waterfall <- df_waterfall %>% mutate( type=factor(waterfall, levels=c("Efficient Default-minimizing",
                                                                         "Principal Forgiveness",
                                                                         "HAMP",
                                                                         "HAMP Principal Reduction Alternative")))

#Payments Owed version
df_waterfall_PO <- df_waterfall_PO %>%
  rowwise() %>%
  mutate(cost_npv = npv_cost(pmt_target=pay_red_cum,
                             waterfall=waterfall,
                             base_terms=base_terms_rhs,
                             assumps=assumps_pay_on_schd_30,
                             dynamic=FALSE))

df_waterfall_PO <- rbind(df_compare_PO_sparse, df_waterfall_PO)
df_waterfall_PO <- df_waterfall_PO %>% mutate( type=factor(waterfall, levels=c("Efficient Default-minimizing",
                                                                               "Principal Forgiveness",
                                                                               "HAMP",
                                                                               "HAMP Principal Reduction Alternative")))
```



###NPV Cost Point Estimates
We compute the average NPV cost for the payment reduction obtained by Chase mods and GSE mods. 

```r
### Point estimates - average cost of GSE and Chase Mods
#JPMC Borrower Data
df <- read_xls('./data/calc_npv_data/disclosed_20180622.xls', sheet='tbl_local_linear')
df <- df %>% select('Type' = pos, 'inc' = cplt_loan_brw_incm_am, 
                    'mtmval' = curr_prop_aprs_val_am, 'pay_mo_pre' = pri_loan_prin_int_am,
                    'pay_reduc' = pi_due_chg_ratio, 'dflt_post' = delin_ever90)
                          
df <- mutate(df, 'Type' =df$Type  %>%lapply((function(pos){
                                        if(pos=='FALSE'){return('lhs')}
                                        if(pos=='TRUE'){return('rhs')}}))
                                            %>% unlist() )
df$dti_pre <- df$pay_mo_pre / df$inc
df$pay_mo_post <- df$pay_mo_pre * (1 + df$pay_reduc)
df$dti_post <- df$pay_mo_post/df$inc

jpmc_bdata <- df %>% as.data.frame()
rownames(jpmc_bdata) = jpmc_bdata$Type
jpmc_bdata$dflt_pre <- c(dflt_5_year_nomod, dflt_5_year_nomod) #Assumption about pre-treatment default rates
jpmc_bdata$dflt_post <- dflt_factor_5year *jpmc_bdata$dflt_post 


#GSE Borrower Data # XXX Needs to be updated hardcode
gse_bdata <- read.csv('./data/calc_npv_data/gse_bdata.csv')
rownames(gse_bdata) = gse_bdata$Type
gse_bdata$dflt_pre <- c(dflt_5_year_nomod, dflt_5_year_nomod) #Assumption about pre-treatment default rates
gse_bdata$dflt_post <- dflt_factor_5year * c(0.216, 0.273) #Eyeballing LHS/RHS from RD in chartbook


#Average cost to investors of the CHAMP and GSE mods
dnpv_gse <- npv_cost((-1)*gse_bdata['lhs', 'pay_reduc'],
                     "Private GSE",
                     base_terms_rhs,
                     assumps_dynamic_30)
dnpv_jpmc <- npv_cost((-1)*jpmc_bdata['lhs', 'pay_reduc'],
                      "Private Chase",
                      base_terms_rhs,
                      assumps_dynamic_30)

df_stats <-df_stats %>% add_row('stat' = 'The average cost per 1% of payment reduction is () for GSE',
                                'value' = dnpv_gse /(-100* gse_bdata['lhs', 'pay_reduc']),
                                'unit' = '$/percentage point')

df_stats <-df_stats %>% add_row('stat' = 'The average cost per 1% of payment reduction is () for Chase',
                                'value' = dnpv_jpmc / (-100* jpmc_bdata['lhs', 'pay_reduc']),
                                'unit' = '$/percentage point')

dnpv_hamp_gse <- npv_cost((-1)*gse_bdata['lhs', 'pay_reduc'],
                          "HAMP",
                          base_terms_rhs,
                          assumps_dynamic_30)

dnpv_hamp_jpmc <- npv_cost((-1)*jpmc_bdata['lhs', 'pay_reduc'],
                           "HAMP",
                           base_terms_rhs,
                           assumps_dynamic_30)

df_stats <-df_stats %>% add_row('stat' = 'GSE is () of the cost of providing an equivalently-sized payment reduction in HAMP',
                                'value' = dnpv_gse / dnpv_hamp_gse,
                                'unit' = 'fraction GSE/HAMP')

df_stats <-df_stats %>% add_row('stat' = 'Chase is () of the cost of providing an equivalently-sized payment reduction in HAMP',
                                'value' = dnpv_jpmc / dnpv_hamp_jpmc,
                                'unit' = 'fraction Chase/HAMP')

##Add points to ggplot
dnpv_point_jpmc <- data.frame('pay_red_cum' = jpmc_bdata['lhs', 'pay_reduc'], 'waterfall'= 'Private Chase', 'cost_npv' = dnpv_jpmc)
dnpv_point_gse <- data.frame('pay_red_cum' = gse_bdata['lhs', 'pay_reduc'],'waterfall'= 'Private GSE' ,'cost_npv' = dnpv_gse)
dnpv_point <- rbind(dnpv_point_gse, dnpv_point_jpmc)

#Payments Owed version
dnpv_gse_PO <- npv_cost((-1)*gse_bdata['lhs', 'pay_reduc'],
                        "Private GSE",
                        base_terms_rhs,
                        assumps_pay_on_schd_30,
                        dynamic=FALSE)
dnpv_jpmc_PO <- npv_cost((-1)*jpmc_bdata['lhs', 'pay_reduc'],
                         "Private Chase",
                         base_terms_rhs,
                         assumps_pay_on_schd_30,
                         dynamic=FALSE)
dnpv_point_jpmc_PO <- data.frame('pay_red_cum' = jpmc_bdata['lhs', 'pay_reduc'], 'waterfall'= 'Private Chase', 'cost_npv' = dnpv_jpmc_PO)
dnpv_point_gse_PO <- data.frame('pay_red_cum' = gse_bdata['lhs', 'pay_reduc'],'waterfall'= 'Private GSE' ,'cost_npv' = dnpv_gse_PO)
dnpv_point_PO <-  rbind(dnpv_point_gse_PO, dnpv_point_jpmc_PO)
```


```
## Scale for 'x' is already present. Adding another scale for 'x', which
## will replace the existing scale.
```

```
## Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which
## will replace the existing scale.
```

```
## Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```
### Social Value/Cost of Modifications
We compare the value to investors and the value to society (based on expected reduction in foreclosures) of implementing the efficient modification waterfall (ED-M). 


```r
dflt_assumps <- read_excel('./data/calc_npv_data/liquidation_probability.xlsx', sheet='lqd_prob')  %>% as.data.frame()
p_fcl_def_base <- dflt_assumps$p_fcl_if_dflt  #Conditional probability of foreclosure
p_fcl_def_pessimistic <- dflt_assumps$p_fcl_if_dflt_pessimistic 
val_fcl <- dflt_assumps$val_fcl #Value in dollars of each avoided foreclosure

#Social value of modification - nonlinear default response
df <- data_frame('pay_red' = 0:70)

df$p_dflt_5_year <-calc_mod_dflt(df$pay_red)
df$p_dflt_2_year <- df$p_dflt_5_year / dflt_factor_5year
df$dflt_reduc <- df$p_dflt_5_year[[1]] - df$p_dflt_5_year # Reduction in defaults in percentage points

df$fcl_reduc <- df$dflt_reduc * p_fcl_def_base 
df$fcl_reduc_pessimistic <- df$dflt_reduc * p_fcl_def_pessimistic 

df$value <- df$fcl_reduc * val_fcl
df$value_pessimistic <- df$fcl_reduc_pessimistic * val_fcl

df$pay_red <- -1 * df$pay_red/100
df$type <- rep('Social Value', nrow(df))

#DF for comparison
df_soc_val <- data_frame('pay_red' = double(),
                         'value' = double(),
                         'type' = character())
df_soc_val_pessimistic <- df_soc_val

df_soc_val <- rbind(df_soc_val, df %>% select(pay_red, value, type))
df_soc_val_pessimistic <- rbind(df_soc_val_pessimistic, df %>% select(pay_red, value_pessimistic, type))
df_soc_val_pessimistic <- rename (df_soc_val_pessimistic, value = value_pessimistic)
df_soc_val_pessimistic$type <- rep('Social Value, Pessimistic', nrow(df_soc_val_pessimistic))

#Add to waterfall
df_waterfall_soc_val <- df_soc_val

#Compare to ED-M waterfall
df <- filter(df_waterfall, waterfall=='Efficient Default-minimizing')
df <- df %>% select(pay_red_cum, cost_npv, type)
df <- df %>% rename(pay_red =pay_red_cum, value = cost_npv )
df$pay_red <- -1 * df$pay_red
df_soc_val <-  rbind(df_soc_val, df)

#Add 2 more bars (pessimistic recovery)
df <- filter(df_waterfall_pessimistic, waterfall=='Efficient Default-minimizing')
df$type <- rep('Efficient Default-minimizing, Pessimistic', nrow(df))
df <- select(df, pay_red_cum, cost_npv, type)
df <- df %>% rename(pay_red = pay_red_cum, value = cost_npv )
df$pay_red <- df$pay_red * -1
df_soc_val_pessimistic <- rbind(df_soc_val_pessimistic, df)
df_soc_val_pessimistic <- rbind(df_soc_val_pessimistic, df_soc_val)

###Find the break-even point between social value and EDM
target_zero_soc_val_edm <- function(pct_pay_red){
  cost_edm <-npv_cost(pct_pay_red/100, 'Efficient Default-minimizing', base_terms_rhs, assumps_dynamic_30)
  dflt_reduc <- calc_mod_dflt(0) - calc_mod_dflt(pct_pay_red)
  soc_val <- dflt_reduc * p_fcl_def_base  * val_fcl
  return(cost_edm - soc_val)
}
edm_soc_val_breakeven <- uniroot(target_zero_soc_val_edm, c(1,100), maxiter=8)
df_stats <-df_stats %>% add_row('stat' = 'The social value of a modification is equal to its cost under EDM at ()% pay red',
                                'value' = edm_soc_val_breakeven$root,
                                'unit' = '% Points')
dflt_reduc <- calc_mod_dflt(0) - calc_mod_dflt(edm_soc_val_breakeven$root)
df_stats <-df_stats %>% add_row('stat' = 'This has a cost/value of $() per modification',
                                'value' = dflt_reduc * p_fcl_def_base  * val_fcl,
                                'unit' = 'Dollars')


#Make the plots
df_soc_val <- df_soc_val %>%
  mutate(line=TRUE,
         type=factor(type,
                     levels=c("Efficient Default-minimizing",
                              "Social Value")))
df_soc_val_pessimistic <- df_soc_val_pessimistic %>%
  mutate(line=TRUE,
         type=factor(type,
                     levels=c("Efficient Default-minimizing",
                              "Social Value",
                              "Efficient Default-minimizing, Pessimistic",
                              "Social Value, Pessimistic")))


soc_val_plots <- list(list('df' = df_soc_val, 'filename'='./out/social_value.pdf'),
                      list('df' = df_soc_val_pessimistic, 'filename'='./out/social_value_robust.pdf'))

for(soc_val_plot in soc_val_plots){
  if(soc_val_plot$filename == './out/social_value_robust.pdf'){
    ylim_max <- 50000
  }
  else{
    ylim_max <- 30000
  }
  
  df <- soc_val_plot$df
  plt <- ggplot() +
    geom_line(data=df, aes(x=pay_red, y=value, colour=type, group=type, linetype=type)) +
    fte_theme() +
    scale_y_continuous(labels = scales::dollar, limits=c(min(df$value), ylim_max)) +
    scale_x_reverse(labels = scales::percent,
                    breaks = seq(-0.70, 0, 0.1)) +
    coord_cartesian(xlim = c(-0.70, 0)) +
    labs(x = 'Change in Mortgage Payment',
         y = 'Investor NPV Cost/Social Value') +
    theme(legend.title = element_text(size=10,
                                      color=brewer.pal("Greys", n=9)[7],
                                      family="serif"),
          legend.justification = c(0,1), legend.position = c(0,1),
          legend.text = element_text(size=9),
          legend.margin = margin(0, 0.2, 0, 0.2, unit="cm"),
          legend.box.margin = margin(0, 0.2, 0.2, 0, unit="cm"),
          legend.spacing.y = unit(0, "cm")) +
    scale_colour_manual(name='Modification Type',
                        values=c('navy',
                                 'navy',
                                 cbPalette_blue[1],
                                 cbPalette_blue[1])) +
    scale_linetype_manual(name='Modification Type',
                          values=c(1, 2, 4, 3)) +
    scale_fill_manual(name=element_blank(),
                      values=c(cbPalette_blue[1],
                               cbPalette_blue[1])) +
    scale_shape_manual(name=element_blank(),
                       values=c(21, 24))
  plt
  ggsave(soc_val_plot$filename, width = 6, height = 4, device='pdf')
}

plt <- plt_dynamic_60 + 
  geom_line(data=df_waterfall_soc_val,
            aes(x=pay_red, y=value, colour='Social Value', linetype='Social Value')) +
  scale_colour_manual(name='Modification Type',
                      breaks=c("Efficient Default-minimizing",
                               "Principal Forgiveness",
                               "HAMP",
                               "HAMP Principal Reduction Alternative",
                               "Social Value"),
                      values=c('navy',
                               cbPalette_blue[1],
                               cbPalette_blue[1],
                               'navy',
                               cbPalette_blue[1])) +
  scale_linetype_manual(name='Modification Type',
                        breaks=c("Efficient Default-minimizing",
                                 "Principal Forgiveness",
                                 "HAMP",
                                 "HAMP Principal Reduction Alternative",
                                 "Social Value"),
                        values=c(1, 4, 3, 2, 5)) +
  scale_x_reverse(labels = scales::percent,
                  breaks = seq(-0.6, 0, 0.1)) +
  labs(x = 'Change in Mortgage Payment',
         y = 'Cost in NPV to Investors and Taxpayers') +
  coord_cartesian(xlim = c(-0.65, 0)) +
   guides(linetype=guide_legend(order=1),
           colour=guide_legend(order=1),
           fill = guide_legend(order=2),
           shape = guide_legend(order=2))
```

```
## Scale for 'colour' is already present. Adding another scale for
## 'colour', which will replace the existing scale.
```

```
## Scale for 'linetype' is already present. Adding another scale for
## 'linetype', which will replace the existing scale.
```

```
## Scale for 'x' is already present. Adding another scale for 'x', which
## will replace the existing scale.
```

```
## Coordinate system already present. Adding new coordinate system, which will replace the existing one.
```

```r
plt
```

![plot of chunk social_value](figure/social_value-1.png)

```r
ggsave('./out/compare_waterfalls_dynamic_60_social_value.pdf', width = 6, height = 4)
```




### Stats for text
We want the following statistics:

1. "Marginal cost" of payment reduction using maturity extension, and maximum reduction possible when extending to 40 years.
2. Subsequently, "marginal cost" of payment reduction using temporary rate reduction, and maximum reduction possible when reducing to 2%
3. What is the breakeven amount of payment reduction, and at what maturity/temporary rate reduction?
4. What is the 'marginal cost' of forbearance?
5. What is the marginal cost of principal reduction?
6. Until what amount of payment reduction reduction does HAMP-PRA follow principal reduction (most inefficient policy)?
7. What is the average cost of payment reduction per %-point for Chase?
8. What is the average cost of payment reduction per %-point for GSEs?

### Headline Stats and robustness checks



