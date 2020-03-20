source("./func/binscatter_MultiControl.R")
source("./func/winsor.R")


##################Function to get value if Cure##################
calc_npv_cure <- function(pays_vec, upb_vec, prepay_vec, disc_vec){
  # Computes the NPV_if_cure for a given stream of payments and upb
  #
  # Args:
  #   pays_vec: Vector of expected annual payments
  #   upb_vec: Vector of expected unpaid balances
  #   prepay_vec: Vector of prepayment probability in each period
  #   disc_vec: vector of discount rates
  #
  # Returns:
  #   NPV of mortgage if cure

  #Cumulative discount rates
  disc_cum <- cumprod(disc_vec)

  #Cumulative survival (non-prepayment) rates
  survive_p <- 1- prepay_vec
  survive_cum <- cumprod(survive_p)
  prepay_p_abs <- c(prepay_vec[1], -diff(survive_cum))
  #NPV if cure
  npv_cure <- sum(disc_cum* (pays_vec  *  survive_cum + upb_vec  * prepay_p_abs))
  return(npv_cure)
}


lqd_probs <- read_excel('./data/calc_npv_data/liquidation_probability.xlsx', sheet='lqd_prob')  %>% as.data.frame()
p_lqd_if_dflt_base <- lqd_probs$p_lqd_if_dflt_base %>% round(digits = 2)
p_lqd_if_dflt_opt <- lqd_probs$p_lqd_if_dflt_opt %>% round(digits = 2)
p_lqd_if_dflt_pess <- lqd_probs$p_lqd_if_dflt_pess %>% round(digits = 2)

val_if_lqd <- read_excel('./data/calc_npv_data/liquidation_probability.xlsx', sheet='lqd_val')  %>% as.data.frame()
lqd_val_mult_base <- val_if_lqd$lqd_val_mult_base
lqd_val_mult_opt <- val_if_lqd$lqd_val_mult_opt
lqd_val_mult_pess <- val_if_lqd$lqd_val_mult_pess


##################Function to get value if Default##################
calc_npv_dflt <- function(pays_vec, upb_vec, prepay_vec , disc_vec, mtmval, recovery='baseline', prin_red_only_amt=0){
  # Calculate the value to investor if mortgage defaults - assuming default happens immediatley
  #
  # Args:
  #   pays_vec:         Vector of expected annual payments
  #   upb_vec:          Vector of expected unpaid balances
  #   prepay_vec:       Vector of prepayment probability in each period
  #   disc_vec:         Vector of discount rates
  #   mtmval:           Mark-to-market value of property
  #   recovery:         Optimism of recovery in case of default
  #   prin_red_only_amt:If mod was only principal reduction, this is amt of prin. red already received
  #
  # Returns:
  #   Value to investor if default

  #69% of defaults go into liquidation, rest self-cure
  p_lqd_if_dflt <- p_lqd_if_dflt_base
  p_cure_if_dflt <- 1 - p_lqd_if_dflt_base
  #Investor losess 56% of upb_0 if liquidated
  lqd_val_mult <- lqd_val_mult_base


  #High liqudation rate (all defaults go into liquidation)
  if(recovery=='hi_lqd'){
    p_lqd_if_dflt <- 1.0
    p_cure_if_dflt <- 0.0
  }

  #Optimistic recovery
  if(recovery=='optimistic'){
    p_lqd_if_dflt <- p_lqd_if_dflt_opt
    p_cure_if_dflt <- 1 - p_lqd_if_dflt_opt
    lqd_val_mult <- lqd_val_mult_opt
  }

  #Pessimistic recovery
  if(recovery=='pessimistic'){
    p_lqd_if_dflt <- p_lqd_if_dflt_pess
    p_cure_if_dflt <- 1 - p_lqd_if_dflt_pess
    lqd_val_mult <- lqd_val_mult_pess
  }

  #GSE Mortgages (principal reduction only)
  if(recovery=='herkenhoff'){
    p_lqd_if_dflt <- 0.5
    p_cure_if_dflt <- 1 - 0.5
  }

  #value if cure
  npv_cure <- calc_npv_cure(pays_vec, upb_vec, prepay_vec , disc_vec)

  #Value if default
  val_dflt <- (p_lqd_if_dflt * lqd_val_mult * (upb_vec[[1]]+ prin_red_only_amt)) +
    (p_cure_if_dflt * npv_cure)

  return(val_dflt)
}

#############Func to calculate Default Rates###############
#### In default_rate_assess.R ###
###########################################################


#############Code calculating prepayment rates ####################
#Define logistic regression model
logistic_f <- function(num) {
  return(exp(num)/(1 + exp(num)))
}

#Define coefficients of the model
beta <- list(intercept = -1.9662,
             inct = c(0.9148, 0.1083, -0.1567, -0.2999, -0.0871, -0.067, -0.0352, -0.0993, 0.00414),
             #hpa = c(16.6011, -5.5936, 26.5244, -0.2564, 10.2817, -4.0629),
             hpa = rep(0, 6),
             mtmltv = c(-0.0207, -0.0416, -0.0454, -0.0627, -0.0912, -0.0859, -0.0114),
             #fico = c(0.000252, 0.00104, 0.00668, 0.00149),
             fico = rep(0, 4),
             amt = c(0.0109, 0.00523, 0.000974, -0.00101, -0.00198))

#Define variables to extract and create matrix
x_vars <- c("intercept",
            "inct1","inct2","inct3","inct4","inct5","inct6","inct7","inct8","inct9",
            "hpa1","hpa2","hpa3","hpa4","hpa5","hpa6",
            "mtmltv1","mtmltv2","mtmltv3","mtmltv4","mtmltv5","mtmltv6","mtmltv7",
            "fico1","fico2","fico3","fico4",
            "amt1","amt2","amt3","amt4","amt5")

calc_prepayment <- function(upb, mtmval) {

  mtmval_evolve <- rep(0, 40)
  mtmval_evolve[1] <- mtmval

  for (i in 2:40) {
    mtmval_evolve[i] <- mtmval_evolve[i-1]*1.03
  }

  #Incentive simplified
  df <- data.frame(inct=(0.0049)*upb/upb[1],
                   hpa=0,
                   mtmltv=upb/mtmval_evolve*100,
                   fico=0,
                   orig_amt=upb[1])

  #Min/max definition from page 49
  df <- df %>%
    mutate(inct=pmax(-5, pmin(3, inct)),
           hpa=pmax(-0.5, pmin(0.5, hpa)),
           mtmltv=pmax(40, pmin(180, mtmltv)),
           fico=pmax(400, pmin(800, fico)),
           amt=orig_amt/1000,
           amt=pmax(50, pmin(500, amt)))

  #Define variables feeding into the model, read page 67
  df <- df %>%
    mutate(intercept=1,
           inct1=pmin(-1.5, inct),
           inct2=pmax(-1.5, pmin(-1, inct))-(-1.5),
           inct3=pmax(-1, pmin(0, inct))-(-1),
           inct4=pmax(0, pmin(0.5, inct))-0,
           inct5=pmax(0.5, pmin(1, inct))-0.5,
           inct6=pmax(1, pmin(1.5, inct))-1,
           inct7=pmax(1.5, pmin(2, inct))-1.5,
           inct8=pmax(2, pmin(2.5, inct))-2,
           inct9=pmax(2.5, inct)-2.5,
           hpa1=pmin(-0.08, hpa),
           hpa2=pmax(-0.08, pmin(-0.04, hpa))-(-0.08),
           hpa3=pmax(-0.04, pmin(0, hpa))-(-0.04),
           hpa4=pmax(0, pmin(0.05, hpa))-0,
           hpa5=pmax(0.05, pmin(0.1, hpa))-0.05,
           hpa6=pmax(0.1, hpa)-0.1,
           mtmltv1=pmin(50, mtmltv),
           mtmltv2=pmax(50, pmin(70, mtmltv))-50,
           mtmltv3=pmax(70, pmin(80, mtmltv))-70,
           mtmltv4=pmax(80, pmin(90, mtmltv))-80,
           mtmltv5=pmax(90, pmin(100, mtmltv))-90,
           mtmltv6=pmax(100, pmin(110, mtmltv))-100,
           mtmltv7=pmax(110, mtmltv)-110,
           fico1=pmin(640, fico),
           fico2=pmax(640, pmin(700, fico))-640,
           fico3=pmax(700, pmin(760, fico))-700,
           fico4=pmax(760, fico)-760,
           amt1=pmin(80, amt),
           amt2=pmax(80, pmin(140, amt))-80,
           amt3=pmax(140, pmin(220, amt))-140,
           amt4=pmax(220, pmin(300, amt))-220,
           amt5=pmax(300, amt)-300)

  #Calculate prepayment rates for 40 years
  x_mat <- as.matrix(df[,x_vars])

  prepayment_rate <- logistic_f(x_mat %*% unlist(beta))

  return(prepayment_rate)
}



######### Function to calculate NPV to investor ###########
calc_npv_invest <- function(npv_cure, npv_dflt, p_dflt){
  #Helper func: Calculates npv to investor from npv_cure, npv_dflt, p_dflt
  p_cure <-  1-p_dflt
  return(p_cure * npv_cure +  p_dflt * npv_dflt)
}

calc_npv_frm_terms <- function(base_terms, assumps, prin_red_only_amt=0, verbose=FALSE){
  #Calculates npv to investor from mortgage terms and assumptions
  # Args:
  #   base_terms:         list of r_temp, r_perm, term, upb_0, forbear
  #
  #   assumps: list of the following:
  #     mtmval:           mark-to-market value of property
  #     prepay_vec:       vector of prepayment probability for each of 40 years
  #     disc_vec:         vector of discount factors (<1) for each of 40 years
  #     p_dflt:           probability of default within first 5 years
  #
  #   prin_red_only_amt:  Amt of principal reduction already received.
  #                       If mod is prin. red. only, accounts for fact that prin. red.
  #                       doesn't reduce value|default
  #   verbose:            If TRUE, returns npv_if_cure, npv_if_default, p_dflt, as well
  #
  # Returns:
  #   npv to investor

  pmt_stream_base <- pmt_stream(base_terms)
  pays <- pmt_stream_base$pay
  upbs <- pmt_stream_base$upb

  prepay_vec <- assumps$prepay_vec
  disc_vec <- assumps$disc_vec
  mtmval <- assumps$mtmval
  p_dflt <- assumps$p_dflt
  recovery <- assumps$recovery

  npv_if_cure <- calc_npv_cure(pays, upbs, prepay_vec, disc_vec)
  npv_if_dflt <- calc_npv_dflt(pays, upbs, prepay_vec, disc_vec, mtmval, recovery = recovery, prin_red_only_amt = prin_red_only_amt)
  npv <- calc_npv_invest(npv_if_cure, npv_if_dflt, p_dflt)
  if(verbose==FALSE){
    return(npv)
  }else{
    out = list(npv, p_dflt, npv_if_cure,npv_if_dflt)
    names(out) = c("npv", "p_dflt", "npv_cure", "npv_dflt")
    return(out)
  }

}

############### Function to calculate d_npv for a given modification ##################
calc_cost_mod <- function(mod_terms, base_terms, assumps, dynamic=TRUE, recovery_mod = 'baseline', recovery_nomod = 'baseline'){
  #Function to calculate the change in investor npv for a modification
  #
  # Args:
  #   mod_terms: Terms of modified mortgage; list of r_temp, r_perm, term, upb_0, forbear
  #   base_terms: Terms of baseline mortgage
  #   assumps: List of mtmval, discounting vector, baseline(pre-treatment) prepay vector,
  #            baseline(pre-treatment) prob. default, and recovery scenario
  #   recovery_mod: Recovery scenario, for modded mortgages
  #   recovery_nomod: Recovery scenario, for unmodded mortgages
  #   dynamic: Do prepayment and default change in response to modification
  # Returns:
  #   NPV cost of modification to investor

  assumps_init <- assumps
  assumps_mod <- assumps

  assumps_init$recovery <- recovery_nomod
  assumps_mod$recovery <- recovery_mod

  if(dynamic==TRUE){
    #What is 5-year default probability after modification?
    pmt_stream_base <- pmt_stream(base_terms)
    pmt_stream_mod <- pmt_stream(mod_terms)
    pmt_reduc_pct <- 100 * -(pmt_stream_mod$pay[[1]] - pmt_stream_base$pay[[1]]) / pmt_stream_base$pay[[1]]

    assumps_mod$p_dflt <- calc_mod_dflt(pmt_reduc_pct)

    #What is prepayment after modification
    assumps_mod$prepay_vec <- calc_prepayment(pmt_stream_mod$upb, assumps_mod$mtmval)
  }
  #Incorporate the yield curve into discounting
  disc_vec <- rep(1/(1+disc_rates[max(30, min(mod_terms$term, 40))-29]), 40)
  assumps_mod$disc_vec <- disc_vec


  npv_init <- calc_npv_frm_terms(base_terms, assumps_init)
  npv_mod <- calc_npv_frm_terms(mod_terms, assumps_mod)
  return(-(npv_mod - npv_init))
}

npv_cost <- function(pmt_target, waterfall, base_terms, assumps, dynamic=TRUE, recovery_mod = 'baseline', recovery_nomod = 'baseline'){
  #Wrapper function - gets cost of achieving target pay_red using a certain waterfall
  mod_terms <- wfall_mod_terms(pmt_target, waterfall, base_terms, mtmval = assumps$mtmval, min_r_perm = disc_rate_30)
  cost_npv <- calc_cost_mod(mod_terms, base_terms, assumps, dynamic=dynamic, recovery_mod = recovery_mod, recovery_nomod = recovery_nomod)
  return(cost_npv)
}

######### Function to Calculate different treatment effects ##########
npv_treat_diff <- function(delta,
                           lhs_pre_args, lhs_post_args,
                           rhs_pre_args, rhs_post_args,
                           recovery, find_delta=FALSE){

  #Function to calculate NPV to investor in each scenario
  calc_investor_vals <- function(arglist){
    npv_cure <- calc_npv_cure(arglist$pays_vec, arglist$upb_vec,
                              arglist$prepay_vec, arglist$disc_vec)
    npv_dflt <- calc_npv_dflt(arglist$pays_vec, arglist$upb_vec,
                              arglist$prepay_vec, arglist$disc_vec,
                              arglist$mtmval, recovery=recovery)
    p_dflt <- arglist$p_dflt

    print(calc_npv_invest(npv_cure, npv_dflt, p_dflt))
    return(calc_npv_invest(npv_cure, npv_dflt, p_dflt))
  }

  #List of scenarios
  scenarios <- list('lhs_pre'=lhs_pre_args, 'lhs_post'=lhs_post_args,
                    'rhs_pre'=rhs_pre_args, 'rhs_post'=rhs_post_args)

  #Shared vector of discount factors if finding breakeven delta
  if (find_delta == TRUE){
    for(arglist in names(scenarios)){
      print(typeof(arglist))
      scenarios[[arglist]]$disc_vec <- rep(1/(1+delta), 40)
    }
  }

  #Calculate npvs for each scenario, return difference in NPVs
  npvs <- lapply(scenarios, calc_investor_vals)
  treat_diff <- (npvs$lhs_post - npvs$lhs_pre) - (npvs$rhs_post - npvs$rhs_pre)

  if (find_delta == TRUE){
    return(treat_diff)
  }
  else{
    return(npvs)
  }

}

## Omnibus function to calculate d(payment) for a given mod #####
dpmt <- function(amt_mod, base_terms, mod_type, limit){
  #Computes the change in monthly payments for a maturity extension
  #
  # Args:
  #   amt_mod: years to extend mortgage / xxx / xxx
  #   base_terms: list of r_temp, r_perm, term, upb_0, forbear
  #   max_limit: maximum mortgage term
  # Returns:
  #     dpmt: change in monthly payments in dollars

  #Get modified terms
  base_terms <- base_terms
  mod_terms <- base_terms

  if(mod_type == 'mat_extend'){
    if(amt_mod + base_terms$term >limit){
      warning(sprintf('New maturity greater than %d years. Setting to %d', limit, limit))
      amt_mod <- limit - base_terms$term
    }
    mod_terms$term <- mod_terms$term + amt_mod
  }

  if(mod_type == 'r_temp'){
    if(base_terms$r_temp - amt_mod  < limit ){
      warning(sprintf('New r_first_five less than %.02f%%. Setting to %.02f%%', limit, limit))
    }
    mod_terms$r_temp <- max(limit, mod_terms$r_temp - amt_mod)
  }

  if(mod_type == 'r_perm'){
    if(base_terms$r_temp - amt_mod  < limit ){
      warning(sprintf('New r_first_five is less than %.02f %%. Setting to %.02f', limit, limit))
    }
    if(base_terms$r_perm - amt_mod  < limit ){
      warning(sprintf('New r_perm is less than %.02f %%. Setting to %.02f', limit, limit))
    }
    mod_terms$r_temp <- max(limit, mod_terms$r_temp - amt_mod)
    mod_terms$r_perm <- max(limit, mod_terms$r_perm - amt_mod)
  }


  if(mod_type == 'forgive'){
    if(amt_mod  > limit ){
      warning(sprintf('Forgiveness greater than %d dollars. Setting to %d dollars', limit, limit))
      amt_mod <- limit
    }
    mod_terms$upb_0 <- mod_terms$upb_0 - amt_mod
  }

  if(mod_type == 'forbear'){
    if(amt_mod  > limit ){
      warning(sprintf('Forbearance greater than %d dollars. Setting to %d dollars', limit, limit))
      amt_mod <- limit
    }
    mod_terms$forbear <- amt_mod
  }

  #Calculate pmt streams for base and mod
  pmt_stream_base <- pmt_stream(base_terms)
  pmt_stream_mod <- pmt_stream(mod_terms)
  dpmt <- pmt_stream_base$pay[1] - pmt_stream_mod$pay[1]

  return(dpmt)
}

##### Function to generate stream of payments and UPB from mortgage terms #######
pmt_stream <- function(base_term) {
  # Calculates payments and upbs for a mortgage given terms
  #
  # Args:
  #   base_term: List of:
  #     r_temp: interest rate for first 5 years; fraction
  #     r_perm: interest rate for rest of mortgage; fraction,
  #     term: length of mortgage in years; integer
  #     upb_0: initial unpaid balance in dollars
  #     forbear: amount of forbearance in dollars
  #
  # Returns:
  #   list of:
  #     pay: length 40 stream of payments with padded zeros if necessary
  #     upb: length 40 stream of upb with padded zeros if necessary
  #
  #
  schedule <- data.frame(yrsPaid=c(1:base_term$term),
                         yrsLeft=c(base_term$term:1),
                         r=NA, pay=NA,
                         int_pmt_amt=NA, princ_pmt_amt=NA,
                         upb_beg=NA, upb_end=NA) %>%
    #Set first UPB value from base_term (subtract forbearance amount)
    mutate(upb_beg=ifelse(yrsPaid==1, base_term$upb_0-base_term$forbear, upb_beg))

  #Define a vector of interest rates
  for (year in c(1:min(base_term$term, 5))) {
    schedule$r[year] <- base_term$r_temp
  }
  #Increase by 1% until it reaches r_perm
  if (base_term$term > 5) {
    for (year in c(6:base_term$term)) {
      schedule$r[year] <- min(schedule$r[year-1]+0.01, base_term$r_perm)
    }
  }

  #Fill up dataframe year by year
  pay_ann <- function(upb, term, r){ #annuity formula for fixed rate, annual term
    return((r*upb)/(1-((1+r)^-term)))
  }

  for(i in 1:base_term$term){
    row <- schedule[i,]

    row$int_pmt_amt <- row$r*row$upb_beg
    row$pay <- pay_ann(row$upb_beg, row$yrsLeft, row$r)
    row$princ_pmt_amt <- row$pay-row$int_pmt_amt
    row$upb_end <- row$upb_beg - row$princ_pmt_amt
    schedule[i,] <- row

    #Update upb_beg for next row
    if(i<base_term$term){
      schedule[i+1,'upb_beg'] <- row$upb_end
    }
  }


  #Add back forbearance amount and keep only relevant variables
  schedule <- schedule %>%
    mutate(pay=ifelse(yrsPaid==base_term$term, pay+base_term$forbear, pay),
           upb_end=ifelse(yrsPaid!=base_term$term, upb_end+base_term$forbear, upb_end)) %>%
    select(pay, upb_end)

  #Pad extra zeros
  if (nrow(schedule) < 40) {
    add_years <- 40-nrow(schedule)
    pad_zeros <- rep(0, add_years)
    schedule_pad <- data.frame(pay=pad_zeros, upb_end=pad_zeros)

    schedule <-  bind_rows(schedule, schedule_pad)
  }

  payment <- list('pay'=schedule$pay,
                  'upb'=schedule$upb_end)

  return(payment)
}

############Function that outputs modified mortgage contract (waterfall) #############
wfall_mod_terms <- function(pmt_target, waterfall, base_terms,
                            mtmval, min_r_perm) {

  #Helper functions so that I can allow any ordering of waterfall sequence
  mod_forgive <- function(mod_terms) {
    contract <- mod_terms$contract
    max_limit_forgive <- contract$upb_0-1.15*mtmval

    #For Principal Forgiveness Waterfall, we set no limit on forgiveness
    if (waterfall == "Principal Forgiveness") {
      max_limit_forgive <- contract$upb_0
    }

    #Forgiveness
    if (dpmt(max_limit_forgive, contract, mod_type='forgive', limit = max_limit_forgive) < mod_terms$pay_left) {
      amt_mod <- max_limit_forgive

      if (waterfall == "Principal Forgiveness") {
        contract$upb_0 <- 0
      }
      else {
        contract$upb_0 <- 1.15*mtmval
      }
    }

    else {
      if (waterfall == "Principal Forgiveness") {
        amt_mod <- uniroot(function(amt_mod, base_terms) dpmt(amt_mod, base_terms, mod_type='forgive', limit =max_limit_forgive )-mod_terms$pay_left,
                           c(0, contract$upb_0),
                           extendInt="no",
                           base_terms=contract)$root
        contract$upb_0 <- contract$upb_0-amt_mod
      }
      else {
        amt_mod <- uniroot(function(amt_mod, base_terms) dpmt(amt_mod, base_terms, mod_type='forgive', limit =max_limit_forgive )-mod_terms$pay_left,
                           c(0, contract$upb_0-1.15*mtmval),
                           extendInt="no",
                           base_terms=contract)$root
        contract$upb_0 <- contract$upb_0-amt_mod
      }
    }

    pay_left <- mod_terms$pay_left-dpmt(amt_mod, mod_terms$contract, mod_type='forgive', limit = max_limit_forgive)
    mod_terms <- list(contract=contract, pay_left=pay_left)
    return(mod_terms)
  }

  #Permanent Rate Reduction
  mod_r_perm <- function(mod_terms) {
    contract <- mod_terms$contract
    max_limit_r_perm <- contract$r_perm-min_r_perm

    if (dpmt(max_limit_r_perm, contract, mod_type = 'r_perm', limit =min_r_perm ) < mod_terms$pay_left) {
      amt_mod <- max_limit_r_perm
      contract$r_perm <- min_r_perm
      contract$r_temp <- max(0.02, contract$r_temp-max_limit_r_perm)
    }

    else {
      amt_mod <- uniroot(function(amt_mod, base_terms) dpmt(amt_mod, base_terms, mod_type='r_perm', limit = min_r_perm)-mod_terms$pay_left,
                         c(0, contract$r_perm-min_r_perm),
                         extendInt="no",
                         base_terms=contract)$root
      contract$r_perm <- max(min_r_perm, contract$r_perm-amt_mod)
      contract$r_temp <- max(0.02, contract$r_temp-amt_mod)
    }

    pay_left <- mod_terms$pay_left-dpmt(amt_mod, mod_terms$contract, mod_type='r_perm', limit=min_r_perm)
    mod_terms <- list(contract=contract, pay_left=pay_left)
    return(mod_terms)
  }

  #Temporary Rate Reduction
  mod_r_temp <- function(mod_terms) {
    contract <- mod_terms$contract
    max_limit_r_temp <- contract$r_temp-0.02

    if (dpmt(max_limit_r_temp, contract, mod_type = 'r_temp', limit = 0.02) < mod_terms$pay_left) {
      amt_mod <- max_limit_r_temp
      contract$r_temp <- 0.02
    }

    else {
      amt_mod <- uniroot(function(amt_mod, base_terms) dpmt(amt_mod, base_terms, mod_type='r_temp', limit=0.02)-mod_terms$pay_left,
                         c(0, contract$r_temp-0.02),
                         extendInt="no",
                         base_terms=contract)$root
      contract$r_temp <- contract$r_temp-amt_mod
    }

    pay_left <- mod_terms$pay_left-dpmt(amt_mod, mod_terms$contract, mod_type = 'r_temp', limit=0.02)
    mod_terms <- list(contract=contract, pay_left=pay_left)
    return(mod_terms)
  }

  #Maturity Extension
  mod_mat_extend <- function(mod_terms) {
    contract <- mod_terms$contract
    max_limit_mat_extend <- 40-contract$term

    if (dpmt(max_limit_mat_extend, contract, mod_type = 'mat_extend', limit=40) < mod_terms$pay_left) {
      amt_mod <- max_limit_mat_extend
      contract$term <- 40
    }

    else {
      amt_mod <- ceiling(uniroot(function(amt_mod, base_terms) dpmt(amt_mod, base_terms, mod_type='mat_extend', limit =40)-mod_terms$pay_left,
                                 c(0, 40-contract$term),
                                 extendInt="no",
                                 base_terms=contract)$root)
      contract$term <- contract$term+amt_mod
    }

    pay_left <- mod_terms$pay_left-dpmt(amt_mod, mod_terms$contract, mod_type = 'mat_extend', limit=40)
    mod_terms <- list(contract=contract, pay_left=pay_left)
    return(mod_terms)
  }

  #Forbearance
  mod_forbear <- function(mod_terms) {
    contract <- mod_terms$contract
    max_limit_forbear <- contract$upb_0

    if (dpmt(max_limit_forbear, contract, mod_type='forbear', limit =max_limit_forbear) < mod_terms$pay_left) {
      amt_mod <- max_limit_forbear
      contract$forbear <- max_limit_forbear
    }

    else {
      amt_mod <- uniroot(function(amt_mod, base_terms) dpmt(amt_mod, base_terms, mod_type='forbear', limit=max_limit_forbear)-mod_terms$pay_left,
                         c(0, max_limit_forbear),
                         extendInt="no",
                         base_terms=contract)$root
      contract$forbear <- amt_mod
    }

    pay_left <- mod_terms$pay_left-dpmt(amt_mod, mod_terms$contract, mod_type ='forbear', limit = max_limit_forbear)
    mod_terms <- list(contract=contract, pay_left=pay_left)
    return(mod_terms)
  }

  mod_set_principal_reduction <- function(mod_terms){
    contract <- mod_terms$contract
    amt_mod <- prin_red_rd
    max_limit_forgive <- amt_mod
    contract$upb_0 <- contract$upb_0 - amt_mod
    pay_left <- mod_terms$pay_left-dpmt(amt_mod, mod_terms$contract, mod_type='forgive', limit = max_limit_forgive)
    mod_terms <- list(contract=contract, pay_left=pay_left)

    warning("Using global envirinment value of prin_red_rd")

    return(mod_terms)
  }

  #Set a switch
  switch_waterfall <- function(base_terms, type) {
    switch(type,
           forgive = mod_forgive(base_terms),
           r_perm = mod_r_perm(base_terms),
           r_temp = mod_r_temp(base_terms),
           mat_extend = mod_mat_extend(base_terms),
           forbear = mod_forbear(base_terms),
           set_forgive = mod_set_principal_reduction(base_terms))
  }

  #Define the sequence of waterfall
  if (waterfall == "HAMP Principal Reduction Alternative") {
    seq_waterfall <- c("forgive", "r_perm", "r_temp", "mat_extend", "forbear")
  }
  if (waterfall == "HAMP") {
    seq_waterfall <- c("r_perm", "r_temp", "mat_extend", "forbear")
  }
  if (waterfall == "Private Chase") {
    seq_waterfall <- c("mat_extend", "r_perm", "r_temp", "forbear")
  }
  if (waterfall == "Private GSE") {
    seq_waterfall <- c("mat_extend", "r_perm", "r_temp", "forbear") #Same as CHAMP for now
  }
  if (waterfall == "Efficient Default-minimizing") {
    seq_waterfall <- c("mat_extend", "r_temp", "forbear")
  }
  if (waterfall == "Principal Forgiveness") {
    seq_waterfall <- c("forgive")
  }
  if (waterfall == "HAMP PRA set forgiveness") {
    seq_waterfall <- c("set_forgive", "r_perm", "r_temp", "mat_extend", "forbear")
  }

  #Calculate pmt streams for base and mod
  pmt_stream_base <- pmt_stream(base_terms)
  pmt_target_dollar <- pmt_target*pmt_stream_base$pay[1]

  mod_terms <- list(contract=base_terms, pay_left=pmt_target_dollar)

  #Loop through waterfall
  for (seq in seq_waterfall) {
    #Stop if payment left is > 5 (5 to avoid rounding error)
    if (mod_terms$pay_left > 5) {
      mod_terms <- switch_waterfall(mod_terms, seq)
    }
  }

  return(mod_terms$contract)
}
