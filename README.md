# Ganong and Noel "Liquidity vs. Wealth in Household Debt Obligations: Evidence from Housing Policy in the Great Recession" (2020)

Please send feedback and questions to ganong@uchicago.edu.

This repo is a partial replication kit for Ganong and Noel (2020). This README has three parts:
1. Partial repkit of empirical analysis of principal reduction (Sections 3 and 4)
1. Full repkit for model analysis of principal reduction (Section 6.2)
1. Table and variable manifest for JPMCI data (Section 5, various appendix items)

It will be archived as openicpsr 118401

---
# Part 1: `empirics` directory

We are able to release the code for this section, but not the proprietary Transunion credit bureau data. We are glad to work with anyone who seeks to access these data.

This section of the analysis uses code (that is public and included in this repo) and proprietary Transunion credit bureau data (that is not public and therefore cannot be included in this repo). The Transunion file is stored on a Harvard server. To obtain permission from Transuion to access these data, email Erikka.Leighton@transunion.com. We grant any researchers with appropriate approval to conduct research using these files. We describe the code below.

`master.R` is the master file for the code. The `environment` argument in the first line may be specified as:

1. 'Odyssey' - Runs the analysis requiring proprietary data on the Odyssey server 
2. 'Local' - Runs the NPV comparison analysis on the local machine

## Odyssey

- `build/01_load_Npv`. `build/02_load_1stLien.R`, `build/03_clean_Npv.R`, and `build/04_clean_1stLien.R` load and clean HAMP NPV and loan modification file from [the U.S. Department of Treansury](https://www.treasury.gov/initiatives/financial-stability/reports/Pages/mha_publicfile.aspx).
- `build/05_make_hamp_matching_file.R` creates the HAMP file that will later be matched to the TransUnion data.
- `analyze/01_analyze_takeup.R` analyzes modification takeup status.
- `analyze/02_set_sample_nonmissing_dnpv.R` and `analyze/03_calcModNpv_event_study.R` calculates NPV cashflow for non-GSE loans with no missing running variable.
- `build/06_load_tu.R` loads the TransUnion header segment, trade segment, and trade history segment, and save them in `.rds` format.
- `build/07_make_tu_tradefiles.R` appends all the TransUnion trade segment.
- `build/08_make_tu_mortgage_file.R` creates a mortgage panel dataset for loans modified under federal government plan from the TransUnion record.
- `build/09_build_foreclosure_outcome.R` calculates foreclosure information from the mortgage file.
- `build/10_make_tu_matching_file.R` creates the TransUnion file will be used to match with HAMP data using the mortgage file.
- `build/11_make_tu_creditcard_file.R` and `build/12_make_tu_auto_file.R` create a credit card spending and auto spending panel dataset from the TransUnion record.
- `build/13_compare_hamp_tu.R` compares the HAMP and TU match file and cleans both datasets to get ready for matching.
- `build/14_match_hamp_tu.R` matches loans between the HAMP public files and TransUnion using Euclidean distance.
- `build/15_clean_matched_hamp.R` further cleans the matched HAMP TU file.
- `build/16_build_event_study_dataset.R` merges the credit card file and the matched HAMP TU file to create the consumption event study base sample.
- `analyze/04_set_sample_es_base.R` and `analyze/03_calcModNpv.R` calcualtes NPV cashflow for the consumption event study base sample and plots payment change.
- `analyze/05_analyze_event_study_cc.R` analyzes the impact of principal reduction on credit card expenditure.
- `analyze/06_analyze_event_study_auto_part1.R` and `analyze/07_analyze_event_study_auto_part2.R` analyzes the impact of principal reduction on auto expenditure.
- `build/17_build_rd_dataset.R` creates the regression discontinuity sample and plots the histograms of the running variable.
- `build/18_rd_preamble.R`, `build/19_rd_last_steps.R`, and `build/20_npv_model.R` further clean the RD sample to prepare the data for RD analysis.
- `analyze/08_rd_bw.R` calculates the optimal bandwidth for the RD analysis.
- `analyze/09_rd_delin.R` conducts RD analysis to examine the impact of principal reduction on default.
- `build/21_rd_cons_last_steps.R` and `analyze/10_rd_cons.R` analyzes the credit card and auto spending change around principal reduction discontinuity.
- `analyze/11_representativeness.R` conducts representativeness analysis on principal reduction RD sample, payment reduction RD sample, the matched HAMP TU sample, and the difference-in-difference sample. 
- `analyze/12_rd_data_quality.R` plots HAMP TU match rate and HAMP modification take-up rate around principal reduction discontinuity.
- `analyze/13_plot_external_data_figures.R` plots mortgage delinquency, mortgage originations, and CLTV for HELOCs using external data.
- `build/build_rd_unormalized.R` creates the regression discontinuity sample without normalization of the running variable
- `analyze/rd_unnormalized_running_var.R` conducts the RD analysis without normalization of the running variable

## Local

This project uses R package `versions` to reproduce results. By default, the `snapshot_pkgs_local` toggle at the top of the file is set to `FALSE`. If set to `TRUE` (and `environment == 'Local'`), the script will attempt to install the required versions of packages into the first element of .libPaths().

To view the packages versions used or to change the install location, modify the `install_dates()` call near the bottom of `master.R`

- `analyze/14_maturity_extension_npv.Rmd` analyzes the impact on investors from assigning a mortgage to the left-hand side of the 31 percent payment-to-income discontinuity.
- `analyze/15_npv_cost_benefit.Rmd` conducts NPV cost-benefit analysis for different modification types.
- `analyze/16_maturity_extension_default.R` estimates the long-term default rate arising from maturity extension.


# Part 2: `model` directory

We are able to release a full repkit for this section.

The model subdirectory is forked from Heterogeneous Agents Resources and toolKit (HARK) beta release - June 25, 2016.

All new scripts by Ganong and Noel can be found in the `/gn/` subdirectory. 

## Scripts 
1. `gn/cons_model.py` - Implements a partial equilibrium life-cycle model with optimizing households to predict consumption in the presence of housing debt and collateral constraints. Can be run from terminal or in IDE. See Appendix D for details.
1. `gn/default_model.py` - Uses the above model to predict default decisions. Can be run from terminal or in IDE. See Appendix D.4 for details.

## Other Files and Directories

* `out` - Plot output from code files.
* `params` - Model parameters.

## Replication Environments

- Use `HAMP_201904_env.yml` to set up replication environment using Anaconda. To do so, use  the terminal or an Anaconda Prompt and run:  

```
 conda env create -f HAMP_201904_env.yml      
 conda activate HAMP_201904
 ```

 For more details, see the [conda documentation](https://docs.conda.io/projects/conda/en/latest/user-guide/tasks/manage-environments.html#creating-an-environment-from-an-environment-yml-file). WARNING: This replication environment contains a lot of the base conda packages; it might be very large if you do not already have Anaconda.

 - Alternatively, use the `requirements.txt` file and `pip` and `virtualenv` for a more lightweight option. To do so, install the `virtualenv` package then run:

 ```
virtualenv hamp_env
source hamp_env/bin/activate
pip install -r requirements.txt
 ```

# Part 3: JPMCI table and variable manifest

The code and data used in this section were prepared in JPMorganChase Insitute's (JPMCI) secure computing facilities. Due to JPMCI's rules on access and confidentiality, the programming code and analysis files cannot be made available publicly. The analysis files and programming code created by the authors will be available within JPMCI's secure computing facilities until 2022, and can be requested by researchers with approved projects (email institute@jpmchase.com). We grant any researchers with appropriate approval to conduct research on JPMCI's secure computing facilities access to these files. Below, we list the tables needed to replicate the analysis as well as some key variables.

## Effect of payment reduction on default (Section 5)

### Script sequence by `master_payred.R`
1. `prelim_payred.R` loads required parameters, libraries and functions for payment reduction section.
1. `build_rd_ac.R` loads HDFS tables from csvs and filters sample.
1. The following scripts are run twice (Chase sample and GSE sample)
    1. `build_npv.R` constructs NPV calculations under payment reduction.
    1. `an_bw.R` estimates ideal RD bandwiths using several methods.
    1. `an_rd.R` constructs main RD graphs.
    1. `an_npv.R` constructs statistics related to NPV change.
1. `an_rd_chase.R` constructs additional RD graphs for Chase sample and representativeness table.
1. `an_npv_chase.R` constructs NPV graphs for Chase sample.

### Archived tables
The results can be reproduced using the following tables:

* `institute_consumer.ganong_mtgmods_aer_need_res_dir_approval_to_delete_pay_delinq`
* `institute_consumer.ganong_mtgmods_aer_need_res_dir_approval_to_delete_pay_mbrisk` 

##### Key variables
1. `pay_delinq`
   * mtg_term_delq_status
2. `pay_mbrisk`
   * mod_pgm_type_cd
   * ivst_cd
   * pri_loan_hs_dti_rt
   * cplt_loan_prin_int_am
   * pri_loan_prin_int_am
   * cplt_loan_amz_term_cn
   * pri_loan_amz_term_cn
   * cplt_loan_rt
   * pri_loan_rt
   * curr_prop_aprs_val_am
   * prin_frgv_am
   * prin_dfr_am
   * pri_upb_am
   * premod_arm_in
   

## Effect of Principal Reduction on Consumption (Appendix Figure 10 and Appendix Table 3)

### Script sequence by `master_prinred.R`
1. `prelim_prinred.R` loads required parameters, libraries and functions.
1. `build_intellistor.R` pulls mortgage servicing, mortgage modification, and credit card spending data from HDFS csvs.
1. `build_sample.R` constructs the analysis sample.
1. `an_academic.R` creates Appendix Figure 10 and Appendix Table 3.

### Archived tables
The results can be reproduced using the following tables:

* `institute_consumer.ganong_mtgmods_aer_need_res_dir_approval_to_delete_prin_svcg` 
* `institute_consumer.ganong_mtgmods_aer_need_res_dir_approval_to_delete_prin_cons`

##### Key variables
1. `prin_cons`
   * cc_spend 
   * cc_payment  
   * per_chk_eom_balance
   * age
   * cbsa_cd
2. `prin_svcg`
   * mod_pgm_type_cd
   * ivst_cd
   * pri_loan_hs_dti_rt
   * cplt_loan_hs_dti_rt
   * cplt_loan_prin_int_am
   * pri_loan_prin_int_am
   * mtg_term_delq_status
   * eff_cltv_rt
   * prin_frgv_am
   * orgn_prop_val_am
   * unpd_prin_bal_am
   * prop_valn_am
   * cplt_loan_brw_incm_am
   * curr_prop_aprs_val_am
