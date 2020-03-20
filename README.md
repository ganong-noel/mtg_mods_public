# mtg_mods_public
Repkit for Liquidity vs. Wealth in Household Debt Obligations: Evidence from Housing Policy in the Great Recession.


### Script sequence by `master_prinred.R`
1. `prelim_prinred.R` loads required parameters, libraries and functions.
1. `build_intellistor.R` pulls mortgage servicing, mortgage modification, and credit card spending data from HDFS csvs.
1. `build_sample.R` constructs the analysis sample.
1. `an_academic.R` creates Appendix Figure 10 and Appendix Table 3.

### Archived tables
The results can be reproduced using only the following tables in MTD:

##### build_intellistor.R
* `institute_consumer.ganong_mtgmods_aer_need_res_dir_approval_to_delete_prin_svcg` 
* `institute_consumer.ganong_mtgmods_aer_need_res_dir_approval_to_delete_prin_cons`

(These tables have been saved as csv files, which are loaded in build_intellistor.R)

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


## Effect of payment reduction on default 

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
The results can be reproduced using only the following tables in MTD:

##### build_rd_ac.R
* `institute_consumer.ganong_mtgmods_aer_need_res_dir_approval_to_delete_pay_delinq`
* `institute_consumer.ganong_mtgmods_aer_need_res_dir_approval_to_delete_pay_mbrisk` 

(These tables have been saved as csv files, which are loaded in build_rd_ac.R)

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
   
### Note
We have lost access to the RDRobust package. Therefore, we have commented out the bandwidth analysis in an_bw.R and the quadratic part of the bandwidth robustness graph in an_rd_chase.R
