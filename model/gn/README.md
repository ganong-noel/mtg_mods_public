# Ganong and Noel "Liquidity vs. Wealth in Household Debt Obligations: Evidence from Housing Policy in the Great Recession" (2019) 

By Peter Ganong and Pascal Noel 

Please send feedback and questions to ganong@uchicago.edu.
-------

# Code

## `settings.py` 

 - Preliminary settings; imported to other files.

## `cons_model.py` 

- Implements a partial equilibrium life-cycle model with optimizing households to predict consumption in the presence of housing debt and collateral constraints. Can be run from terminal or in IDE. See Appendix D for details.

## `default_model.py` 

- Uses the above model to predict default decisions. Can be run from terminal or in IDE. See Appendix D.4 for details.

# Other Files and Directories

## `out`

- Plot output from code files.

## `params`

- Model parameters.

# Replication Environments

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




