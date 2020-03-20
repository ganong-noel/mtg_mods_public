# -*- coding: utf-8 -*-
"""
Created on Mon Jun 20 15:55:59 2016

@author: ganong
"""
import os
import settings
import sys 
import pdb
import json
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from copy import deepcopy
from operator import sub, add


out_path = "out/"
plt.rcParams['font.family'] = 'serif'
plt.rc('font', size=10)
plt.rc('legend', fontsize=12) 

sys.path.insert(0,'../')
sys.path.insert(0,'../ConsumptionSaving')
sys.path.insert(0,'../SolvingMicroDSOPs')

from HARKinterpolation import LinearInterp
import ConsumptionSavingModel_gn as Model
import EstimationParameters as Params

mystr = lambda number : "{:.4f}".format(number)
do_simulation           = True

#Read in HAMP parameters 
hamp_params = json.loads(open('./params/hamp_params.json').read())
inc_params = json.loads(open('./params/inc_params.json').read())
rd_params = json.loads(open('./params/rd_params.json').read())
hamp_coh = inc_params['cash_on_hand']

df_ltv  = pd.read_csv('./params/HAMPRA_ltv_dist.csv')


#imports specific to default code
from scipy.optimize import fsolve
def findIntersection(fun1,fun2,x0):
    return fsolve(lambda x : fun1(x) - fun2(x),x0)
from functools import partial

###########################################################################
#set economic parameters 
###########################################################################
rebate_years_until_death = 25
age_of_rebate = 90 - rebate_years_until_death
t_eval = age_of_rebate - 25 - 20
def mpc(cF, rebate = 1, a = 0.1):
    return round((cF(a+rebate) - cF(a)) / rebate,3)
tmp_vlo = Params.IncUnemp


settings.init()
settings.lil_verbose = True
settings.min_age, settings.max_age = 60, 65
###########################################################################
#plotting functions 
###########################################################################
from cons_model import mpl_funcs

###########################################################################
#housing wealth functions
###########################################################################
#xx move to outside script and import in the future. got errors because I was modifying the wrong do file before...
#remark: right now you are actually selling the house one year before retirement rather than at retirement. not sure if this is a problem.
def hsg_wealth(initial_debt, annual_hp_growth, collateral_constraint, baseline_debt, 
               initial_price, int_rate, pra_forgive, hsg_rent_p, hsg_own_p, maint,
               d_house_price = 0, age_at_mod = 45, hsg_pmt_wk_own = True, hsg_pmt_ret_y = False, default = False,
               annual_hp_growth_base = None):
    '''
    Calculates annual mortgage contract using parameters at date of mod.
    Everything is measured in years of income.
    
    Parameters
    ----------
    initial_debt : float
        Debt at date of mod
    annual_hp_growth : float
        Annual house price growth
    collateral_constraint : float
        Fraction of house needed for collateral (can borrow against rest)
    baseline_debt : float
        parameter from HAMP spreadsheet. Unused currently.        
    initial_price : float
        Value of house at origination
    int_rate: float
        Interest rate on liquid assets
    pra_forgive: float
        Amount of debt forgiven by PRA
    hsg_rental_rate : float
        int_rate minus annual_hp_growth
    age_at_mod : float
        Age at date of origination
                    
    Returns
    -------
    sale_proceeds : float
        Amount rebated at age 65 when house sold
    equity : list
        Value of house minus debt at each date (can be negative) 
    limit : list 
        Borrowing limit each period max{(1-collateral)*price - debt,0}
    mtg_pmt : list
        Annual mortgage payment from origination to age 65
    '''
    if initial_debt < 0:
        print("Error: cannot have negative mortgage")
        return
    price = [initial_price + d_house_price]
    if settings.verbose:
        print "Hsg wealth params: P=", price, " D=", baseline_debt, " g=", annual_hp_growth, " r=", int_rate, " phi=", collateral_constraint, " owner cost while work:", hsg_pmt_wk_own , " share inc while ret: ", hsg_pmt_ret_y
    T = 65 - age_at_mod
    
    #working life housing payments and collateral    
    debt = [initial_debt]
    amort = int_rate*(1+int_rate)**30/((1+int_rate)**30-1)
    hsg_pmt = [initial_debt*amort]
    maint_pmt = [initial_price*maint]
    for i in range(1,T):
        #print "age: " + str(i + age_at_mod) + " has growth fac: " + str(Params.PermGroFac[(i-1) + age_at_mod - 25])
        perm_gro = Params.PermGroFac[i + age_at_mod - 26]
        price.append(price[-1]*(1+annual_hp_growth)/perm_gro)
        debt.append((debt[-1]*(1+int_rate))/perm_gro - hsg_pmt[-1]/perm_gro) #xx double-check timing assumptions here
        maint_pmt.append(maint_pmt[-1]/perm_gro)
        hsg_pmt.append(hsg_pmt[-1]/perm_gro)
    equity = np.array(price) - np.array(debt)
    limit = np.min(np.vstack((-(np.array(price)*(1-collateral_constraint) - np.array(debt)),np.zeros(T))),axis=0).tolist()
    if hsg_pmt_wk_own and not default:
        user_cost = [x * hamp_params['hsg_own_p'] for x in price]
        hsg_pmt = map(add, hsg_pmt, user_cost)
    elif default:
        hsg_pmt = [x * hamp_params['hsg_rent_p'] for x in price]
    if max(hsg_pmt) >= baseline_params['IncUnemp'] + 0.3:
        print("Error: cannot have housing payment > UI Benefit")
        print hsg_pmt
        return
    hsg_pmt = map(add, hsg_pmt, maint_pmt)

    
    #housing payments in retirement
    if default:
        hsg_pmt_ret_y = False
    if hsg_pmt_ret_y:
        if annual_hp_growth_base is None:
            annual_hp_growth_base = annual_hp_growth
        price_baseline = [initial_price]
        for i in range(1,T): 
            perm_gro = Params.PermGroFac[i + age_at_mod - 26]
            price_baseline.append(price[-1]*(1+annual_hp_growth_base)/perm_gro)        
        hsg_pmt_ret = [hamp_params['hsg_rent_p']*price_baseline[-1]/Params.PermGroFac[39]]
        hsg_pmt_ret = hsg_pmt_ret * 26
    else:
        hsg_pmt_ret = [hamp_params['hsg_rent_p']*price[-1]*(1+annual_hp_growth)/Params.PermGroFac[39]]
        for i in range(25):
            hsg_pmt_ret.append(hsg_pmt_ret[-1]*(1+annual_hp_growth)) 
    if max(hsg_pmt_ret) >= 1:
        print("Error: cannot have housing payment > income")
        print hsg_pmt_ret  
        return
    #pdb.set_trace()
    hsg_pmt_ret = map(add, hsg_pmt_ret, [maint_pmt[-1]/Params.PermGroFac[39]] * 26)

    #fill out arguments to return
    hsg_pmt = [0.0] * (age_at_mod - 26) + hsg_pmt + hsg_pmt_ret
    equity = [0.0] * (age_at_mod - 26) + equity.tolist() + [0.0] * 26
    limit = [0.0] * (age_at_mod - 26) + limit + [0.0] * 26
    sale_proceeds = max(equity[38],0)
    return sale_proceeds, equity, limit, hsg_pmt



def pra_pmt(annual_hp_growth, collateral_constraint, baseline_debt, initial_price, 
            int_rate, pra_forgive, hsg_rent_p, hsg_own_p,  maint, hsg_pmt_wk_own = True, hsg_pmt_ret_y = False,
            age = 45, forgive = hamp_params['pra_forgive']):
    r, e, L, d_hamp = hsg_wealth(initial_debt =  rd_params['baseline_debt_rd'], age_at_mod = age, hsg_pmt_wk_own = hsg_pmt_wk_own, hsg_pmt_ret_y = hsg_pmt_ret_y, **hamp_params)
    r, e, L, d_prin_red = hsg_wealth(initial_debt =  rd_params['baseline_debt_rd'] - forgive, age_at_mod = age, hsg_pmt_wk_own = hsg_pmt_wk_own, hsg_pmt_ret_y = hsg_pmt_ret_y, **hamp_params)
    pra_pmt = d_prin_red[:age-26] + d_hamp[age-26:age-21] + d_prin_red[age-21:]
    return pra_pmt

#calculate NPV of mortgage payments at mod date
def npv_mtg_nominal(initial_debt, annual_hp_growth, collateral_constraint, 
                    baseline_debt, initial_price, int_rate, pra_forgive, 
                    hsg_rent_p, hsg_own_p, maint, age_at_mod = 45):
    if settings.verbose:
        print "Hsg wealth params: P=", initial_price, " D=", baseline_debt, " g=", annual_hp_growth, " r=", int_rate, " phi=", collateral_constraint
    T = 65 - age_at_mod
    price = [initial_price]
    debt = [initial_debt]
    amort = int_rate*(1+int_rate)**30/((1+int_rate)**30-1)
#    mtg_pmt = [initial_debt*amort]
    nom_pmt = initial_debt*amort
    npv_debt = nom_pmt
    for i in range(1,T):
        price.append(price[-1]*(1+annual_hp_growth))
#        mtg_pmt.append(mtg_pmt[-1])
        debt.append((debt[-1]*(1+int_rate)) - nom_pmt) #xx double-check timing assumptions here
        npv_debt += nom_pmt/((1+int_rate)**i)
    npv_debt = npv_debt + debt[T-1]/((1+int_rate)**T) 
    npv_asset = price[T-1]/((1+int_rate)**T)   
    npv_stay = npv_asset - npv_debt
    return npv_asset, npv_debt, npv_stay

a, d, npv_stay = npv_mtg_nominal(initial_debt =  rd_params['baseline_debt_rd'],**hamp_params)
a, d, npv_stay_pra = npv_mtg_nominal(initial_debt =  rd_params['baseline_debt_rd'] -hamp_params['pra_forgive'],**hamp_params)


def hsg_params(params,ltv,default = False, pra = False, pra_start = rd_params['baseline_debt_rd'], add_hsg = 0): #xxx code is missing uw_house_params['BoroCnstArt']
    new_params = deepcopy(params)
    new_params['rebate_amt'], e, L, new_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['initial_price']*(ltv/100.),default = default, **hamp_params) 
    if default:
        new_params['rebate_amt'] = 0
    if pra:
        forgive_amt = pra_start - hamp_params['initial_price']*(ltv/100.)
        new_params['rebate_amt'], e, L, d = \
            hsg_wealth(initial_debt =  rd_params['baseline_debt_rd'] - forgive_amt, **hamp_params)
        new_params['HsgPay'] = pra_pmt(age = 45, forgive = forgive_amt, **hamp_params) 
        #new_params['BoroCnstArt'] = L
    new_params['HsgPay'] = map(add,new_params['HsgPay'],[0] * 19 + [add_hsg] * 20 + [0] * 26)
    return new_params
    
    
###########################################################################
# Solve consumer problems
###########################################################################
#pandas2ri.activate() 
reload(Params)  
baseline_params = Params.init_consumer_objects

def solve_unpack(params):
    settings.rebate_size = params['rebate_amt']
    settings.t_rebate = params['rebate_age']
    params['Rfree'] = 1+ hamp_params['int_rate']
    if settings.lil_verbose:
        print "Rebate is " + str(round(settings.rebate_size,2)) + " at age " + str(90-settings.t_rebate) #+ " & int rate is: " + str(params['Rfree'])
    IndShockConsumerType = Model.IndShockConsumerType(**params)
    IndShockConsumerType.solve()
    IndShockConsumerType.unpack_cFunc()
    IndShockConsumerType.timeFwd()
    return IndShockConsumerType
    

baseline_params['aXtraCount'] = 30
baseline_params['DiscFac'] = (np.ones(65)*0.96).tolist()
baseline_params['vFuncBool'] = True
settings.verbose = False
baseline_params['IncUnemp'] = inc_params['inc_unemp']
baseline_params['UnempPrb'] = inc_params['prb_unemp']
#xxx this code exists only on default side and not on consumption side

settings.lil_verbose = False
IndShockExample = solve_unpack(baseline_params)

###########################################################################
# Set parameters
###########################################################################
def set_inc_params(TranShkCount = 15, inc_shock_rescale = inc_params['inc_shk_rescale'], p_unemp = inc_params['prb_unemp'], CRRA = 4  ):
    agent_params = deepcopy(baseline_params)
    agent_params['TranShkCount'] = TranShkCount
    agent_params['TranShkStd'] = [item*inc_shock_rescale for item in agent_params['TranShkStd']]
    agent_params['UnempPrb'] = p_unemp
    agent_params['CRRA'] = CRRA
    return agent_params

stig_cnst = -5.4 #was 12
#can also be set further down
def_p = pd.DataFrame({ "grid_n": 75, "inc_sd": 3.5, "p_unemp": 0., "stig": -6, "CRRA": 4}, index=["std"])
params_std = set_inc_params(TranShkCount = int(def_p.loc["std","grid_n"]), inc_shock_rescale = def_p.loc["std","inc_sd"], p_unemp = def_p.loc["std","p_unemp"])
def_p = def_p.append(pd.DataFrame({ "grid_n": 25, "inc_sd": inc_params['inc_shk_rescale'], "p_unemp": inc_params['prb_unemp'], "stig": stig_cnst, "CRRA": 4}, index=["u"]))
params_u = set_inc_params(TranShkCount = int(def_p.loc["u","grid_n"]))
def_p.to_csv(out_path + "default_params.csv")

uw_house_params = deepcopy(params_u) #uw_house_params['BoroCnstArt']
uw_house_params['rebate_amt'], e, L, uw_house_params['HsgPay'] = \
    hsg_wealth(initial_debt =  rd_params['baseline_debt_rd'], **hamp_params) 
default_params = deepcopy(params_u)
default_params = hsg_params(default_params, ltv = rd_params['baseline_debt_rd']/hamp_params['initial_price'], default = True)


def v_stig(m,stig,vf):
    return(vf(m)+stig)

###########################################################################
# Default rates and loan-to-value
###########################################################################  
def default_rate_solved(v_func_def,agent):
    if agent.solution[t_eval].vFunc(3) < v_func_def(3):
        m_star = 3
    elif agent.solution[t_eval].vFunc(0.3) > v_func_def(0.3):
        m_star = 0.3
    else:
        m_star = findIntersection(v_func_def,agent.solution[t_eval].vFunc,0.5)
    share_default = sum(agent.IncomeDstn[0][0][agent.IncomeDstn[0][2]<m_star])
    return m_star, share_default

ltv_rows = range(30,190,10) 
m_std_pra_list = []
def_std_pra_list = []
m_u_pra_list = []
def_u_pra_list = []

def_params = hsg_params(params_u, ltv = rd_params['baseline_debt_rd']/hamp_params['initial_price'], default = True)
agent_d = solve_unpack(def_params)
v_def_stig_tmp = partial(v_stig, stig = def_p.loc["u","stig"], vf = agent_d.solution[t_eval].vFunc) 
def_params = hsg_params(params_std, ltv = rd_params['baseline_debt_rd']/hamp_params['initial_price'], default = True)
agent_d = solve_unpack(def_params)
v_def_stig_std = partial(v_stig, stig = def_p.loc["std","stig"], vf = agent_d.solution[t_eval].vFunc) 

    
for ltv in ltv_rows:
    mtg_params = hsg_params(params_u, ltv = ltv, pra = True) 
    agent_nd = solve_unpack(mtg_params)
    m_star, share_default = default_rate_solved(v_def_stig_tmp,agent_nd)
    m_u_pra_list.append(m_star)
    if ltv <= 100: share_default = 0
    def_u_pra_list.append(share_default)    

    mtg_params = hsg_params(params_std, ltv = ltv, pra = True) 
    agent_nd = solve_unpack(mtg_params)
    m_star, share_default = default_rate_solved(v_def_stig_std,agent_nd)
    m_std_pra_list.append(m_star)
    if ltv <= 100: share_default = 0
    def_std_pra_list.append(share_default)
    
mstar_u_pra_f = LinearInterp(ltv_rows,np.array(m_u_pra_list))
def_u_pra_f = LinearInterp(ltv_rows,np.array(def_u_pra_list))

mstar_std_pra_f = LinearInterp(ltv_rows,np.array(m_std_pra_list))
def_std_pra_f = LinearInterp(ltv_rows,np.array(def_std_pra_list))



labels = ["Model: Baseline","Model: Match Xsec Correlation"]
labels_main = ["Baseline"]

g = mpl_funcs([def_u_pra_f], 
            min(ltv_rows),max(ltv_rows)+10, N=len(ltv_rows), 
        ser_colors = ["#9ecae1"],
        show_legend=False,
        title = "", labels = [labels_main[0]],
        ylab = "Share Defaulting ", xlab = "Loan-to-Value", 
        file_name = "default_rate")

g = mpl_funcs([mstar_u_pra_f], 
            100,max(ltv_rows), N=50, 
            title = "",
        labels = [labels_main[0]],
        ser_colors = ["#9ecae1",],
        show_legend=False,
        marker="",                      
        ylab = "Income + Assets Threshold", xlab = "Loan-to-Value",
        y_lim = [0.3, 2.2],
        file_name = "default_inc_threshold")

g = mpl_funcs([def_u_pra_f,def_std_pra_f], 
            min(ltv_rows),max(ltv_rows), N=len(ltv_rows), 
        title = "", labels = labels,
        ser_colors = ["#9ecae1","#3182bd",],
        loc = (0,0.75),
        ylab = "Share Defaulting ", xlab = "Loan-to-Value", 
        file_name = "default_rate_inc_backup")



