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
boom_params = json.loads(open('./params/boom_params.json').read())
rd_params = json.loads(open('./params/rd_params.json').read())
heloc_L = json.loads(open('./params/heloc_L.json').read())['heloc_L']
hamp_coh = inc_params['cash_on_hand']

df_ltv  = pd.read_csv('./params/HAMPRA_ltv_dist.csv')

###########################################################################
#set economic parameters 
###########################################################################
rebate_years_until_death = 25
age_of_rebate = 90 - rebate_years_until_death
t_eval = age_of_rebate - 25 - 20
def mpc(cF, rebate = 1, a = 0.1):
    return round((cF(a+rebate) - cF(a)) / rebate,3)
tmp_vlo = Params.IncUnemp
tmp_lo = 0.8
tmp_norm = 1
tmp_hi = 3
tmp_vhi = 6

def mpc_pih(cF, rebate = 1, a = 10):
    return round((cF(a+rebate) - cF(a)) / rebate,3)

settings.init()
settings.lil_verbose = True
settings.min_age, settings.max_age = 60, 65
###########################################################################
#plotting functions 
###########################################################################
plt.style.use('ggplot')
def mpl_funcs(functions, bottom, top, N=1000,
              labels = [], ser_lims = None,
             title = "Consumption and Cash-on-Hand",
             ser_colors = None, ser_linestyles = None,
             ylab = "y", xlab="x", 
             loc = "best", l_title = '',
             show_legend=True,
             x_lim = None, invert_x = False, x_ticks = None,
             y_lim = None, y_ticks = None,
             marker = 'o',
             vert_lines = [], arrow = None,
             file_name = None, show_plot = True):
    style = ['-rd', '-go', '-bx', '-cv', '-m,', '-ys', '-kp', '-w+']
    if type(functions)==list:
        function_list = functions
    else:
        function_list = [functions]    
    
    # Different limits for each series
    lims = []
    if ser_lims is not None:
        for i, x in enumerate(ser_lims):
            lims.append(x)
    else:
        lims = len(functions) * [(bottom, top )]        
    
    #Plot functions    
    fig, ax = plt.subplots(figsize = (6,4))
    for i, function in enumerate(function_list):
        n = N *(lims[i][1] - lims[i][0])/(top - bottom)
        step = (lims[i][1] - lims[i][0])/n
        x = np.arange(lims[i][0], lims[i][1], step)
        
        ser_color = "#DDDDDD"
        ser_linestyle = "-"
        if ser_colors is not None:
            ser_color = ser_colors[i]
        if ser_linestyles is not None:
            ser_linestyle = ser_linestyles[i]
        ax.plot(x, function(x), style[i],
                label = labels[i], marker=marker,
                color=ser_color, linestyle = ser_linestyle)
            
    #Additional plot elements
    for x_coord in vert_lines:
        plt.axvline(x=x_coord, color='#808080', linestyle='--',
                    linewidth = 0.8, alpha=1.0)       
    if arrow is not None:
        plt.arrow(arrow['x'], arrow['y'], arrow['dx'], arrow['dy'],
                  width = arrow['width'],
                  head_length = abs(0.1 * arrow['dx']),
                  head_width = abs(0.0007 * arrow['dx']),
                  color = '#d62f2f',edgecolor='none',
                  length_includes_head=True)
        
            
    #Cosmetics      
    ax.tick_params(axis=u'both', which=u'both',length=0)
    ax.set_facecolor('white')      
    plt.ylabel(ylab, alpha=1.)
    plt.xlabel(xlab, alpha=1.)
    plt.title(title, fontsize = 12)
    plt.grid(b=True, which='major', linestyle='solid', color='gray', alpha=0.3)
    
    for spine in plt.gca().spines.values():
        spine.set_visible(False)
    
    plt.legend(loc=loc, frameon='None', title = l_title,
               edgecolor='white', facecolor='white', framealpha=1.0)
    if show_legend==False:
        plt.legend().set_visible(False)
    if x_lim is not None:
        plt.xlim(x_lim)
    if invert_x is True:
        plt.gca().invert_xaxis()
    if x_ticks is not None:
        ax.xaxis.set_ticks(x_ticks)
    if y_lim is not None:
        plt.ylim(y_lim)
    if y_ticks is not None:
        ax.yaxis.set_ticks(y_ticks)
    if file_name is not None:
        plt.savefig(out_path + file_name + ".png", dpi=300)
    if show_plot:
        plt.show()

###########################################################################
#housing wealth functions
###########################################################################
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
            int_rate, pra_forgive, hsg_rent_p, hsg_own_p, maint, hsg_pmt_wk_own = True, hsg_pmt_ret_y = False,
            age = 45, forgive = hamp_params['pra_forgive']):
    r, e, L, d_hamp = hsg_wealth(initial_debt =  hamp_params['baseline_debt'], age_at_mod = age, hsg_pmt_wk_own = hsg_pmt_wk_own, hsg_pmt_ret_y = hsg_pmt_ret_y, **hamp_params)
    r, e, L, d_prin_red = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] - forgive, age_at_mod = age, hsg_pmt_wk_own = hsg_pmt_wk_own, hsg_pmt_ret_y = hsg_pmt_ret_y, **hamp_params)
    pra_pmt = d_prin_red[:age-26] + d_hamp[age-26:age-21] + d_prin_red[age-21:]
    return pra_pmt

if __name__ == "__main__":
    baseline_params = Params.init_consumer_objects
    
    #construct results w default specification for how to set up house prices (neg wealth effect)
    uw_house_params = deepcopy(baseline_params)
    uw_house_params['rebate_amt'], e, uw_house_params['BoroCnstArt'], uw_house_params['HsgPay'] = \
        hsg_wealth(initial_debt =  hamp_params['baseline_debt'], **hamp_params)
    pra_params = deepcopy(baseline_params)
    pra_params['rebate_amt'], e, pra_params['BoroCnstArt'], pra_params['HsgPay'] = \
        hsg_wealth(initial_debt =  hamp_params['baseline_debt'] - hamp_params['pra_forgive'], **hamp_params)
    pra_params['HsgPay'] = pra_pmt(age = 45, **hamp_params)
    
    #slide 3 -- housing equity 
    labels = ["Payment Reduction", "Payment & Principal Reduction"]
    def neg(x): return -1*x
    boro_cnst_pre_pra = LinearInterp(np.arange(26,91),list(map(neg, uw_house_params['BoroCnstArt'])))
    boro_cnst_post_pra = LinearInterp(np.arange(26,91),list(map(neg, pra_params['BoroCnstArt'])))
    
    mpl_funcs([boro_cnst_pre_pra, boro_cnst_post_pra], 45.001, 64, N=round(64-45.001), 
              labels = labels,
                 title = "", 
                 ser_colors = ["#9ecae1","#3182bd",],
                 loc = (0, 0.7),
                 ylab = "Borrowing Limit (Years of Income)", xlab = "Age", 
                 file_name = "borrowing_limits_and_pra")
    
    x_min = 45.001
    #slide 4 -- housing payments 
    pmt_pre_pra =  LinearInterp(np.arange(26,91),uw_house_params['HsgPay']) 
    pmt_post_pra = LinearInterp(np.arange(26,91),pra_pmt(age = 45, **hamp_params))
    g = mpl_funcs([pmt_pre_pra,pmt_post_pra],
                  x_min,65, N=round(65-x_min), loc=(0, 0.1),
            title = "",
            ser_colors = ["#9ecae1","#3182bd",],
            labels = labels, y_lim = [0.15, 0.35],
            ylab = "Payment As Share of Income", xlab = "Age", file_name = "hsg_pmt_and_pra")
    
    

    
    ###########################################################################
    # Solve consumer problems
    #pandas2ri.activate() 
    
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
        
    #rrr want to reset this back to a lower number now to see what happens to speed
    baseline_params['aXtraCount'] = 30
    baseline_params['DiscFac'] = (np.ones(65)*0.96).tolist()
    baseline_params['vFuncBool'] = True
    settings.verbose = False
    baseline_params['IncUnemp'] = inc_params['inc_unemp']
    baseline_params['UnempPrb'] = inc_params['prb_unemp']
    baseline_params['TranShkStd'] = [item*inc_params['inc_shk_rescale'] for item in baseline_params['TranShkStd']]
    
    IndShockExample = solve_unpack(baseline_params)
    mpc_pih(IndShockExample.cFunc[0]), mpc_pih(IndShockExample.cFunc[20]), mpc_pih(IndShockExample.cFunc[40])    
    
    #alternative consumption functions
    example_params = deepcopy(baseline_params)
    example_params['rebate_age'] = 39
    example_params['rebate_amt'] = 1
    RebateAge51 = solve_unpack(example_params)
    example_params['rebate_age'] = 44
    RebateAge46 = solve_unpack(example_params)
    settings.t_rebate = rebate_years_until_death
    grant_now = lambda x: IndShockExample.cFunc[t_eval](x+1)
    
    #relax collateral constraint
    example_params = deepcopy(baseline_params)
    l = example_params['BoroCnstArt']
    for i in range(len(l)):
        l[i] = -1
    for i in range(40,65):
        l[i] = 0
    Boro1YrInc = solve_unpack(example_params)
    for i in range(21):
        l[i] = 0
    BoroAge46 = solve_unpack(example_params)
    for i in range(26):
        l[i] = 0
    BoroAge51 = solve_unpack(example_params)
    l = example_params['BoroCnstArt']
    for i in range(len(l)):
        l[i] = heloc_L
    Boro_heloc = solve_unpack(example_params)
       
    #paper plots
    g = mpl_funcs([IndShockExample.cFunc[t_eval],grant_now,RebateAge46.cFunc[t_eval],
                   RebateAge51.cFunc[t_eval]],
            -0.001,3, N=50, 
            title = "",
            ser_colors = ["#DDDDDD" , "#DEEBF7", "#9ECAE1", "#3182BD"],
            ser_linestyles = ["-" , "--", "-.", ":"],              
            marker = "",
            loc = (0.4, 0.1),
            labels = ["Baseline","Grant 0 Years Away","Grant 1 Year Away", "Grant 6 Years Away"],
            ylab = "Consumption (Ratio to Permanent Income)", 
            xlab = "Cash-on-Hand (Ratio to Permanent Income)", 
            file_name = "cf_fut_wealth")
    
    
    g = mpl_funcs([IndShockExample.cFunc[t_eval],Boro1YrInc.cFunc[t_eval],
                   BoroAge46.cFunc[t_eval],BoroAge51.cFunc[t_eval]],
            -0.001,3, N=50, 
            title = "",
            labels = ["Baseline","Collateral 0 Years Away","Collateral 1 Year Away", 
                      "Collateral 6 Years Away"],
            ser_colors = ["#DDDDDD" , "#DEEBF7", "#9ECAE1", "#3182BD"],
            ser_linestyles = ["-" , "--", "-.", ":"],              
            marker = "",
            loc = (0.4, 0.1),
            ylab = "Consumption (Ratio to Permanent Income)", 
            xlab = "Cash-on-Hand (Ratio to Permanent Income)", 
            file_name = "cf_fut_collateral")
    
    
    #####################################
    #analyze principal forgiveness
    ######################################
    
    cFuncs = []
    cFuncs_w = []
    cFuncs_L = []
    cFuncs_rL = []
    cFuncs_0_pct =[]
    cFuncs_heloc =[]
    hw_cf_list = []
    hw_cf_coh_hi_list = []
    hw_cf_coh_vhi_list = []
    hw_cf_w_list = []
    hw_cf_rL_list = []
    hw_cf_L_list = []
    hw_cf_0_pct_list = []
    hw_cf_heloc_list = []
    grid_len = 20
    grid_int = 0.25
    grid_max = grid_len*grid_int
    
    for i in range(grid_len-1,-1,-1):
        #full specification
        hw_cf_params = deepcopy(baseline_params)
        hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], d = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] - i*grid_int, **hamp_params)
        hw_cf_params['HsgPay'] =  pra_pmt(age = 45, forgive = i*grid_int, **hamp_params)   
        cf = solve_unpack(hw_cf_params)
        cFuncs.append(cf.cFunc)
        hw_cf_list.append(cf.cFunc[t_eval](hamp_coh))
        hw_cf_coh_hi_list.append(cf.cFunc[t_eval](tmp_hi))
        hw_cf_coh_vhi_list.append(cf.cFunc[t_eval](6))
    
        #full specification with heloc
        hw_cf_params['BoroCnstArt']   = map(lambda x:x-heloc_L, hw_cf_params['BoroCnstArt'])
        cf = solve_unpack(hw_cf_params)
        cFuncs_heloc.append(cf.cFunc)
        hw_cf_heloc_list.append(cf.cFunc[t_eval](hamp_coh))
    
        #cash payments only
        hw_cf_params = deepcopy(baseline_params)
        hw_cf_params['rebate_amt'], e, L, d = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] - i*grid_int, **hamp_params)
        hw_cf_params['HsgPay'] =  pra_pmt(age = 45, forgive = i*grid_int, **hamp_params)   
        cf = solve_unpack(hw_cf_params)
        cFuncs_w.append(cf.cFunc)
        hw_cf_w_list.append(cf.cFunc[t_eval](hamp_coh))
        
        #collateral only
        hw_cf_params = deepcopy(baseline_params)
        r, e, hw_cf_params['BoroCnstArt'], d = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] - i*grid_int, **hamp_params)
        hw_cf_params['HsgPay']  = uw_house_params['HsgPay']
        hw_cf_params['rebate_amt'] = 0
        cf = solve_unpack(hw_cf_params)
        cFuncs_L.append(cf.cFunc)
        hw_cf_L_list.append(cf.cFunc[t_eval](hamp_coh))
        
        #collateral and rebate specification
        hw_cf_params = deepcopy(baseline_params)
        hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], d = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] - i*grid_int, **hamp_params)
        hw_cf_params['HsgPay']  = uw_house_params['HsgPay']
        cf = solve_unpack(hw_cf_params)
        cFuncs_rL.append(cf.cFunc)
        hw_cf_rL_list.append(cf.cFunc[t_eval](hamp_coh))
        
        #0% LTV specification
        hamp_params['collateral_constraint'] = 0
        hw_cf_params = deepcopy(baseline_params)
        hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], d = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] - i*grid_int, **hamp_params)
        hw_cf_params['HsgPay'] =  pra_pmt(age = 45, forgive = i*grid_int, **hamp_params)   
        cf = solve_unpack(hw_cf_params)
        cFuncs_0_pct.append(cf.cFunc)
        hw_cf_0_pct_list.append(cf.cFunc[t_eval](hamp_coh))
        hamp_params['collateral_constraint'] = 0.20
        
    equity_initial = hamp_params['baseline_debt'] - hamp_params['initial_price']
    gr_max = 100*(1+equity_initial/hamp_params['initial_price']) + 2
    gr_min = 100*(1-((grid_len*grid_int - equity_initial)/hamp_params['initial_price']))
    grid_int2 = (gr_max-gr_min)/grid_len
    
    hw_cf = LinearInterp(np.arange(gr_min,gr_max,grid_int2),np.array(hw_cf_list))
    hw_cf_coh_hi = LinearInterp(np.arange(gr_min,gr_max,grid_int2),np.array(hw_cf_coh_hi_list))
    hw_cf_coh_vhi = LinearInterp(np.arange(gr_min,gr_max,grid_int2),np.array(hw_cf_coh_vhi_list))
    hw_cf_w = LinearInterp(np.arange(gr_min,gr_max,grid_int2),np.array(hw_cf_w_list))
    hw_cf_L = LinearInterp(np.arange(gr_min,gr_max,grid_int2),np.array(hw_cf_L_list))
    hw_cf_rL = LinearInterp(np.arange(gr_min,gr_max,grid_int2),np.array(hw_cf_rL_list))
    hw_cf_0_pct = LinearInterp(np.arange(gr_min,gr_max,grid_int2),np.array(hw_cf_0_pct_list))
    hw_cf_heloc = LinearInterp(np.arange(gr_min,gr_max,grid_int2),np.array(hw_cf_heloc_list))
    
    ltv_start = 100*(hamp_params['baseline_debt']/hamp_params['initial_price'])
    ltv_end = 100*((hamp_params['baseline_debt'] - hamp_params['pra_forgive'])/hamp_params['initial_price'])
    
    #slide 5 -- Consumption function out of principal forgiveness
    g = mpl_funcs([hw_cf,],0,153, N=51.0, 
            title = "",
            labels = ["Baseline"],
            ser_colors = ["#9ecae1","#3182bd",],
            ylab = "Consumption (Ratio to Permanent Income)", 
            xlab = "Loan-to-Value (> 100 is Underwater)",
            show_legend=False,
            x_lim = (0, 160), invert_x = True,
            marker = '.',
            arrow = {'x':150, 'y':0.6, 'dx':-44, 'dy':0, 'width':0.003},
            vert_lines = [100*(1-hamp_params['collateral_constraint'])], #Have to reverse the order
            file_name = "cons_and_prin_forgive")
 
    
    ############################################################################
    ## Housing MPC and Cash MPC by LTV
    ############################################################################
    dhp = 0.25
    coh = 2
    #While the other scenarios are useful for diagnostics, only the hsg, debt and cash 
    #are used in the paper
    scenarios = ['cash','hsg','debt']
    mpc_df_list = []
    mpc_low_coh_list = [] 


    for ltv_rows in [range(160,0,-10), df_ltv['LTV_Midpoint'].tolist()]:
        index = pd.Index(ltv_rows, name='rows')
        columns = pd.Index(scenarios, name='cols')
        
        hp_mpc = pd.DataFrame(np.zeros((len(ltv_rows), len(scenarios))), index=index, columns=columns) 
        hp_mpc_low_coh = pd.DataFrame(np.zeros((len(ltv_rows), len(scenarios))), index=index, columns=columns) 
        for eq in ltv_rows: #
            hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  (eq/100.)*hamp_params['initial_price'] , **hamp_params)
            hsg_pay_pre_neg = deepcopy(hw_cf_params['HsgPay'])
            #settings.verbose = True
            cf_pre = solve_unpack(hw_cf_params)  
            #settings.verbose = False
            hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt = (eq/100.)*hamp_params['initial_price'] - dhp, **hamp_params)
            cf_debt = solve_unpack(hw_cf_params)    
            hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt = (eq/100.)*hamp_params['initial_price'],  d_house_price = dhp, **hamp_params)
            cf_hp = solve_unpack(hw_cf_params)
            #print "Change in housing payment after HP increase, spec: negative", map(sub,hw_cf_params['HsgPay'],hsg_pay_pre_neg)
            hp_mpc.loc[eq,'hsg'] = (cf_hp.cFunc[t_eval](coh) - cf_pre.cFunc[t_eval](coh))/dhp
            hp_mpc.loc[eq,'debt'] = (cf_debt.cFunc[t_eval](coh) - cf_pre.cFunc[t_eval](coh))/dhp
            hp_mpc.loc[eq,'cash'] = (cf_pre.cFunc[t_eval](coh+dhp) - cf_pre.cFunc[t_eval](coh))/dhp
            
            hp_mpc_low_coh.loc[eq,'hsg'] = (cf_hp.cFunc[t_eval](hamp_coh) - cf_pre.cFunc[t_eval](hamp_coh))/dhp
            hp_mpc_low_coh.loc[eq,'debt'] = (cf_debt.cFunc[t_eval](hamp_coh) - cf_pre.cFunc[t_eval](hamp_coh))/dhp
            hp_mpc_low_coh.loc[eq,'cash'] = (cf_pre.cFunc[t_eval](hamp_coh+dhp) - cf_pre.cFunc[t_eval](hamp_coh))/dhp
            
            #zero wealth shock spec
            hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  (eq/100.)*hamp_params['initial_price'] ,  hsg_pmt_wk_own = True, hsg_pmt_ret_y = True, **hamp_params)
            hsg_pay_pre_zero = deepcopy(hw_cf_params['HsgPay'])  
            cf_pre_zero = solve_unpack(hw_cf_params)
            hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt = (eq/100.)*hamp_params['initial_price'],  d_house_price = dhp, hsg_pmt_wk_own = True, hsg_pmt_ret_y = True, **hamp_params)
            hw_cf_params['HsgPay'] = hw_cf_params['HsgPay'][:39] + hsg_pay_pre_zero[39:]   
            #print "Change in housing payment after HP increase, spec: zero", map(sub,hw_cf_params['HsgPay'],hsg_pay_pre_zero)   
            cf_hp_zero = solve_unpack(hw_cf_params)
            
            #positive wealth shock spec
            hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  (eq/100.)*hamp_params['initial_price'] ,  hsg_pmt_wk_own = False, hsg_pmt_ret_y = True, **hamp_params)
            hsg_pay_pre_pos = deepcopy(hw_cf_params['HsgPay'])
            cf_pre_pos = solve_unpack(hw_cf_params)
            hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], d = hsg_wealth(initial_debt = (eq/100.)*hamp_params['initial_price'],  d_house_price = dhp, hsg_pmt_wk_own = False, hsg_pmt_ret_y = True, **hamp_params)
            hw_cf_params['HsgPay'] = hsg_pay_pre_pos  
            #print "Change in housing payment after HP increase, spec: pos", map(sub,hw_cf_params['HsgPay'],hsg_pay_pre_pos)
            cf_hp_pos = solve_unpack(hw_cf_params)
            
            hp_mpc.loc[eq,'hsg_pos'] = (cf_hp_pos.cFunc[t_eval](coh) - cf_pre_pos.cFunc[t_eval](coh))/dhp
            hp_mpc.loc[eq,'hsg_zero'] = (cf_hp_zero.cFunc[t_eval](coh) - cf_pre_zero.cFunc[t_eval](coh))/dhp
            hp_mpc_low_coh.loc[eq,'hsg_pos'] = (cf_hp_pos.cFunc[t_eval](hamp_coh) - cf_pre_pos.cFunc[t_eval](hamp_coh))/dhp
            hp_mpc_low_coh.loc[eq,'hsg_zero'] = (cf_hp_zero.cFunc[t_eval](hamp_coh) - cf_pre_zero.cFunc[t_eval](hamp_coh))/dhp
        mpc_df_list.append(hp_mpc)
        mpc_low_coh_list.append(hp_mpc_low_coh)
        

    ltv_rows = range(160,0,-10)
    equity_a = range(10,170,10) #range(160,0,-10) #np.arange(-60,100,10) 
    

    
    gr_min = 10
    gr_max = 155
    hp_mpc_low_coh =    mpc_low_coh_list[0]
    hp_mpc_low_coh.to_csv(out_path + 'mpc_cash_hsg_low_coh_slide2.csv')
    mpc_hsg_low_coh_f = LinearInterp(equity_a,np.array(hp_mpc_low_coh['hsg'])[::-1])
    mpc_cash_low_coh_f = LinearInterp(equity_a,np.array(hp_mpc_low_coh['cash'])[::-1])
    mpc_debt_low_coh_f = LinearInterp(equity_a,np.array(hp_mpc_low_coh['debt'])[::-1])

    g = mpl_funcs([mpc_cash_low_coh_f,mpc_debt_low_coh_f],gr_min,gr_max, N=len(ltv_rows), 
                  loc=(0.00, 0.85),
            title = "",
            labels = ["Cash MPC", "Housing MPC",],
            ser_lims = [(gr_min, 82), (gr_min, 82)],
            ser_colors = ["#9ecae1","#3182bd",],
            ylab = "Marginal Propensity to Consume", xlab = "Loan-to-Value",
            x_lim = (0, 163), invert_x = False,
            y_lim = [min(hp_mpc_low_coh['debt']), max(hp_mpc_low_coh['cash']) +0.01],
            y_ticks = [0.1, 0.2, 0.3],
            vert_lines = [100*(1-hamp_params['collateral_constraint'])],
            file_name = "mpc_cash_hsg_low_coh_slide0")   
    
    g = mpl_funcs([mpc_cash_low_coh_f,mpc_debt_low_coh_f],gr_min,gr_max, N=len(ltv_rows), 
                  loc=(0.00, 0.85),
            title = "",
            labels = ["Cash MPC", "Housing MPC",],
            ser_colors = ["#9ecae1","#3182bd",],
            ser_lims = [(gr_min, gr_max), (gr_min, 82)],
            ylab = "Marginal Propensity to Consume", xlab = "Loan-to-Value",
            x_lim = (0, 163), invert_x = False,
            y_lim = [min(hp_mpc_low_coh['debt']), max(hp_mpc_low_coh['cash']) +0.01],
            y_ticks = [0.1, 0.2, 0.3],
            vert_lines = [100*(1-hamp_params['collateral_constraint'])],
            file_name = "mpc_cash_hsg_low_coh_slide1")       
    
    g = mpl_funcs([mpc_cash_low_coh_f,mpc_debt_low_coh_f],gr_min,gr_max, N=len(ltv_rows), 
                  loc=(0.00, 0.85),
            title = "",
            labels = ["Cash MPC", "Housing MPC",],
            ser_colors = ["#9ecae1","#3182bd",],
            ser_lims = [(gr_min, gr_max), (gr_min, gr_max)],
            ylab = "Marginal Propensity to Consume", xlab = "Loan-to-Value",
            x_lim = (0, 163), invert_x = False,
            y_lim = [min(hp_mpc_low_coh['debt']), max(hp_mpc_low_coh['cash']) +0.01],
            y_ticks = [0.1, 0.2, 0.3],
            vert_lines = [100*(1-hamp_params['collateral_constraint'])],
            file_name = "mpc_cash_hsg_low_coh_slide2")
        
        
        
    #For table 6:
    mid_point_mpcs = mpc_df_list[1]
    hp_mpc_tbl_ltv = pd.merge(df_ltv, mpc_df_list[1], left_on="LTV_Midpoint", right_index=True)
    hp_mpc_tbl_ltv['weights'] = hp_mpc_tbl_ltv['Share_2005'] + hp_mpc_tbl_ltv['Share_2010']
    hp_mpc_tbl_ltv.loc[:,'hsg'] = hp_mpc_tbl_ltv['hsg']*hp_mpc_tbl_ltv['weights']/sum(hp_mpc_tbl_ltv['weights'])
    hp_mpc_tbl_ltv.loc[:,'cash'] = hp_mpc_tbl_ltv['cash']*hp_mpc_tbl_ltv['weights']/sum(hp_mpc_tbl_ltv['weights'])
    hp_mpc_tbl_ltv.loc[:,'debt'] = hp_mpc_tbl_ltv['debt']*hp_mpc_tbl_ltv['weights']/sum(hp_mpc_tbl_ltv['weights'])
    
    hp_mpc_tbl_sum = pd.DataFrame({"Mean": hp_mpc_tbl_ltv[['cash','hsg','debt', 'hsg_pos', 'hsg_zero']].sum(),
                                    "95_LTV":mpc_df_list[1].loc[95,:]}).round(3)
    
    hp_mpc_tbl_sum.to_csv(out_path + "mpc_appendix_table_6.csv")

    
    ### Generate MPCs for table 7
        
    index = pd.Index(list(["High Cash-on-Hand"]), name='rows')
    columns = pd.Index(['c_pre', 'c_post'], name='cols')
    pra_mpc = pd.DataFrame(np.zeros((1,2)), index=index, columns=columns)
    
    #baseline case & heloc case
    hw_cf_params = deepcopy(baseline_params)
    r, e, L, hw_cf_params['HsgPay'] = \
        hsg_wealth(initial_debt =  hamp_params['baseline_debt'], default = True, **hamp_params) 
    cf_d = solve_unpack(hw_cf_params)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] , **hamp_params)
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Baseline','c_pre'] = cf.cFunc[t_eval](hamp_coh)
    pra_mpc.loc['Low Cash-on-Hand','c_pre'] = cf.cFunc[t_eval](tmp_vlo)
    pra_mpc.loc['High Cash-on-Hand','c_pre'] = cf.cFunc[t_eval](tmp_hi)
    pra_mpc.loc['Baseline','c_pre'] = cf.cFunc[t_eval](hamp_coh)
    hw_cf_params['BoroCnstArt']   = map(lambda x:x-heloc_L, hw_cf_params['BoroCnstArt'])
    cf_heloc = solve_unpack(hw_cf_params)
    pra_mpc.loc['Has HELOC','c_pre'] = cf_heloc.cFunc[t_eval](hamp_coh)
    
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] -hamp_params['pra_forgive'] , **hamp_params)
    cf = solve_unpack(hw_cf_params)
    hw_cf_params['HsgPay'] =  pra_pmt(age = 45, forgive = hamp_params['pra_forgive'] , **hamp_params)   
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Baseline','c_post'] = cf.cFunc[t_eval](hamp_coh)
    pra_mpc.loc['Low Cash-on-Hand','c_post'] = cf.cFunc[t_eval](tmp_vlo)
    pra_mpc.loc['High Cash-on-Hand','c_post'] = cf.cFunc[t_eval](tmp_hi)
    hw_cf_params['BoroCnstArt']   = map(lambda x:x-heloc_L, hw_cf_params['BoroCnstArt'])
    cf_heloc = solve_unpack(hw_cf_params)
    pra_mpc.loc['Has HELOC','c_post'] = cf_heloc.cFunc[t_eval](hamp_coh)
    
    
    #change baseline debt to 135% LTV
    tmp = deepcopy(hamp_params['baseline_debt'])
    hamp_params['baseline_debt'] = 1.35*hamp_params['initial_price']
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt']  , **hamp_params)
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Write down to 90% LTV','c_pre'] = cf.cFunc[t_eval](hamp_coh)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], d = hsg_wealth(initial_debt = hamp_params['baseline_debt']  -hamp_params['pra_forgive'] , **hamp_params)
    hw_cf_params['HsgPay'] =  pra_pmt(age = 45, forgive = hamp_params['pra_forgive'] , **hamp_params)   
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Write down to 90% LTV','c_post'] = cf.cFunc[t_eval](hamp_coh)
    hamp_params['baseline_debt'] = deepcopy(tmp)
        
    
    #collateral constraint
    hamp_params['collateral_constraint'] = 0
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] , **hamp_params)
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Collateral Constraint = 0','c_pre'] = cf.cFunc[t_eval](hamp_coh)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], d = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] -hamp_params['pra_forgive'] , **hamp_params)
    hw_cf_params['HsgPay'] =  pra_pmt(age = 45, forgive = hamp_params['pra_forgive'] , **hamp_params)   
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Collateral Constraint = 0','c_post'] = cf.cFunc[t_eval](hamp_coh)
    tmp = deepcopy(hamp_params['baseline_debt'])
    hamp_params['baseline_debt'] = 1.35*hamp_params['initial_price']
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt = hamp_params['baseline_debt']   , **hamp_params)
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['90% LTV & Constraint = 0','c_pre'] = cf.cFunc[t_eval](hamp_coh)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], d = hsg_wealth(initial_debt = hamp_params['baseline_debt']   -hamp_params['pra_forgive'] , **hamp_params)
    hw_cf_params['HsgPay'] =  pra_pmt(age = 45, forgive = hamp_params['pra_forgive'] , **hamp_params)   
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['90% LTV & Constraint = 0','c_post'] = cf.cFunc[t_eval](hamp_coh)
    hamp_params['collateral_constraint'] = 0.20
    #hamp_params['baseline_debt'] = tmp
    
    #age 35
    t_eval_35 = 10
    age_young = 35
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'], age_at_mod = age_young , **hamp_params)
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Age At Mod = 35','c_pre'] = cf.cFunc[t_eval_35](hamp_coh)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], d = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] -hamp_params['pra_forgive'], age_at_mod = age_young , **hamp_params)
    hw_cf_params['HsgPay'] =  pra_pmt(age = 35, forgive = hamp_params['pra_forgive'] , **hamp_params)   
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Age At Mod = 35','c_post'] = cf.cFunc[t_eval_35](hamp_coh)
    
    
    #rapid house price growth
    tmp = deepcopy(hamp_params['annual_hp_growth'])
    hamp_params['annual_hp_growth'] = 0.05
    hw_cf_params = deepcopy(baseline_params)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] , **hamp_params)
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Fast House Price Growth (5%)','c_pre'] = cf.cFunc[t_eval](hamp_coh)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] -hamp_params['pra_forgive'] , **hamp_params)
    hw_cf_params['HsgPay'] =  pra_pmt(age = 45, forgive = hamp_params['pra_forgive'] , **hamp_params)   
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Fast House Price Growth (5%)','c_post'] = cf.cFunc[t_eval](hamp_coh)
    
    hamp_params['collateral_constraint'] = 0
    hw_cf_params = deepcopy(baseline_params)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] , **hamp_params)
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Fast House Price Growth (5%) & Collateral = 0','c_pre'] = cf.cFunc[t_eval](hamp_coh)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] -hamp_params['pra_forgive'] , **hamp_params)
    hw_cf_params['HsgPay'] =  pra_pmt(age = 45, forgive = hamp_params['pra_forgive'] , **hamp_params)   
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Fast House Price Growth (5%) & Collateral = 0','c_post'] = cf.cFunc[t_eval](hamp_coh)
    
    hamp_params['collateral_constraint'] = 0.20
    hamp_params['annual_hp_growth'] = tmp
    
    #CRRA = 2
    hw_cf_params = deepcopy(baseline_params)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] , **hamp_params)
    hw_cf_params['CRRA'] = 2
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['CRRA = 2','c_pre'] = cf.cFunc[t_eval](hamp_coh)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] -hamp_params['pra_forgive'] , **hamp_params)
    hw_cf_params['HsgPay'] =  pra_pmt(age = 45, forgive = hamp_params['pra_forgive'] , **hamp_params)   
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['CRRA = 2','c_post'] = cf.cFunc[t_eval](hamp_coh)
    
    #raise impatience
    hw_cf_params = deepcopy(baseline_params)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] , **hamp_params)
    hw_cf_params['DiscFac'] = (np.ones(65)*0.90).tolist()
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Beta = 0.90','c_pre'] = cf.cFunc[t_eval](hamp_coh)
    hw_cf_params['rebate_amt'], e, hw_cf_params['BoroCnstArt'], hw_cf_params['HsgPay'] = hsg_wealth(initial_debt =  hamp_params['baseline_debt'] -hamp_params['pra_forgive'] , **hamp_params)
    hw_cf_params['HsgPay'] =  pra_pmt(age = 45, forgive = hamp_params['pra_forgive'] , **hamp_params)   
    cf = solve_unpack(hw_cf_params)
    pra_mpc.loc['Beta = 0.90','c_post'] = cf.cFunc[t_eval](hamp_coh)
    
    
    pra_mpc['mpc'] = (pra_mpc['c_post'] - pra_mpc['c_pre'])/hamp_params['pra_forgive']
    pra_mpc
    
    pra_mpc = pra_mpc.round(3)
    pra_mpc.to_csv(out_path + "mpc_appendix_table_7.csv")
