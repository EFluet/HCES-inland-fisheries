# Description: 
# run all scripts on hh survey consumption 
# Author: Etienne Fluet-Chouinard

# set working directory
#setwd('C:/Users/efluet/Dropbox/chap5_global_inland_fish_catch/scripts/consumption')
source('./consumption/import_libraries.r')



### Format original survey data ---------------------------------------------
source('./consumption/data_proc/format_asia_pacific_survey_data.R')
source('./consumption/data_proc/format_all_hh_consump_survey_data.R')


### Combine original surveys files  -------------------------------------------
# - combine and format the consumption surveys (from ADepT and other sources)
# - calculates total national consumption for countries and 
# - writes consump_df and consump_nat_agg_df to file
# - corrects for Malawi consumtpion with lower IFPRI value 
source('./consumption/data_proc/format_nat_fish_consumption_surveys_remove_codes_and_imports_v7.R')


### Apply refuse factors to surveys --------------------------------------------
source('./consumption/data_proc/format_refuse_factors.r')


### match FWEAs conversion factors to consumption ------------------------------
source('./consumption/data_proc/fwaes_consumption_conv_v4.r')


### Sum to national consumption (with mc)  -------------------------------------
source('./consumption/data_proc/country_consump_sum_mc_wfmdist_v3.r')
    ### this runs another script that removes the contribution of imports, 
    # exports and aquaculture from FAO ------
    #source('./consumption/data_proc/country_sum_trade_corr_mc.r')


# ### calculate difference percentages between FAO and surveys
# source('./consumption/data_proc/diff_surv_fao_catch_v2.r')
# source('./consumption/data_proc/consump_tr_Corr_prep_for_plotting.r')

source('./consumption/plots/barplot_survey_v_faocatch_montecarlo_v2.r')

source('./consumption/data_proc/fishstatj_flag_tally/combined_fao_flag_tally_v2.r')




### Supplementary Material

# facet barplot
source('./consumption/plots/barplot_mc_consump_tr_country_facet.r')







### PLOTTING ---------------------------------------------





### prepare the trade corrected data for plotting
source('./consumption/data_proc/consump_tr_corr_prep_for_plotting.r')

### Dashboard PLOT per country
source('./consumption/plots/country_consump_dashboard_loop_process_steps_wuncert_widestassumption_v3.r')

### country barplot of survey and FAO catch
source('./consumption/plots/barplot_surveyconsump_v_fao_inland_catch_v3.r')



# script that compares survey diff to BNP
source('./consumption/plots/bnp_surv_comparison_table.r')

###
source('calc_diff_survey_fao_extrapolation_percdif_barplot.R')



#source('scatter_plot_consump_v_nat_inland_catch_sep8020_v2_werrbars.R')


### FishStatJ Flag Tally --------------------------



### Post-Survey Catch Trend -----------------------
source('./consumption/data_proc/catch_trend_postsurvey.r')




