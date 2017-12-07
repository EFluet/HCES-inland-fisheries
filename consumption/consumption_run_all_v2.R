# Description: 
# run all scripts on hh survey consumption 
# Author: Etienne Fluet-Chouinard


### Prep for scripts -----

# ~~~ set working directory ----
setwd('C:/Users/efluet/Dropbox/chap5_global_inland_fish_catch/scripts')

# ~~~ import libraries ----
source('./consumption/import_libraries.r')


### Calc HCES catch  --------------------------------------------------------------

# ~~~ Format original survey data ----------------------------------------------
source('./consumption/data_proc/format_asia_pacific_survey_data.R')
source('./consumption/data_proc/format_all_hh_consump_survey_data.R')

# ~~~ Combine original surveys files -------------------------------------------
# - combine and format the consumption surveys (from ADepT and other sources)
# - sums national consumption
# - corrects for Malawi consumtpion with lower IFPRI value 
source('./consumption/data_proc/format_nat_fish_consumption_surveys_remove_codes_and_imports_v7.R')

# ~~~ Apply refuse factors to surveys ------------------------------------------
source('./consumption/data_proc/format_refuse_factors.r')

# ~~~ match FWEAs conversion factors to consumption ----------------------------
source('./consumption/data_proc/fwaes_consumption_conv_v4.r')

# ~~~ Sum nat consumpt (with mc) & calcs diff ----------------------------------
source('./consumption/data_proc/country_consump_sum_mc_wfmdist_v4_gam.r')




### Catch diff analysis ------------------------------------------------------- 

# ~~~ Calculate difference percentages between FAO and surveys -----------------
source('./consumption/data_proc/fishstatj_flag_tally/byweight/combined_fao_flag_tally_v3_byweight.r')

# ~~~ summary statistics for the  paper ----------------------------------------
source('./consumption/data_proc/summary_stats_for_results.r')

# ~~~ GLM regression of under-estimation --------------------
source('./consumption/data_proc/catch_diff_calc_regressions_v6.r')

# ~~~ calculate person equivalent --------------------------------
source('./consumption/data_proc/global_correlates_for_diffpredictions_v3_wCI.r')


# ~~~ calculate person equivalent --------------------------------
# uses output from
source('./consumption/data_proc/FtoM_appconsump_gam_v3.r')
source('./consumption/data_proc/calc_pers_equiv_fed_v4_wCI.r')








### Manuscript figures ---------------------------------------------------------

#~~ Fig 1 maps of hh survey data --------------------------------------------
source('./consumption/maps/fig1_map_hhsurvey_totconsump_estimcatch_percdiff.r')


# ~~~ Fig 2 diff barplot ------------------------------------------------
source('./consumption/plots/fig2_diff_flag_plot_wpercdifcolornonneg.r')

# ~~~ Fig 3 country FAO catch timeline & estimates ------------------
source('./consumption/plots/fig3_catch_trend_lineplot_bycontinent_v2.r')

# ~~~ Fig.4 global timeline----------------------------------------------
source('./consumption/plots/fig4_global_trend_ca_flag_norect.r')

#~~ Fig.5 person equivalent --------------------------------
source('./consumption/data_proc/calc_pers_equiv_fed_v3.r')




### Supp Material Plots --------------------------------------------------------

# ~~~ facet trade stats barplot ------------------------------------------------
source('./consumption/plots/suppmaterial_barplot_country_stats_facet.r')


# ~~~ Fig 1 maps of hh survey data --------------------------------------------
source('./consumption/maps/fig1_map_hhsurvey_countries_totalconsump_percF_v2.r')




# ~~~ Dashboard country plots -------------------------------------------------- 
# prepare country sum data table
source('./consumption/data_proc/format_national_suppmat_table.r')

# prepare product data table

# plot FWAE factors

# plot GAMs model




### Dashboard PLOT per country
#source('./consumption/plots/country_consump_dashboard_loop_process_steps_wuncert_widestassumption_v3.r')

### OLD TRASH -----------------------------------------------------
# source('./consumption/plots/barplot_survey_v_faocatch_montecarlo_v2.r')
# 
# # script that compares survey diff to BNP
# source('./consumption/plots/bnp_surv_comparison_table.r')
# 
# source('./consumption/plots/barplot_surveyconsump_v_fao_inland_catch_v3.r')
# 
# ### prepare the trade corrected data for plotting
# source('./consumption/data_proc/consump_tr_corr_prep_for_plotting.r')
# 
# ### country barplot of survey and FAO catch
# source('./consumption/plots/barplot_surveyconsump_v_fao_inland_catch_v3.r')

### Unused ---------------------------------------------------------------------
# source('calc_diff_survey_fao_extrapolation_percdif_barplot.R')

#source('scatter_plot_consump_v_nat_inland_catch_sep8020_v2_werrbars.R')
 
### Post-Survey Catch Trend -----------------------
#source('./consumption/data_proc/catch_trend_postsurvey.r')