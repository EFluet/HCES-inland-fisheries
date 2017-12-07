# format table of national data for the supplementary material

# do not use scientific format
options(scipen=999)

# read input data
f<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm.csv'
#f<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc_gamfm_diff.csv'
#f<-'../output/consumption/hh_survey_consump_nat_aggprod_refuse_fw_mc.csv'
nat <- read.csv(f, stringsAsFactors=FALSE)

# round values to the neares tonne (not frational tonnes)
nat[,c("mean", "p025", "p975")] <- format(round(nat[,c("mean", "p025", "p975")],6), nsmall=6)
# reconvert columns to numeric (to remove spacing before characters)
nat$mean <- as.numeric(nat$mean)
nat$p025 <- as.numeric(nat$p025)
nat$p975 <- as.numeric(nat$p975)




# format data table
nat<-nat %>%
    
    # exclude uncertaty entries
    filter(datatype != 'uncert_fwae_ur',
           datatype != 'uncert_prodfm_ur',
           datatype != 'uncert_fwae_matched_mc',
           datatype != 'uncert_fwae_assumed_mc') %>%
  
    # convert to thousand tonnes
    mutate(mean=mean*1000, 
           p025=p025*1000, 
           p975=p975*1000) %>%
  
    # rename labels
    mutate(datatype=ifelse(datatype=="cons_pretrcorr","HH Survey consumption\n(FWEA)",datatype),
           datatype=ifelse(datatype=="tr_corr_consump","HH Survey production\n(FWEA, trade corr.)",datatype),
           datatype=ifelse(datatype=="import","Freshwater Import\n(FishStatJ)",datatype),
           datatype=ifelse(datatype=="export","Freshwater Export\n(FishStatJ)",datatype),
           datatype=ifelse(datatype=="sum_catch","Inland Catch (FishStatJ)", datatype),
           datatype=ifelse(datatype=="sum_aquacul","Freshwater aquaculture\nproduction (FishStatJ)",datatype)) %>%
  
    mutate(combined_ci = paste("(", p025, " to ", p975, ")", sep=""),
           combined_vals = ifelse(combined_ci != "(NA to NA)", 
                                  paste(mean, combined_ci, sep=' '),
                                  mean)) %>%
    # remove unneeded columns
    select(-one_of( "X", "country.x", "country_code", "year_start","year_end", 
                    "grp","confidence_lvl","mean", "p025", "p975", "combined_ci")) %>%
    spread(datatype, combined_vals)



# change NA to 0 in entire df
nat[is.na(nat)] <- 0

# rename label column
names(nat)[names(nat) == 'country_label'] <- 'Country & Survey year'



nat<-nat[,c("Country & Survey year",
            "HH Survey consumption\n(FWEA)" ,
            "Freshwater aquaculture\nproduction (FishStatJ)",
            "Freshwater Export\n(FishStatJ)",
            "Freshwater Import\n(FishStatJ)", 
            "HH Survey production\n(FWEA, trade corr.)",
            "Inland Catch (FishStatJ)")]


write.csv(nat, '../docs/man/for_submission/SP_national_table_nov.csv', 
          row.names = FALSE)

