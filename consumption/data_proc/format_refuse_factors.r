### EFluet note: 
# - modified sheet names to remove underscores within the country name, 
#   but kept between country name and years
# - copied the list of 


### Import libraries #--------------------------------------------------------
library(openxlsx)
library(stringr)
library(dplyr)
library(countrycode)



# read conversion factors table
consump_df <- read.csv('../output/consumption/hh_survey_consump_sep_prods.csv',
                       stringsAsFactors=F)

consump_df <- consump_df %>%
  mutate(country_code = countrycode(country.x, 'country.name', 
                                    'iso3c', warn = TRUE))



### Read in data and sheet names #----------------------------------------------

# Set directory for input file
file_path <- "../data/consumption/refuse_factors/hhsurveys_refuse_factors_final_modEF.xlsx"


# load workbook and get the sheet names
wb <- loadWorkbook(file_path, xlsxFile = NULL)
# exclude the first two sheets that do not match the country table format.
sheet_names <- names(wb)[-1]

# divide sheet names into two vectors
sheet_cntry <- unlist(lapply(strsplit(sheet_names,"_"), function(x) x[1]))
sheet_yrs <- unlist(lapply(strsplit(sheet_names,"_"), function(x) x[2]))


comb_df <- data.frame(X1 = numeric(0), 
                      X2 = character(0), 
                      X3 = numeric(0),
                      country = character(0),
                      years = character(0),
                      stringsAsFactors = FALSE)


### Loop through the excel sheets #---------------------------------------------
for (i in 1:length(sheet_names)) {
  
  # read in one sheet of excel file
  df <- read.xlsx(xlsxFile = file_path, sheet = sheet_names[i], 
                  skipEmptyRows = TRUE, colNames = FALSE)
  
  # filter rows in sheet and apprend country-year columns
  df <- df %>%
        filter(!is.na(X3),
               X3 != "refuse factor",
               X3 != "Refuse Factor") %>%
        mutate(X1 = as.numeric(X1),
               X2 = as.character(X2),
               X3 = as.numeric(X3),
               country = sheet_cntry[i],
               years = sheet_yrs[i])
  
  comb_df <- bind_rows(comb_df, df)
  
}

# rename columns
names(comb_df) <- c("item_code","food_descrip","refuse_factor", 
                    "country", "year")


refuse_factors <- comb_df %>%
  # Remove word fish from strings of both tables to improve match
  mutate(food_descrip = gsub("'","", food_descrip),
         country_code = countrycode(country, 'country.name', 
                                    'iso3c', warn = TRUE),
         food_descrip = tolower(food_descrip)) 
  # remove item code, because unused in the match with consumed products
  #select(-item_code)
  


# join refuse factors with years
refuse_factors <- refuse_factors %>%
  semi_join(., unique(consump_df[,c('country_code','year')]), 
            by=c('country_code'='country_code', 'year'='year')) %>%
  unique(.)


# 
consump_df <- consump_df %>%
  #select(country.x, year, country_code, prod_name) %>%
  left_join(., refuse_factors, by=c('country_code'='country_code',
                                    'year'='year',
                                    'prod_name'='food_descrip')) %>%
  mutate(pre_refuse_consump=consump_million.tons.yr,
         refuse_factor = ifelse(is.na(refuse_factor), 0, refuse_factor),
         consump_million.tons.yr= consump_million.tons.yr/ (1-refuse_factor/100),
         Averageediblequantityconsumedgpersonday_wrefuse=  Averageediblequantityconsumedgpersonday/ (1-refuse_factor/100))


# sum per country for verification
# consump_df2 <- consump_df2 %>%
#                 group_by(country_code) %>%
#                 summarize(sum_consump = (sum(consump_million.tons.yr)),
#                           sum_consump2 = (sum(consump_million.tons.yr2)))
  





# read conversion factors table
write.csv(consump_df, '../output/consumption/hh_survey_consump_sep_prods_refuse.csv')





rm(df, file_path, i, sheet_cntry, sheet_names, sheet_yrs, comb_df, wb)


