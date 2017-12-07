### Information #---------------------------------------------------------------
# Description: This script formats and parses the ADePT household surveys into a
# standard machine readable format. Code intakes one excel file with
# multiple sheets, and outputs a number of files (6) organized by sections 
# cutting across all countries. 

# Note: The original ADePT databasse has been slightly modified manually:
#  - Removed some totals functions besides the Comsumption of Fish Statistics.
#  - Replace "_" between country and year in sheetname by " " for consistency.
#    This was done for Congo and Cote d'Ivoire, Sudan former, Timor Leste.
#  - Added two missing table headers of 'Food item protein consumption at 
#    national level
#    for Kazakhstan 2005, 2011.
#  - Modified year in sheet name for conformity of Sri Lanka and Ethipia,
#  - from '1999-2000' to '1999-00'

#' ---
#' author: Etienne Fluet
#' date: 10 June 2015
#' ---



### Import libraries #----------------------------------------------------------
library(openxlsx)
library(stringr)
library(dplyr)


### Read in data and sheet names #----------------------------------------------

# Set directory for input file
file_path <- "..\\data\\consumption\\FishData_HHSurveys_ADePT-FSM.xlsx"


# load workbook and get the sheet names
wb <- loadWorkbook(file_path, xlsxFile = NULL)
sheet_names <- names(wb)


# exclude the first two sheets that do not match the country table format.
sheet_names <- sheet_names[-(1:2)]


# List the section titles to be used for splitting the dataframe
section_names <- c('Food consumption of fish and fish products',
                   'Contribution of Fish and fish products food groups',
                   'Consumption statistics of fish',
                   'Food item protein consumption at national level',
                   'Food item protein consumption by area',
                   'Food item protein consumption by region')

# Create a concatenated version of section name list - for matching purposes.
section_names_concat <- paste(section_names, collapse = "|")



### Create empty dataframes for each section  #---------------------------------

# loop through section names in the list 
for (j in section_names) {
  
  # replace spaces by underscore in the string
  j <- gsub(" ", "_", j)
  
  # create empty data frame
  temp_df <- data.frame()
  
  # assign the string as the dataframe name
  assign(j, temp_df)
  
}

# remove unneeded variables
rm(temp_df, j, wb)


### Loop through the excel sheets #---------------------------------------------
for (i in 1:length(sheet_names)) {

  
  # read in one sheet of excel file
  df <- read.xlsx(xlsxFile = file_path, sheet = sheet_names[i], 
                  skipEmptyRows = TRUE, colNames = FALSE)

  # remove empty column and rows (not memory efficient in this way)
  df <- df[rowSums(is.na(df)) < ncol(df),]

  # get the index of the rows matching the headers and a logical vector of the match.
  # get a logic vector of length of rows matching the section title in first column.
  section_header_logic_vec <- grepl(section_names_concat, df[,1])
  
  # get a vector containing index of rows where matching section headers are found.
  section_header_idx <- grep(section_names_concat, df[,1], value = FALSE)
  
  
  ### Loop through the match logical vector and add  #--------------------------
  for (j in seq(length(section_header_logic_vec))) {
    
    # if the logic vector says TRUE ad that index is the index list 
    # (criteria are redundant - but I included both for safety)
    if (section_header_logic_vec[j] == TRUE & j %in% section_header_idx) { 
      curr_section_name <- df[j,1]}
    
    # write the section name to its own column
    df[j,"section"] <- curr_section_name
    }
  

  # Remove rows of section headers, as section names are now stored in a column.
  df <- df[-section_header_idx,]  
  
  # Add columns for year and country to each row
  df$year <- strsplit(sheet_names[i], "_")[[1]][2]  
  df$country <- strsplit(sheet_names[i], "_")[[1]][1]

  
  ### Assemble section df of all countries #------------------------------------
  #Loop through section names, split df by section and rbind sections 
  # from different countries together.
  
  for (j in section_names) {
    

    # select rows containing setion name
    temp_df <- df %>% filter(grepl(j, section))

    print(paste(i, sheet_names[i], j, nrow(temp_df), sep="  -  "))
    
    
    if (nrow(temp_df) > 0 ) {
      
      # Change column names and order #-------------------------------------------
      
      # find index of section column
      sec_col_idx <- grep("section", colnames(temp_df))
      
      # Name column headers of some columns, remove "*" to match
      newcolnames <- gsub("[*]", "", temp_df[1, 1:sec_col_idx-1])
      
      # Name column headers of some columns, remove "*" to match
      colnames(temp_df)[1:sec_col_idx-1]  <- newcolnames
      
      temp_df <- temp_df[-1, ]  # Remove first row that contained the headers
      colnames(temp_df)[1] <- "FishType" # Rename the first column as Region
      
      #switch(j == "")  #assign(parse(j), temp_df)    #eval(parse(text = j)) 
      
      # remove empty column and rows (not memory efficient in this way)
      temp_df <- temp_df[, colSums(is.na(temp_df)) != nrow(temp_df)]
      
      # make vector of new column order, varying with ncol
      new_col_order <- c(ncol(temp_df),
                         ncol(temp_df)-1,
                         ncol(temp_df)-2,
                         c(seq(ncol(temp_df)-3)))
      #print(new_col_order)
      
      # modify order of columns
      temp_df <- temp_df[,c(new_col_order)]
      
      # replace spaces by underscores
      j <- gsub(" ", "_", j)
      
      # append the rows iteratively 
      temp_df <- bind_rows(list(get(j), temp_df))
      
      # assign the 
      do.call("<-",list(j, temp_df))
    }
  }
}


### Write output to csv filesfor each section #---------------------------------
for (j in section_names) {
  
  # replace spaces by underscores
  j <- gsub(" ", "_", j)
  
  # declare string of output file and directory 
  output_file <- paste("../output/consumption/", j,".csv", sep="")
  
  #get(j)[,c("country","year"),1:(ncol(j)-2)]
  
  # write file
  write.csv(get(j), output_file, row.names=FALSE)
  }


### Delete temporary objects and variables -------------------------------------
rm(i, file_path, section_names_concat, section_header_logic_vec, j, temp_df,
   output_file, df, section_names, section_header_idx, curr_section_name,
   sheet_names, new_col_order, newcolnames, sec_col_idx)
