

### 

# Read data file
c <- read.csv('../data/consumption/Asia-Pacific exel files/Master_file_EF_compil_cleaned.csv')


# Calculate daily consumption based on the 
c$Daily.fish.consumption..g.person.day.[is.na(c$Daily.fish.consumption..g.person.day.)] <-   
  (c$Annual.fish.consumption..kg.pearson.yr.[is.na(c$Daily.fish.consumption..g.person.day.)]/365*1000)


# Delete columns
c$Annual.fish.consumption..kg.pearson.yr. <- NULL
c$Measure <- NULL
c$Source <- NULL
c$section <- NA

# reorder the columns in the same order as the other surveys
c <- c[,c(1,2,6,4,5,3)]

# filter unwanted entries
c <- c %>%
     filter(Fish.Type != 'Rice')
