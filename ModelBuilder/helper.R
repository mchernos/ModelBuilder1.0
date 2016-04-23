# Alces Correlations to Riparian Health
# Clean up workspace
rm(list = ls())

# Check for installed packages (install if need be)
packages = c('shiny', 'dplyr', 'tidyr','corrplot', 'DT', 
             'MASS', 'ggplot2', 'Kendall', 'fitdistrplus', 
             'e1071', 'relaimpo', 'mgcv', 'rmarkdown')
x = lapply(packages, function(x){if (!require(x, character.only = T)) install.packages(x)})
x = lapply(packages, require, character.only = T)
rm(x, packages)

############ LOAD FUNCTIONS ###################
# source('denscomp1.R') # Density histogram plot for interactive breaks
source(file.path('ModelBuilder', 'denscomp1.R'))
# source('fitfuncs.R') # Fit Functions
source(file.path('ModelBuilder', 'fitfuncs.R'))

####################
# Read in the Data #
####################

# filelist = list.files('Data', pattern = '*.csv')
filelist = list.files(file.path('ModelBuilder','Data'), pattern = '*.csv')

# Function to read in the Data
read.data = function(filename){
  # temp = read.csv(paste0('ModelBuilder/','Data/',filename))
  temp = read.csv(file.path('ModelBuilder','Data',filename))
    if(colnames(temp)[1] == 'name'){
      temp = gather(temp, Year, Value, -name)
    }else {
      temp = gather(temp, Year, Value, -row, -col) 
      }
  temp$Year = gsub('X','',temp$Year)
  colnames(temp)[length(temp)] = gsub('.csv', '',filename)
  temp
}

# Need to use as template for reading in all the rest of 'em
temp = read.csv(file.path('ModelBuilder','Data',filelist[1]))
regionalized = (colnames(temp)[1] == 'name')
if(regionalized){   non_data_cols = c('name', 'Year')
  }else {           non_data_cols = c('row', 'col', 'Year') }

# Read in and merge all datasets, Conditional on if data are regionalized
  data = lapply(filelist, function(x) read.data(x)) %>%
    Reduce(function(x,y) full_join(x,y, by = non_data_cols), .)

  if(!regionalized){ data = data %>% sample_n(2500) }
  
data$Year = as.numeric(data$Year)
rm(temp)

# END HELPER #