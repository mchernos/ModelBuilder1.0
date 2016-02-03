# Alces Correlations to Riparian Health
# Clean up workspace
rm(list = ls())

# Check for installed packages (install if need be)
packages = c('shiny', 'dplyr', 'tidyr','corrplot', 'DT', 
             'MASS', 'ggplot2', 'Kendall', 'fitdistrplus', 
             'e1071', 'relaimpo', 'mgcv')
x = lapply(packages, function(x){if (!require(x, character.only = T)) install.packages(x)})
x = lapply(packages, require, character.only = T)
rm(x, packages)
source('ModelBuilder/denscomp1.R')

# Fit Plot Function
fit.plot = function(predictand, x){
  
  # Plot Predicted Model against Observed Values
  plot(na.omit(predictand), x$fitted.values, pch = 19, 
       xlab = 'Observed', ylab = 'Predicted',
       col = rgb(0,0,0,0.6),
       main = '')
  mtext('Predicted vs. Observed', 3)
  abline(0,1, col = 'red', lwd = 2)
  
  # Plot Residuals
  plot(x$residuals, pch = 19, ylab = 'Residuals', 
       col = rgb(0,0,0,0.6),
       main = '')
  mtext('Indexed Residuals', 3)
  abline(h = 0, lwd = 2, lty = 2, col = 'red')
}

# Mode Function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

####################
# Read in the Data #
####################

filelist = list.files('Data', pattern = '*.csv')

# Function to read in the Data
read.data = function(filename){
  temp = read.csv(paste0('Data/',filename)) %>%
    gather(Year, Value, -row, -col)
  temp$Year = gsub('X','',temp$Year)
  colnames(temp)[4] = gsub('.csv', '',filename)
  temp
}

# Read in and merge all datasets
data = lapply(filelist, function(x) read.data(x)) %>%
  Reduce(function(x,y) full_join(x,y, by = c('row', 'col', 'Year')), .) %>%
  sample_n(2500)


# TEMP DATA
# data = read.csv('Data/data2.csv')

data$Year = as.numeric(data$Year)
