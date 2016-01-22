# Alces Correlations to Riparian Health
rm(list = ls())
library('shiny')
library('dplyr')
library('tidyr')
library('corrplot')                     # package corrplot
library('MASS')
library('Kendall')


# Fit Plot FUNCTION
fit.plot = function(predictand, x){
  par(mfrow = c(1,2))
  # Plot Predicted Model against Observed Values
  plot(predictand, x$fitted.values, pch = 19,  # NEED TO FIX
       xlab = 'Observed', ylab = 'Predicted',
       col = rgb(0,0,0,0.6),
       main = '')
  abline(0,1, col = 'red', lwd = 2)
  
  # Plot Residuals
  plot(x$residuals, pch = 19, ylab = 'Residuals', col = rgb(0,0,0,0.6))
  abline(h = 0, lwd = 2, lty = 2, col = 'red')
}


####################
# Read in the Data #
####################

filelist = list.files('Data', pattern = '*.csv')

read.data = function(filename){
  temp = read.csv(paste0('Data/',filename)) %>%
    gather(Year, Value, -row, -col)
  temp$Year = gsub('X','',temp$Year)
  colnames(temp)[4] = gsub('.csv', '',filename)
  temp
}

# Read in and merge all datasets
# data = lapply(filelist, function(x) read.data(x)) %>%
#   Reduce(function(x,y) full_join(x,y, by = c('row', 'col', 'Year')), .)

# Y = F
# 
# data = data %>%
#   filter(Year %in% 1900:1930)
# if (Y){ 
# data = data %>%
#   gather(Stat, Value,  -row, -col, -Year) %>%
#   group_by(Year, Stat) %>% 
#   summarise(Value = mean(Value)) %>% 
#   spread(key = Stat, value =Value)
# }
# 
# 
# X %>% 
#   add(1) %>% 
#   {if(Y) add(1) else .}

# data = data[sample(nrow(data), 1000), ]
# TEMP DATA
data = read.csv('Data/data.csv')

# ggplot(aes(x = row, y = col, colour = fish_habitat), data = data) + geom_point()
data$Year = as.numeric(data$Year)
