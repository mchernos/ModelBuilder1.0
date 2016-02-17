# MISCELLANEOUS FUNCTIONS FOR MODELBUILDER
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