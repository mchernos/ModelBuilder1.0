shinyServer(function(input, output) {

  LiveData <- reactive({
    data %>% filter(Year %in% input$year_range[1]:input$year_range[2])})
  
  # Correlation Matrix
  output$corplot <- renderPlot({

    cor_matrix = cor(LiveData()[,!(colnames(LiveData()) %in% 
                                     c('row', 'col', 'Year'))]) # get correlations
    corrplot(cor_matrix, method = "ellipse", order = 'AOE') # plot matrix
    })


  # MULTIVARIATE REGRESSION
  # predictand <- reactive({input$predictand}) 
  model_output <- reactive({
    fit <- lm(LiveData()[,input$predictand]~. , 
              data=LiveData()[,!(colnames(LiveData()) %in% c('row', 'col', 'Year', input$predictand) ) ]   )
    stepAIC(fit, scope = list(upper = fit, lower = ~1), direction="both", steps = 1000)
    # step
    # fit.plot(data$fish_habitat, step)
  })
  
  # Regression Plot
  output$regplot <- renderPlot({
    fit.plot(LiveData()[,input$predictand], model_output() )
    mtext(input$predictand, font = 2, cex = 2, adj = -1)
    })
    # Stepwise Regression uing AIC as criterion
    # step$anova          # display results
    # summary(step)

  output$stat_summary <- renderPrint({ return(summary(model_output() )     )  })
  
  ########################
  # Time Series Analysis #
  ########################

  output$time_plot <- renderPlot({
    plot(LiveData()[,'Year'], LiveData()[,input$predictand], 
         pch = 19, col = rgb(0,0,0,0.6))
    # abline(lm(Year~input$predictand, data = LiveData()), col = 'red')
  })
  
  output$mannkendall <- renderPrint({ return(MannKendall(LiveData()[,input$predictand])) })
  
})# END SERVER

###########################################
# # Find Best Predictors of Riparian Health #
# ###########################################

# 
# data = data[,rev(colnames(data))]
# 

# mtext('Best Model Predictors: Elevation and Slope', adj = 1.5, font = 2)
# 
# # Test Model formula
# xx = step$fitted.values
# step_fits = coefficients(step)[1] + 
#   coefficients(step)[2]*data$Elevation + 
#   coefficients(step)[3]*data$Slope
# 
# 
# # Relative Importance
# library('relaimpo')
# calc.relimp(step,type=c("lmg","last","first","pratt"),rela=TRUE)
# 
# 
# # Test fit (Lesser models)
# fit1 = lm(`Riparian Health`~Crops + Slope + `Beaver Dams`,data=data)
# fit2 = lm(`Riparian Health`~Crops + Slope + `Beaver Dams` + Elevation,data=data)
# fit3 = lm(`Riparian Health`~Crops + Slope + Elevation,data=data) 
# fit4 = lm(`Riparian Health`~Slope + Crops, data=data)
# 
# calc.relimp(fit2,type=c("lmg","last","first","pratt"),rela=TRUE)
