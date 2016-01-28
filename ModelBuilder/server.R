shinyServer(function(input, output) {
  
  ##################
  # Aggregate Data #
  ##################
  LiveData <- reactive({
    
    # Code Value as NA
    if(input$code_zero){
      data[,input$predictand] = ifelse(data[,input$predictand] == 0, NA, 
                                       data[,input$predictand] ) }
    if(input$code_one){
      data[,input$predictand] = ifelse(data[,input$predictand] == 1, NA, 
                                       data[,input$predictand] ) }
    
    # Conditional Aggregation Methods 
    if (input$Aggregate != 'None'){ 
      data = data %>%
        gather(Stat, Value,  -row, -col, -Year) %>%
        group_by(Year, Stat)
      
      if(input$Aggregate == 'Mean')   {
        data = data %>% 
          summarise(Value = mean(Value, na.rm = T)) %>% 
          spread(key = Stat, value =Value) }
      
      if(input$Aggregate == 'Median')  {
        data = data %>% 
          summarise(Value = median(Value, na.rm = T)) %>% 
          spread(key = Stat, value =Value) }
      
      if(input$Aggregate == 'Sum')     {
        data = data %>% 
          summarise(Value = sum(Value, na.rm = T)) %>% 
          spread(key = Stat, value =Value) }
    }
    
    # Filter by Year
    data = data %>% filter(Year %in% input$year_range[1]:input$year_range[2])
    
    # Filter by predictand range
    data = filter(data, data[,input$predictand] >= input$min_data & 
                    data[,input$predictand] <= input$max_data )
    
    # Logarithm Transform Predictand
#     data[,input$predictand] = ifelse(input$log_transform == T,
#                                      log10(data[,input$predictand]),
#                                      data[,input$predictand])
#     
    # Return Data in data.frame() format
    data.frame(data)
  })
  
  # Data Table
  output$datatable <- DT::renderDataTable({ 
    DT::datatable( LiveData(), rownames = FALSE,
                   options = list(searching = F, 
                                  lengthMenu = c(10, 50,100)) ) })
  
  ########################
  #  Correlation Matrix  #
  ########################
  cor_matrix = reactive({ cor(LiveData()[,!(colnames(LiveData()) %in% c('row', 'col', 'Year'))]) })
  
  # Output PLot and Table
  output$corplot <- renderPlot({corrplot(cor_matrix(), method = "ellipse", order = 'AOE')  })
  output$corr_table <- DT::renderDataTable({ 
    DT::datatable(round(cor_matrix(),2), options = list(paging = FALSE, searching = FALSE)) })
  output$correlation_table <- renderUI({ if(input$print_cor) DT::dataTableOutput('corr_table') })
  
  ###########################
  # MULTIVARIATE REGRESSION #
  ###########################
  model_output <- reactive({
    fit <- lm(LiveData()[,input$predictand]~. , 
              data=LiveData()[,!(colnames(LiveData()) %in% 
                                   c('row', 'col', 'Year', input$predictand) ) ]   )
    step = stepAIC(fit, scope = list(upper = fit, lower = ~1), 
                   direction="both", steps = 1000, trace = F)
    return(step)
  })
  
  # Regression Plot
  output$regplot <- renderPlot({ 
    par(mfrow = c(1,2))
    fit.plot(LiveData()[,input$predictand], model_output() )  })
  
  output$full_regplot <- renderPlot({ par(mfrow = c(2,2)); plot(model_output()) })
  
  # Model Output
  output$stat_summary <- renderPrint({ return(summary( model_output() ) )  })
  output$model_deva <- renderPrint({  model_output()$anova })
  output$model_anova <- renderPrint({  anova(model_output() ) })
  output$rel_impo <- renderPrint({ 
    calc.relimp(model_output(),type=c("lmg","last","first","pratt"),rela=TRUE)   })
  
  ########################
  # Time Series Analysis #
  ########################
  
  output$time_plot <- renderPlot({
    LiveData() %>%
      ggplot(aes(x = Year, y = get(input$predictand))) + 
      geom_point() + 
      labs(y = input$predictand) + 
      theme_bw() + 
      stat_smooth(method = input$smooth_method)
    })
  
  output$summary_variables <- DT::renderDataTable({
    x = LiveData()[,input$predictand]
    stats = paste0('<b>',c('Mean', 'St. Dev.','Mode','Skewness', 'Kurtosis' ),'</b>')
    stats2 = paste0('<b>',c( 'Min','25% Quartile','Median','75% Quartile','Max' ),'</b>')
    results = c(mean(x,na.rm = T), sd(x,na.rm = T), Mode(x), skewness(x), kurtosis(x) )
    results2 = c(quantile(x))
    
    DT::datatable(data.frame(stats, results, stats2, results2), 
                  options = list(paging = FALSE, searching = FALSE),
                                 colnames = rep('',4), rownames = FALSE,
                  escape = FALSE)
  })
  
  output$mannkendall <- renderPrint({ return(MannKendall(LiveData()[,input$predictand])) })
  
  ########################
  # Distribution Fitting #
  ######################## 
  
  fitted_distribution <- reactive({
    fitdist( LiveData()[,input$predictand], input$distribution, 
             method = input$fit_method)
  })
  
  # Histogram
  output$histogram <- renderPlot({
    denscomp(fitted_distribution(), probability = F, 
             xlab = input$predictand, datacol = 'navy',
             legendtext = input$distribution)
  })
  
  # Summary Stats
  output$distribution_summary <- renderPrint({
    list(summary(fitted_distribution() ),
         gofstat(fitted_distribution() ),
         if(input$distribution == 'norm'){
           shapiro.test(LiveData()[,input$predictand])
         }else {NA}
    ) })

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
