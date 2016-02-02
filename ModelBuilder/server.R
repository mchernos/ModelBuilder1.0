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
    data = data %>% 
      filter(data[,input$predictand] >= input$min_data & 
               data[,input$predictand] <= input$max_data )
    
    # Return Data in data.frame() format with nice looking column names
    data = data.frame(data)
    colnames(data) = gsub('\\.',' ', colnames(data))
    data
  })
  
  # Data Table
  output$datatable <- DT::renderDataTable({ 
    DT::datatable( LiveData(), rownames = FALSE,
                   options = list(searching = F, 
                                  lengthMenu = c(10, 50,100)) ) })
  
  # Download LiveData()
  output$downloadLiveData <- downloadHandler(
    filename = function() { paste('ALCESModelBuilderData', Sys.Date(), '.csv', sep='') },
    content = function(file) {
      write.csv(LiveData(), file, row.names = F)
    }
  )
  
  
  ########################
  #  Correlation Matrix  #
  ########################
  cor_matrix = reactive({ cor(LiveData()[,!(colnames(LiveData()) %in% c('row', 'col', 'Year'))]) })
  
  # Output Plot and Table
  CorPlot <- function()({corrplot(cor_matrix(), method = "ellipse")})
  output$corplot <- renderPlot({  CorPlot() })
  
  output$corr_table <- DT::renderDataTable({ 
    DT::datatable(round(cor_matrix(),2), 
                  rownames = paste0('<b>', rownames(cor_matrix()), '</b>'),
                  options = list(paging = FALSE, searching = FALSE),
                  escape = F) })
  output$correlation_table <- renderUI({ if(input$print_cor) DT::dataTableOutput('corr_table') })
  
  # Save CorPlot
  output$downloadCorPlot <- downloadHandler(
    filename = paste('Correlation_Plot', Sys.Date(), '.pdf', sep=''),
    content = function(file) {
      pdf(file, width=input$CorPlot_width, height=input$CorPlot_height)
      CorPlot()
      dev.off()     }
    )
  
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
  
  # Regression Plots
  RegPlot <- function()({
    par(mfrow = c(1,2))
    fit.plot(LiveData()[,input$predictand], model_output() )
    # plot(model_output())
    })
  RegPlot2 <- function()({ par(mfrow = c(2,2)); plot(model_output()) })
  
  # Render
  output$regplot <- renderPlot({RegPlot() })
  output$full_regplot <- renderPlot({ RegPlot2() })
  
  # Save RegPlot
  output$downloadRegPlot <- downloadHandler(
    filename = paste(input$predictand, 'model', '.pdf', sep=''),
    content = function(file) {
      pdf(file, width=10, height=5)
      RegPlot()
      dev.off()     }
  )
  output$downloadRegPlot2 <- downloadHandler(
    filename = paste(input$predictand, 'model2', '.pdf', sep=''),
    content = function(file) {
      pdf(file, width=10, height=10)
      RegPlot2()
      dev.off()     }
  )
  
  # Model Output
  output$stat_summary <- renderPrint({ return(summary( model_output() ) )  })
  output$model_deva <- renderPrint({  model_output()$anova })
  output$model_anova <- renderPrint({  anova(model_output() ) })
  output$rel_impo <- renderPrint({ 
    calc.relimp(model_output(),type=c("lmg","last","first","pratt"),rela=TRUE)   })
  
  ##################
  # Trend Analysis #
  ##################
  
  # Conditional Additional Model Controls
  output$loess_span <- renderUI({ 
    if(input$smooth_method == 'loess'){numericInput('loess_span', 'Span:', 0.75, step = 0.1) } })
  output$gam_family <- renderUI({ 
    if(input$smooth_method == 'gam'){
      selectInput('gam_family', 'Family:', 
                  choices = c('Tweedie Distributed' = 'tw', 
                              'Negative Binomial' = 'nb', 
                              'Beta Regression' = 'betar', 
                              'Scaled t' = 'scat', 
                              'zero inflated Poisson' = 'ziP')) } })
  
  # The Plot
  output$manual_plot <- renderPlot({ TrendPlot() })
  TrendPlot <- reactive({
    LiveData() %>%
      ggplot(aes(x = get(input$x_variable), y = get(input$predictand))) + 
      geom_point() + 
      labs(y = input$predictand, x = input$x_variable) + 
      theme_bw() + 
      stat_smooth(method = if(input$smooth_method == 'log' | input$smooth_method == 'exp'){
        'lm'} else{input$smooth_method}, 
        span = input$loess_span,
        formula = if(input$smooth_method == 'log'){y~log10(x)}
        else if(input$smooth_method == 'exp'){y~exp(x)}
        else{y~x}, 
        method.args = list(if(input$smooth_method == 'gam'){family=input$gam_family}else{NULL} )
      )
  })
  
  # Download Graphic
  output$downloadTrendPlot <- downloadHandler(
    filename = function() { paste(input$predictand,'_',input$x_variable, '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = TrendPlot(), device = "pdf", 
             width = input$TrendPlot_width, height = input$TrendPlot_height)
    }
  )
  
  
  # Conditional Statistical Output
  output$mannkendall <- renderPrint({ 
    y = LiveData()[,input$predictand]
    x = LiveData()[,input$x_variable]
    
    if(input$x_variable == 'Year') {return(MannKendall(y))          }
    if(input$smooth_method == 'lm') {return(summary(lm(y~x)) )      }
    if(input$smooth_method == 'log') {return(summary(lm(y~log10(x)) ) ) }
    if(input$smooth_method == 'exp') {return(summary(lm(y~exp(x)  ) ) ) }
    if(input$smooth_method == 'loess') {return(summary(loess(y~x, span = input$loess_span)))                                   }
    if(input$smooth_method == 'gam') {return(summary(gam(y~x, family = input$gam_family)))                                   }                                 
    if(input$smooth_method == 'rlm') {return(summary(rlm(y~x )) ) }
  })
  
  output$plot_stats <- renderUI({ 
    if(input$x_variable == 'Year') {h4('Mann Kendall Test:')} 
    else{ h4('Model Coefficients and Statistics:')}
  })
  
  # Summary Table
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
  
  ########################
  # Distribution Fitting #
  ######################## 
  
  fitted_distribution <- reactive({
    fitdist( LiveData()[,input$predictand], input$distribution, 
             method = input$fit_method)     })
  
  # Histogram/Density Line
  HistPlot <- function()({
    if(input$automatic_breaks){
      denscomp(fitted_distribution(), 
               probability = F, 
               xlab = input$predictand, datacol = 'navy',
               legendtext = input$distribution,
               xlegend = input$legend_pos)
    }else{
      denscomp1(fitted_distribution(), 
                breaks = input$n_breaks,
                probability = F, 
                xlab = input$predictand, datacol = 'navy',
                legendtext = input$distribution,
                xlegend = input$legend_pos) }
  })
  output$histogram <- renderPlot({ HistPlot() })

  # Save HistPlot
  output$downloadHistPlot <- downloadHandler(
    filename = paste(input$predictand, input$distribution, '.pdf', sep=''),
    content = function(file) {
      pdf(file, width=input$HistPlot_width, height=input$HistPlot_height)
      HistPlot()
      dev.off()     }
  )
  
  # Summary Stats
  output$distribution_summary <- renderPrint({
    list(summary(fitted_distribution() ),
         gofstat(fitted_distribution() ),
         if(input$distribution == 'norm'){
           shapiro.test(LiveData()[,input$predictand])
         }else {NA}
    ) })
  
  # N Breaks?  
  output$n_breaks <- renderUI({ 
    if(input$automatic_breaks == F)  
      column(6,numericInput('n_breaks', 'Approximate Breaks:', round(dim(data)[1]/100), min = 1) ) })
  
  ################################
  # Principal Component Analysis #
  ################################
  
  pca <- reactive({
    prcomp(LiveData()[,!(colnames(LiveData()) %in% c('row', 'col', 'Year'))] ) 
  })
  
  output$pca_plot <- renderPlot({
    plot( pca(), type = 'l', pch = 19, 
          main = 'Principal Component Analysis' )})
  
  output$pca_summary <- renderPrint({  list(summary(pca()), pca() )})
  
})# END SERVER
