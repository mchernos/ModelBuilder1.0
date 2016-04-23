source('helper.R')



# ModelBuilder User Interface
shinyUI(                
 fluidPage(
   titlePanel('ALCES ModelBuilder 1.0 (BETA)'),
  sidebarLayout(
    sidebarPanel(
      #helpText(h4('Subset Data')),
      # Action Button to effectuate subsetting
      actionButton("subsetButton", strong(h4("Update Data")), width = '100%'),
      br(),
      br(),
      selectInput('predictand', 'Predictand:',
                    choices = colnames(data)[-1:-length(non_data_cols)]
                  ),
      # Specification of range within an interval
      sliderInput("year_range", "Year Range:", step = 10,
                  1900,2060, c(1900,2060),
                  animate = TRUE,
                  sep=""),
      fluidRow(h5('Filter Based on Predictand Threshold:', align ='center'),
               column(6, numericInput("min_data", "Minimum:",0) ),
               column(6, numericInput("max_data", "Maximum:", 
                                      max(data[-1:-length(non_data_cols)], na.rm = T) ) ) ),
      
      # Specify Aggregation Method (if any)
      selectInput('Aggregate',  label = 'Calculate Central Tendency:', 
                  choices = c('None','Mean', 'Median', 'Sum'), 
                  selected = 'None', multiple = F),
      checkboxInput('code_zero', 'Code predictand 0s as "NA" '),
      checkboxInput('code_one', 'Code predictand 1s as "NA" '),
      selectizeInput('params',label = 'Select Parameters:',
                     choices = c('All',colnames(data)[!(colnames(data)%in% non_data_cols)]),
                     selected = 'All', multiple = T),
      uiOutput('region_subset'),
      downloadButton('downloadLiveData', 'Download Subset Data')
    ),
    
    #### MAIN PANEL ####
    mainPanel(navbarPage('',
        
        # ABOUT
        tabPanel('About', includeMarkdown('../Readme.Rmd')),
                         
      navbarMenu('Data Viewer',                  
        # RAW DATA TABLE
        tabPanel('Data Table', DT::dataTableOutput('datatable') ),
        
        # DISTRIBUTION FITTING
        tabPanel('Distribution Fitting', 
                 fluidRow(
                   column(6, selectInput('distribution', 'Fit Distribution:',
                                         choices = c('Normal' = 'norm', 
                                                     'Gamma' = 'gamma', 
                                                     'Log-Normal' = 'lnorm', 
                                                     'Weibull' = 'weibull', 
                                                     'Binomial' = 'nbinom', 
                                                     'Poission' = 'pois'),
                                         selected = 'norm', multiple = F)  ),
                   column(6, checkboxInput('automatic_breaks', 'Automatic Breaks? (Recommended)', 
                                           value = T) ) ),
                 fluidRow(
                   column(6, selectInput('fit_method', 'Optimization Method',
                                         choices = c('Maximum Likelihood' = "mle", 
                                                     'Method of Moments' = "mme", 
                                                     # 'Quantile Matching' = "qme", 
                                                     'Maximum Goodness-of-Fit' = "mge")) ), 
                   # column(6,
                   uiOutput('n_breaks') ),
                 column(12, plotOutput('histogram')),
                 column(2, downloadButton('downloadHistPlot')),
                 column(2, numericInput('HistPlot_width', 'Plot Width:', value = 6, min = 1)),
                 column(2, numericInput('HistPlot_height', 'Plot Height:', value = 5, min = 1)),
                 column(6, selectInput('legend_pos', 'Legend Position',
                                       choices = c('top', 'topright', 'topleft', 
                                                   'bottom', 'bottomleft', 'bottomright'))),
                 column(12, h4('Optomized Distribution Parameters')),
                 column(12, verbatimTextOutput('distribution_summary'))
        ),
        
        # CORRELATION MATRIX
        tabPanel('Correlation Matrix', 
                 plotOutput('corplot'),
                 column(4,checkboxInput('print_cor', 'Display Correlation Matrix?')),
                 column(2, downloadButton('downloadCorPlot')),
                 column(2, numericInput('CorPlot_width', 'Plot Width:', value = 5, min = 1)),
                 column(2, numericInput('CorPlot_height', 'Plot Height:', value = 5, min = 1)),
                 uiOutput('correlation_table')
                 )
      ),
        
      navbarMenu('Statistics',
                 
        # MULTIPLE REGRESSION AUTOMATOR
        tabPanel('Automated Mutiple Regression', 
                 plotOutput('regplot'),
                 column(2, downloadButton('downloadRegPlot')),
                 plotOutput('full_regplot'),
                 column(2, downloadButton('downloadRegPlot2')),
                 column(12,h4('Model Output')),
                 verbatimTextOutput('stat_summary'),
                 h4('Parameter Relative Importance'),
                 verbatimTextOutput('rel_impo'),
                 h4('Model Deviance'),
                 verbatimTextOutput('model_deva'),
                 h4('Model Anova'),
                 verbatimTextOutput('model_anova') ),
        
        # TREND ANALYSIS
        tabPanel('Bivariate Analysis',
                 column(6, selectInput('smooth_method', 'Smoothing Method',
                                       choices = c('Linear' = 'lm', 
                                                   'Logarithmic (Base 10)' = 'log',
                                                   'Exponential' = 'exp',
                                                   'General Additive' =  'gam', 
                                                   'Local Polynomial' = 'loess',
                                                   'Robust Linear' = 'rlm'),
                                       selected = 'loess') ),
                 column(6, selectInput('x_variable', 'Predictor (X Variable)', 
                                       choices = names(data), selected = 'Year' )  ),
                 column(6, numericInput('CI', 'Confidence Interval', 0.95, 
                                        min = 0, max = 1, step = 0.01)),
                 column(6, uiOutput('loess_span')),
                 column(6, uiOutput('gam_family')),
                 column(12, h6('Blue line corresponds to specified best fit, 
                               while shaded areas correspond to specified 
                               confidence intervals of chosen fit')),
                 column(12, plotOutput('manual_plot') ),
                 column(2, downloadButton('downloadTrendPlot')),
                 column(2, numericInput('TrendPlot_width', 'Plot Width:', value = 7, min = 1)),
                 column(2, numericInput('TrendPlot_height', 'Plot Height:', value = 5, min = 1)),
                 column(12, uiOutput('plot_stats')),
                 column(12, verbatimTextOutput('mannkendall')),
                 column(12, DT::dataTableOutput('summary_variables')) ),
        
        # PRINCIPAL COMPONENT ANALYSIS
        tabPanel('Principal Component Analysis',
                 plotOutput('pca_plot'),
                 h4('Output:'),
                 verbatimTextOutput('pca_summary') )
        )
      )
    )
  )
)) # Closes UI
