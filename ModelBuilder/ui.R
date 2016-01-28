# ModelBuilder User Interface

shinyUI(fluidPage(
  title = 'ALCES ModelBuilder 1.0',
  sidebarLayout(
    sidebarPanel(
      helpText('Hello, World'),
      selectInput('predictand', 'Predictand', colnames(data)[-1:-3]),
      
      # Specification of range within an interval
      sliderInput("year_range", "Year Range:", step = 10,
                  1900,2060, c(1900,2060),
                  # min = min(data$Year), max = max(data$Year), 
                  # value = c(min(data$Year), max(data$Year)),
                  sep=""),
      fluidRow(h5('Filter Based on Predictand Threshold:', align ='center'),
        column(6, numericInput("min_data", "Minimum:",0) ),
        column(6, numericInput("max_data", "Minimum:", max(data) ) ) ),
      
      # Specify Aggregation Method (if any)
      selectInput('Aggregate',  label = 'Aggregate Data:', 
                  choices = c('None','Mean', 'Median', 'Sum'), 
                  selected = 'None', multiple = F),
      checkboxInput('code_zero', 'Code predictand 0s as "NA" '),
      checkboxInput('code_one', 'Code predictand 1s as "NA" ')#,
      # checkboxInput('log_transform', 'Log-Transform Predictand (Base 10)')
      # checkboxInput('Aggregate', label = 'Aggregate Data by Annual Mean?'),
      # checkboxInput('Sum', label = 'Aggregate Data by Annual Sum?')
      # selectInput('x', 'Build a regression model of mpg against:',
                  # choices = names(mtcars)[-1]),
      # radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   # inline = TRUE),
#       downloadButton('downloadReport')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('DataTable', DT::dataTableOutput('datatable') ),
        tabPanel('Correlation Matrix', 
                 plotOutput('corplot'),
                 checkboxInput('print_cor', 'Display Correlation Matrix?'),
                 uiOutput('correlation_table')),
        tabPanel('Mutiple Regression Builder', 
                 plotOutput('regplot'),
                 plotOutput('full_regplot'),
                 h4('Model Output'),
                 verbatimTextOutput('stat_summary'),
                 h4('Parameter Relative Importance'),
                 verbatimTextOutput('rel_impo'),
                 h4('Model Deviance'),
                 verbatimTextOutput('model_deva'),
                 h4('Model Anova'),
                 verbatimTextOutput('model_anova')),
        tabPanel('Trend Analysis', 
               selectInput('smooth_method', 'Smoothing Method',
                           choices = c('Linear' = 'lm', 
                                       'Generalized Linear' = 'glm',
                                       'General Additive' =  'gam', 
                                       'Local Polynomial' = 'loess',
                                       'Robust Linear' = 'rlm'),
                           selected = 'loess'),
                 plotOutput('time_plot'),
                  DT::dataTableOutput('summary_variables'),
                 h4('Mann Kendall Test:'),
                 verbatimTextOutput('mannkendall')),
        tabPanel('Distribution Fitting', 
                 # fluidRow(
                   # column(6, 
                          selectInput('distribution', 'Fit Distribution:',
                                      choices = c('Normal' = 'norm', 
                                                  'Gamma' = 'gamma', 
                                                  'Log-Normal' = 'lnorm', 
                                                  'Weibull' = 'weibull', 
                                                  'Binomial' = 'nbinom', 
                                                  'Poission' = 'pois'),
                                      selected = 'norm', multiple = F),
                          selectInput('fit_method', 'Optimization Method',
                                      choices = c('Maximum Likelihood' = "mle", 
                                                  'Method of Moments' = "mme", 
                                                  # 'Quantile Matching' = "qme", 
                                                  'Maximum Goodness-of-Fit' = "mge")), 
                          # ),
                   # column(6,numericInput('n_breaks', 'Breaks:', round(dim(data)[1]/100), min = 1))
                 # ),
                 plotOutput('histogram'),
                 h4('Optomized Distribution Parameters'),
                 verbatimTextOutput('distribution_summary'))
                 # h4('Mann Kendall Test:'),
                 # verbatimTextOutput('mannkendall'))
      )
    )
  )

)) # Closes UI
