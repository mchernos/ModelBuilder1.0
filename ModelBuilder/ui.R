# ModelBuilder User Interface

shinyUI(fluidPage(
  # title = 'Correlation Matrix',
  sidebarLayout(
    sidebarPanel(
      helpText('Hello, World'),
      selectInput('predictand', 'Predictand', colnames(data)[-1:-3]),
      # Specification of range within an interval
      sliderInput("year_range", "Year Range:", 
                  min = min(data$Year), max = max(data$Year), 
                  value = c(min(data$Year), max(data$Year)),
                  sep=""),
      fluidRow(
        column(6, numericInput("min_data", "Minimum:", 
                                    min(data[,!(colnames(data) %in% c('row','col','Year') ) ]))        ),
        column(6, numericInput("max_data", "Maximum:", 
                                    round(max(data[,!(colnames(data ) %in% c('row','col','Year') ) ])) ) )
      ),
      
      # sliderInput("data_range", "Filter Data on Predictand Values:", min = 0, 
                  # max = round(max(LiveData()[,!(colnames(LiveData() ) %in% c('row','col','Year') ) ]) ), 
                  # value = c(0,10)),
      selectInput('Aggregate',  label = 'Aggregate Data:', 
                  choices = c('None','Mean', 'Median', 'Sum'), selected = 'None', multiple = F)
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
        tabPanel('DataTable', dataTableOutput('datatable') ),
        tabPanel('Correlation Matrix', plotOutput('corplot') ),
        tabPanel('Mutiple Regression Builder', 
                 plotOutput('regplot'),
                 h4('Model Output'),
                 verbatimTextOutput('stat_summary')),
        tabPanel('Trend Analysis', 
                 plotOutput('time_plot'),
                 h4('Mann Kendall Test:'),
                 verbatimTextOutput('mannkendall')),
        tabPanel('Distribution Fitting',  
                 numericInput('n_breaks', 'Breaks:', round(dim(data)[1]/100), min = 1),
                 plotOutput('histogram') )
                 # h4('Mann Kendall Test:'),
                 # verbatimTextOutput('mannkendall'))
      )
    )
  )

)) # Closes UI
