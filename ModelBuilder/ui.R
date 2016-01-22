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
                  sep="")
      # selectInput('x', 'Build a regression model of mpg against:',
                  # choices = names(mtcars)[-1]),
      # radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                   # inline = TRUE),
#       downloadButton('downloadReport')
    ),
    mainPanel(
      tabsetPanel(
        tabPanel('Correlation Matrix', plotOutput('corplot') ),
        tabPanel('Mutiple Regression Builder', 
                 plotOutput('regplot'),
                 h4('Model Output'),
                 verbatimTextOutput('stat_summary')),
        tabPanel('Trend Analysis', 
                 plotOutput('time_plot'),
                 h4('Mann Kendall Test:'),
                 verbatimTextOutput('mannkendall'))
      )
    )
  )

)) # Closes UI
