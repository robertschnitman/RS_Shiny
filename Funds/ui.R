ui <- fluidPage(
  # Title
  titlePanel('Price Analysis of Select Funds, 2007-Current'),
  
  # Sidebar
  sidebarLayout(
    #Inputs
    sidebarPanel(
      # y-axis
      selectInput(inputId = 'y', label = 'Y-axis for Correlations Tab:',
                  choices = c('SWPPX', 'ROGSX', 'SWMCX', 'SCHH', 'SCHM', 'SCHK', 'SCHF'),
                  selected = 'SWPPX'),
      # x-axis
      selectInput(inputId = 'x', label = 'X-axis for Correlations Tab:',
                  choices = c('SWPPX', 'ROGSX', 'SWMCX', 'SCHH', 'SCHM', 'SCHK', 'SCHF'),
                  selected = 'SCHK')
  ),
  
  # Output
  mainPanel(
    tabsetPanel(type = 'tabs',
                tabPanel('Correlations', plotly::plotlyOutput(outputId = 'scatterplot')),
                tabPanel('Annual Returns (Graph)', plotly::plotlyOutput(outputId = 'potg')),
                tabPanel('Annual Returns (Table)', tableOutput(outputId = 'pott')),
                tabPanel('Overall Daily Performance (Graph)', plotly::plotlyOutput(outputId = 'bars')),
                tabPanel('Overall Daily Performance (Table)', tableOutput(outputId = 'table'))
                )
    
  )
  
 )

)