ui <- fluidPage(
  # Title
  titlePanel('Adjusted Daily Prices of Select Funds, 2007-Current'),
  
  # Sidebar
  sidebarLayout(
    #Inputs
    sidebarPanel(
      # y-axis
      selectInput(inputId = 'y', label = 'Y-axis for Daily Comparisons:',
                  choices = c('SWPPX', 'ROGSX', 'SWMCX', 'SCHH', 'SCHM', 'SCHK', 'SCHF'),
                  selected = 'SWPPX'),
      # x-axis
      selectInput(inputId = 'x', label = 'X-axis for Daily Comparisons:',
                  choices = c('SWPPX', 'ROGSX', 'SWMCX', 'SCHH', 'SCHM', 'SCHK', 'SCHF'),
                  selected = 'SCHK')
  ),
  
  # Output
  mainPanel(
    tabsetPanel(type = 'tabs',
                tabPanel('Daily Comparisons', plotly::plotlyOutput(outputId = 'scatterplot')),
                tabPanel('Overall (Graph)', plotly::plotlyOutput(outputId = 'bars')),
                tabPanel('Overall (Table)', tableOutput(outputId = 'table'))
                )
    
  )
  
 )

)