ui <- fluidPage(
  # Title
  titlePanel('Scatterplots with the gapminder dataset'),
  
  # Sidebar
  sidebarLayout(
    #Inputs
    sidebarPanel(
      # y-axis
      selectInput(inputId = 'y', label = 'Y-axis:',
                  choices = names(gapminder::gapminder_unfiltered),
                  selected = 'lifeExp'),
      # x-axis
      selectInput(inputId = 'x', label = 'X-axis:',
                  choices = names(gapminder::gapminder_unfiltered),
                  selected = 'gdpPercap'),
      # size
      selectInput(inputId = 'size', label = 'Size of points based on:',
                  choices = names(gapminder::gapminder_unfiltered),
                  selected = 'pop'),
      # year selector
      selectInput(inputId = 'year', label = 'Year:',
                  choices = sort(unique(gapminder::gapminder_unfiltered$year)),
                  selected = '1950')
  ),
  
  # Output
  mainPanel(
    plotly::plotlyOutput(outputId = 'scatterplot')
  )
  
 )

)