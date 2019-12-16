server <- function(input, output) {
  library(tidyverse)
  library(plotly)
  #auto <- read.csv('auto.csv', stringsAsFactors = FALSE)  
  
  output$scatterplot <- plotly::renderPlotly({ # This is what the outputId is set to!!
    
    g <- ggplot(subset(gapminder::gapminder_unfiltered, year == input$year)) + 
      aes_string(y = input$y,         # From ui.R's selectInput lines.
                 x = input$x,
                 size = input$size,
                 color = 'continent',
                 label = 'country')  + 
      geom_text(alpha = 0.8, position = position_jitter()) +
      scale_size(range = c(2.5, 6)) +
      facet_wrap(~ continent) +
      guides(color = FALSE, size = FALSE) + 
      labs(col = '') + 
      theme_light() + 
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    
    #ggplotly(g)
    
    ggplotly(g, width = 1200, height = 800)
    
  })
  
  
}