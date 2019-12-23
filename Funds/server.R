server <- function(input, output) {
  
  ### 1. Load libraries and data
  library(tidyverse)
  library(plotly)
  library(knitr)
  library(kableExtra)
  
  funds <- c('SWPPX', 'ROGSX', 'SWMCX', 'SCHH', 'SCHM', 'SCHK', 'SCHF')
  
  for (i in funds) {
    
    quantmod::getSymbols(i, src = 'yahoo')
    
  }
  
  funds_data <- list(SWPPX, ROGSX, SWMCX, SCHH, SCHM, SCHK, SCHF) %>%
    set_names(funds) %>%
    map(as.data.frame) %>%
    map(~ mutate(.x, Date = rownames(.x)))
  
  stocks <- funds_data %>%
    plyr::join_all(by = 'Date')
  
  ### 2. Create daily-percent-change function.
  pctchg <- function(s, n = 1) {(s/lag(s, n)) - 1}
  
  ### 3. Calculate percent changes
  stocks2  <- stocks[, grep('Adjusted', names(stocks), value = TRUE)]
  
  pctchg_df <- map_dfc(stocks2, pctchg) %>%
    mutate(Date = stocks$Date) %>%
    na.omit()
  
  msd <- pctchg_df %>%
    select(contains('Adjusted')) %>%
    map(~ c(Mean = mean(.x, na.rm = TRUE), SD = sd(.x, na.rm = TRUE))) %>%
    do.call(rbind, .)
  
  msd2 <- msd*100
  
  rownames(msd2) <- gsub('\\.Adjusted', '', rownames(msd))
  
  names(stocks2) <- gsub('\\.Adjusted', '', names(stocks2))
  
  ### 4. Color setup
  mycolors <- c('forestgreen', 'salmon', 'gold', 'cyan3', 'tomato4', 'gray4', 'orchid4')
  
  col_alpha  <- apply(sapply(mycolors, col2rgb)/255, 
                      2, 
                      function(x) {
                        
                        rgb(x[1], x[2], x[3], alpha = 0.40)
                      }) 
  # REFERENCE: https://magesblog.com/post/2013-04-30-how-to-change-alpha-value-of-colours-in/
  
  
  ### Scatterplot
  output$scatterplot <- plotly::renderPlotly({ # This is what the outputId is set to!!
    
    g <- ggplot(stocks2) + 
      aes_string(y = input$y,         # From ui.R's selectInput lines.
                 x = input$x)  + 
      geom_jitter(alpha = 0.8, col = 'purple', aes(label = stocks$Date)) +
      scale_size(range = c(2.5, 6)) +
      guides(color = FALSE, size = FALSE) + 
      labs(col = '',
           label = 'Date',
           title = 'Adjusted Prices Since 2007') + 
      theme_light() + 
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank())
    
    #ggplotly(g)
    
    ggplotly(g, width = 1200, height = 800)
    
  })
  
  
  ### Bar chart
  output$bars <- plotly::renderPlotly({
    
    msd2 <- as.data.frame(msd2) %>%
      mutate(Fund = rownames(msd2))
    
    b <- ggplot(msd2, 
                aes(x = reorder(Fund, -Mean), 
                    y = Mean, 
                    fill = Fund)) + 
      geom_col(alpha = 0.5) + 
      #geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.1) +
      scale_fill_manual(values = mycolors, guide = FALSE) +
      labs(x = '',
           y = 'Mean Daily Returns (%)',
           fill = '',
           title = paste0('Average Daily % Returns Since ', min(stocks$Date))) + 
      guides(fill = FALSE) + 
      theme_light() +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = 'none')
    
    ggplotly(b, width = 1200, height = 800)
    
  })
    
  ### Table
  output$table <- renderTable({
    
    format_pct <- function(x, d) {
      
      apply(x, 2, function(x) ifelse(!is.na(x), paste0(round(x, d), '%'), ''))
      
    }
    
    msd3 <- format_pct(msd2, 2)[order(-msd2[, 'Mean']), ]
    
    msd3 %>%
      as.data.frame() %>%
      mutate(Fund = rownames(msd3)) %>%
      select(Fund, everything())
      #kable(., booktabs = TRUE, type = 'html', linesep = '', align = 'r') %>%
      #kable_styling(full_width = TRUE)
    
    

    
  })
  


  
  
}
