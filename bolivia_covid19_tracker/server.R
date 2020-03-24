source("Bolivia_covid19_tracker.R")


# crear funciones de servidor 
server <- function(input, output) { 
  
  #ultima fecha de actualizacion
  output$Refresh1 <- renderText({
    toString(paste("Última fecha de actualización: ", format(as.Date(ultima_fecha, format = "%m/%d/%y"), format = "%d/%m/%Y")))
  })
  
  #caja: total confirmados
  output$value1 <- renderValueBox({
    valueBox(value = formatC(total_confirmados, format="d", big.mark=','),
             subtitle = 'Total casos confirmados',
             color = 'purple')
    
    
  })
  
  
  #caja: total recuperados
  # output$value2 <- renderValueBox({
  #   
  #   valueBox(value = formatC(total_recuperados, format="d", big.mark=','),
  #            subtitle = 'Total recuperados',
  #            color = 'green')
  #   
  # })
  
  
  #caja: total fallecidos
  output$value2 <- renderValueBox({
    
    valueBox(value = formatC(total_fallecidos, format="d", big.mark=','),
             subtitle = 'Total fallecidos',
             color = 'yellow')
    
  })
  
  #grafico: bolivia vs ref
  output$paisRef <- renderPlot({
    plot_global
  })
  
  #grafico: bolivia vs sudam
  output$paisSudAm <- renderPlot({
    plot_sudam
  })
  
  
  
  #texto
  output$tabset1 <- renderText({
    
    input$tabset1
    
  })
  
}