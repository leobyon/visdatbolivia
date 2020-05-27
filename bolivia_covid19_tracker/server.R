source("Bolivia_covid19_tracker_casos_y_decesos.R")
source("Bolivia_covid19_tracker_departamentos.R")

library(shiny)
library(shinydashboard)

library(readr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggrepel)

library(RJSONIO)
library(gt)

# create the server functions for the dashboard  
server <- function(input, output) { 
  
  
  output$Refresh1 <- renderText({
    toString(paste("Última fecha de actualización: ", format(as.Date(ultima_fecha_dmy, format = "%d/%m/%y"), format = "%d/%m/%Y")))
    
  })
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(value = formatC(total_confirmados, format="d", big.mark=','),
             subtitle = 'Total casos confirmados',
             color = 'purple')
    
    
  })
  
  
  # 
  # output$value2 <- renderValueBox({
  #   
  #   valueBox(value = formatC(total_recuperados, format="d", big.mark=','),
  #            subtitle = 'Total recuperados',
  #            color = 'green')
  #   
  # })
  
  
  
  output$value2 <- renderValueBox({
    
    valueBox(value = formatC(total_fallecidos, format="d", big.mark=','),
             subtitle = 'Total decesos',
             color = 'yellow')
    
  })
  
  #creating the plotOutput content
  output$boliviaDptoConfirm <- renderPlot({
    plot_dpto_confirm
  })
  
  output$boliviaDptoDeceso <- renderPlot({
    plot_dpto_fallecidos
  })
  
  output$paisRefConfirm <- renderPlot({
    plot_global_confirm
  })
  
  
  output$paisSudamConfirm <- renderPlot({
    plot_sudam_confirm
  })
  
  output$paisRefDeceso <- renderPlot({
    plot_global_fallecidos
  })
  
  
  output$paisSudamDeceso <- renderPlot({
    plot_sudam_fallecidos
  })
  
  
  #gt tables
  output$tb_dpto_doblaje_confir <-render_gt(
    tb_dpto_doblaje_confir,
    height = px(750),
    width = "100%"
  )
  
  output$tb_dpto_doblaje_fallecidos <-render_gt(
    tb_dpto_doblaje_fallecidos,
    height = px(750),
    width = "100%"
  )
  
  output$tb_sudam_doblaje_confirm <-render_gt(
    tb_sudam_doblaje_confirm,
    height = px(750),
    width = "100%"
  )
  
  output$tb_sudam_doblaje_fallecidos <-render_gt(
    tb_sudam_doblaje_fallecidos,
    height = px(750),
    width = "100%"
  )
  
  
  
  
  
  
  
  output$tabset1 <- renderText({
    
    input$tabset1
    
  })
  
}