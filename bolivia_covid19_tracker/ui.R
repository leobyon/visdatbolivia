library(shiny)
library(shinydashboard)

library(readr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggrepel)

library(RJSONIO)
library(gt)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Bolivia COVID-19",
                          tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("Refresh1"))))))



#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Bolivia COVID-19 Tracker", tabName = "Principal", icon = icon("dashboard")),
    menuItem("Código Fuente", icon = icon("console",lib='glyphicon'),
             href = "https://github.com/visdatbolivia/visdatbolivia/tree/master/bolivia_covid19_tracker"),
    menuItem("@leo_byon", icon = icon("twitter"),
             href = "https://twitter.com/leo_byon"))
)


frow1 <- fluidRow(
  valueBoxOutput("value1", width = 6)
  ,valueBoxOutput("value2", width = 6)
)


frow2 <- fluidRow(
  
  box(
    #title = "Casos Confirmados en Bolivia a Nivel Departamental",
    #status = "primary",
    #solidHeader = TRUE,
    background = "purple",
    plotOutput("boliviaDptoConfirm", height = "600px")
  )
  
  ,box(
    #title = "Decesos en Bolivia a Nivel Departamental",
    #status = "primary",
    #solidHeader = TRUE,
    background = "yellow",
    plotOutput("boliviaDptoDeceso", height = "600px")
  ) 
  
)




frow2.5 <- fluidRow(
  
  
  box(
    #title = "Ritmo de Crecimiento de Casos Confirmados en Bolivia a Nivel Departamental",
    #status = "primary",
    #solidHeader = TRUE,
    background = "purple",
    gt_output(outputId = "tb_dpto_doblaje_confir")
  )
  
  ,box(
    #title = "Ritmo de Crecimiento de Decesos por Covid-19 en Bolivia a Nivel Departamental",
    #status = "primary",
    #solidHeader = TRUE,
    background = "yellow",
    gt_output(outputId = "tb_dpto_doblaje_fallecidos")
  ) 
  
  
  
)

frow3 <- fluidRow(
  
  box(
    #title = "Casos Confirmados en Bolivia Relativo a Paises de Sudamérica",
    #status = "primary",
    #solidHeader = TRUE,
    background = "purple",
    plotOutput("paisSudamConfirm", height = "600px")
  )
  
  ,box(
    #title = "Decesos en Bolivia Relativo a Paises de Sudamérica",
    #status = "primary",
    #solidHeader = TRUE,
    background = "yellow",
    plotOutput("paisSudamDeceso", height = "600px")
  ) 
  
)



frow3.5 <- fluidRow(
  
  box(
    #title = "Ritmo de Crecimiento de Casos Confirmados de Covid-19 en Bolivia Relativo a Paises de Sudamérica",
    #status = "primary",
    #solidHeader = TRUE,
    background = "purple",
    gt_output(outputId = "tb_sudam_doblaje_confirm")
  )
  
  ,box(
    #title = "Ritmo de Crecimiento de Decesos por Covid-19 en Bolivia Relativo a Paises de Sudamérica",
    #status = "primary",
    #solidHeader = TRUE,
    background = "yellow",
    gt_output(outputId = "tb_sudam_doblaje_fallecidos")
  ) 
  
)

frow4 <- fluidRow(
  
  box(
    #title = "Casos Confirmados en Bolivia Relativo a Paises de Referencia",
    #status = "primary",
    #solidHeader = TRUE,
    background = "purple",
    plotOutput("paisRefConfirm", height = "600px")
  )
  
  ,box(
    #title = "Decesos en Bolivia Relativo a Paises de Referencia",
    #status = "primary",
    #solidHeader = TRUE,
    background = "yellow",
    plotOutput("paisRefDeceso", height = "600px")
  ) 
  
)


frow5 <- fluidRow(
  tabBox(title = '',
         id = 'tabset1', height = '200', width = 12,
         tabPanel('Proposito', 
                  "Los expertos afirman que en este momento lo mas importante es el ritmo de crecimiento
                 de los casos confirmados y fallecidos y nó el número de casos y fallecidos en si. Por tanto, el propósito de 
                 esta página es monitorear el ritmo de crecimiento en casos confirmados de COVID-19 en Bolivia relativo a otros 
                 paises y de esta manera, ayudar guiar y evaluar las políticas implementadas por el gobierno. 
                 "),
         tabPanel('Datos', 
                  'Los datos provienen de Johns Hopkins University Center for Systems Science and Engineering y 
                  son actualizados una vez al día alrededor de las 23:59 UTC (20:59 en hora de Bolivia)'),
         tabPanel('Acerca del Autor',
                  'El autor es un ciudadano preocupado y no es un profesional calificado en temas de salud o epidemiología.
                  El autor puede ser contactado por email a visdatbolivia@gmail.com'),
         tabPanel('Descargo de Responsabilidad',
                  'Si bien se tomó mucho cuidado para brindar la información y el análisis que aparece en esta página, el autor
                  no garantiza su exactitud. La información y el análisis que aparece en esta página es meramente referencial y 
                  no constituye una información o análisis médico, epidemiológico o de índole profesional. El autor declina toda
                  responsabilidad por los daños y perjuicios que puedan emergir por el uso de la información o análisis 
                  que aparece en esta página.')
  )
)


# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow2.5, frow3, frow3.5, frow4, frow5)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Bolivia COVID-19 Tracker', header, sidebar, body, skin='red')


