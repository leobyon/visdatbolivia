library(shiny)
library(shinydashboard)



#Dashboard header 
header <- dashboardHeader(title = "Bolivia COVID-19",
                          tags$li(class = "dropdown", tags$a(HTML(paste(uiOutput("Refresh1"))))))

#Dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Bolivia COVID-19 Tracker", tabName = "Principal", icon = icon("dashboard")),
    menuItem("Código Fuente", icon = icon("console",lib='glyphicon'),
             href = "https://github.com/visdatbolivia/visdatbolivia/tree/master/bolivia_covid19_tracker"),
    menuItem("@leo_byon", icon = icon("twitter"),
             href = "https://twitter.com/leo_byon"))
)

#fila de principales indicadores
frow1 <- fluidRow(
  valueBoxOutput("value1")
  ,valueBoxOutput("value2")
  ,valueBoxOutput("value3")
)

#fila de graficos
frow2 <- fluidRow(
  
  box(
    title = "Casos de Coronavirus en Bolivia Relativo a otros Paises de Referencia",
    status = "primary",
    solidHeader = TRUE,
    plotOutput("paisRef", height = "600px")
  )
  
  ,box(
    title = "Casos de Coronavirus en Bolivia Relativo a otros Paises en Sudamérica",
    status = "primary",
    solidHeader = TRUE,
    plotOutput("paisSudAm", height = "600px")
  ) 
  
)

frow3 <- fluidRow(
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
                  son actualizados una vez al día alrededor de las 23:59 UTC (20:59 en hora de Bolivia). Los datos
                  de esta página se actualizarán entre las 21:00 y 00:00, hora de Bolivia'),
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


# cuerpo
body <- dashboardBody(frow1, frow2, frow3)


#finalizar
dashboardPage(title = '', header, sidebar, body, skin='red')

