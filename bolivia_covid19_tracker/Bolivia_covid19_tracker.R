library(readr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggrepel)
library(shiny)
library(shinydashboard)





###############
#funciones
###############

#tematica personalizada
opts <- function(base_size = 12, base_family = "sans"){
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      panel.grid.major = element_line(color = "grey"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "aliceblue"),
      strip.background = element_rect(fill = "darkgrey", color = "grey", size = 1),
      strip.text = element_text(face = "bold", size = 12, color = "white"),
      legend.position = "right",
      legend.justification = "top", 
      panel.border = element_rect(color = "grey", fill = NA, size = 0.5)
    )
}


#preparar datos para graficar
func_preparar_datos_plot <- function(df, paises_de_ref){
  
  paises <- c(pais_central, paises_de_ref)
  
  datos_plot = df %>% 
    filter(Country %in% c(pais_central, paises_de_ref)) %>%
    group_by(Country) %>%
    mutate(label = if_else(Day == max(Day), Country, NULL)) %>%
    ungroup() %>%
    mutate(Country = factor(Country, levels = rev(paises)))
  
  return(datos_plot)
  
}




#preparar colores para graficar
func_preparar_colores <- function(df, paises_de_ref){
  
  color_pal <- c(col_pais_central, 
                 rep(col_pais_ref, length(paises_de_ref)))
  
  names(color_pal) <- rev(levels(df[["Country"]]))
  
  return(color_pal)
  
}


#calcular doblaje
func_doblaje <- function(valor_inicio, tasa_doblaje, tiempo){
  
  out <- valor_inicio * 2 ^ (tiempo / tasa_doblaje)
  round(out)
}

#preparar matriz para graficar lineas de referencia

func_preparar_lineas_ref <- function(df){
  
  seq_dias <- seq(0, max(df[["Day"]]) + 1)
  
  datos_ref<- data.frame(cada_dia = sapply(seq_dias, function(x) func_doblaje(valor_inicio = min_casos_pais, tasa_doblaje = 1, tiempo = x )),
                         cada_dos_dias = sapply(seq_dias, function(x) func_doblaje(valor_inicio = min_casos_pais, tasa_doblaje = 2, tiempo = x )),
                         cada_tres_dias = sapply(seq_dias, function(x) func_doblaje(valor_inicio = min_casos_pais, tasa_doblaje = 3, tiempo = x )),
                         cada_semana = sapply(seq_dias, function(x) func_doblaje(valor_inicio = min_casos_pais, tasa_doblaje = 7, tiempo = x )),
                         cada_mes = sapply(seq_dias, function(x) func_doblaje(valor_inicio = min_casos_pais, tasa_doblaje = 30, tiempo = x ))) %>%
    melt() %>%
    dplyr::rename(Country = variable,
                  Count = value) %>%
    group_by(Country) %>%
    mutate(Day = row_number(),
           NumDays = max(Day),
           label = if_else(Count == max(Count), Country, NULL)) %>%
    ungroup()
  
  return(datos_ref)
  
  
}



###############
#importar datos
###############

#urls
url_confirmados = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'
url_fallecidos = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv'
#url_recuperados = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv'

#leer datos
datos_confirmados <- read_csv(url(url_confirmados))
datos_fallecidos <- read_csv(url(url_fallecidos))
#datos_recuperados <- read_csv(url(url_recuperados))


#############################
#asignar variables globales
############################

#casos minimos por pais
min_casos_pais <- 10

#ultima fecha de actualizacion
ultima_fecha <- names(datos_confirmados)[ncol(datos_confirmados)]
ultima_fecha_dmy <- as.character(format(strptime(ultima_fecha, "%m/%d/%y"), "%d/%m/%y"))

#paises
pais_central <- "Bolivia"
pais_ref <- c("Corea del Sur", "Italia", "España", "EEUU", "Iran", "Singapur")
pais_sudam <- c("Argentina", "Brasil", "Chile", "Colombia", "Paraguay", "Peru", "Uruguay", "Venezuela")

#color
col_pais_central <- "#D55E00" #rojizo
col_pais_ref <- "#999999" #gris



###################################
#calcular estadisticas globales
###################################


total_confirmados <- datos_confirmados %>% 
  filter(`Country/Region` == "Bolivia") %>%
  pull(.data[[ultima_fecha]])

total_fallecidos <- datos_fallecidos %>% 
  filter(`Country/Region` == "Bolivia") %>%
  pull(.data[[ultima_fecha]])

# total_recuperados <- datos_recuperados %>% 
#   filter(`Country/Region` == "Bolivia") %>%
#   pull(.data[[ultima_fecha]])




########################
#limpiar datos
########################

datos_limpio <- datos_confirmados %>% 
  rename(Country=`Country/Region`) %>% 
  mutate(Country=recode(Country, 
                        `Korea, South`='Corea del Sur',
                        US='EEUU',
                        Italy = 'Italia',
                        Spain = 'España',
                        Singapore = 'Singapur',
                        Brazil = 'Brasil')) %>% 
  mutate(Country = if_else(!is.na(`Province/State`) & 
                             `Province/State`=='Hong Kong', 'Hong Kong', Country)) %>% 
  select(-`Province/State`, -Lat, -Long) %>% 
  group_by(Country) %>% 
  summarize_all(sum) %>% 
  filter(.data[[ultima_fecha]] >= min_casos_pais) %>% 
  pivot_longer(cols = -Country, names_to='Date', values_to='Count') %>% 
  mutate(Date=mdy(Date)) %>%
  filter(Count >= min_casos_pais) %>%
  group_by(Country) %>%
  arrange(Country, Date) %>%
  group_by(Country) %>%
  mutate(Day = row_number(),
         NumDays = max(Day)) %>%
  ungroup() %>%
  select(-Date)

##########################################
# bolivia vs paises de referencia
##########################################


#preparar matriz y colores para graficar
datos_plot <- func_preparar_datos_plot(df = datos_limpio,
                                       paises_de_ref = pais_ref)

#preparar colores para graficar
color_pal <- func_preparar_colores(df = datos_plot,
                                   paises_de_ref = pais_ref)

#preparar matriz para graficar lineas de referencia
datos_ref <- func_preparar_lineas_ref(df = datos_plot)

#grafico bolivia vs paises de referencia
plot_global <- datos_plot %>%
  ggplot(aes(Day, Count,  color= Country, group = Country, label = label)) +
  geom_line(data = datos_ref, aes(Day, Count), linetype = "dotted", size = 1, colour = "black")+
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(0, max(datos_plot$NumDays) + 10, 5), 
                     minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma,
                minor_breaks=NULL,
                breaks = c(0, 10, 20, 30, 40, 50, 100, 250, 500, 750, 10^(3:ceiling(log10(max(datos_plot$Count))))),
                limits = c(10,10^(ceiling(log10(max(datos_plot$Count)))+1)))+
  scale_colour_manual(values = color_pal)+
  geom_label_repel(aes(label = label, color = Country),
                   nudge_x = 3,
                   nudge_y = 0.1)+
  annotate(geom = "text", x = 10, y = 500000, label = "Número de casos \nse duplica cada 1 día")+
  annotate(geom = "text", x = 27, y = 400000, label = "...cada 2 días")+
  annotate(geom = "text", x = 42, y = 400000, label = "...cada 3 días")+
  annotate(geom = "text", x = 48, y = 2000, label = "...cada semana")+
  annotate(geom = "text", x = 50, y = 25, label = "...cada mes")+
  guides(color = FALSE) +
  labs(x = str_glue('Número de Días desde el Caso Número {min_casos_pais}'),
       y = "Número de casos (Escala Log 10)",
       title = "Casos de Coronavirus en Bolivia Relativo a otros Paises de Referencia", 
       subtitle = str_glue("Número cumulativo de casos, ",
                           "por número de días desde el caso número {min_casos_pais}\n",
                           "Fecha mas reciente de actualización {ultima_fecha_dmy}"),
       caption = str_glue("Fuente: Johns Hopkins CSSE, https://github.com/CSSEGISandData/COVID-19 \n",
                          "Inspiración 1: FT graphic: John Burn-Murdoch / @jburnmurdoch \n", 
                          "Inspiración 2: https://blog.datawrapper.de/weekly-chart-coronavirus-growth/ \n",
                          "Código Fuente: https://github.com/visdatbolivia/visdatbolivia \n",
                          "Autor: @leo_byon"))+
  opts()
# 
#  plot_global+
#    ggsave("E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_global.png", width = 25, height = 20, units = "cm")





##########################################
# bolivia vs paises de sudamerica
##########################################


#preparar matriz y colores para graficar
datos_plot <- func_preparar_datos_plot(df = datos_limpio,
                                       paises_de_ref = pais_sudam)

#preparar colores para graficar
color_pal <- func_preparar_colores(df = datos_plot,
                                   paises_de_ref = pais_sudam)

#preparar matriz para graficar lineas de referencia
datos_ref <- func_preparar_lineas_ref(df = datos_plot)

#grafico bolivia vs paises de sudamerica
plot_sudam <- datos_plot %>%
  ggplot(aes(Day, Count,  color= Country, group = Country, label = label)) +
  geom_line(data = datos_ref, aes(Day, Count), linetype = "dotted", size = 1, colour = "black")+
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(0, max(datos_plot$NumDays) + 10, 5), 
                     minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma,
                minor_breaks=NULL,
                breaks = c(0, 10, 20, 30, 40, 50, 100, 250, 500, 750, 10^(3:ceiling(log10(max(datos_plot$Count))))),
                limits = c(10,10^(ceiling(log10(max(datos_plot$Count))) - 0.5)))+
  scale_colour_manual(values = color_pal)+
  geom_label_repel(aes(label = label, color = Country),
                   nudge_x = 0.2,
                   nudge_y = 0.1)+
  annotate(geom = "text", x = 6, y = 1500, label = "Número de casos \nse duplica cada 1 día")+
  annotate(geom = "text", x = 14, y = 1500, label = "...cada 2 días")+
  annotate(geom = "text", x = 15, y = 400, label = "...cada 3 días")+
  annotate(geom = "text", x = 16, y = 55, label = "...cada semana")+
  annotate(geom = "text", x = 17, y = 17, label = "...cada mes")+
  guides(color = FALSE) +
  labs(x = str_glue('Número de Días desde el Caso Número {min_casos_pais}'),
       y = "Número de casos (Escala Log 10)",
       title = "Casos de Coronavirus en Bolivia Relativo a otros Paises en Sudamérica", 
       subtitle = str_glue("Número cumulativo de casos, ",
                           "por número de días desde el caso número {min_casos_pais}\n",
                           "Fecha mas reciente de actualización {ultima_fecha_dmy}"),
       caption = str_glue("Fuente: Johns Hopkins CSSE, https://github.com/CSSEGISandData/COVID-19 \n",
                          "Inspiración 1: FT graphic: John Burn-Murdoch / @jburnmurdoch \n", 
                          "Inspiración 2: https://blog.datawrapper.de/weekly-chart-coronavirus-growth/ \n",
                          "Código Fuente: https://github.com/visdatbolivia/visdatbolivia \n",
                          "Autor: @leo_byon"))+
  opts()
# 
#  plot_sudam+
#    ggsave("E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_sudam.png", width = 25, height = 20, units = "cm")





