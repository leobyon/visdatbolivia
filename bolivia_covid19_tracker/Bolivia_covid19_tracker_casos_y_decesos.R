
library(readr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggrepel)
library(shiny)
library(shinydashboard)

library(gt)


save = FALSE


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
  
  seq_dias <- seq(0, max(df[["Day"]]) + 10)
  
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


#calcular tiempo doblaje

func_calcular_tiempo_doblaje <- function(df, intervalo_dias, ultima_semana){
  
  cum_final <- df$Count[nrow(df)- intervalo_dias * (ultima_semana - 1)]
  cum_inicio <- df$Count[nrow(df)-intervalo_dias * ultima_semana]
  
  if (length(cum_final) != 1 | length(cum_inicio) != 1){
    return(NA)
  } else {
    
    if (cum_final == cum_inicio){
      return("Sin Cambio")
    } else {
      
      tiempo_doblaje_en_dias <- (intervalo_dias*log(2))/(log(cum_final/cum_inicio))
      return(round(tiempo_doblaje_en_dias, 0))
      
    }
  }
}


# func_calcular_tiempo_doblaje <- function(df, intervalo_dias, ultima_semana){
#   
#   
#   
#   cum_final <- df$Count[nrow(df)- intervalo_dias * (ultima_semana - 1)]
#   cum_inicio <- df$Count[nrow(df)-intervalo_dias * ultima_semana]
#   
#   if (length(cum_final) != 1 | length(cum_inicio) != 1){
#     return(NA)
#   } else {
#     tiempo_doblaje_en_dias <- (intervalo_dias*log(2))/(log(cum_final/cum_inicio))
#     
#     if (length(tiempo_doblaje_en_dias) > 1 | is.infinite(tiempo_doblaje_en_dias[1])){
#       return(NA)
#     } else {
#       return(round(tiempo_doblaje_en_dias, 0))
#     }
#     
#   }
#   
# }


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
pais_sudam <- c("Argentina", "Brasil", "Chile", "Colombia", "Paraguay", "Peru", "Uruguay", "Venezuela", "Ecuador")

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



#########################################################
# casos confirmados:
#########################################################

########################
# limpiar datos
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
plot_global_confirm <- datos_plot %>%
  ggplot(aes(Day, Count,  color= Country, group = Country, label = label)) +
  geom_line(data = datos_ref, aes(Day, Count), linetype = "dotted", size = 1, colour = "black")+
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(0, max(datos_plot$NumDays) + 30, 5), 
                     minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma_format(accuracy = 1),
                minor_breaks=NULL,
                breaks = c(0, 10, 20, 30, 40, 50, 100, 250, 500, 750, 10^(3:ceiling(log10(max(datos_plot$Count))))),
                limits = c(10,10^(ceiling(log10(max(datos_plot$Count)))+1)))+
  scale_colour_manual(values = color_pal)+
  geom_label_repel(aes(label = label, color = Country),
                   nudge_x = 3,
                   nudge_y = 0.1)+
  annotate(geom = "text", x = 10, y = 10000000, label = "Número de casos \nse duplica cada 1 día")+
  annotate(geom = "text", x = 35, y = 10000000, label = "...cada 2 días")+
  annotate(geom = "text", x = 55, y = 10000000, label = "...cada 3 días")+
  annotate(geom = "text", x = 120, y = 1000000, label = "...cada semana")+
  annotate(geom = "text", x = 120, y = 100, label = "...cada mes")+
  guides(color = FALSE) +
  labs(x = str_glue('Número de Días desde el Décimo Caso Confirmado'),
       y = "Número de Casos Confirmados (Escala Log 10)",
       title = "Casos Confirmados de Covid-19 en Bolivia Relativo a Paises de Referencia", 
       subtitle = str_glue("Número cumulativo de casos confirmados, ",
                           "por número de días desde el décimo caso confirmado\n",
                           "Fecha mas reciente de actualización {ultima_fecha_dmy}"),
       caption = str_glue("Fuente: Johns Hopkins CSSE, https://github.com/CSSEGISandData/COVID-19 \n",
                          "Inspiración 1: FT graphic: John Burn-Murdoch / @jburnmurdoch \n", 
                          "Inspiración 2: https://blog.datawrapper.de/weekly-chart-coronavirus-growth/ \n",
                          "Código Fuente: https://github.com/leobyon/visdatbolivia \n",
                          "Autor: @leo_byon"))+
  opts()

if (save){
   plot_global_confirm+
      ggsave("E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_global_confirmados.png", width = 25, height = 20, units = "cm")
  
}





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
plot_sudam_confirm <- datos_plot %>%
  ggplot(aes(Day, Count,  color= Country, group = Country, label = label)) +
  geom_line(data = datos_ref, aes(Day, Count), linetype = "dotted", size = 1, colour = "black")+
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(0, max(datos_plot$NumDays) + 10, 5), 
                     minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma_format(accuracy = 1),
                minor_breaks=NULL,
                breaks = c(0, 10, 20, 30, 40, 50, 100, 250, 500, 1000, 2500, 5000, 7500, 10^(3:ceiling(log10(max(datos_plot$Count)))+1)),
                limits = c(10,10^(ceiling(log10(max(datos_plot$Count)))+1)))+
  scale_colour_manual(values = color_pal)+
  geom_label_repel(aes(label = label, color = Country),
                   nudge_x = 0.2,
                   nudge_y = 0.1)+
  annotate(geom = "text", x = 8, y = 1000000, label = "Número de casos \nse duplica cada 1 día")+
  annotate(geom = "text", x = 30, y = 1000000, label = "...cada 2 días")+
  annotate(geom = "text", x = 45, y = 1000000, label = "...cada 3 días")+
  annotate(geom = "text", x = 25, y = 50, label = "...cada semana")+
  annotate(geom = "text", x = 90, y = 50, label = "...cada mes")+
  guides(color = FALSE) +
  labs(x = str_glue('Número de Días desde el Décimo Caso Confirmado'),
       y = "Número de Casos Confirmados (Escala Log 10)",
       title = "Casos Confirmados de Covid-19 en Bolivia Relativo a Paises de Sudamérica", 
       subtitle = str_glue("Número cumulativo de casos confirmados, ",
                           "por número de días desde décimo caso confirmado\n",
                           "Fecha mas reciente de actualización {ultima_fecha_dmy}"),
       caption = str_glue("Fuente: Johns Hopkins CSSE, https://github.com/CSSEGISandData/COVID-19 \n",
                          "Inspiración 1: FT graphic: John Burn-Murdoch / @jburnmurdoch \n", 
                          "Inspiración 2: https://blog.datawrapper.de/weekly-chart-coronavirus-growth/ \n",
                          "Código Fuente: https://github.com/leobyon/visdatbolivia \n",
                          "Autor: @leo_byon"))+
  opts()

if (save){
   plot_sudam_confirm+
     ggsave("E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_sudam_confirmados.png", width = 25, height = 20, units = "cm")
  
}





#########################################################
# casos confirmados : tasa doblaje sudam:
#########################################################





df_tiempo_dupl <- plyr::ldply(as.character(unique(datos_plot$Country)), function(x){
  
  sub <- datos_plot %>% filter(Country == x) 
  
  semana_menos_uno <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 1)
  semana_menos_dos <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 2)
  semana_menos_tres <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 3)
  semana_menos_cuatro <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 4)
  
  return(c(x, semana_menos_uno, semana_menos_dos, semana_menos_tres, semana_menos_cuatro))
  
}) %>%
  `colnames<-`(c("País", "Última Semana", "Penúltima Semana", "Antepenúltima Semana", "Trasantepenúltima Semana")) %>%
  mutate_at(vars(contains("Semana")), ~paste(., "días")) %>%
  na_if(., "NA días") %>%
  replace(is.na(.), "NA") %>%
  replace(. == "Sin Cambio días", "Sin Cambio")



tb_sudam_doblaje_confirm <- df_tiempo_dupl %>%
  gt() %>%
  tab_header(
    title = "Ritmo de Crecimiento de Casos Confirmados de Covid-19 en Bolivia Relativo a Paises de Sudamérica",
    subtitle = "A base de las últimas cuatro semanas, la tabla muestra el ritmo de crecimiento 
    (cuantificado como tiempo de duplicación en número de días) de casos confirmados por intervalos semanales. 
    Es decir, muestra el ritmo de     crecimiento de la semana pasada, la semana anterior a la semana pasada 
    y asi sucesivamente hasta cubrir los últimos cuatro periodos semanales."
  ) %>%
  opt_align_table_header(align = "left") %>%
  data_color(
    columns = vars("Última Semana"),
    colors = c("snow4")
  ) %>%
  data_color(
    columns = vars("Penúltima Semana"),
    colors = c("snow3")
  ) %>%
  data_color(
    columns = vars("Antepenúltima Semana"),
    colors = c("snow2")
  ) %>%
  data_color(
    columns = vars("Trasantepenúltima Semana"),
    colors = c("snow1")
  ) %>%
  cols_align(align = "center") %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "red",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      rows = (País == "Bolivia")
    )) %>%
  tab_source_note(
    source_note = str_glue("Fecha mas reciente de actualización: {ultima_fecha_dmy}")
  ) %>%
  tab_source_note(
    source_note = "NA: En el intervalo semanal correspondiente, no hay suficientes datos para realizar el cómputo"
  ) %>%
  tab_source_note(
    source_note = "Sin Cambio: En el intervalo semanal correspondiente, no hubo cambio en el número de nuevos casos confirmados"
  ) %>%
  tab_source_note(
    source_note = "Fuente: Johns Hopkins CSSE, https://github.com/CSSEGISandData/COVID-19"
  ) %>%
  tab_source_note(
    source_note = "Código Fuente: https://github.com/leobyon/visdatbolivia \n"
  ) %>%
  tab_source_note(
    source_note = "Autor: @leo_byon"
  ) %>%
  tab_options(
    source_notes.font.size = "10px"
  )

if (save) {
   gtsave(tb_sudam_doblaje_confirm,
          "E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_sudam_confirmados_doblaje.png")
  
}













#########################################################
# casos fallecidos:
#########################################################

########################
# limpiar datos
########################

datos_limpio <- datos_fallecidos %>% 
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
plot_global_fallecidos <- datos_plot %>%
  ggplot(aes(Day, Count,  color= Country, group = Country, label = label)) +
  geom_line(data = datos_ref, aes(Day, Count), linetype = "dotted", size = 1, colour = "black")+
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(0, max(datos_plot$NumDays) + 30, 5), 
                     minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma_format(accuracy = 1),
                minor_breaks=NULL,
                breaks = c(0, 10, 20, 30, 40, 50, 100, 250, 500, 750, 10^(3:ceiling(log10(max(datos_plot$Count))))),
                limits = c(10,10^(ceiling(log10(max(datos_plot$Count)))+1)))+
  scale_colour_manual(values = color_pal)+
  geom_label_repel(aes(label = label, color = Country),
                   nudge_x = 3,
                   nudge_y = 0.1)+
  annotate(geom = "text", x = 10, y = 500000, label = "Número de decesos \nse duplica cada 1 día")+
  annotate(geom = "text", x = 27, y = 400000, label = "...cada 2 días")+
  annotate(geom = "text", x = 42, y = 400000, label = "...cada 3 días")+
  annotate(geom = "text", x = 100, y = 100000, label = "...cada semana")+
  annotate(geom = "text", x = 100, y = 50, label = "...cada mes")+
  guides(color = FALSE) +
  labs(x = str_glue('Número de Días desde el Décimo Deceso'),
       y = "Número de Decesos (Escala Log 10)",
       title = "Número de Decesos por Covid-19 en Bolivia Relativo a Paises de Referencia", 
       subtitle = str_glue("Número cumulativo de decesos, ",
                           "por número de días desde el décimo deceso\n",
                           "Fecha mas reciente de actualización {ultima_fecha_dmy}"),
       caption = str_glue("Fuente: Johns Hopkins CSSE, https://github.com/CSSEGISandData/COVID-19 \n",
                          "Inspiración 1: FT graphic: John Burn-Murdoch / @jburnmurdoch \n", 
                          "Inspiración 2: https://blog.datawrapper.de/weekly-chart-coronavirus-growth/ \n",
                          "Código Fuente: https://github.com/leobyon/visdatbolivia \n",
                          "Autor: @leo_byon"))+
  opts()

if (save){
   plot_global_fallecidos+
     ggsave("E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_global_fallecidos.png", width = 25, height = 20, units = "cm")

  
}




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
plot_sudam_fallecidos <- datos_plot %>%
  ggplot(aes(Day, Count,  color= Country, group = Country, label = label)) +
  geom_line(data = datos_ref, aes(Day, Count), linetype = "dotted", size = 1, colour = "black")+
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(0, max(datos_plot$NumDays) + 10, 5), 
                     minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma_format(accuracy = 1),
                minor_breaks=NULL,
                breaks = c(0, 10, 20, 30, 40, 50, 100, 250, 500, 1000, 2500, 5000, 7500, 10^(3:ceiling(log10(max(datos_plot$Count)))+1)),
                limits = c(10,10^(ceiling(log10(max(datos_plot$Count)))+1)))+
  scale_colour_manual(values = color_pal)+
  geom_label_repel(aes(label = label, color = Country),
                   nudge_x = 0.2,
                   nudge_y = 0.1)+
  annotate(geom = "text", x = 8, y = 80000, label = "Número de decesos \nse duplica cada 1 día")+
  annotate(geom = "text", x = 21, y = 50000, label = "...cada 2 días")+
  annotate(geom = "text", x = 34, y = 40000, label = "...cada 3 días")+
  annotate(geom = "text", x = 75, y = 10000, label = "...cada semana")+
  annotate(geom = "text", x = 75, y = 30, label = "...cada mes")+
  guides(color = FALSE) +
  labs(x = str_glue('Número de Días desde el Décimo Deceso'),
       y = "Número de Decesos (Escala Log 10)",
       title = "Número de Decesos por Covid-19 en Bolivia Relativo a Paises de Sudamérica", 
       subtitle = str_glue("Número cumulativo de decesos, ",
                           "por número de días desde el décimo deceso\n",
                           "Fecha mas reciente de actualización {ultima_fecha_dmy}"),
       caption = str_glue("Fuente: Johns Hopkins CSSE, https://github.com/CSSEGISandData/COVID-19 \n",
                          "Inspiración 1: FT graphic: John Burn-Murdoch / @jburnmurdoch \n", 
                          "Inspiración 2: https://blog.datawrapper.de/weekly-chart-coronavirus-growth/ \n",
                          "Código Fuente: https://github.com/leobyon/visdatbolivia \n",
                          "Autor: @leo_byon"))+
  opts()

if (save) {
  plot_sudam_fallecidos+
    ggsave("E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_sudam_fallecidos.png", width = 25, height = 20, units = "cm")
  
}



#########################################################
# decesos : tasa doblaje nivel sudam
#########################################################

df_tiempo_dupl <- plyr::ldply(as.character(unique(datos_plot$Country)), function(x){
  
  sub <- datos_plot %>% filter(Country == x) 
  
  semana_menos_uno <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 1)
  semana_menos_dos <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 2)
  semana_menos_tres <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 3)
  semana_menos_cuatro <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 4)
  
  return(c(x, semana_menos_uno, semana_menos_dos, semana_menos_tres, semana_menos_cuatro))
  
}) %>%
  `colnames<-`(c("País", "Última Semana", "Penúltima Semana", "Antepenúltima Semana", "Trasantepenúltima Semana")) %>%
  mutate_at(vars(contains("Semana")), ~paste(., "días")) %>%
  na_if(., "NA días") %>%
  replace(is.na(.), "NA") %>%
  replace(. == "Sin Cambio días", "Sin Cambio")




tb_sudam_doblaje_fallecidos <- df_tiempo_dupl %>%
  gt() %>%
  tab_header(
    title = "Ritmo de Crecimiento de Decesos por Covid-19 en Bolivia Relativo a Paises de Sudamérica",
    subtitle = "A base de las últimas cuatro semanas, la tabla muestra el ritmo de crecimiento 
    (cuantificado como tiempo de duplicación en número de días) de casos confirmados por intervalos semanales. 
    Es decir, muestra el ritmo de     crecimiento de la semana pasada, la semana anterior a la semana pasada 
    y asi sucesivamente hasta cubrir los últimos cuatro periodos semanales."
  ) %>%
  opt_align_table_header(align = "left") %>%
  data_color(
    columns = vars("Última Semana"),
    colors = c("snow4")
  ) %>%
  data_color(
    columns = vars("Penúltima Semana"),
    colors = c("snow3")
  ) %>%
  data_color(
    columns = vars("Antepenúltima Semana"),
    colors = c("snow2")
  ) %>%
  data_color(
    columns = vars("Trasantepenúltima Semana"),
    colors = c("snow1")
  ) %>%
  cols_align(align = "center") %>%
  tab_style(
    style = cell_borders(
      sides = c("top", "bottom"),
      color = "red",
      weight = px(1.5),
      style = "solid"
    ),
    locations = cells_body(
      rows = (País == "Bolivia")
    )) %>%
  tab_source_note(
    source_note = str_glue("Fecha mas reciente de actualización: {ultima_fecha_dmy}")
  ) %>%
  tab_source_note(
    source_note = "NA: En el intervalo semanal correspondiente, no hay suficientes datos para realizar el cómputo"
  ) %>%
  tab_source_note(
    source_note = "Sin Cambio: En el intervalo semanal correspondiente, no hubo cambio en el número de nuevos decesos"
  ) %>%
  tab_source_note(
    source_note = "Notas de cómputo: El periodo de análisis por dpto es a partir del décimo caso confirmado 
    Por tanto, dptos que no cumplen este criterio son excluidos y para los que cumplen, 
    el periodo analizado puede ser menor al periodo total desde su primer caso confirmado."
  ) %>%
  tab_source_note(
    source_note = "Fuente: Johns Hopkins CSSE, https://github.com/CSSEGISandData/COVID-19"
  ) %>%
  tab_source_note(
    source_note = "Código Fuente: https://github.com/leobyon/visdatbolivia \n"
  ) %>%
  tab_source_note(
    source_note = "Autor: @leo_byon"
  ) %>%
  tab_options(
    source_notes.font.size = "10px"
  )


if (save){
  gtsave(tb_sudam_doblaje_fallecidos,
         "E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_sudam_fallecidos_doblaje.png")
  
  
}
