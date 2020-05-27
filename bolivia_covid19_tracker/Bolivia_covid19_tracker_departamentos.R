
library(readr)
library(tidyverse)
library(lubridate)
library(reshape2)
library(ggrepel)
library(shiny)
library(shinydashboard)

library(RJSONIO)
library(gt)


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
func_preparar_datos_plot <- function(df){
  
  datos_plot = df %>% 
    group_by(Dpto) %>%
    mutate(label = if_else(Day == max(Day), Dpto, NULL)) %>%
    ungroup() %>%
    mutate(Dpto = factor(Dpto, levels = rev(levels(df$Dpto))))
  
  return(datos_plot)
  
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
                         cada_2_semanas = sapply(seq_dias, function(x) func_doblaje(valor_inicio = min_casos_pais, tasa_doblaje = 14, tiempo = x )),
                         cada_mes = sapply(seq_dias, function(x) func_doblaje(valor_inicio = min_casos_pais, tasa_doblaje = 30, tiempo = x ))) %>%
    melt() %>%
    dplyr::rename(Dpto = variable,
                  Count = value) %>%
    group_by(Dpto) %>%
    mutate(Day = row_number(),
           NumDays = max(Day),
           label = if_else(Count == max(Count), Dpto, NULL)) %>%
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
    tiempo_doblaje_en_dias <- (intervalo_dias*log(2))/(log(cum_final/cum_inicio))
    
    if (length(tiempo_doblaje_en_dias) > 1 | is.infinite(tiempo_doblaje_en_dias[1])){
      return(NA)
    } else {
      return(round(tiempo_doblaje_en_dias, 0))
    }
    
  }
  
}

# func_calcular_tiempo_doblaje <- function(df, intervalo_dias, ultima_semana){
#   
#   
#   cum_final <- df$Count[nrow(df)- intervalo_dias * (ultima_semana - 1)]
#   cum_inicio <- df$Count[nrow(df)-intervalo_dias * ultima_semana]
#   
#   tiempo_doblaje_en_dias <- (intervalo_dias*log(2))/(log(cum_final/cum_inicio))
#   
#   if (length(tiempo_doblaje_en_dias) > 1 | is.infinite(tiempo_doblaje_en_dias[1])){
#     return(NA)
#   } else {
#     return(round(tiempo_doblaje_en_dias, 0))
#   }
#   
#   
# }


###############
#importar datos
###############

js <- fromJSON("https://mauforonda.github.io/covid19-bolivia/data.json")


#############################
#asignar variables globales
############################

min_casos_pais <- 10

#ultima fecha de actualizacion
ultima_fecha <- js$confirmados[[1]]$fecha #no incluir datos de esta fecha por posibilidad a ser valores dupicados a las del dia anterior
ultima_fecha_dmy <- as.character(format(strptime(ultima_fecha, "%Y-%m-%d") - 1, "%d/%m/%y")) #fecha de referencia


#asignar colores
#library(randomcoloR)

#n <- 9
#palette <- distinctColorPalette(n)
palette <- c("#D69C5E", "#DC7099", "#A090DC", "#D5C1D0", "#77E099", "#BA53DB", "#C1E257", "#CDDCAD", "#80D3DC")

#########################################################
# casos confirmados nivel departamental:
#########################################################

########################
# limpiar datos
########################

df <- plyr::ldply(js$confirmados, function(x) x$dep) %>%
  mutate(fha = sapply(js$confirmados, function(x) x$fecha)) %>%
  `colnames<-`(c("La Paz", "Cochabamba", "Santa Cruz", "Oruro", "Potosí",
                 "Tarija", "Chuquisaca", "Beni", "Pando", "Fecha")) %>%
  filter(Fecha != ultima_fecha) %>%
  pivot_longer(cols = -Fecha, names_to='Dpto', values_to='Count') %>%
  arrange(Dpto, Fecha, Count) %>% 
  filter(Count >= min_casos_pais) %>% 
  mutate(Fecha=as.Date(Fecha)) %>%
  group_by(Dpto) %>%
  mutate(Day = row_number(),
         NumDays = max(Day)) %>%
  ungroup() %>%
  mutate(Dpto = as.factor(Dpto))
  

datos_plot <- func_preparar_datos_plot(df = df)
datos_ref <- func_preparar_lineas_ref(df = datos_plot)

#paste(levels(df$Dpto), palette, sep = "=")
cols <- c("Beni" = "#D69C5E", 
          "Chuquisaca" = "#DC7099", 
          "Cochabamba" = "#A090DC", 
          "La Paz" = "#D5C1D0", 
          "Oruro" = "#77E099", 
          "Pando" = "#BA53DB", 
          "Potosí" = "#C1E257", 
          "Santa Cruz" = "#CDDCAD", 
          "Tarija" = "#80D3DC")

plot_dpto_confirm <- datos_plot %>%
  ggplot(aes(Day, Count,  color= Dpto, group = Dpto, label = label)) +
  geom_line(data = datos_ref, aes(Day, Count), linetype = "dotted", size = 1, colour = "black")+
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(0, max(datos_plot$NumDays) + 30, 5), 
                     minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma_format(accuracy = 1),
                minor_breaks=NULL,
                breaks = c(0, 10, 20, 30, 40, 50, 100, 250, 500, 750, 10^(3:ceiling(log10(max(datos_plot$Count))))),
                limits = c(10,10^(ceiling(log10(max(datos_plot$Count)))+1)))+
  scale_colour_manual(values = cols)+
  geom_label_repel(aes(label = label, color = Dpto),
                   nudge_x = 3,
                   nudge_y = 0.1)+
  annotate(geom = "text", x = 8, y = 80000, label = "Número de casos \nse duplica cada 1 día")+
  annotate(geom = "text", x = 21, y = 50000, label = "...cada 2 días")+
  annotate(geom = "text", x = 34, y = 40000, label = "...cada 3 días")+
  annotate(geom = "text", x = 65, y = 10000, label = "...cada semana")+
  annotate(geom = "text", x = 70, y = 250, label = "...cada 2 semanas")+
  annotate(geom = "text", x = 70, y = 40, label = "...cada mes")+
  guides(color = FALSE) +
  labs(x = str_glue('Número de Días desde el Décimo Caso Confirmado'),
       y = "Número de Casos Confirmados (Escala Log 10)",
       title = "Casos Confirmados de Coronavirus en Bolivia a Nivel Departamental", 
       subtitle = str_glue("Número cumulativo de casos confirmados, ",
                           "por número de días desde décimo caso confirmado\n",
                           "Fecha mas reciente de actualización {ultima_fecha_dmy}"),
       caption = str_glue("Fuente: https://boliviasegura.agetic.gob.bo/ via https://mauforonda.github.io/covid19-bolivia/data.json mantenida por @mauforonda \n",
                          "Inspiración 1: FT graphic: John Burn-Murdoch / @jburnmurdoch \n", 
                          "Inspiración 2: https://blog.datawrapper.de/weekly-chart-coronavirus-growth/ \n",
                          "Código Fuente: https://github.com/leobyon/visdatbolivia \n",
                          "Autor: @leo_byon"))+
  opts()
  


# plot_dpto_confirm+
#   ggsave("E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_bolivia_dpto_confirmados.png", width = 25, height = 20, units = "cm")

  
#########################################################
# casos confirmados : tasa doblaje nivel departamental:
#########################################################




df_tiempo_dupl <- plyr::ldply(as.character(unique(datos_plot$Dpto)), function(x){
  
  sub <- datos_plot %>% filter(Dpto == x) 
  
  semana_menos_uno <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 1)
  semana_menos_dos <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 2)
  semana_menos_tres <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 3)
  semana_menos_cuatro <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 4)
  

  return(c(x, semana_menos_uno, semana_menos_dos, semana_menos_tres, semana_menos_cuatro))
  
}) %>%
  `colnames<-`(c("Dpto", "Última Semana", "Penúltima Semana", "Antepenúltima Semana", "Trasantepenúltima Semana")) %>%
  mutate_at(vars(contains("Semana")), ~paste(., "días")) %>%
  na_if(., "NA días") %>%
  replace(is.na(.), "NA") 
  #mutate(Dpto=recode(Dpto, 
  #                      Potosí='Potosi')) 

tb_dpto_doblaje_confir <- df_tiempo_dupl %>%
  gt() %>%
  tab_header(
    title = "Ritmo de Crecimiento de Casos Confirmados en Bolivia a Nivel Departamental",
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
  tab_source_note(
    source_note = str_glue("Fecha mas reciente de actualización: {ultima_fecha_dmy}")
  ) %>%
  tab_source_note(
    source_note = "NA: En el interval semanal correspondiente, no existe datos o no hubo incremento de nuevos casos confirmados"
  ) %>%
  tab_source_note(
    source_note = "Notas de cómputo: El periodo de análisis por dpto es a partir del décimo caso confirmado 
    Por tanto, dptos que no cumplen este criterio son excluidos y para los que cumplen, 
    el periodo analizado puede ser menor al periodo total desde su primer caso confirmado."
  ) %>%
  tab_source_note(
    source_note = "Fuente: https://boliviasegura.agetic.gob.bo/ via https://mauforonda.github.io/covid19-bolivia/data.json mantenida por @mauforonda"
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


# gtsave(tb_dpto_doblaje_confir, 
#        "E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_bolivia_dpto_confirmados_doblaje.png")
# 





#########################################################
# decesos confirmados:
#########################################################

########################
# limpiar datos
########################

df <- plyr::ldply(js$decesos, function(x) x$dep) %>%
  mutate(fha = sapply(js$decesos, function(x) x$fecha)) %>%
  `colnames<-`(c("La Paz", "Cochabamba", "Santa Cruz", "Oruro", "Potosí",
                 "Tarija", "Chuquisaca", "Beni", "Pando", "Fecha")) %>%
  filter(Fecha != ultima_fecha) %>%
  pivot_longer(cols = -Fecha, names_to='Dpto', values_to='Count') %>%
  arrange(Dpto, Fecha, Count) %>%
  filter(Count >= min_casos_pais) %>%
  mutate(Fecha=as.Date(Fecha)) %>%
  group_by(Dpto) %>%
  mutate(Day = row_number(),
         NumDays = max(Day)) %>%
  ungroup() %>%
  mutate(Dpto = as.factor(Dpto))


datos_plot <- func_preparar_datos_plot(df = df)
datos_ref <- func_preparar_lineas_ref(df = datos_plot)



plot_dpto_fallecidos <- datos_plot %>%
  ggplot(aes(Day, Count,  color= Dpto, group = Dpto, label = label)) +
  geom_line(data = datos_ref, aes(Day, Count), linetype = "dotted", size = 1, colour = "black")+
  geom_line(size=2) +
  scale_x_continuous(breaks=seq(0, max(datos_plot$NumDays) + 30, 5),
                     minor_breaks=NULL) +
  scale_y_log10(labels=scales::comma_format(accuracy = 1),
                minor_breaks=NULL,
                breaks = c(0, 10, 20, 30, 40, 50, 100, 250, 500, 750, 10^(3:ceiling(log10(max(datos_plot$Count))))),
                limits = c(10,10^(ceiling(log10(max(datos_plot$Count)))+1)))+
  scale_colour_manual(values = cols)+
  geom_label_repel(aes(label = label, color = Dpto),
                   nudge_x = 3,
                   nudge_y = 0.1)+
  annotate(geom = "text", x = 8, y = 1000, label = "Número de casos \nse duplica cada 1 día")+
  annotate(geom = "text", x = 15, y = 1000, label = "...cada 2 días")+
  annotate(geom = "text", x = 25, y = 1000, label = "...cada 3 días")+
  annotate(geom = "text", x = 45, y = 500, label = "...cada semana")+
  annotate(geom = "text", x = 45, y = 70, label = "...cada 2 semanas")+
  annotate(geom = "text", x = 50, y = 30, label = "...cada mes")+
  guides(color = FALSE) +
  labs(x = str_glue('Número de Días desde el Décimo Deceso'),
       y = "Número de Decesos (Escala Log 10)",
       title = "Número de Decesos de Coronavirus en Bolivia a Nivel Departamental",
       subtitle = str_glue("Número cumulativo de decesos, ",
                           "por número de días desde el décimo deceso\n",
                           "Fecha mas reciente de actualización {ultima_fecha_dmy}"),
       caption = str_glue("Fuente: https://boliviasegura.agetic.gob.bo/ via https://mauforonda.github.io/covid19-bolivia/data.json mantenida por @mauforonda \n",
                          "Inspiración 1: FT graphic: John Burn-Murdoch / @jburnmurdoch \n",
                          "Inspiración 2: https://blog.datawrapper.de/weekly-chart-coronavirus-growth/ \n",
                          "Código Fuente: https://github.com/leobyon/visdatbolivia \n",
                          "Autor: @leo_byon"))+
  opts()




# plot_dpto_fallecidos+
#   ggsave("E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_bolivia_dpto_fallecidos.png", width = 25, height = 20, units = "cm")



#########################################################
# decesos : tasa doblaje nivel departamental:
#########################################################

#calcular tiempo doblaje



df_tiempo_dupl <- plyr::ldply(as.character(unique(datos_plot$Dpto)), function(x){
  
  sub <- datos_plot %>% filter(Dpto == x) 
  
  semana_menos_uno <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 1)
  semana_menos_dos <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 2)
  semana_menos_tres <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 3)
  semana_menos_cuatro <- func_calcular_tiempo_doblaje(df = sub, intervalo_dias = 7, ultima_semana = 4)
  
  
  return(c(x, semana_menos_uno, semana_menos_dos, semana_menos_tres, semana_menos_cuatro))
  
}) %>%
  `colnames<-`(c("Dpto", "Última Semana", "Penúltima Semana", "Antepenúltima Semana", "Trasantepenúltima Semana")) %>%
  mutate_at(vars(contains("Semana")), ~paste(., "días")) %>%
  na_if(., "NA días") %>%
  replace(is.na(.), "NA")

tb_dpto_doblaje_fallecidos <- df_tiempo_dupl %>%
  gt() %>%
  tab_header(
    title = "Ritmo de Crecimiento de Decesos por Covid-19 en Bolivia a Nivel Departamental",
    subtitle = "A base de las últimas cuatro semanas, la tabla muestra el ritmo de crecimiento 
    (cuantificado como tiempo de duplicación en número de dias) de decesos por intervalos semanales. 
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
  tab_source_note(
    source_note = str_glue("Fecha mas reciente de actualización: {ultima_fecha_dmy}")
  ) %>%
  tab_source_note(
    source_note = "NA: En el interval semanal correspondiente, no existe datos o no hubo incremento de nuevos casos confirmados"
  ) %>%
  tab_source_note(
    source_note = "Notas de cómputo: El periodo de análisis por dpto es a partir del décimo deceso. 
    Por tanto, dptos que no cumplen este criterio son excluidos y para los que cumplen, 
    el periodo analizado puede ser menor al periodo total desde su primer deceso."
  ) %>%
  tab_source_note(
    source_note = "Fuente: https://boliviasegura.agetic.gob.bo/ via https://mauforonda.github.io/covid19-bolivia/data.json mantenida por @mauforonda"
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


# gtsave(tb_dpto_doblaje_fallecidos, 
#        "E:/github_projects/visdatbolivia/bolivia_covid19_tracker/coronavirus_bolivia_dpto_fallecidos_doblaje.png")




