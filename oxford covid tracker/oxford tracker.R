library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(readxl)
library(RCurl)

library(readr)


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

#################################
# casos confirmados
#################################

#urls
url_confirmados = 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv'

#leer datos
datos_confirmados <- read_csv(url(url_confirmados))

#casos minimos por pais
min_casos_pais <- 1

#ultima fecha de actualizacion
ultima_fecha <- names(datos_confirmados)[ncol(datos_confirmados)]

#limpiar datos
datos_limpio <- datos_confirmados %>% 
  rename(Country=`Country/Region`) %>% 
  mutate(Country=recode(Country, 
                        `Korea, South`='South Korea',
                        US='United States')) %>% 
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
  ungroup() 




#################################
# indice de severidad
#################################

#url
dta_url <- "https://www.bsg.ox.ac.uk/sites/default/files/OxCGRT_Download_latest_data.xlsx"
tmp_file <- tempfile(".xlsx")

#download (fecha de descarga: abril 5, 2020)
utils::download.file(dta_url, tmp_file, mode = "wb")

#leer y limpiar
raw_data <- read_xlsx(
  tmp_file,
  col_types = c("text", "text", "numeric",
                rep(c("numeric", "numeric", "text"), 6),
                rep(c("numeric", "text"), 5), rep("numeric", 3), "skip")
) %>%
  dplyr::rename(
    Country = CountryName,
    iso3c = CountryCode
  ) %>%
  dplyr::mutate(Date = lubridate::ymd(Date)) %>%
  arrange(iso3c, Date) %>%
  select(Country, iso3c, Date, StringencyIndex, ConfirmedCases)



#################################
# combinar
#################################
options(scipen = 9999)


df <- raw_data %>%
  inner_join(datos_limpio, by = c("Country", "Date"))



df2  <- df %>%
  filter(Country %in% c("Bolivia", "Brazil", "Ecuador", "United States", "South Korea", "Italy")) %>%
  mutate(Country = factor(Country, levels = c("South Korea", "Italy", "United States", "Bolivia", "Brazil", "Ecuador"))) %>%
  #filter(!is.na(StringencyIndex)) %>%
  filter(!is.na(ConfirmedCases)) %>%
  mutate(ConfirmedCases_logged = log10(ConfirmedCases)) %>%
  mutate(Country=recode(Country, 
                        Brazil='Brasil',
                        `United States`='Estados Unidos',
                        `South Korea` = 'Corea del Sur',
                        Italy = 'Italia')) 


#scaleFactor<-max(df2$ConfirmedCases_logged, na.rm = TRUE)/max(df2$StringencyIndex, na.rm = TRUE)
#0.05

scaleFactor <- 0.05

p <- ggplot(data=df2, aes(x=Day))+
  geom_line(aes(y=ConfirmedCases_logged), col="black", size = 1.5)+
  geom_point(aes(y=StringencyIndex*scaleFactor), col = "red", size = 2)+
  scale_y_continuous(name="Número de Casos (Escala Log10)", 
                     limits= c(1,5), 
                     labels = format(c(10,100,1000,10000, 100000), big.mark=","),
                     sec.axis=sec_axis(~./scaleFactor, 
                                       name="Indice de Severidad", 
                                       breaks = seq(0,100,20)))+
  scale_x_continuous(breaks=seq(0, 100,20), 
                     minor_breaks=NULL,
                     limits = c(0,100),
                     name = "Días despues del Primer Caso")+
  facet_wrap(~Country)+
  opts()+
  theme(axis.text.y.right = element_text(color = "red"),
        axis.title.y.right = element_text(color = "red"))+
  labs(title = "Comparación de la Respuesta del Gobierno de Bolivia vs Cinco Paises según la Cantidad de Casos", 
       subtitle = str_glue("A. El Indice de Severidad mide el nivel de severidad de las intervenciones no farmacológicas del gobierno (0-100).\n",
                           "B. Se observan cuatro tipos de respuestas (no exhaustivas):\n",
                           "     - Proactivo (Corea del Sur, Bolivia)\n",
                           "     - Reactivo (Italia, Brasil)\n",
                           "     - Está alguien en casa? (Estados Unidos)\n",
                           "     - Retardada (Ecuador)"),
       caption = str_glue("*El índice se compone a base de 7 indicadores: 1) cierre de escuelas, 2) cierre de empresas, 3) prohibición de eventos públicos,\n",
                          "4) prohibición de transporte público, 5) campañas informativas, 6) restricción en mobilidad interna, 7) control de vuelos internacionales\n",
                          "** Última fecha de actualización: 31 de marzo de 2020\n",
                          "***Nota: Este indice es para comparar las respuestas de los gobiernos y no debe ser interpretado como el nivel de efectividad de las mismas.\n",
                          "Fuente: Variation in Government Responses to COVID-19\n,", 
                          "https://www.bsg.ox.ac.uk/research/research-projects/oxford-covid-19-government-response-tracker \n",
                          "Código Fuente: https://github.com/visdatbolivia/visdatbolivia \n",
                          "Autor: @leo_byon"))

#p +
#  ggsave("E:/github_projects/visdatbolivia/oxford covid tracker/test.png", width = 25, height = 20, units = "cm")
  

