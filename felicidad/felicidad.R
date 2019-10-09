#cargar librerias

library(dplyr)
library(ggplot2)
library(reshape2)
library(ggrepel)

#definir vis theme
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

#extraer codigos de paises correspondientes a LATAM
countryCodeLAC <- read.csv("./EdStatsCountry.csv") %>%
  filter(Region == "Latin America & Caribbean") %>%
  dplyr::rename(Country.Code = "ï..Country.Code") %>% 
  pull("Country.Code") %>%
  unique() %>% 
  as.character()

#importar datos
df <- read.csv("./felicidad_escalera_cantril.csv", col.names = c("Entity", "Code", "Year", "Index"))

#extraer por pais y regiones
df.feliz <- bind_rows(
  df %>% 
    filter(Code %in% c("BOL", "USA", "KOR")) %>% 
    mutate(Region = ifelse(grepl("BOL", Code), "Bolivia",
                           ifelse(grepl("KOR", Code), "Corea del Sur", "EEUU"))) %>%
    select(Region, Year, Index),
  df %>% 
    filter(Code %in% countryCodeLAC) %>%
    group_by(Year) %>%
    summarise(Index = mean(Index)) %>%
    mutate(Region = "LATAM"),
  df %>% 
    group_by(Year) %>%
    summarise(Index = mean(Index)) %>%
    mutate(Region = "Mundo")
) %>%
  mutate(RegionTipo = ifelse(Region == "Bolivia", "Bolivia",
                             ifelse(Region %in% c("LATAM", "Mundo"), "Benchmark Regiones", "Benchmark Paises"))) %>%
  mutate(Region = factor(Region, levels = unique(Region)),
         RegionTipo = factor(RegionTipo, levels = c("Bolivia", "Benchmark Paises", "Benchmark Regiones"))) %>%
  rename(Año = Year,
         Indice = Index)

#visualizar
df.feliz %>%
  ggplot(aes(x=Año, y=round(Indice,2), color = Region)) +
  geom_label_repel(data = filter(df.feliz, Año == max(Año)) , aes(label = round(Indice, 2)),
                   direction = "y", hjust = -1, show.legend = FALSE)+
  geom_line(aes(linetype = RegionTipo), size = 1.5)+
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))+
  scale_color_manual(values =  c("orangered", "royalblue", "yellowgreen", "firebrick","orange"))+
  scale_y_continuous(breaks = seq(4, 8, 0.5))+
  scale_x_continuous(breaks = seq(2005, 2020, 1))+
  guides(linetype = FALSE)+
  labs(x = "",
       y = "Escala (0 - 10)",
       title = "Escala de la Satisfacción con la Vida", 
       subtitle = "Escala basada en la respuesta a la siguiente pregunta: 'Por favor imagine una escalera, con gradas numeradas desde 0 en el suelo hasta 10 en el tope. \nEl tope de la escalera representa la mejor vida posible para usted y el suelo de la escalera representa la peor vida posible para usted. \n¿En qué escalón de la escalera diría usted que siente se encuentra en este momento?'",
       caption = "Fuente: Reporte de la Felicidad Mundial (2005-2016) \nDatos y Código Fuente: https://github.com/visdatbolivia/visdatbolivia")+
  opts()+
  ggsave("felicidad.png", width = 31, height = 16, units = "cm")

