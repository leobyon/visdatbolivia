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
df.Free <- read.csv("./libertad_todo.csv", header = T, stringsAsFactors = F,
                    col.names = c("Entity", "Code", "Year", "Score"))

#por pais
df.Free.Pais <- df.Free %>%
  filter(grepl("united states|south korea|bolivia", tolower(Entity))) %>%
  mutate(Entity = ifelse(grepl("south korea", tolower(Entity)), "Corea del Sur", 
                         ifelse(grepl("united states", tolower(Entity)), "EEUU", "Bolivia"))) %>%
  select(Entity, Year, Score)

#Mundo
df.Free.Mundo <- df.Free %>%
  group_by(Year) %>%
  summarise(Score = round(mean(Score),0)) %>%
  mutate(Entity = "Mundo")

#LATAM
df.Free.LAC  <- df.Free %>%
  filter(Code %in% setdiff(countryCodeLAC, "VEN")) %>% #remove venezuela
  group_by(Year) %>%
  summarise(Score = round(mean(Score),0)) %>%
  mutate(Entity = "LATAM")

#combinar y visualizar
df.Free.Comb <- df.Free.Pais %>%
  rbind(df.Free.Mundo) %>%
  rbind(df.Free.LAC) %>%
  rename(Region = Entity,
         Año = Year,
         Puntuacion = Score) %>%
  mutate(RegionType = ifelse(Region == "Bolivia", "Bolivia",
                             ifelse(Region %in% c("LATAM", "Mundo"), "Benchmark Regiones", "Benchmark Paises"))) %>%
  mutate(Region = factor(Region, levels = c("Bolivia", "Corea del Sur", "EEUU", "LATAM", "Mundo")),
         RegionType = factor(RegionType, levels = c("Bolivia", "Benchmark Paises", "Benchmark Regiones"))) %>%
  ggplot(aes(x=Año,
                         y=Puntuacion,
                         color=Region))+
  geom_line(aes(linetype = RegionType), size = 1.5)+
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))+
  scale_color_manual(values =  c("orangered", "royalblue", "yellowgreen", "firebrick","orange"))+
  scale_y_continuous(breaks = c(-20, -10, -5, 0, 5, 10))+
  guides(linetype = FALSE)+
  labs(x = "",
       y = "",
       title = "Régimen Político", 
       subtitle = "Los valores tienen un rango de -10 (autocratico) a +10 (democracia completa). Anocracias son aquellas con un valor entre -5 y +5. \nSi un pais fue colonizado o todavía no fué un estado soberano para el año correspondiente, el valor es -20. \nPeriodo 1816 - 2015. \nLATAM no incluye Venezuela.",
       caption = "Fuente: OWID https://ourworldindata.org/grapher/political-regime-updated2016?year=2015 \nDatos y Código Fuente: https://github.com/visdatbolivia/visdatbolivia")+
  opts()+
  ggsave("libertad.png", width = 31, height = 16, units = "cm")

