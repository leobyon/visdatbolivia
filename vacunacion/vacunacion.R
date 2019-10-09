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

#importar datos por pais y procesar
df.Vacc.Pais <- read.csv("./Diphtheria tetanus toxoid and pertussis_paises.csv", 
                         header = T, 
                         skip = 1,
                         stringsAsFactors = F) %>%
  filter(grepl("^rep.*korea|united st|bolivia", tolower(Country))) %>%
  rename(Region = Country) %>%
  mutate(Region = ifelse(grepl("^rep.*korea", tolower(Region)), "Corea del Sur", 
                         ifelse(grepl("united st", tolower(Region)), "EEUU", "Bolivia"))) %>%
  melt(., id.vars = "Region") %>% 
  rename(Año = variable) %>% 
  mutate(Año = as.integer(substr(Año, 2, nchar(as.character(Año)))))

#importar datos por region y procesar
df.Vacc.Region <- read.csv("./Diphtheria tetanus toxoid and pertussis_region.csv", 
                           header = T, 
                           skip = 1, 
                           stringsAsFactors = F) %>% 
  filter(WHO.region %in% c("Americas", "(WHO) Global")) %>%
  dplyr::rename(Region = WHO.region) %>%
  mutate(Region = ifelse(grepl("who", tolower(Region)), "Mundo", Region)) %>%
  melt(., id.vars = "Region") %>% 
  rename(Año = variable) %>% 
  mutate(Año = as.integer(substr(Año, 3, nchar(as.character(Año)))))  

#combinar
df.Vacc <- rbind(df.Vacc.Pais, df.Vacc.Region) %>% 
  mutate(RegionType = ifelse(Region == "Bolivia", "Bolivia",
                             ifelse(Region %in% c("Americas", "Mundo"), "Benchmark Regiones", "Benchmark Paises"))) %>%
  mutate(Region = factor(Region, levels = c("Bolivia", "Corea del Sur", "EEUU", "Americas", "Mundo")),
         RegionType = factor(RegionType, levels = c("Bolivia", "Benchmark Paises", "Benchmark Regiones")))

#visualizar
ggplot(df.Vacc, aes(x = Año, 
                    y = value, 
                    color = Region )) +
  geom_line(aes(linetype = RegionType), size = 1.5)+
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))+
  scale_color_manual(values =  c("orangered", "royalblue", "yellowgreen", "firebrick","orange"))+
  scale_x_continuous(breaks = seq(1980,2020, 5), limits = c(1980,2020))+
  guides(linetype = FALSE)+
  labs(x = "",
       y = "Proporción (%)",
       title = "Cobertura de Vacunación DTP3 (Difteria, Tétanos, Tos Ferina (DTP3)", 
       subtitle = "Periodo 1980 - 2018",
       caption = "Fuente: OMS, http://apps.who.int/gho/data/node.main.A827?lang=en# \nDatos y Código Fuente: https://github.com/visdatbolivia/visdatbolivia")+
  opts()+
  ggsave("vacunacion.png", width = 31, height = 16, units = "cm")
