
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
df <- read.csv("./serie_pib_per_capita_real.csv")

#por pais
df.gdppc.pais <- df %>%
  filter(countrycode %in% c("USA", "BOL", "KOR")) %>%
  filter(year >= 1950) %>%
  select(countrycode, year, cgdppc)

#LATAM
df.gdppc.region <- df %>%
  filter(countrycode %in% countryCodeLAC & year >= 1950) %>% 
  left_join(., df %>%
              filter(countrycode %in% countryCodeLAC & year >= 1950) %>%
              group_by(year) %>%
              summarise(region_pop = sum(pop, na.rm=T)) , by = "year") %>%
  group_by(year) %>%
  summarise(cgdppc = sum((pop/region_pop *cgdppc), na.rm=T)) %>%
  mutate(countrycode = "LATAM")

#procesar
df.gdppc <- rbind(df.gdppc.pais, df.gdppc.region) %>%
  dcast(., year ~countrycode , value.var = "cgdppc") %>%
  mutate(BOL_NORM = BOL/USA,
         KOR_NORM = KOR/USA,
         LATAM_NORM = LATAM/USA) %>%
  select(year, BOL_NORM, KOR_NORM, LATAM_NORM) %>%
  melt(., id.vars = "year") %>% 
  rename(Año = year) %>%
  mutate(Region = ifelse(grepl("BOL", variable), "Bolivia",
                         ifelse(grepl("KOR", variable), "Corea del Sur", "LATAM")),
         RegionTipo = ifelse(Region == "Bolivia", "Bolivia",
                             ifelse(Region == "LATAM", "Benchmark Regiones", "Benchmark Paises"))) %>% 
  mutate(Region = factor(Region, levels = unique(Region)),
         RegionTipo = factor(RegionTipo, levels = c("Bolivia", "Benchmark Paises", "Benchmark Regiones")))

#visualizar
df.gdppc %>%
  ggplot(aes(x=Año, y=round(value*100,2), color = Region)) +
  geom_label_repel(data = filter(df.gdppc, Año == max(Año)) , aes(label = round(value * 100, 1)),
                   direction = "y", hjust = -2, show.legend = FALSE)+
  geom_line(aes(linetype = RegionTipo), size = 1.5)+
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))+
  scale_color_manual(values =  c("orangered", "royalblue", "firebrick"))+
  scale_y_continuous(breaks = seq(0, 80, 10))+
  scale_x_continuous(breaks = seq(1950, 2020, 5))+
  guides(linetype = FALSE)+
  labs(x = "",
       y = "Proporción (%)",
       title = "PIB per cápita Relativo a Estados Unidos", 
       subtitle = "PIB per cápita a precios constantes 2011 en US$",
       caption = "Fuente: Maddison Project Database, version 2018. (https://www.rug.nl/ggdc/historicaldevelopment/maddison/releases/maddison-project-database-2018) \nInspirado por: Figura 27 en Bolivia Systematic Country Diagnostic: Rebalancing inclusive growth to deepen gains on poverty and inequality reduction. The World Bank. 2015. \nDatos y Código Fuente: https://github.com/visdatbolivia/visdatbolivia")+
  opts()+
  ggsave("ppcr_relativo.png", width = 31, height = 16, units = "cm")

