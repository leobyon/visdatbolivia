
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

#por pais
df.Edu.Tert.Pais <- read.csv("./BL2013_EduAttain5yrAgeGroup.csv", header = T, stringsAsFactors = F) %>%
  filter(grepl("usa|korea|bolivia", tolower(country))) %>% 
  mutate(country = ifelse(grepl("korea", tolower(country)), "Corea del Sur", 
                          ifelse(grepl("usa", tolower(country)), "EEUU", "Bolivia"))) %>%
  select(country, year, agefrom, ageto, lhc, pop) %>%
  filter(agefrom == 25 & ageto == 29 |
           agefrom == 30 & ageto == 34 |
           agefrom == 55 & ageto == 59 |
           agefrom == 60 & ageto == 64 ) %>%
  mutate(age_range = ifelse(agefrom == 25 & ageto == 29 | agefrom == 30 & ageto == 34, "25-34", "55-64"),
         lhc_cnt = lhc/100 * pop) %>%
  group_by(country, year, age_range) %>%
  summarise(seg_pop = sum(pop),
            seg_lhc_cnt = sum(lhc_cnt)) %>%
  ungroup() %>%
  mutate(seg_lhc = seg_lhc_cnt/seg_pop * 100) %>%
  filter(year == 2010) %>%
  select(country, age_range, seg_lhc) %>%
  rename(Region = country,
         Edad = age_range,
         Porc.Completado.Edu.Terciaria = seg_lhc)

#LATAM (Media ponderada por poblacion)
df.Edu.Tert.LATAM <- read.csv("BL2013_EduAttain5yrAgeGroup.csv", header = T, stringsAsFactors = F) %>% 
  filter(grepl("latin", tolower(region_code))) %>% 
  filter(agefrom == 25 & ageto == 29 |
           agefrom == 30 & ageto == 34 |
           agefrom == 55 & ageto == 59 |
           agefrom == 60 & ageto == 64 ) %>%
  filter(year == 2010) %>% 
  select(country, year, agefrom, ageto, lhc, pop) %>%
  mutate(age_range = ifelse(agefrom == 25 & ageto == 29 | agefrom == 30 & ageto == 34, "25-34", "55-64"),
         lhc_cnt = lhc/100 * pop) %>% 
  group_by(country, year, age_range) %>%
  summarise(seg_pop = sum(pop),
            seg_lhc_cnt = sum(lhc_cnt)) %>% 
  ungroup() %>%
  mutate(seg_lhc_prop = seg_lhc_cnt/seg_pop) %>%
  group_by(age_range) %>%
  mutate(seg_region_pop = sum(seg_pop)) %>%
  group_by(country, add = TRUE) %>%
  mutate(seg_pop_prop = seg_pop / seg_region_pop) %>%
  group_by(age_range) %>%
  summarise(region_seg_prop = sum(seg_pop_prop * seg_lhc_prop) * 100) %>%
  mutate(Region = "LATAM") %>%
  select(Region, age_range, region_seg_prop) %>%
  rename(Edad = age_range,
         Porc.Completado.Edu.Terciaria = region_seg_prop)


#Mundo (Media ponderada por poblacion)
df.Edu.Tert.Mundo <- read.csv("BL2013_EduAttain5yrAgeGroup.csv", header = T, stringsAsFactors = F) %>% 
  filter(agefrom == 25 & ageto == 29 |
           agefrom == 30 & ageto == 34 |
           agefrom == 55 & ageto == 59 |
           agefrom == 60 & ageto == 64 ) %>%
  filter(year == 2010) %>% 
  mutate(age_range = ifelse(agefrom == 25 & ageto == 29 | agefrom == 30 & ageto == 34, "25-34", "55-64"),
         lhc_cnt = lhc/100 * pop) %>% 
  group_by(country, year, age_range) %>%
  summarise(seg_pop = sum(pop),
            seg_lhc_cnt = sum(lhc_cnt)) %>% 
  ungroup() %>%
  mutate(seg_lhc_prop = seg_lhc_cnt/seg_pop) %>%
  group_by(age_range) %>%
  mutate(seg_region_pop = sum(seg_pop)) %>%
  group_by(country, add = TRUE) %>%
  mutate(seg_pop_prop = seg_pop / seg_region_pop) %>%
  group_by(age_range) %>%
  summarise(region_seg_prop = sum(seg_pop_prop * seg_lhc_prop) * 100) %>%
  mutate(Region = "Mundo") %>%
  select(Region, age_range, region_seg_prop) %>%
  rename(Edad = age_range,
         Porc.Completado.Edu.Terciaria = region_seg_prop)


#visualizar
df.Edu.Tert.Pais %>%
  rbind(df.Edu.Tert.LATAM, df.Edu.Tert.Mundo) %>% 
  mutate(Region = factor(Region, levels = c("Mundo", "LATAM", "Bolivia", "EEUU", "Corea del Sur")),
         Porc.Completado.Edu.Terciaria = round(Porc.Completado.Edu.Terciaria,1)) %>%
  ggplot(aes(x=Region, y = Porc.Completado.Edu.Terciaria)) +
  geom_line(aes(group = Region), size = 2)+
  geom_point(aes(color=Edad), size = 4)+
  geom_label_repel(aes(label = Porc.Completado.Edu.Terciaria),
                   direction = "y", hjust = -1, show.legend = FALSE)+
  scale_y_continuous(breaks = seq(0, 70, 10), limits = c(0, 70))+
  guides(linetype = FALSE)+
  labs(x = "",
       y = "Proporcion dentro del grupo de edad (%)",
       title = "Población con Educación Terciaria Completada", 
       subtitle = "Grupo de Edad: 25-34 años y 55-64 años. \nMedia ponderada por población para LATAM Y Mundo.",
       caption = "Fuente: Barro, Robert and Jong-Wha Lee, 2013, 'A New Data Set of Educational Attainment in the World, 1950-2010.' Journal of Development Economics, vol 104, pp.184-198. \nhttp://www.barrolee.com/ \nDatos y Código Fuente: https://github.com/visdatbolivia/visdatbolivia")+
  opts()+
  ggsave("educacion_terciaria.png", width = 31, height = 16, units = "cm")

  
