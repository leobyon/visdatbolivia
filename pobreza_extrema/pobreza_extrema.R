library(dplyr)
library(ggplot2)
library(reshape2)
library(ggrepel)




##################
#extrema pobreza
##################

df.Pob.Pais <- read.csv("./poverty_countries.csv", stringsAsFactors = F) %>%
  filter(grepl("bolivia|korea|united states", tolower(CountryName))) %>% 
  select(CountryName, 
         RequestYear, 
         HeadCount, 
         ReqYearPopulation) %>% 
  dplyr::rename(Region = CountryName, 
                Año = RequestYear, 
                Proporcion = HeadCount, 
                Poblacion = ReqYearPopulation) %>%
  mutate(Region = ifelse(grepl("korea", tolower(Region)), "Corea del Sur", 
                         ifelse(grepl("united", tolower(Region)), "EEUU", "Bolivia")))

df.Pob.Region <- read.csv("./poverty_regional.csv", stringsAsFactors = F) %>%
  filter(grepl("latin america|world total", tolower(regionTitle))) %>%
  select(regionTitle, 
         requestYear, 
         hc, 
         population) %>%
  dplyr::rename(Region = regionTitle, 
                Año = requestYear, 
                Proporcion = hc, 
                Poblacion = population) %>%
  mutate(Region = ifelse(grepl("latin america", tolower(Region)), "LATAM", "Mundo"))

df.Pob <- rbind(df.Pob.Pais, df.Pob.Region) %>%
  mutate(RegionTipo = ifelse(Region == "Bolivia", "Bolivia",
                             ifelse(Region %in% c("LATAM", "Mundo"), "Benchmark Regiones", "Benchmark Paises"))) %>%
  mutate(Region = factor(Region, levels = unique(Region)),
         RegionTipo = factor(RegionTipo, levels = c("Bolivia", "Benchmark Paises", "Benchmark Regiones")))



ggplot(df.Pob, aes(x = Año, 
                         y = round(Proporcion * 100,2), 
                         color = Region))+
  geom_label_repel(data = filter(df.Pob, Año == max(Año)) , aes(label = round(Proporcion * 100, 2)),
                   direction = "y", hjust = -1, show.legend = FALSE)+
  geom_line(aes(linetype = RegionTipo), size = 1.5)+
  scale_linetype_manual(values = c("solid", "dashed", "dotted"))+
  scale_color_manual(values =  c("orangered", "royalblue", "yellowgreen", "firebrick","orange"))+
  scale_x_continuous(breaks = seq(1980,2015, 5), limits = c(1980,2015))+
  guides(linetype = FALSE)+
  labs(x = "",
       y = "Proporción (%)",
       title = "Proporción de la Población Viviendo en Extrema Pobreza", 
       subtitle = "Extrema Pobreza se refiere a la población con ingresos de US$ 1.90/día (2011 PPP)",
       caption = "Fuente: PovcalNet, http://iresearch.worldbank.org/PovcalNet/home.aspx")+
  opts()