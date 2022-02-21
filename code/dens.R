library(sf)
library(dplyr)
library(ggplot2)
cens_stgo <- 
  st_read("data/xn--Censo_2017_Distrito_Censal__Poblacin,_viviendas_por_rea_y_densidad-gmf02j.geojson") %>%
  filter(REGION == 13 & NOM_PROVIN == "SANTIAGO" | NOM_COMUNA %in% c("SAN BERNARDO", "PUENTE ALTO")) %>%
  st_transform(32719)

stops_trips <- st_join(stops_trips, select(cens_stgo, Densidad))

###########add to stops trips

tm_shape(cens_stgo) +
  tm_polygons(col = "Densidad")


ggplot(stops_trips, aes(y = tviaje)) +
  geom_boxplot(width = .3, fill = "lightblue") +
  xlab("Paradas") +
  ylab("Tiempos de Viaje") +
  labs(title="Estadística Tiempos de Viaje",x="Paradas", y = "Tiempo") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(stops_trips, aes(tviaje)) +
  geom_density(fill = "lightblue")
