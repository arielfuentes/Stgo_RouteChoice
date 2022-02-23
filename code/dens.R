library(sf)
library(dplyr)
library(ggplot2)
library(tidyr)
cens_stgo <- 
  st_read("data/xn--Censo_2017_Distrito_Censal__Poblacin,_viviendas_por_rea_y_densidad-gmf02j.geojson") %>%
  filter(REGION == 13 & NOM_PROVIN == "SANTIAGO" | NOM_COMUNA %in% c("SAN BERNARDO", "PUENTE ALTO")) %>%
  st_transform(32719)

stops_trips <- st_join(stops_trips, select(cens_stgo, Densidad))

tm_shape(stops_trips) +
  tm_dots(col = "Densidad")
###########add to stops trips

tm_shape(cens_stgo) +
  tm_polygons(col = "Densidad")


zoi_tripsCG %>%
  group_by(paraderosubida_SIMT) %>%
  summarise(Demanda = mean(Demanda),
            tviaje = mean(tviaje),
            tesp = mean(tesp),
            tb2 = mean(tb2),
            tcam = mean(tcam),
            time_acc = mean(time_acc),
            time_egg = mean(time_egg),
            Tarifa = mean(Tarifa)) %>%
  pivot_longer(!paraderosubida_SIMT,
               names_to = "variables",
               values_to = "valor") %>%
  filter(!variables %in% c("Tarifa", "Demanda", "time_acc", "time_egg")) %>%
  ggplot(aes(variables, valor)) +
  geom_violin() +
  geom_boxplot(width = .3, fill = "lightblue")
pivot_longer(st_drop_geometry(stops_trips), 
             !paraderosubida, 
             names_to = "variables", 
             values_to = "valor") %>%
  filter(!variables %in% c("Demanda", "Densidad")) %>%
  ggplot(aes(variables, valor, fill = "variables")) +
  # geom_violin(fill = "lightblue") +
  geom_boxplot(width = .3, fill = "#FFC300") +
  xlab("") +
  ylab("Tiempo") +
  labs(title="Diagrama de Cajas de Tiempos", y = "Tiempo") +
  theme(plot.title = element_text(hjust = 0.5)
    # axis.text.x=element_blank(),
    #     axis.ticks.x=element_blank()
    )
  
ggplot(stops_trips, aes(y = tviaje)) +
  geom_boxplot(width = .3, fill = "lightblue") +
  xlab("Paradas") +
  ylab("Tiempos de Viaje") +
  labs(title="Estadística Tiempos de Viaje",x="Paradas", y = "Tiempo") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(stops_trips, aes(tviaje)) +
  geom_density(fill = "lightblue")
