library(sf)
cens_stgo <- 
  st_read("data/xn--Censo_2017_Distrito_Censal__Poblacin,_viviendas_por_rea_y_densidad-gmf02j.geojson") %>%
  filter(REGION == 13 & NOM_PROVIN == "SANTIAGO" | NOM_COMUNA %in% c("SAN BERNARDO", "PUENTE ALTO")) %>%
  st_transform(32719)

###########add to stops trips

tm_shape(cens_stgo) +
  tm_polygons(col = "Densidad")
