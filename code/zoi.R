library(sf)
library(tmap)
# plot(zones[,1])
rts_shp <- st_read("data/Shapes 06Jul2019.shp") %>%
  filter(UN == 2)
# plot(rts_shp[,1])

zoi <- st_filter(x = zones, y = rts_shp, .predicate = st_intersects) 

tm_shape(zoi) +
  tm_polygons(col = "orange") +
  tm_shape(zones) +
  tm_polygons(col = "gray")

tm_shape(zones) +
  tm_grid(col = "white", lwd = 2, labels.size = .8) +
  tm_polygons(col = "gray") +
  tm_shape(zoi) +
  tm_polygons(col = "red", border.col = "black", border.alpha = .1) +
  tm_layout(bg.color = "lightblue", title = "Área de Estudio") +
  tm_compass(position = c("right", "top"), type = "rose", size = 3) +
  tm_scale_bar(position = c("right", "bottom")) 

