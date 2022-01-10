library(tmap)
library(ClustGeo)
library(dplyr)
tm_shape(zones) +
  tm_grid(col = "white", lwd = 2, labels.size = .8) +
  tm_polygons(col = "lightblue", border.col = "black", border.alpha = 1) +
  tm_shape(stops_trips) +
  tm_dots(col = "tcam", style = "quantile") +
  tm_layout(bg.color = "#DAF7A6", 
            main.title = "Tiempo de Caminata", 
            main.title.position = "center", 
            title.size = 1, 
            legend.show = F) +
  tm_compass(position = c("left", "top"), type = "4star", size = 2) +
  tm_scale_bar(position = c("right", "bottom"))

D0 <- stops_trips %>%
  st_drop_geometry() %>%
  select(-paraderosubida) %>% 
  dist()

D1 <- as.dist(st_distance(stops_trips, stops_trips))

tree <- hclustgeo(D0)
plot(tree, hang = -1, label = F, xlab = "", ylab = "", main = "")

P1.5 <- cutree(tree, 1500)

hclus_stops <- mutate(stops_trips, P1.5 = P1.5)

tm_shape(hclus_stops) +
  tm_dots(col = "P1.5", breaks = seq(0, 1500, 300)) +
  tm_layout(legend.outside = T)

# cr <- choicealpha(D0, D1, range.alpha = seq(0, 1, .1), K = 3000, graph = F)

hclustgeo(D0, D1, alpha =.2)


w_tree <- lapply(seq(0, 1, .1), function(x) hclustgeo(D0, D1, alpha = x))
w_P1.5 <- lapply(w_tree, function(x) cutree(x, k = 1500))
hclust_lst <- lapply(w_P1.5, function(x) mutate(stops_trips, P1.5 = x))
lapply(1:length(hclust_lst), function(x) tm_shape(hclust_lst[[x]]) +
         tm_dots("P1.5", breaks = seq(0, 1500, 300)) +
         tm_layout(title = x))

hcent <- hclust_lst[[2]] %>%
  mutate(x = st_coordinates(.)[, 1],
         y = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  group_by(P1.5) %>% 
  summarise(x = mean(x), 
            y = mean(y)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 32719)

tm_shape(hcent) +
  tm_dots(col = "black", size = .2, shape = 10)

# st_voronoi(st_union(hcent), st_bbox(hcent)) %>%
#   st_write("output/voronoi.gpkg")
#   plot()

  st_vor <- st_collection_extract(st_voronoi(do.call(c, st_geometry(hcent)))) %>%
    `st_crs<-`(32719) %>%
    st_intersection(nngeo::st_remove_holes(st_union(zones)))
  
  
    
  
    tm_shape(zones) +
      tm_polygons(col = "red") +
    tm_shape(st_vor) +
    tm_polygons(alpha = .5) +
      tm_shape(hcent) +
      tm_dots(alpha = .1)
##########################################################
############################################################
library(rmarkdown)
rmarkdown::render(input = "code/Stops_Clus_Gnrl.Rmd", output_dir = "output/", output_format = "html_document")
