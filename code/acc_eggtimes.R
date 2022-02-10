library(sfnetworks)
library(tidygraph)
ctd <- st_centroid(zoi)
zones_vial <- st_join(vial_zoi, zoi, .predicate = st_within) %>%
  na.omit() %>%
  # filter(Zona %in% c(375, 376)) %>%
  as_sfnetwork(directed = F) %>% 
  activate("edges") %>%
  mutate(weight = edge_length())
start_pts <- distinct(select(st_drop_geometry(zoi_trips), 
                             paraderosubida_SIMT, 
                             x_sub, 
                             y_sub)) %>%
  st_as_sf(coords = c("x_sub", "y_sub"), crs = 32719) %>% 
  st_join(zoi, .predicate = st_within) #%>%
  # na.omit() 

end_pts <- distinct(select(st_drop_geometry(zoi_trips), 
                             paraderobajada_SIMT, 
                             x_baj, 
                             y_baj)) %>%
  st_as_sf(coords = c("x_baj", "y_baj"), crs = 32719) %>% 
  st_join(zoi, .predicate = st_within) #%>%
  # na.omit()

start_pts2 <- distinct(select(st_drop_geometry(zoi_trips), 
                             paraderosubida_SIMT, 
                             x_sub, 
                             y_sub)) %>%
  filter(!paraderosubida_SIMT %in% c("PD553", 
                                     "PD535", 
                                     "PF286", 
                                     "PG1213", 
                                     "MONSENOR EYZAGUIRRE", 
                                     "PG192")) %>%
  bind_rows(tibble(paraderosubida_SIMT = c("PD553", 
                                          "PD535", 
                                          "PF286", 
                                          "PG1213", 
                                          "MONSENOR EYZAGUIRRE", 
                                          "PG192"),
                  x_sub = c(353538.5262269145,
                            353534.8736346469,
                            355734.5458669194,
                            344202.2974691084,
                            350011.18743581336,
                            349015.1965470637),
                  y_sub = c(6295645.183735825,
                            6295629.964601376,
                            6283374.502934581,
                            6278005.192301198,
                            6297309.400843736, 
                            6281213.9946082905))) %>%
  st_as_sf(coords = c("x_sub", "y_sub"), crs = 32719) %>% 
  st_join(zoi, .predicate = st_within)

end_pts2 <- distinct(select(st_drop_geometry(zoi_trips), 
                              paraderobajada_SIMT, 
                              x_baj, 
                              y_baj)) %>%
  filter(!paraderobajada_SIMT %in% c("PD553", 
                                     "PD535", 
                                     "PF286", 
                                     "PG1213", 
                                     "MONSENOR EYZAGUIRRE", 
                                     "PG192")) %>%
  bind_rows(tibble(paraderobajada_SIMT = c("PD553", 
                                           "PD535", 
                                           "PF286", 
                                           "PG1213", 
                                           "MONSENOR EYZAGUIRRE", 
                                           "PG192"),
                   x_baj = c(353538.5262269145,
                             353534.8736346469,
                             355734.5458669194,
                             344202.2974691084,
                             350011.18743581336,
                             349015.1965470637),
                   y_baj = c(6295645.183735825,
                             6295629.964601376,
                             6283374.502934581,
                             6278005.192301198,
                             6297309.400843736, 
                             6281213.9946082905))) %>%
  st_as_sf(coords = c("x_baj", "y_baj"), crs = 32719) %>% 
  st_join(zoi, .predicate = st_within)
  
t_net <- function(pts, nm){
  #path order
  short_net <- lapply(sort(unique(pts$Zona)), function(x) 
    lapply(1:nrow(filter(pts, Zona == x)), 
           function(y) filter(zones_vial, Zona == x) %>%
             convert(to_spatial_shortest_paths, 
                     from = filter(pts, Zona == x)[y,], 
                     to = filter(ctd, Zona == x)) %>%
             st_as_sf()))
  #shortest time
  time_net <- lapply(short_net, function(x)
    lapply(1:length(x), function(y)
      sum(st_distance(x[[y]])/units::set_units(48, "m/min"))
    )
  ) %>%
     unlist()
  time_net <- bind_cols(!!sym(nm) := time_net, 
                        arrange(filter(pts, Zona %in% unique(pts$Zona)), Zona)) 
  
  return(time_net)
}

time_acc <- t_net(pts = start_pts2, nm = "time_acc")
time_egg <- t_net(pts = end_pts2, nm = "time_egg")