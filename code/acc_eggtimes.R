library(sfnetworks)
library(tidygraph)
ctd <- st_centroid(zoi)
zones_vial <- st_join(vial_zoi, zoi, .predicate = st_within) %>%
  na.omit() %>%
  filter(Zona %in% c(375, 376)) %>%
  as_sfnetwork(directed = F) %>% 
  activate("edges") %>%
  mutate(weight = edge_length())
start_pts <- distinct(select(st_drop_geometry(zoi_trips), 
                             paraderosubida_SIMT, 
                             x_sub, 
                             y_sub)) %>%
  st_as_sf(coords = c("x_sub", "y_sub"), crs = 32719) %>% 
  st_join(zoi, .predicate = st_within) %>%
  na.omit() 

end_pts <- distinct(select(st_drop_geometry(zoi_trips), 
                             paraderobajada_SIMT, 
                             x_baj, 
                             y_baj)) %>%
  st_as_sf(coords = c("x_baj", "y_baj"), crs = 32719) %>% 
  st_join(zoi, .predicate = st_within) %>%
  na.omit()

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

t_net(pts = filter(start_pts, Zona %in% c(375:376)), nm = "time_acc")
t_net(pts = filter(end_pts, Zona %in% c(375:376)), nm = "time_egg")