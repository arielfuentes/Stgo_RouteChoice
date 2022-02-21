library(dplyr)
library(tidyr)
source("code/zoi.R", encoding = "utf-8")
vjs_pma <- BBDD_vjs(DDBB_v = "[viajes201904]", per = "'04 - PUNTA MANANA'") %>%
  #handle NAs
  mutate_if(is.character, ~replace(., .x == "-", NA)) %>%
  #fix data types
  mutate(netapa = as.integer(netapa)) %>%
  mutate(across(starts_with("t"), as.numeric)) %>%
  #create OD validation to imputation process ----
  mutate(valid = case_when(netapa == 1 & 
                               !is.na(serv_1era_etapa) & 
                               !is.na(t_1era_etapa) |
           (netapa == 2 & 
              !is.na(serv_1era_etapa) & 
              !is.na(t_1era_etapa) &
              !is.na(tespera_1era_etapa) & 
              !is.na(ttrasbordo_1era_etapa) & 
              !is.na(tcaminata_1era_etapa) &
              !is.na(serv_2da_etapa) &
              !is.na(t_2da_etapa) & 
              !is.na(paraderosubida_2da)) |
             (netapa == 3 & 
                !is.na(serv_1era_etapa) & 
                !is.na(t_1era_etapa) &
                !is.na(tespera_1era_etapa) & 
                !is.na(ttrasbordo_1era_etapa) & 
                !is.na(tcaminata_1era_etapa) &
                !is.na(serv_2da_etapa) &
                !is.na(t_2da_etapa) &
                !is.na(tespera_2da_etapa) & 
                !is.na(ttrasbordo_2da_etapa) & 
                !is.na(tcaminata_2da_etapa) &
                !is.na(serv_3era_etapa) & 
                !is.na(t_3era_etapa) & 
                !is.na(paraderosubida_2da) & 
                !is.na(paraderosubida_3era)) |
             (netapa >= 4 & 
                !is.na(serv_1era_etapa) & 
                !is.na(t_1era_etapa) &
                !is.na(tespera_1era_etapa) & 
                !is.na(ttrasbordo_1era_etapa) & 
                !is.na(tcaminata_1era_etapa) &
                !is.na(serv_2da_etapa) &
                !is.na(t_2da_etapa) &
                !is.na(tespera_2da_etapa) & 
                !is.na(ttrasbordo_2da_etapa) & 
                !is.na(tcaminata_2da_etapa) &
                !is.na(serv_3era_etapa) & 
                !is.na(t_3era_etapa) &
                !is.na(tespera_3era_etapa) & 
                !is.na(ttrasbordo_3era_etapa) & 
                !is.na(tcaminata_3era_etapa) &
                !is.na(serv_4ta_etapa) & 
                !is.na(t_4ta_etapa) & 
                !is.na(paraderosubida_2da) & 
                !is.na(paraderosubida_3era) & 
                !is.na(paraderosubida_4ta)) ~ 1,
           T ~ 0)) %>%
  #create OD
  unite(paraderosubida, paraderobajada,
        col = "OD",
        sep = "*",
        remove = F)
#valid status ----
valid_stat <- group_by(vjs_pma, 
                       OD, 
                       valid) %>%
  summarise(Demanda = sum(Demanda)) %>%
  pivot_wider(names_from = valid, values_from = Demanda) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate(f.valid = ((`1` + `0`)/`1`)) %>%
  ungroup() %>%
  select(OD, f.valid) %>%
  filter(f.valid != Inf)
#create valid data ----
vjs_pma <- left_join(vjs_pma, valid_stat) %>%
  mutate(Demanda = Demanda*f.valid,
         interpar = case_when(paraderosubida == paraderobajada ~ 0,
                              T ~ 1)) %>%
  filter(valid ==1,
         interpar ==1) %>%
  select(-c("valid", "f.valid", "interpar", "OD")) %>%
  mutate_if(is.numeric, ~replace_na(., 0))
rm(valid_stat)
#add lines dictionary
lines <- dicc_lines("Shapes 06Jul2019.shp") %>%
  select(COD_SINRUT, COD_USUSEN) %>%
  distinct() %>%
  arrange(COD_USUSEN)
lines_user_id <- distinct(lines, COD_USUSEN) %>%
  mutate(id_serv = row_number())
lines <- left_join(lines, lines_user_id)
rm(lines_user_id)
#add user service's names ----
vjs_pma <- left_join(vjs_pma, 
                     rename(lines, 
                            serv_1era_etapa = COD_SINRUT,
                            serv_1era_usu = COD_USUSEN,
                            id_1era = id_serv)) %>%
  left_join(rename(lines, 
                   serv_2da_etapa = COD_SINRUT,
                   serv_2da_usu = COD_USUSEN,
                   id_2da = id_serv)) %>%
  left_join(rename(lines, 
                   serv_3era_etapa = COD_SINRUT,
                   serv_3era_usu = COD_USUSEN,
                   id_3era = id_serv)) %>%
  left_join(rename(lines, 
                   serv_4ta_etapa = COD_SINRUT,
                   serv_4ta_usu = COD_USUSEN,
                   id_4ta = id_serv)) %>%
  ##Identify routes ----
  mutate(rts = case_when(netapa == 1 ~ as.character(id_1era),
                         netapa == 2 ~ paste(id_1era, "|", id_2da),
                         netapa == 3 ~ paste(id_1era, "|", id_2da, "|", id_3era),
                         netapa == 4 ~ paste(id_1era, "|", id_2da, "|", id_3era, "|", id_4ta))) %>%
  select(-c("serv_1era_etapa", 
            "serv_2da_etapa", 
            "serv_3era_etapa", 
            "serv_4ta_etapa",
            "serv_1era_usu", 
            "serv_2da_usu", 
            "serv_3era_usu", 
            "serv_4ta_usu",
            "id_1era",
            "id_2da",
            "id_3era",
            "id_4ta")) #%>%
  # na.omit()
##fix stops names ----
vjs_pma <- mutate(vjs_pma,
                  paraderosubida = if_else(paraderosubida == "IRARRAZAVAL  L3", 
                                           "IRARRAZAVAL L3",
                                           paraderosubida),
                  paraderosubida_2da = if_else(paraderosubida_2da == "IRARRAZAVAL  L3", 
                                           "IRARRAZAVAL L3",
                                           paraderosubida_2da),
                  paraderosubida_3era = if_else(paraderosubida_3era == "IRARRAZAVAL  L3", 
                                           "IRARRAZAVAL L3",
                                           paraderosubida_3era),
                  paraderosubida_4ta = if_else(paraderosubida_4ta == "IRARRAZAVAL  L3", 
                                                "IRARRAZAVAL L3",
                                                paraderosubida_4ta),
                  paraderobajada = if_else(paraderobajada == "IRARRAZAVAL ", 
                                           "IRARRAZAVAL",
                                           paraderobajada)) %>%
  ##add SIMT codes ----
  left_join(rename(stops_df, 
                   paraderosubida = CODINFRA,
                   paraderosubida_SIMT = SIMT,
                   x_sub = x,
                   y_sub = y)) %>%
  left_join(rename(stops_df, 
                   paraderosubida_2da = CODINFRA,
                   paraderosubida_2da_SIMT = SIMT,
                   x_sub2 = x,
                   y_sub2 = y)) %>%
  left_join(rename(stops_df, 
                   paraderosubida_3era = CODINFRA,
                   paraderosubida_3era_SIMT = SIMT,
                   x_sub3 = x,
                   y_sub3 = y)) %>%
  left_join(rename(stops_df, 
                   paraderosubida_4ta = CODINFRA,
                   paraderosubida_4ta_SIMT = SIMT,
                   x_sub4 = x,
                   y_sub4 = y)) %>%
  left_join(rename(stops_df, 
                   paraderobajada = CODINFRA,
                   paraderobajada_SIMT = SIMT,
                   x_baj = x,
                   y_baj = y)) %>%
  # filter((netapa == 2 & !is.na(paraderosubida)
  #         & !is.na(paraderosubida_2da_SIMT)
  #         & !is.na(paraderobajada_2da_SIMT)) |
  #          (netapa %in% c(3, 4) & !is.na(paraderosubida_2da_SIMT)
  #           & !is.na(paraderosubida_2da_SIMT) 
  #           & !is.na(paraderosubida_3era_SIMT)
  #           | paraderobajada_SIMT != "PF1126")) %>%
  select(-c("paraderosubida", 
            "paraderosubida_2da", 
            "paraderosubida_3era", 
            "paraderosubida_4ta", 
            "paraderobajada")) %>%
  ##group trip parameters ----
  mutate(tviaje = t_1era_etapa + t_2da_etapa + t_3era_etapa + t_4ta_etapa,
         tesp = tespera_1era_etapa + tespera_2da_etapa + tespera_3era_etapa,
         tb2 = ttrasbordo_1era_etapa + ttrasbordo_2da_etapa + ttrasbordo_3era_etapa,
         tcam = tcaminata_1era_etapa + tcaminata_2da_etapa + tcaminata_3era_etapa,
         geom = case_when(netapa == 1 ~ sprintf("LINESTRING(%s %s, %s %s)", 
                                                x_sub, y_sub, x_baj, y_baj),
                          netapa == 2 ~ sprintf("LINESTRING(%s %s, %s %s, %s %s)", 
                                                x_sub, y_sub, x_sub2, y_sub2, x_baj, y_baj),
                          netapa == 3 ~ sprintf("LINESTRING(%s %s, %s %s, %s %s, %s %s)", 
                                                        x_sub, y_sub, x_sub2, y_sub2, x_sub3, y_sub3, x_baj, y_baj),
                          netapa == 4 ~ sprintf("LINESTRING(%s %s, %s %s, %s %s, %s %s, %s %s)", 
                                                x_sub, y_sub, x_sub2, y_sub2, x_sub3, y_sub3, x_sub4, y_sub4, x_baj, y_baj))) %>%
  filter(!is.na(geom)) %>%
  st_as_sf(wkt = "geom", crs = 32719) %>%
  filter(Demanda > .2)
  # select(-c("t_1era_etapa", "t_2da_etapa", "t_3era_etapa", "t_4ta_etapa",
  #           "tespera_1era_etapa", "tespera_2da_etapa", "tespera_3era_etapa",
  #           "ttrasbordo_1era_etapa", "ttrasbordo_2da_etapa", "ttrasbordo_3era_etapa", 
  #           "tcaminata_1era_etapa", "tcaminata_2da_etapa", "tcaminata_3era_etapa")) #%>%
  # na.omit()

# vjs_pma <- vjs_pma %>% filter(Demanda > 0)
# zoi_trips <- st_join(head(vjs_pma), zoi, join = st_intersects) #%>%
  # filter(!is.na(id))
# zoi_trips_a <- st_filter(vjs_pma[1:200000,], zoi, .predicate = st_within)
# zoi_trips_b <- st_filter(vjs_pma[200001:400000,], zoi, .predicate = st_within)
# zoi_trips_c <- st_filter(vjs_pma[400001:600000,], zoi, .predicate = st_within)
# zoi_trips_d <- st_filter(vjs_pma[600001:nrow(vjs_pma),], zoi, .predicate = st_within)
# zoi_trips <- bind_rows(zoi_trips_a, zoi_trips_b, zoi_trips_c, zoi_trips_d)
# rm(zoi_trips_a, zoi_trips_b, zoi_trips_c, zoi_trips_d)
zoi_trips <- st_filter(vjs_pma, nngeo::st_remove_holes(st_union(zoi)), .predicate = st_within)

tm_shape(nngeo::st_remove_holes(st_union(zoi))) +
  tm_grid(col = "white", lwd = 2, labels.size = .8) +
  tm_polygons(col = "red", border.col = "black", border.alpha = 1) +
  tm_shape(arrange(zoi_trips, desc(Demanda))) +
  tm_lines(lwd = "Demanda", scale = 6) +
  tm_layout(bg.color = "#DAF7A6", 
            main.title = "Principales Flujos", 
            main.title.position = "center", 
            title.size = 1, 
            legend.show = F) +
  tm_compass(position = c("left", "top"), type = "4star", size = 2) +
  tm_scale_bar(position = c("right", "bottom")) 

#test threshold for demand
# mutate(st_drop_geometry(zoi_trips), 
#        flt = if_else(Demanda > .2, Demanda, 0)) %>%
#   summarise(D_new = (sum(flt)/sum(Demanda)-1)*100)
# 
# nrow(filter(st_drop_geometry(zoi_trips), Demanda >.2))

# zoi_trips <- st_drop_geometry(zoi_trips) %>%
#   filter(Demanda > .2)

vjs_pma <- st_drop_geometry(vjs_pma)

stops_trips_1era <- vjs_pma %>% 
  group_by(paraderosubida = paraderosubida_SIMT) %>% 
  summarise(Demanda = sum(Demanda), 
            tviaje = mean(t_1era_etapa), 
            tesp = 0, 
            tb2 = mean(tb2), 
            tcam = mean(tcam), 
            x = mean(x_sub), 
            y = mean(y_sub)) %>%
  na.omit()

stops_trips_2da <- vjs_pma %>% 
  filter(netapa > 1) %>%
  group_by(paraderosubida = paraderosubida_2da_SIMT) %>% 
  summarise(Demanda = sum(Demanda), 
            tviaje = mean(t_2da_etapa), 
            tesp = mean(tespera_1era_etapa), 
            tb2 = mean(tb2), 
            tcam = mean(tcam), 
            x = mean(x_sub), 
            y = mean(y_sub)) %>%
  na.omit()

stops_trips_3era <- vjs_pma %>% 
  filter(netapa > 2) %>% 
  group_by(paraderosubida = paraderosubida_3era_SIMT) %>% 
  summarise(Demanda = sum(Demanda), 
            tviaje = mean(t_3era_etapa), 
            tesp = mean(tespera_2da_etapa), 
            tb2 = mean(tb2), 
            tcam = mean(tcam), 
            x = mean(x_sub), 
            y = mean(y_sub)) %>%
  na.omit()

stops_trips_4ta <- vjs_pma %>% 
  filter(netapa > 3) %>% 
  group_by(paraderosubida = paraderosubida_4ta_SIMT) %>% 
  summarise(Demanda = sum(Demanda), 
            tviaje = mean(t_4ta_etapa), 
            tesp = mean(tespera_3era_etapa), 
            tb2 = mean(tb2), 
            tcam = mean(tcam), 
            x = mean(x_sub), 
            y = mean(y_sub)) %>%
  na.omit()

stops_trips <- bind_rows(stops_trips_1era, 
                         stops_trips_2da, 
                         stops_trips_3era, 
                         stops_trips_4ta) %>%
  group_by(paraderosubida) %>%
  summarise(Demanda = sum(Demanda), 
            tviaje = mean(tviaje), 
            tesp = mean(tesp), 
            tb2 = mean(tb2), 
            tcam = mean(tcam), 
            x = mean(x), 
            y = mean(y)) %>%
  st_as_sf(coords = c("x", "y"), crs = 32719)
#add population variable
stops_trips <- st_join(stops_trips, select(cens_stgo, Densidad))

rm(stops_trips_1era, 
   stops_trips_2da, 
   stops_trips_3era, 
   stops_trips_4ta)

tm_shape(zones) +
  tm_grid(col = "white", lwd = 2, labels.size = .8) +
  tm_polygons(col = "lightblue", border.col = "black", border.alpha = 1) +
  tm_shape(stops_trips) +
  tm_dots(col = "red") +
  tm_layout(bg.color = "#DAF7A6", 
            main.title = "Paradas", 
            main.title.position = "center", 
            title.size = 1, 
            legend.show = F) +
  tm_compass(position = c("left", "top"), type = "4star", size = 2) +
  tm_scale_bar(position = c("right", "bottom")) 
################################################
library(sfnetworks)
library(tidygraph)
sub_net <- lapply(split(zoi, zoi$Zona), 
                  function(x) st_filter(vial_zoi, x, .predicate = st_within))
sub_net <- lapply(1:length(sub_net), 
                  function(x) as_sfnetwork(mutate(sub_net[[x]], 
                                                  Zona = names(sub_net)[x]),
                                           directed = FALSE))
# vjs_pma <- read_delim("data/vjs_pma.csv", delim = ",")
start_pts <- distinct(select(st_drop_geometry(zoi_trips), 
                             paraderosubida_SIMT, 
                             x_sub, 
                             y_sub)) %>%
  st_as_sf(coords = c("x_sub", "y_sub"), crs = 32719)
start_pts <- st_join(start_pts, zoi, .predicate = st_within)
# start_pts <- split(start_pts, start_pts$Zona)[1:3]
# d_cen <- sub_net[[1]] %>% 
#   activate("edges") %>%
#   mutate(weight = edge_length()) %>%
#   convert(to_spatial_shortest_paths, 
#           from = filter(start_pts, Zona == 4)[1,], 
#           to = st_centroid(filter(zoi, Zona == 4))) %>%
#   st_as_sf() %>%
#   summarise(vel_centr = sum(st_distance(.)/units::set_units(48, "m/min"))) %>%
#   st_drop_geometry()
# lapply(1:length(sub_net), function(x) sub_net[[x]] %>%
#          activate("edges") %>%
#          mutate(weight = edge_length()) %>%
#          convert(to_spatial_shortest_paths, 
#                  from = split(start_pts, start_pts$Zona)[[x]], 
#                  to = st_centroid(split(zoi, zoi$Zona)[[x]])))
start_pts_lst <- split(start_pts, start_pts$Zona)[1:3]
# lapply(1:nrow(split(start_pts, start_pts$Zona)[[1]]), function(x) sub_net[[1]] %>%
#          activate("edges") %>%
#          mutate(weight = edge_length()) %>%
#          convert(to_spatial_shortest_paths,
#                  from = split(start_pts, start_pts$Zona)[[1]][x,],
#                  to = st_centroid(split(zoi, zoi$Zona)[[1]])) %>%
#   st_as_sf() %>%
#   summarise(vel_centr = sum(st_distance(.)/units::set_units(48, "m/min"))) %>%
#   st_drop_geometry()) %>%
#   bind_rows() %>%
#   bind_cols(split(start_pts, start_pts$Zona)[[1]])
time_acc <- lapply(1:length(start_pts_lst), function(x) lapply(1:nrow(start_pts_lst[[x]]), 
                                                   function(y) sub_net[[x]] %>%
                                                     activate("edges") %>%
                                                     mutate(weight = edge_length()) %>%
                                                     convert(to_spatial_shortest_paths,
                                                             from = start_pts_lst[[x]][y,],
                                                             to = st_centroid(split(zoi, zoi$Zona)[[x]]))%>%
                                                     st_as_sf() %>%
                                                     summarise(vel_centr = sum(st_distance(.)/units::set_units(48, "m/min"))) %>%
                                                     st_drop_geometry()) %>%
         bind_rows() %>%
         bind_cols(start_pts_lst[[x]])) %>%
  bind_rows()
tm_shape(filter(zoi, Zona == 4)) + 
  tm_polygons() + 
  tm_shape(st_centroid(filter(zoi, Zona == 4))) + 
  tm_dots(col = "red") + 
  tm_shape(filter(start_pts, Zona == 4)[2,]) + 
  tm_dots(col = "blue") + 
  tm_shape(sub_net[[1]] %>% activate("edges") %>% st_as_sf()) + 
  tm_lines(col = "white") + 
  tm_shape(xy) + 
  tm_lines(col = "black")
# xy %>% 
#   mutate(dist_mt = st_distance(.)) %>% 
#   st_drop_geometry() %>% 
#   summarise(dist_mt = sum(dist_mt))
# net = as_sfnetwork(vial_zoi, directed = FALSE) %>%
#   # st_transform(3035) %>%
#   activate("edges") %>%
#   mutate(weight = edge_length()) %>%
#   morph(to_spatial_shortest_paths, from = 1, to = 10) %>%
#   mutate(in_paths = TRUE) %>%
#   unmorph()
# net = as_sfnetwork(vial_zoi, directed = FALSE) %>%
#   # st_transform(3035) %>%
#   activate("edges") %>%
#   mutate(weight = edge_length()) %>%
#   convert(to_spatial_shortest_paths, from = 1, to = 10)
# net
# 
# paths = st_network_paths(net, from = 495, to = c(458, 121))
# st_network_cost(net, from = 495, to = c(458, 121))
# ?to_spatial_shortest_paths(net, from = 495, to = c(458, 121))
# to_spatial_shortest_paths(paths)
# paths
# 
# paths %>%
#   slice(1) %>%
#   pull(node_paths) %>%
#   unlist()
# 
# paths %>%
#   slice(1) %>%
#   pull(edge_paths) %>%
#   unlist()
# 
# plot_path = function(node_path) {
#   net %>%
#     activate("nodes") %>%
#     slice(node_path) %>%
#     plot(cex = 1.5, lwd = 1.5, add = TRUE)
# }
# 
# colors = sf.colors(3, categorical = TRUE)
# 
# plot(net, col = "grey")
# paths %>%
#   pull(node_paths) %>%
#   walk(plot_path)
# net %>%
#   activate("nodes") %>%
#   st_as_sf() %>%
#   slice(c(495, 121, 458)) %>%
#   plot(col = colors, pch = 8, cex = 2, lwd = 2, add = TRUE)
# 
# net %>%
#   activate("edges") %>%
#   st_as_sf() %>% summarise(sum(SHAPE_Leng))

zoi_tripsCG <- zoi_trips %>%
  select(Demanda, paraderosubida_SIMT, paraderobajada_SIMT, netapa, rts, tviaje, tesp, tb2, tcam) %>%
  st_drop_geometry() %>%
  left_join(st_drop_geometry(select(time_acc, time_acc, paraderosubida_SIMT))) %>%
  left_join(st_drop_geometry(select(time_egg, time_egg, paraderobajada_SIMT))) %>%
  group_split(netapa)

zoi_tripsCG <- bind_rows(
zoi_tripsCG[[1]] %>%
  mutate(e1 = rts, e2 = NA_character_, e3 = NA_character_, e4 = NA_character_),
zoi_tripsCG[[2]] %>%
  separate(rts, into = c("e1", "e2"), remove = F) %>%
  mutate(e3 = NA_character_, e4 = NA_character_),
zoi_tripsCG[[3]] %>%
  separate(rts, into = c("e1", "e2", "e3"), remove = F) %>%
  mutate(e4 = NA_character_),
zoi_tripsCG[[4]] %>%
  separate(rts, into = c("e1", "e2", "e3", "e4"), remove = F)
) %>%
  mutate(e1 = as.numeric(e1), 
         e2 = as.numeric(e2), 
         e3 = as.numeric(e3), 
         e4 = as.numeric(e4),
         Tarifa = case_when(e1 >= 756 | e2 >= 756 | e3 >= 756 | e4 >= 756 ~ 800,
                            T ~ 700)) %>%
  select(-c("e1", "e2", "e3", "e4"))

#Usando VST: Valor Social del Tiempo de viaje, transformamos variables

alpha = 2434/60
zoi_tripsCG <- mutate(zoi_tripsCG, tviaje = tviaje*alpha, 
                      tesp = 2*tesp*alpha, 
                      tb2 = 2*tb2*alpha, 
                      tcam = 2*tcam*alpha,
                      time_acc = 2*time_acc*alpha,
                      time_egg = 2*time_egg*alpha)

library(ggplot2)
ggplot(zoi_tripsCG, aes(tviaje, Demanda)) +
  geom_point()
