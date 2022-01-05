library(dplyr)
library(tidyr)
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
#add lines dictionary
lines <- dicc_lines("Shapes 06Jul2019.shp") %>%
  select(COD_SINRUT, COD_USUSEN) %>%
  distinct() %>%
  arrange(COD_USUSEN)
lines_user_id <- distinct(lines, COD_USUSEN) %>%
  mutate(id_serv = row_number())
lines <- left_join(lines, lines_user_id)
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
  select(-c("paraderosubida", "paraderosubida_2da", "paraderosubida_3era", "paraderobajada")) %>%
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
  select(-c("t_1era_etapa", "t_2da_etapa", "t_3era_etapa", "t_4ta_etapa",
            "tespera_1era_etapa", "tespera_2da_etapa", "tespera_3era_etapa",
            "ttrasbordo_1era_etapa", "ttrasbordo_2da_etapa", "ttrasbordo_3era_etapa", 
            "tcaminata_1era_etapa", "tcaminata_2da_etapa", "tcaminata_3era_etapa")) #%>%
  # na.omit()

vjs_pma <- vjs_pma %>% filter(Demanda > 0)
zoi_trips <- st_join(head(vjs_pma), zoi, join = st_intersects) #%>%
  # filter(!is.na(id))
zoi_trips_a <- st_filter(vjs_pma[1:200000,], 1, zoi, join = st_within)
zoi_trips_b <- st_filter(vjs_pma[200001:400000,], zoi, join = st_within)
zoi_trips_c <- st_filter(vjs_pma[400001:600000,], zoi, join = st_within)
zoi_trips_d <- st_filter(vjs_pma[600001:832883,], zoi, join = st_within)
zoi_trips <- bind_rows(zoi_trips_a, zoi_trips_b, zoi_trips_c, zoi_trips_d)
rm(zoi_trips_a, zoi_trips_b, zoi_trips_c, zoi_trips_d)

tm_shape(nngeo::st_remove_holes(st_union(zoi))) +
  tm_grid(col = "white", lwd = 2, labels.size = .8) +
  tm_polygons(col = "red", border.col = "black", border.alpha = 1) +
  tm_shape(arrange(zoi_trips, desc(Demanda))[1:120,]) +
  tm_lines(lwd = "Demanda", scale = 6) +
  tm_layout(bg.color = "#DAF7A6", 
            main.title = "Principales Flujos", 
            main.title.position = "center", 
            title.size = 1, 
            legend.show = F) +
  tm_compass(position = c("left", "top"), type = "4star", size = 2) +
  tm_scale_bar(position = c("right", "bottom")) 


#test threshold for demanda (d >= .5)