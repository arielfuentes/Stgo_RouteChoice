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
              !is.na(t_2da_etapa)) |
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
                !is.na(t_3era_etapa)) |
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
                !is.na(t_4ta_etapa)) ~ 1,
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
  select(-c("Demanda", "valid", "f.valid", "interpar", "OD")) %>%
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
  #identify routes ----
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
            "id_4ta"))
