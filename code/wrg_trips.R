vjs_pma <- BBDD_vjs(DDBB_v = "[viajes201904]", per = "'04 - PUNTA MANANA'") %>%
  #handle NAs
  mutate_if(is.character, ~replace(., .x == "-", NA)) %>%
  #fix data types
  mutate(netapa = as.integer(netapa)) %>%
  mutate(across(starts_with("t"), as.numeric)) %>%
  #create OD validation to imputation process
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
           T ~ 0))
