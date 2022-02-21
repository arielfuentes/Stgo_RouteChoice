#adatrap query ----
BBDD_vjs <- function(DDBB_v, per) {
  library(DBI)
  library(dplyr)
  library(tidyr)
  #connection
  con <- DBI::dbConnect(odbc::odbc(),
                        Driver   = "SQL Server",
                        Server   = "10.222.128.21,9433",
                        Database = "uchile",
                        UID      = rstudioapi::askForPassword("Database user"),
                        PWD      = rstudioapi::askForPassword("Database password"),
                        encoding = "latin1")
  #trips
  sql.trips <- paste0("SELECT SUM(CAST(factorexpansion AS FLOAT))/5 AS Demanda, 
  periodomediodeviaje,
  netapa,
  paraderosubida,
  paraderosubida_2da,
  paraderosubida_3era,
  paraderosubida_4ta,
  paraderobajada,
  serv_1era_etapa, AVG(CAST(REPLACE(t_1era_etapa, '-', '') AS FLOAT)) AS t_1era_etapa,	AVG(CAST(REPLACE(tespera_1era_etapa, '-', '') AS FLOAT)) AS tespera_1era_etapa, AVG(CAST(REPLACE(ttrasbordo_1era_etapa, '-', '') AS FLOAT)) AS ttrasbordo_1era_etapa, AVG(CAST(REPLACE(tcaminata_1era_etapa, '-', '') AS FLOAT)) AS tcaminata_1era_etapa,
  serv_2da_etapa,	AVG(CAST(REPLACE(t_2da_etapa, '-', '') AS FLOAT)) AS t_2da_etapa, AVG(CAST(REPLACE(tespera_2da_etapa, '-', '') AS FLOAT)) AS tespera_2da_etapa, AVG(CAST(REPLACE(ttrasbordo_2da_etapa, '-', '') AS FLOAT)) AS ttrasbordo_2da_etapa, AVG(CAST(REPLACE(tcaminata_2da_etapa, '-', '') AS FLOAT)) AS tcaminata_2da_etapa, 
  serv_3era_etapa, AVG(CAST(REPLACE(t_3era_etapa, '-', '') AS FLOAT)) AS t_3era_etapa, AVG(CAST(REPLACE(tespera_3era_etapa, '-', '') AS FLOAT)) AS tespera_3era_etapa, AVG(CAST(REPLACE(ttrasbordo_3era_etapa, '-', '') AS FLOAT)) AS ttrasbordo_3era_etapa, AVG(CAST(REPLACE(tcaminata_3era_etapa, '-', '') AS FLOAT)) AS tcaminata_3era_etapa, 
  serv_4ta_etapa, AVG(CAST(REPLACE(t_4ta_etapa, '-', '') AS FLOAT)) AS t_4ta_etapa
  FROM [uchile].[dbo].", DDBB_v,
  "WHERE periodomediodeviaje = ", per, " AND paraderosubida <> '-' AND paraderobajada <> '-'
  GROUP BY periodomediodeviaje, 
  netapa,
  paraderosubida,
  paraderosubida_2da,
  paraderosubida_3era,
  paraderosubida_4ta,
  paraderobajada, 
  serv_1era_etapa, 
  serv_2da_etapa,	
  serv_3era_etapa, 
  serv_4ta_etapa;")
  trips <- dbGetQuery(conn = con,
                      statement = sql.trips) %>%
    as_tibble()
  dbDisconnect(con)
  return(trips)
}
#lines dicc ----
dicc_lines <- function(shp){
  library(sf)
  library(dplyr)
  library(readr)
  user_lines <- st_read(paste0("data/", shp)) %>%
    st_drop_geometry() %>%
    as_tibble() %>%
    select(ROUTE_NAME, COD_USUARI, COD_SINRUT, COD_USUSEN) %>%
    filter(COD_SINRUT != "NA") %>%
    distinct() %>%
    bind_rows(read_delim("data/faltantes.csv", ";"))
}
#read TPub stops ---- 
stops <- function(file, sheet){
  library(readxl)
  stops_bus <- read_excel(paste0("data/", file),
                          sheet = sheet, 
                          col_types = c(rep("skip", 6), 
                                        "text", "text", 
                                        rep("skip", 4), 
                                        "numeric", "numeric", 
                                        rep("skip", 3))) %>%
    filter(`Código paradero TS` != "POR DEFINIR") %>%
    distinct()
  return(stops_bus)
}

library(dplyr)
library(readr)
stops_bus <- bind_rows(stops("2019-07-06_consolidado_anexo4_(Circunvalación)_anual.xlsx", 
                "23Jun2019 al 06Jul2019"), 
          stops("2019-07-06_consolidado_anexo4_(Circunvalación)_anual.xlsx", 
                "01Abr2019 al 22Jun2019"),
          stops("2019-07-06_consolidado_anexo4_(Circunvalación)_anual.xlsx", 
                "01Mar2019 al 31Mar2019")) %>%
  group_by(`Código paradero TS`, `Código  paradero Usuario`) %>%
  summarise(x = mean(x), y = mean(y)) %>%
  ungroup() %>%
  select(CODINFRA = `Código paradero TS`, 
         SIMT = `Código  paradero Usuario`, 
         x,
         y) %>%
  filter(!SIMT %in% c("15", "PF1126", "PD1614")) %>%
  bind_rows(tibble(CODINFRA = c("L-34-40-103-OP", "L-32-41-20-NS"), 
                   SIMT = c("PF1126", "PD1614"), 
                   x = c(351877.7, 358184), 
                   y = c(6279407.7, 6292345.8)))
  
stops_mt <- read_delim("data/dicc_mt.csv", delim = ";") %>%
  select(CODINFRA, X, Y) %>%
  mutate(SIMT = CODINFRA) %>%
  select(CODINFRA, SIMT, x = X, y = Y)

stops_df <- bind_rows(stops_bus, stops_mt)
rm(stops_bus, stops_mt)

#read zones ----
library(sf)
zones <- st_read("data/Zonificacion_EOD2012.shp") %>%
  st_make_valid()
vial_stgo <- st_read("data/Capas_Base/Capas_Base.shp") %>%
  st_transform(32719) %>%
  select(-c("NOMBRE_VIA", "CLASE_COMU", "SHAPE_Leng")) %>%
  filter(!CLASE_URBA %in% c("CARRETERA", "PRIVADO"))
source("code/zoi.R", encoding = "utf-8")
vial_zoi <- st_filter(vial_stgo, nngeo::st_remove_holes(st_union(zoi)), .predicate = st_within)
tm_shape(nngeo::st_remove_holes(st_union(zoi))) +
  tm_polygons(col = "red") +
  tm_shape(vial_zoi) +
  tm_lines()
st_write("data/vial_zoi.gpkg", obj = vial_zoi)
#population
cens_stgo <- 
  st_read("data/xn--Censo_2017_Distrito_Censal__Poblacin,_viviendas_por_rea_y_densidad-gmf02j.geojson") %>%
  filter(REGION == 13 & NOM_PROVIN == "SANTIAGO" | NOM_COMUNA %in% c("SAN BERNARDO", "PUENTE ALTO")) %>%
  st_transform(32719)