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
  sql.trips <- paste0("SELECT TOP 1000 SUM(CAST(factorexpansion AS FLOAT)) AS Demanda, periodomediodeviaje, 
paraderosubida, paraderobajada, netapa,
serv_1era_etapa, t_1era_etapa,	tespera_1era_etapa, ttrasbordo_1era_etapa, tcaminata_1era_etapa,
serv_2da_etapa,	t_2da_etapa, tespera_2da_etapa, ttrasbordo_2da_etapa, tcaminata_2da_etapa, 
serv_3era_etapa, t_3era_etapa, tespera_3era_etapa, ttrasbordo_3era_etapa, tcaminata_3era_etapa, 
serv_4ta_etapa, t_4ta_etapa
FROM [uchile].[dbo].", DDBB_v,
"WHERE periodomediodeviaje = ", per, " AND paraderosubida <> '-' AND paraderobajada <> '-'
GROUP BY periodomediodeviaje, 
paraderosubida, paraderobajada, netapa,
serv_1era_etapa, t_1era_etapa,	tespera_1era_etapa, ttrasbordo_1era_etapa, tcaminata_1era_etapa,
serv_2da_etapa,	t_2da_etapa, tespera_2da_etapa, ttrasbordo_2da_etapa, tcaminata_2da_etapa, 
serv_3era_etapa, t_3era_etapa, tespera_3era_etapa, ttrasbordo_3era_etapa, tcaminata_3era_etapa, 
serv_4ta_etapa, t_4ta_etapa;")
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

x <- stops("2019-07-06_consolidado_anexo4_(Circunvalación)_anual.xlsx", 
           "01Abr2019 al 22Jun2019")

library(dplyr)
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
         y)
library(readr)
stops_mt <- read_delim("data/dicc_mt.csv", delim = ";") %>%
  select(CODINFRA, X, Y) %>%
  mutate(SIMT = CODINFRA) %>%
  select(CODINFRA, SIMT, x = X, y = Y)

stops_df <- bind_rows(stops_bus, stops_mt)
rm(stops_bus, stops_mt)
