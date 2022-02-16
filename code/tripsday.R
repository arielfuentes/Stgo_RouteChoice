BBDD_vjs_model <- function(DDBB_v) {
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
  sql.trips <- paste0("SELECT SUM(CAST(factorexpansion AS FLOAT)) AS Demanda, 
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
  serv_4ta_etapa, AVG(CAST(REPLACE(t_4ta_etapa, '-', '') AS FLOAT)) AS t_4ta_etapa,
  DAY(CONVERT(DATETIME, tiemposubida, 120)) AS DIA
  FROM [uchile].[dbo].", DDBB_v,
  "WHERE periodomediodeviaje = '04 - PUNTA MANANA' AND paraderosubida <> '-' AND paraderobajada <> '-'
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
  serv_4ta_etapa,
  DAY(CONVERT(DATETIME, tiemposubida, 120));")
  trips <- dbGetQuery(conn = con,
                      statement = sql.trips) %>%
    as_tibble()
  dbDisconnect(con)
  return(trips)
}

vjs_day <- BBDD_vjs_model(DDBB_v = "[viajes201904]")
