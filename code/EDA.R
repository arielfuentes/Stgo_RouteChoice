library(ggplot2)
library(stplanr)
library(tmap)
ggplot(vjs_pma, aes(as.factor(netapa), tviaje)) + geom_boxplot()
ggplot(vjs_pma, aes(as.factor(netapa), tesp)) + geom_boxplot()
ggplot(vjs_pma, aes(as.factor(netapa), tb2)) + geom_boxplot()
ggplot(vjs_pma, aes(as.factor(netapa), tcam)) + geom_boxplot()

viajesOD <- vjs_pma %>%
  group_by(x_sub,
           y_sub,
           x_baj,
           y_baj,
           paraderosubida_SIMT,
           paraderobajada_SIMT) %>%
  summarise(Demanda = sum(Demanda)) %>%
  ungroup()
  
od_stops <- od_coords2line(viajesOD[,1:4], remove_duplicates = F, crs = 32719) %>%
  left_join(viajesOD)
#overlap with zones
#acumulate by zone
#plot
tm_shape(od_stops) +
  tm_lines(lwd = "Demanda")
# https://stackoverflow.com/questions/51918536/r-create-linestring-from-two-points-in-same-row-in-dataframe