library(tmap)
library(factoextra)
library(ClustGeo)
library(dplyr)
library(cluster)
tm_shape(zones) +
  tm_grid(col = "white", lwd = 2, labels.size = .8) +
  tm_polygons(col = "lightblue", border.col = "black", border.alpha = 1) +
  tm_shape(stops_trips) +
  tm_dots(col = "tcam", style = "quantile") +
  tm_layout(bg.color = "#DAF7A6", 
            main.title = "Tiempo de Caminata", 
            main.title.position = "center", 
            title.size = 1, 
            legend.show = F) +
  tm_compass(position = c("left", "top"), type = "4star", size = 2) +
  tm_scale_bar(position = c("right", "bottom"))

stops_trips <- st_read("output/stops_trips.gpkg")
stops_trips <- stops_trips[1:1000,]
D0 <- stops_trips %>%
  st_drop_geometry() %>%
  select(-paraderosubida) %>% 
  dist()

D1 <- as.dist(st_distance(stops_trips, stops_trips))

tree <- hclustgeo(D0)
plot(tree, hang = -1, label = F, xlab = "", ylab = "", main = "")

# fviz_nbclust(stops_trips %>%
#                st_drop_geometry() %>%
#                select(-paraderosubida), FUN = hcut, method = "silhouette")
# https://uc-r.github.io/hc_clustering
gap_stat <- clusGap(stops_trips %>%
                      st_drop_geometry() %>%
                      select(-paraderosubida), 
                    FUN = hcut, nstart = 25, K.max = 50, B = 50)

gap_stat$Tab %>% 
  as_tibble(rownames = "clus") %>% 
  arrange(desc(gap))

fviz_gap_stat(gap_stat)

P1.5 <- cutree(tree, 800)

hclus_stops <- mutate(stops_trips, P1.5 = P1.5)

tm_shape(hclus_stops) +
  tm_dots(col = "P1.5", breaks = seq(0, 1500, 300)) +
  tm_layout(legend.outside = T)

cr <- choicealpha(D0, D1, range.alpha = seq(0, 1, .2), K = 800, graph = T)

hclustgeo(D0, D1, alpha =.2)


# w_tree <- lapply(seq(0, 1, .1), function(x) hclustgeo(D0, D1, alpha = x))

set.seed(23)
library(furrr)
# seed_lst <- sample(1:10000, 50, replace=FALSE)
future::plan(strategy = multisession, workers = 4)
options <- furrr_options(seed = 123)

range.alpha <- seq(0, 1, .1)
w_tree <- range.alpha %>%
  future_map(~hclustgeo(D0, D1, alpha = .x))
part <- stats::cutree(tree,k=800)

n <- as.integer(attr(D1, "Size"))
wt <- rep(1/n, n)
W <- matrix(0,length(range.alpha),2)
rownames(W)  <- paste("alpha=", range.alpha, sep="")
colnames(W) <- c("W0","W1")
W[i,1] <- withindiss(D0,part,wt)
W[i,2] <- withindiss(D1,part,wt)
w_P1.5 <- lapply(w_tree, function(x) cutree(x, k = 1500))
hclust_lst <- lapply(w_P1.5, function(x) mutate(stops_trips, P1.5 = x))
lapply(1:length(hclust_lst), function(x) tm_shape(hclust_lst[[x]]) +
         tm_dots("P1.5", breaks = seq(0, 1500, 300)) +
         tm_layout(title = x))

hcent <- hclust_lst[[2]] %>%
  mutate(x = st_coordinates(.)[, 1],
         y = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  group_by(P1.5) %>% 
  summarise(x = mean(x), 
            y = mean(y)) %>% 
  st_as_sf(coords = c("x", "y"), crs = 32719)

tm_shape(hcent) +
  tm_dots(col = "black", size = .2, shape = 10)

# st_voronoi(st_union(hcent), st_bbox(hcent)) %>%
#   st_write("output/voronoi.gpkg")
#   plot()

  st_vor <- st_collection_extract(st_voronoi(do.call(c, st_geometry(hcent)))) %>%
    `st_crs<-`(32719) %>%
    st_intersection(nngeo::st_remove_holes(st_union(zoi)))
  
  
    
  
    tm_shape(zoi) +
      tm_polygons(col = "red") +
    tm_shape(st_vor) +
    tm_polygons(alpha = .5) +
      tm_shape(hcent) +
      tm_dots(alpha = .1)
##########################################################
############################################################
library(rmarkdown)
rmarkdown::render(input = "code/Stops_Clus_Gnrl.Rmd", output_dir = "output/", output_format = "html_document")
#################################################################
stops_trips669 <- st_filter(stops_trips, 
                            filter(zoi, Zona == 669), 
                            .predicate = st_within)

tm_shape(filter(zoi, Zona == 669)) +
  tm_polygons(col = "gray") +
  tm_shape(stops_trips669) +
  tm_dots(col = "red")

D0 <- stops_trips669 %>%
  st_drop_geometry() %>%
  select(-paraderosubida) %>% 
  dist()

D1 <- as.dist(st_distance(stops_trips669, stops_trips669))

tree <- hclustgeo(D0)
plot(tree, hang = -1, label = F, xlab = "", ylab = "", main = "")

gap_stat <- clusGap(stops_trips669 %>%
                      st_drop_geometry() %>%
                      select(-paraderosubida), method = "Tibs2001SEmax",
                    FUN = hcut, nstart = 25, K.max =  nrow(stops_trips669) - 1, B = 50)

nclus <- gap_stat$Tab %>% 
  as_tibble(rownames = "clus") %>%
  mutate(GlobalMax = max(gap),
         GapSE = gap - SE.sim, 
         Clus_ch = if_else(GapSE > GlobalMax, 1, if_else(gap == GlobalMax, 1, 0))) %>%
  filter(Clus_ch == 1) %>%
  pull(clus) %>%
  as.numeric()

fviz_gap_stat(gap_stat)

P <- cutree(tree, nclus)
hclus_stops <- mutate(stops_trips669, P = P)

tm_shape(hclus_stops) +
  tm_dots(col = "P", palette = c("blue", "red")) +
  tm_layout(legend.outside = T)

cr <- choicealpha(D0, D1, range.alpha = seq(0, 1, .1), K = nclus, graph = T)

alpha_ch <- cr$Qnorm %>% 
  as_tibble(rownames = "alpha") %>% 
  mutate(Promedio = (Q0norm+Q1norm)/2) %>% 
  filter(Promedio == max(Promedio)) %>% 
  tidyr::separate(alpha, "=", into = c("alpha_nm", "alpha_num")) %>% 
  pull(alpha_num)

tree <- hclustgeo(D0, D1, alpha = as.numeric(alpha_ch)[1])
P <- cutree(tree, nclus)

hclus_stops <- mutate(stops_trips669, P = P)

tm_shape(hclus_stops) +
  tm_dots(col = "P", palette = c("blue", "red")) +
  tm_layout(legend.outside = T)

hull <- st_convex_hull(st_union(group_by(hclus_stops, P), by_feature = T))
lapply(split(hclus_stops, P), function(x) plot(st_convex_hull(st_union(x))))
hcent <- hclus_stops %>%
  group_by(P) %>%
  summarise(.groups = "keep") %>%
  st_convex_hull() %>%
  st_centroid()
# hcent <- hclus_stops %>%
#   mutate(x = st_coordinates(.)[, 1],
#          y = st_coordinates(.)[, 2]) %>%
#   st_drop_geometry() %>%
#   group_by(P) %>% 
#   summarise(x = mean(x), 
#             y = mean(y)) %>% 
#   st_as_sf(coords = c("x", "y"), crs = 32719)

tm_shape(hcent) +
  tm_dots(col = "black", size = .2, shape = 10)

st_vor <- st_intersection(st_cast(st_voronoi(st_union(st_geometry(hcent)), 
           envelope = st_geometry(filter(zoi, Zona == 669)))), 
           st_geometry(filter(zoi, Zona == 669)))

tm_shape(filter(zoi, Zona == 669)) +
  tm_polygons(col = "red") +
  tm_shape(st_vor) +
  tm_polygons(alpha = .8, col = "lightblue") +
  tm_shape(hcent) +
  tm_dots() +
  tm_shape(stops_trips669) +
  tm_dots(col = "white")
#######################################################
anti_zoi <- zones[!lengths(st_within(zones, zoi)), ]

tm_shape(anti_zoi) +
  tm_polygons(col = "lightblue") +
  tm_shape(zoi) +
  tm_polygons(col = "orange")
