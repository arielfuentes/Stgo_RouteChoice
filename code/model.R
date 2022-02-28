library(ggplot2)
library(tidyr)
library(dplyr)
# library(GGally)
zoi_tripsDay <- st_read("data/zoi_tripsDay.gpkg") 
zoi_tripsDayCG <- zoi_tripsDay %>%
  select(Demanda, DIA, paraderosubida_SIMT, paraderobajada_SIMT, netapa, rts, tviaje, tesp, tb2, tcam) %>%
  st_drop_geometry() %>%
  left_join(st_drop_geometry(select(time_acc, time_acc, paraderosubida_SIMT))) %>%
  left_join(st_drop_geometry(select(time_egg, time_egg, paraderobajada_SIMT))) %>%
  group_split(netapa)

zoi_tripsDayCG <- bind_rows(
  zoi_tripsDayCG[[1]] %>%
    mutate(e1 = rts, e2 = NA_character_, e3 = NA_character_, e4 = NA_character_),
  zoi_tripsDayCG[[2]] %>%
    separate(rts, into = c("e1", "e2"), remove = F) %>%
    mutate(e3 = NA_character_, e4 = NA_character_),
  zoi_tripsDayCG[[3]] %>%
    separate(rts, into = c("e1", "e2", "e3"), remove = F) %>%
    mutate(e4 = NA_character_),
  zoi_tripsDayCG[[4]] %>%
    separate(rts, into = c("e1", "e2", "e3", "e4"), remove = F)
) %>%
  mutate(e1 = as.numeric(e1), 
         e2 = as.numeric(e2), 
         e3 = as.numeric(e3), 
         e4 = as.numeric(e4),
         Tarifa = case_when(e1 >= 756 | e2 >= 756 | e3 >= 756 | e4 >= 756 ~ 800,
                            T ~ 700)) %>%
  select(-c("e1", "e2", "e3", "e4")) %>%
  select(-c("paraderosubida_SIMT", "paraderobajada_SIMT")) 

set.seed(123)
smp_idx <- sample(1:nrow(zoi_tripsDayCG), 300000)
zoi_tripsDayCG2 <- zoi_tripsDayCG[smp_idx,]
# zoi_tripsDayCG2 <- bind_rows(zoi_tripsCG3, 
#                           zoi_tripsCG3, 
#                           zoi_tripsCG3, 
#                           zoi_tripsCG3, 
#                           zoi_tripsCG3)
# ggpairs(select(zoi_tripsCG2, -rts))

ggplot(zoi_tripsDayCG2, aes(log1p(tviaje), Demanda, color = factor(Tarifa))) +
  geom_point() +
  facet_wrap(~netapa) +
  scale_color_brewer(palette="Dark2", name = "Tarifa") +
  labs(title="Demanda por Tiempo de viaje") +
  theme(plot.title = element_text(hjust = 0.5))
  geom_smooth(method = "lm", formula = y~x) #+
  # facet_wrap(~Tarifa)

ggplot(zoi_tripsDayCG2, aes(tesp, Demanda, color = factor(Tarifa))) +
  geom_point() +
  facet_wrap(~netapa) +
  scale_color_brewer(palette="Dark2", name = "Tarifa") +
  labs(title="Demanda por Tiempo de Espera") +
  theme(plot.title = element_text(hjust = 0.5))
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

ggplot(zoi_tripsDayCG2, aes(tcam, Demanda, color = factor(Tarifa))) +
  geom_point() +
  facet_wrap(~netapa) +
  scale_color_brewer(palette="Dark2", name = "Tarifa") +
  labs(title="Demanda por Tiempo de Caminata") +
  theme(plot.title = element_text(hjust = 0.5))
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

ggplot(zoi_tripsDayCG2, aes(tb2, Demanda, color = factor(Tarifa))) +
  geom_point() +
  facet_wrap(~netapa) +
  scale_color_brewer(palette="Dark2", name = "Tarifa") +
  labs(title="Demanda por Tiempo de Transbordo", x = "Tiempo de Transbordo") +
  theme(plot.title = element_text(hjust = 0.5))
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

ggplot(zoi_tripsDayCG2, aes(log1p(time_acc), Demanda, color = factor(Tarifa))) +
  geom_point() +
  facet_wrap(~netapa) +
  scale_color_brewer(palette="Dark2", name = "Tarifa") +
  labs(title="Demanda por Tiempo de Acceso", x = "Tiempo de Acceso") +
  theme(plot.title = element_text(hjust = 0.5))
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

ggplot(zoi_tripsCG3, aes(log1p(time_egg), Demanda)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

ggplot(zoi_tripsDayCG2, aes(Tarifa, Demanda)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

#### model
library(tidymodels)
set.seed(23)
trips_split <- initial_split(zoi_tripsDayCG2, prop = .75, strata = netapa)
trips_train <- training(trips_split)
trips_test <- testing(trips_split)

rf_mod <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

set.seed(234)
rf_fit <- rf_mod %>%
  fit(Demanda ~ log1p(tviaje) + log1p(time_acc) + log1p(time_egg) + Tarifa + netapa, 
      data = trips_train)

rf_fit

rf_testing_pred <- predict(rf_fit, trips_test) %>%
  bind_cols(trips_test %>% select(Demanda, netapa))

ggplot(rf_testing_pred, aes(y = Demanda, x = .pred)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x)

ggplot(rf_testing_pred, aes(Demanda - .pred)) +
  geom_density()

rf_all_pred <- predict(rf_fit, zoi_tripsDayCG2) %>%
  bind_cols(zoi_tripsDayCG2 %>% select(Demanda, netapa, Tarifa))

ggplot(rf_all_pred, aes(y = Demanda, x = .pred)) +
  geom_point() +
  facet_wrap(~Tarifa) +
  geom_smooth(method = "lm", formula = y~x)

ggplot(rf_all_pred, aes(Demanda - .pred)) +
  geom_density(fill = "lightblue") +
  facet_wrap(~Tarifa)

############################################################################################
library(tidymodels)
set.seed(1234)
trips_split <- initial_split(zoi_tripsCG3, prop = .75, strata = Tarifa)
trips_train <- training(trips_split)
trips_test <- testing(trips_split)

trips_recipe <- 
  recipe(Demanda ~ tviaje + time_acc + time_egg + Tarifa + netapa, 
         data = trips_train) %>%
  step_log(tviaje, offset = 1) %>%
  step_log(time_acc, offset = 1) %>%
  step_log(time_egg, offset = 1) 

trips_prep <- prep(trips_recipe)
juiced <- juice(trips_prep)


rf_model <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_mode("regression") %>%
  set_engine("ranger")

rf_wf <- workflow() %>%
  # add the recipe
  add_recipe(trips_recipe) %>%
  # add the model
  add_model(rf_model)

set.seed(234)
trips_cv <- vfold_cv(trips_train)

doParallel::registerDoParallel()

set.seed(345)
tune_res <- tune_grid(
  rf_wf,
  resamples = trips_cv,
  grid = 20
)

tune_res

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rsq")

rf_grid <- grid_regular(
  mtry(range = c(10, 25)),
  min_n(range = c(15, 30)),
  levels = 5
)

rf_grid

set.seed(456)
regular_res <- tune_grid(
  rf_wf,
  resamples = mvl_cv,
  grid = rf_grid
)

regular_res

regular_res %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  mutate(min_n = factor(min_n)) %>%
  ggplot(aes(mtry, mean, color = min_n)) +
  geom_line(alpha = 0.5, size = 1.5) +
  geom_point() +
  labs(y = "rsq")

best_rsq <- select_best(regular_res, "rsq")

final_rf <- finalize_model(
  rf_model,
  best_rsq
)

final_rf

library(vip)

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(Demanda ~ tviaje + time_acc + time_egg + Tarifa + netapa, 
      data = trips_train,
      data = juiced) %>%
  vip(geom = "point")

final_wf <- workflow() %>%
  add_recipe(trips_recipe) %>%
  add_model(final_rf)

final_res <- final_wf %>%
  last_fit(trips_split)

final_res %>%
  collect_metrics()

final_res %>%
  collect_predictions() %>%
  ggplot(aes(Pax, .pred)) +
  geom_point() +
  geom_smooth(method='lm', formula= y~x)

final_model <- 
  fit(final_wf, zoi_tripsCG3)

predict(final_model, new_data = zoi_tripsCG3) %>%
  bind_cols(zoi_tripsCG3) %>%
  write_delim("output/resul1.csv", ",")

ranger_obj <- pull_workflow_fit(final_model)$fit

#############################################################################################
zoi_tripsDayCG_newzone <- zoi_tripsDay %>%
  select(Demanda, DIA, paraderosubida_SIMT, paraderobajada_SIMT, netapa, rts, tviaje, tesp, tb2, tcam) %>%
  st_drop_geometry() %>%
  left_join(select(time_acc2, time_acc, paraderosubida_SIMT)) %>%
  left_join(select(time_egg_newzone, time_egg, paraderobajada_SIMT)) %>%
  group_split(netapa)

zoi_tripsDayCG_newzone <- bind_rows(
  zoi_tripsDayCG_newzone[[1]] %>%
    mutate(e1 = rts, e2 = NA_character_, e3 = NA_character_, e4 = NA_character_),
  zoi_tripsDayCG_newzone[[2]] %>%
    separate(rts, into = c("e1", "e2"), remove = F) %>%
    mutate(e3 = NA_character_, e4 = NA_character_),
  zoi_tripsDayCG_newzone[[3]] %>%
    separate(rts, into = c("e1", "e2", "e3"), remove = F) %>%
    mutate(e4 = NA_character_),
  zoi_tripsDayCG_newzone[[4]] %>%
    separate(rts, into = c("e1", "e2", "e3", "e4"), remove = F)
) %>%
  mutate(e1 = as.numeric(e1), 
         e2 = as.numeric(e2), 
         e3 = as.numeric(e3), 
         e4 = as.numeric(e4),
         Tarifa = case_when(e1 >= 756 | e2 >= 756 | e3 >= 756 | e4 >= 756 ~ 800,
                            T ~ 700)) %>%
  select(-c("e1", "e2", "e3", "e4")) %>%
  select(-c("paraderosubida_SIMT", "paraderobajada_SIMT")) 

set.seed(123)
smp_idx <- sample(1:nrow(zoi_tripsDayCG_newzone), 300000)
zoi_tripsDayCG2_newzone <- zoi_tripsDayCG_newzone[smp_idx,]

set.seed(23)
trips_split <- initial_split(zoi_tripsDayCG2_newzone, prop = .75, strata = Tarifa)
trips_train <- training(trips_split)
trips_test <- testing(trips_split)

rf_mod <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

set.seed(234)
rf_fit <- rf_mod %>%
  fit(Demanda ~ log1p(tviaje) + log1p(time_acc) + log1p(time_egg) + Tarifa + netapa, 
      data = trips_train)

rf_fit

rf_testing_pred_newzone <- predict(rf_fit, trips_test) %>%
  bind_cols(trips_test %>% select(Demanda, netapa, Tarifa))

ggplot(rf_testing_pred_newzone, aes(y = Demanda, x = .pred)) +
  geom_point() +
  facet_wrap(~Tarifa) +
  geom_smooth(method = "lm", formula = y~x)

ggplot(rf_testing_pred_newzone, aes(Demanda - .pred), fill = Demanda - .pred, colour = "lightblue") +
  geom_density() +
  facet_wrap(~Tarifa)

rf_all_pred_newzone <- predict(rf_fit, zoi_tripsDayCG2_newzone) %>%
  bind_cols(zoi_tripsDayCG2_newzone %>% select(Demanda, netapa, Tarifa))

ggplot(rf_all_pred_newzone, aes(y = Demanda, x = .pred)) +
  geom_point() +
  facet_wrap(~Tarifa) +
  geom_smooth(method = "lm", formula = y~x)

ggplot(rf_all_pred_newzone, aes(Demanda - .pred)) +
  geom_density(fill = "lightblue") +
  facet_wrap(~Tarifa)

###########################################################################################
set.seed(23)
trips_split <- initial_split(zoi_tripsCG, prop = .75, strata = Tarifa)
trips_train <- training(trips_split)
trips_test <- testing(trips_split)

rf_mod <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

set.seed(234)
rf_fit <- rf_mod %>%
  fit(Demanda ~ log1p(tviaje) + log1p(time_acc) + log1p(time_egg) + Tarifa + netapa, 
      data = trips_train)

rf_fit

rf_testing_pred_mean <- predict(rf_fit, trips_test) %>%
  bind_cols(trips_test %>% select(Demanda, netapa, Tarifa))

ggplot(rf_testing_pred_mean, aes(y = Demanda, x = .pred)) +
  geom_point() +
  facet_wrap(~Tarifa) +
  geom_smooth(method = "lm", formula = y~x)

ggplot(rf_testing_pred_mean, aes(Demanda - .pred), fill = Demanda - .pred, colour = "lightblue") +
  geom_density() +
  facet_wrap(~Tarifa)
