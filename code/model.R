library(ggplot2)
library(dplyr)
# library(GGally)
zoi_tripsCG2 <- zoi_tripsCG %>%
  select(-c("paraderosubida_SIMT", "paraderobajada_SIMT")) 

set.seed(123)
smp_idx <- sample(1:nrow(zoi_tripsCG2), 30000)
zoi_tripsCG3 <- zoi_tripsCG2[smp_idx,]
zoi_tripsCG3 <- bind_rows(zoi_tripsCG3, 
                          zoi_tripsCG3, 
                          zoi_tripsCG3, 
                          zoi_tripsCG3, 
                          zoi_tripsCG3)
# ggpairs(select(zoi_tripsCG2, -rts))

ggplot(zoi_tripsCG3, aes(log1p(tviaje), Demanda)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~Tarifa)

ggplot(zoi_tripsCG2, aes(tesp, Demanda)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

ggplot(zoi_tripsCG2, aes(tcam, Demanda)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

ggplot(zoi_tripsCG2, aes(tb2, Demanda)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

ggplot(zoi_tripsCG3, aes(log1p(time_acc), Demanda)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

ggplot(zoi_tripsCG3, aes(log1p(time_egg), Demanda)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

ggplot(zoi_tripsCG2, aes(Tarifa, Demanda)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x) +
  facet_wrap(~netapa)

#### model
library(tidymodels)
set.seed(23)
trips_split <- initial_split(zoi_tripsCG3, prop = .75, strata = Tarifa)
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
  bind_cols(trips_test %>% select(Demanda))

ggplot(rf_testing_pred, aes(Demanda, .pred)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~x)
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