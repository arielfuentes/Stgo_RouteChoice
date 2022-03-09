library(tidymodels)
zoi_tripsDayCG <-
  # filter(zoi_tripsDayCG, Zona_sb %in% c("287|494", "287|21",  "287|225", "287|619", "287|361", "287|595", 
  #                       "287|437", "287|90",  "287|290", "287|10",  "287|646",
  #                       "287|123", "287|589", "287|287", "287|19",  "287|627", "287|596", 
  #                       "287|597", "287|640", "287|307", "287|424", "287|284",
  #                       "287|296", "287|514", "287|447", "287|182", "287|18",  "287|187", 
  #                       "287|129", "287|29",  "287|471", "287|166", "287|242",
  #                       "287|172", "287|295", "287|13",  "287|497", "287|549", "287|206", 
  #                       "287|292", "287|499", "287|35", "287|226", "287|427",
  #                       "287|429", "287|371", "287|47",  "287|495", "287|43",  "287|594", 
  #                       "287|376", "287|500", "287|281", "287|476",
  #                       "287|15",  "287|455", "287|162", "287|283", "287|45",  "287|122", 
  #                       "287|642", "287|231", "287|22", "287|240", "287|435",
  #                       "287|498", "287|366", "287|511", "287|374", "287|49",  "287|14",  
  #                       "287|41", "287|20", "287|126", "287|578", "287|228",
  #                       "287|96")) %>%
  mutate(zoi_tripsDayCG, netapa = factor(netapa),
         Zona_sb = factor(Zona_sb),
         Tarifa = factor(Tarifa)) %>%
  na.omit()

set.seed(1234)
trips_split <- initial_split(zoi_tripsDayCG, prop = .75, strata = Tarifa)
trips_train <- training(trips_split)
trips_test <- testing(trips_split)

rf_mod <- rand_forest(trees = 1000) %>%
  set_engine("ranger") %>%
  set_mode("regression")

trips_recipe <- 
  recipe(Demanda ~ tviaje + time_acc + time_egg + Tarifa + netapa + Zona_sb, 
         data = zoi_tripsDayCG) %>%
  step_log(tviaje, offset = 1) %>%
  step_log(time_acc, offset = 1) %>%
  step_log(time_egg, offset = 1) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>%
  # step_zv(all_predictors()) %>%
  step_normalize(all_predictors())

rf_wkf <- workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(trips_recipe)

set.seed(234)
rf_fit <- rf_wkf %>%
  fit(zoi_tripsDayCG)

rf_train_pred <- predict(rf_fit, trips_train) %>%
  bind_cols(trips_train)
rf_test_pred <- predict(rf_fit, trips_test) %>%
  bind_cols(trips_test)
rf_all_pred <- predict(rf_fit, zoi_tripsDayCG) %>%
  bind_cols(zoi_tripsDayCG)

ggplot(rf_all_pred, aes(y = Demanda, x = .pred)) +
  geom_point() +
  # facet_wrap(~Tarifa) +
  geom_smooth(method = "lm", formula = y~x) +
  labs(title="Predicción de Demanda", x = "Predicción") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(rf_all_pred, aes(Demanda - .pred), fill = Demanda - .pred, colour = "lightblue") +
  geom_density(alpha = .5, fill = "lightblue") +
  # facet_wrap(~Tarifa)
  xlab("Demanda - Predicción") +
  ylab("Densidad") +
  labs(title="Distribución del Error") +
  theme(plot.title = element_text(hjust = 0.5))
