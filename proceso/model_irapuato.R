###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################



## libraries
library(tidyverse)
library(tidymodels)
library(lubridate)
library(vtreat)
library(ggtext)
library(spatialsample)
library(baguette)
library(themis)
library(SHAPforxgboost)



## data acquisition
dt00 <- 
  read_csv("raw/BASE MUNICIPAL_ACCIDENTES DE TRANSITO GEORREFERENCIADOS_2021.csv",
           locale = locale(encoding = "latin1"))


## data cleaning
# filter data for state 11 and municipality 17
dt01 <- dt00 %>% 
  janitor::clean_names() %>% 
  filter(edo == 11, mpio == 17) %>% 
  mutate(fecha = ymd(paste(anio, mes, dia, sep = "-"))) %>%
  mutate(decesos = as.numeric(if_else(totmuertos >= 1, 1, 0)),
         heridos = as.numeric(if_else(totheridos >= 1, 1, 0)))


## modeling
# variables para convertir a factor
conv_a_fct <- c("diasemana", "urbana", "suburbana", "tipaccid",
                "causaacci", "caparod", "sexo", "aliento", "cinturon",
                "clase", "heridos", "decesos")

# preparar variables
dt02 <- dt01 %>% 
  mutate(diasemana = case_when(diasemana == 1 ~ "Lunes",
                               diasemana == 2 ~ "Martes",
                               diasemana == 3 ~ "Miércoles",
                               diasemana == 4 ~ "Jueves",
                               diasemana == 5 ~ "Viernes",
                               diasemana == 6 ~ "Sábado",
                               diasemana == 7 ~ "Domingo",
                               diasemana == 8 ~ "No especificado",
                               causaacci == 0 ~ "Certificado cero"),
         urbana = case_when(urbana == 0 ~ "Área suburbana",
                            urbana == 1 ~ "En intersección",
                            urbana == 2 ~ "No en intersección"),
         suburbana= case_when(suburbana == 0 ~ "Área urbana",
                              suburbana == 1 ~ "En camino rural",
                              suburbana == 2 ~ "En carretera estatal",
                              suburbana == 3 ~ "En otro camino"),
         tipaccid = case_when(tipaccid == 0 ~ "Certificado cero",
                              tipaccid == 1 ~ "Colisión con vehículo motor",
                              tipaccid == 2 ~ "Atropellamiento",
                              tipaccid == 3 ~ "Colisión con animal",
                              tipaccid == 4 ~ "Colisión con objeto fijo",
                              tipaccid == 5 ~ "Volcadura",
                              tipaccid == 6 ~ "Caída de pasajero",
                              tipaccid == 7 ~ "Salida del camnio",
                              tipaccid == 8 ~ "Incendio",
                              tipaccid == 9 ~ "Colisión con ferrocarril",
                              tipaccid == 10 ~ "Colisión con motocicleta",
                              tipaccid == 11 ~ "Colisión con ciclista",
                              tipaccid == 12 ~ "Otro",),
         causaacci = case_when(causaacci == 1 ~ "Conductor",
                               causaacci == 2 ~ "Peatón o pasajero",
                               causaacci == 3 ~ "Falla del vehículo",
                               causaacci == 4 ~ "Mala condición del camino",
                               causaacci == 5 ~ "Otra"),
         caparod = case_when(caparod == 1 ~ "Pavimentada",
                             caparod == 1 ~ "No pavimentada"),
         sexo = case_when(sexo == 1 ~ "Se fugó",
                          sexo == 2 ~ "Hombre",
                          sexo == 3 ~ "Mujer"),
         aliento = case_when(aliento == 4 ~ "Si",
                             aliento == 5 ~ "No",
                             aliento == 6 ~ "Se ignora"),
         cinturon = case_when(cinturon == 7 ~ "Si",
                              cinturon == 8 ~ "No",
                              cinturon == 9 ~ "Se ignora"),
         clase = case_when(clase == 1 ~ "Fatal",
                           clase == 2 ~ "No fatal",
                           clase == 3 ~ "Solo daños")) %>% 
  mutate(across(.cols = all_of(conv_a_fct), 
                .fns = as_factor))



dt02 %>% 
  map(~ sum(is.na(.)))

# partition
set.seed(223)
acci_split <- initial_split(dt02, 
                            strata = heridos)

acci_train <- acci_split %>% training()

acci_test <- acci_split %>% testing()


# recipe
acci_recipe <- recipe(heridos ~ diasemana + urbana + 
                        tipaccid + causaacci +
                        caparod + sexo + aliento + 
                        cinturon + edad,
                      data = acci_train) %>% 
  step_impute_knn(all_nominal_predictors()) %>% 
  step_discretize(edad, options = list(prefix = "edad")) %>% 
  step_rm(urbana, caparod) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_zv(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_corr(all_predictors(), threshold = 0.95)

prep(acci_recipe) %>% bake(new_data = NULL) %>%
  map(~ sum(is.na(.)))

# cross-validation folds
set.seed(332)
acci_folds <- vfold_cv(acci_train, 
                       strata = heridos)


# workflow
acci_wf <- workflow() %>% 
  add_recipe(acci_recipe)


# random forest spec
randforest_spec <- rand_forest() %>% 
  set_engine("ranger") %>% 
  set_mode("classification")


# fit resamples
randforest_res <- fit_resamples(object = acci_wf %>% add_model(randforest_spec), 
                                resamples = acci_folds,
                                metrics = metric_set(roc_auc, accuracy, sensitivity, specificity),
                                control = control_resamples(save_pred = TRUE))

# metrics
randforest_res %>% 
  collect_metrics()



# logistic regression model
logisregress_spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")


# fit resamples
logisregress_res <- fit_resamples(object = acci_wf %>% add_model(logisregress_spec), 
                                resamples = acci_folds,
                                metrics = metric_set(roc_auc, accuracy, sensitivity, specificity),
                                control = control_resamples(save_pred = TRUE))

# metrics
logisregress_res %>% 
  collect_metrics()


# Final model
final_spec_1 <- rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  set_mode("classification")

final_spec_2 <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")



# final workflow
final_wf <- acci_wf %>% 
  add_model(final_spec_1)

# final fit
final_fit <- final_wf %>% 
  last_fit(acci_split)

# final metrics
collect_metrics(final_fit, 
                metric_set(accuracy, roc_auc, sensitivity, specificity))


# final auc
collect_predictions(final_fit) %>% 
  roc_curve(heridos, .pred_0) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) + 
  geom_line(color = "#970007") +
  geom_abline(linetype = "dashed", color = "darkgrey") +
  coord_equal()


# # coefficients
# final_fit %>% 
#   extract_fit_engine() %>% 
#   tidy() %>%
#   filter(term != "(Intercept)") %>% 
#   filter(p.value < 0.05) %>%
#   group_by(estimate > 0) %>% 
#   slice_max(order_by = estimate, n = 10) %>% 
#   ungroup() %>% 
#   ggplot(aes(x = estimate, 
#              y = fct_reorder(term, estimate),
#              fill = estimate > 0)) +
#   geom_col(alpha = 0.6)


# factor importance
final_fit %>% 
  extract_fit_parsnip() %>% vip::vi_model() %>% 
  arrange(-Importance) %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(x = Importance, y = fct_reorder(Variable, Importance))) +
  geom_col(alpha = 0.85, fill = "#970007") +
  labs(title = "¿Que factores están relacionados con el registro de heridos?",
       subtitle = "Factores reportados en los accidentes de tránsito ocurridos en Irapuato",
       x = "Importancia de factores",
       y = "Factores reportados",
       caption = "Fuente: INEGI: Accidentes de Tránsito Terrestre 
           en Zonas Urbanas y Suburbanas 2021<br>
           Visualización: Juan L. Bretón, PMP") +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(face = "bold", size = 16),
        plot.caption = element_markdown(color = "darkgrey", hjust = 0),
        legend.position = "top",
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.line = element_line(color = "darkgrey"),
        panel.grid = element_line(color = "grey95")) 

ggsave("image/ira_factores_2.jpg", device = "jpeg", dpi = "retina")





# xgboost spec
xgb_spec <- boost_tree(trees = tune(),
                       min_n = tune(),
                       mtry = tune(),
                       learn_rate = 0.01) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")


# hyperparameter tunning
xgb_tune <- tune_grid(object = acci_wf %>% add_model(xgb_spec), 
                      resamples = acci_folds,
                      grid = 6)

# fit resamples
xgb_res <- acci_wf %>% 
  add_model(xgb_spec) %>%
  finalize_workflow(select_best(xgb_tune,
                                metric = "roc_auc")) %>% 
  fit_resamples(resamples = acci_folds,
                control = control_resamples(save_pred = TRUE),
                metrics = metric_set(accuracy, roc_auc, sensitivity, specificity))

# resampling evaluation
xgb_res %>% collect_metrics()  


# last fit
xgb_lastfit <- acci_wf %>% 
  add_model(xgb_spec) %>%
  finalize_workflow(select_best(xgb_tune,
                                metric = "roc_auc")) %>%
  last_fit(acci_split)

# evaluation
xgb_lastfit %>% 
  collect_predictions() %>% 
  roc_curve(truth = heridos,
            estimate = .pred_0) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "darkred") +
  geom_abline(linetype = "dashed") +
  coord_equal()

# factor importance
xgb_lastfit %>% 
  extract_fit_parsnip() %>% 
  vip::vi_model() %>% 
  slice_max(order_by = Importance, n = 7) %>% 
  ggplot(aes(x = Importance, y = fct_reorder(Variable, Importance))) +
  geom_col(alpha = 0.85, fill = "#970007") +
  labs(title = "¿Que factores están más relacionados con el registro de heridos?",
       subtitle = "Factores reportados en los accidentes de tránsito ocurridos en Irapuato",
       x = "Importancia de factores",
       y = "Factores reportados",
       caption = "Fuente: INEGI: Accidentes de Tránsito Terrestre 
           en Zonas Urbanas y Suburbanas 2021<br>
           Visualización: Juan L. Bretón, PMP") +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(face = "bold", size = 16),
        plot.caption = element_markdown(color = "darkgrey", hjust = 0),
        legend.position = "top",
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.line = element_line(color = "darkgrey"),
        panel.grid = element_line(color = "grey95")) 

ggsave("image/ira_factores_xgb.jpg", device = "jpeg", dpi = "retina")


# SHAP
shape <- shap.prep(xgb_model = xgb_lastfit %>% extract_fit_engine(),
                   X_train = bake(prep(acci_recipe),
                                  new_data = NULL,
                                  has_role("predictor"),
                                  composition = "matrix"))

# plot summary
shap.plot.summary(shape)

# dependence plot
shap.plot.dependence(shape,
                     x = "heridos",
                     color_feature = "tipacci",
                     smooth = FALSE,
                     add_hist = FALSE)
















