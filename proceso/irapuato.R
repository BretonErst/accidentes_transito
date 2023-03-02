###############################################
##                                           ##
##            JUAN L. BRETON, PMP            ##
##                                           ##
###############################################



# librerías
library(tidyverse)
library(tidymodels)
library(lubridate)
library(ggtext)
library(spatialsample)
library(baguette)
library(themis)


# adquisición de accidentes de tránsito
dt00 <- 
  read_csv("raw/BASE MUNICIPAL_ACCIDENTES DE TRANSITO GEORREFERENCIADOS_2021.csv",
           locale = locale(encoding = "latin1"))


# clean names of variables
# filter data for state 11 and municipality 17
dt01 <- dt00 %>% 
  janitor::clean_names() %>% 
  filter(edo == 11, mpio == 17) %>% 
  mutate(fecha = ymd(paste(anio, mes, dia, sep = "-"))) %>%
  mutate(decesos = as.numeric(if_else(totmuertos >= 1, 1, 0)),
         heridos = as.numeric(if_else(totheridos >= 1, 1, 0)))


# tabla de accidentes por registro de heridos
table(dt01$heridos)

# porcentaje de accidentes con heridos
dt01 %>% 
  summarize(propor_heridos = mean(heridos)) %>% 
  knitr::kable(digits = 2,
               col.names = c("Proporción de Heridos"),
               format.args = list(decimal.mark = "."))

# media de accidentes diarios
dt01 %>% 
  count(fecha) %>% 
  summarize(media_accidentes_diarios = sum(n) / 365)


# number of accidents per week
dt01 %>% 
  mutate(fecha = floor_date(fecha, unit = "week")) %>% 
  count(fecha, heridos) %>% 
  filter(fecha != last(fecha),
         fecha != first(fecha)) %>% 
  ggplot(aes(x = fecha, y = n, color = as_factor(heridos))) +
    geom_line(linewidth = 1.2) +
    scale_y_continuous(limits = c(0, NA)) +
    labs(title = "¿Cuántos accidentes de tránsito ocurren en Irapuato?",
         subtitle = "Ocurridos en zonas urbana y semiurbanas del territorio municipal de Irapuato por semana",
         x = NULL,
         y = "Accidentes",
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
          panel.grid = element_line(color = "grey95")) +
    scale_color_manual(name = "Registro de Heridos",
                       values = c("#03468C", "#970007"),
                       labels = c("Sin heridos",
                                  "Se registran heridos"))
  

# percent of accidents involving injuries
dt01 %>% 
  mutate(fecha = floor_date(fecha, unit = "week")) %>% 
  count(fecha, heridos) %>% 
  filter(fecha != last(fecha),
         fecha != first(fecha)) %>% 
  group_by(fecha) %>% 
  mutate(porcen = n / sum(n)) %>% 
  ungroup() %>% 
  filter(heridos == 1) %>% 
  ggplot(aes(x = fecha, y = porcen)) +
    geom_line(linewidth = 1.2,
              color = "#970007") +
    scale_y_continuous(limits = c(0, NA), 
                       labels = scales::percent) +
    labs(title = "¿En qué proporción de los accidentes vehiculares se registran personas heridas?",
         subtitle = "Accidentes de tránsito ocurridos en zonas urbana y semiurbanas de Irapuato por semana",
         x = NULL,
         y = "Porcentaje de accidentes que reportan heridos",
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
          panel.grid = element_line(color = "grey95")) +
  geom_smooth(se = FALSE, method = "lm")

ggsave("image/ira_percent.jpg", device = "jpeg", dpi = "retina")


# injuries per day of the week
dt01 %>% 
  mutate(fecha = wday(fecha, label = TRUE)) %>%
  count(fecha, heridos) %>% 
  group_by(heridos) %>% 
  mutate(percent = n / sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x = percent, 
             y = fct_rev(as_factor(fecha)), 
             fill = as_factor(heridos))) +
    geom_col(position = "dodge", alpha = 0.8) +
    labs(title = "¿Cuándo ocurren los accidentes de tránsito en Irapuato?",
         subtitle = "Por día de la semana",
         x = "Porcentaje de acidentes ocurridos",
         y = "Día de la semana",
         caption = "Fuente: INEGI: Accidentes de Tránsito Terrestre 
         en Zonas Urbanas y Suburbanas 2021<br>
         Visualización: Juan L. Bretón, PMP") +
    scale_x_continuous(labels = scales::percent) +
    theme(text = element_text(family = "Encode Sans Condensed"),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.title = element_text(face = "bold", size = 16),
          plot.caption = element_markdown(color = "darkgrey", hjust = 0),
          legend.position = "top",
          panel.background = element_rect(fill = "#FFFFFF"),
          axis.line = element_line(color = "darkgrey"),
          panel.grid = element_line(color = "grey95")) +
    scale_fill_manual(name = "Registro de Heridos",
                       values = c("#03468C", "#970007"),
                       labels = c("Sin heridos",
                                  "Se registran heridos"))


# injuries per cause
dt01 %>% 
  mutate(causaacci = case_when(causaacci == 1 ~ "Conductor",
                               causaacci == 2 ~ "Peatón o pasajero",
                               causaacci == 3 ~ "Falla del vehículo",
                               causaacci == 4 ~ "Mala condición del camino",
                               causaacci == 5 ~ "Otra")) %>% 
  count(causaacci, heridos) %>% 
  group_by(heridos) %>% 
  mutate(percent = n / sum(n)) %>% 
  ungroup() %>% 
  ggplot(aes(x = percent, y = causaacci, fill = as_factor(heridos))) +
  geom_col(position = "dodge", alpha = 0.8) +
  labs(title = "¿Qué causa los accidentes vehiculares en Irapuato?",
       subtitle = "Causas reportadas",
       x = "Porcentaje de acidentes",
       y = NULL,
       caption = "Fuente: INEGI: Accidentes de Tránsito Terrestre 
         en Zonas Urbanas y Suburbanas 2021<br>
         Visualización: Juan L. Bretón, PMP") +
  scale_x_continuous(labels = scales::percent) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(face = "bold", size = 16),
        plot.caption = element_markdown(color = "darkgrey", hjust = 0),
        legend.position = "top",
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.line = element_line(color = "darkgrey"),
        panel.grid = element_line(color = "grey95")) +
  scale_fill_manual(name = "Registro de Heridos",
                    values = c("#03468C", "#970007"),
                    labels = c("Sin heridos",
                               "Se registran heridos"))


dt01 %>% 
  ggplot(aes(x = longitud, y = latitud, color = as_factor(heridos))) +
  geom_point(size = 0.5, alpha = 0.4) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(face = "bold", size = 16),
        plot.caption = element_markdown(color = "darkgrey", hjust = 0),
        legend.position = "top",
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.line = element_line(color = "darkgrey"),
        panel.grid = element_line(color = "grey95")) +
  scale_color_manual(name = "Registro de Heridos",
                    values = c("#03468C", "#970007"),
                    labels = c("No se registran",
                               "Se registran heridos")) +
  coord_fixed()


# correlation
dt01 %>% 
  select(heridos, diasemana, tipaccid, causaacci,
         caparod, sexo, aliento, cinturon, edad) %>% 
  corrr::correlate() %>% 
  corrr::focus(heridos) %>% 
  na.omit() %>% 
  ggplot(aes(x = heridos, 
             y = fct_reorder(term, heridos),
             fill = heridos > 0)) +
  geom_col(alpha = 0.75) +
  scale_fill_manual(name = "Registro de Heridos",
                    values = c("#03468C", "#970007"),
                    labels = c("Sin heridos",
                               "Se registran heridos")) +
  labs(title = "¿Cómo se relacionan los factores con el registro de heridos?",
       subtitle = "Factores reportados",
       x = "Correlación",
       y = NULL,
       caption = "Fuente: INEGI: Accidentes de Tránsito Terrestre 
         en Zonas Urbanas y Suburbanas 2021<br>
         Visualización: Juan L. Bretón, PMP") +
  scale_x_continuous(labels = scales::percent) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(face = "bold", size = 16),
        plot.caption = element_markdown(color = "darkgrey", hjust = 0),
        legend.position = "top",
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.line = element_line(color = "darkgrey"),
        panel.grid = element_line(color = "grey95")) 





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
         sexo = case_when(sexo == 0 ~ "Se fugó",
                          sexo == 1 ~ "Hombre",
                          sexo == 2 ~ "Mujer"),
         aliento = case_when(aliento == 4 ~ "Si",
                             aliento == 5 ~ "No",
                             aliento == 6 ~ "Se ignora"),
         cinturon = case_when(cinturon == 7 ~ "Si",
                              cinturon == 8 ~ "No",
                              cinturon == 9 ~ "Se ignora"),
         clase = case_when(clase == 1 ~ "Fata",
                           clase == 2 ~ "No fatal",
                           clase == 3 ~ "Solo daños")) %>% 
  mutate(across(.cols = all_of(conv_a_fct), 
                .fns = as_factor))
  
  
glimpse(dt02)


# partition
set.seed(223)
acci_split <- initial_split(dt02, 
                            strata = heridos)

acci_train <- acci_split %>% training()

acci_test <- acci_split %>% testing()


# recipe
acci_recipe <- recipe(heridos ~ diasemana + urbana + 
                        suburbana + tipaccid + causaacci +
                        caparod + sexo + aliento + 
                        cinturon + edad,
                      data = acci_train) %>% 
  step_zv(all_predictors()) %>% 
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% 
  step_normalize(all_numeric_predictors())


# cross-validation folds
set.seed(332)
acci_folds <- vfold_cv(acci_train, 
                       strata = heridos)


# workflow
acci_wf <- workflow() %>% 
  add_recipe(acci_recipe)


# bag of tree spec
bag_spec <- bag_tree(min_n = 10) %>% 
  set_engine("rpart", times = 25) %>% 
  set_mode("classification")

# fit resamples
doParallel::registerDoParallel()
bag_res <- fit_resamples(object = acci_wf %>% add_model(bag_spec),
                         resamples = acci_folds,
                         control = control_resamples(save_pred = TRUE))

# evaluate
collect_metrics(bag_res)

# last fit
bag_fit <- last_fit(acci_wf %>% add_model(bag_spec),
                    acci_split)

# predictions of last fit
bag_results <- bag_fit$.predictions[[1]] %>% 
  select(heridos, .pred_class, .pred_0, .pred_1)

# confusion matrix
conf_mat(bag_results,
         truth = heridos,
         estimate = .pred_class) %>% 
  autoplot()

# auc data
bag_auc <- bag_results %>% 
  roc_curve(heridos, .pred_0) %>% 
  mutate(algo = "Bag of trees")

# metrics last fit
collect_metrics(bag_fit)

# feature importance
fit_imp <- bag_fit %>% 
  extract_fit_engine()

fit_imp$imp %>% 
  ggplot(aes(x = value, y = fct_reorder(term, value))) +
  geom_col(fill = "darkgreen", alpha = 0.7)



# logistic regression spec
logreg_spec <- logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

# fit resamples
doParallel::registerDoParallel()
logreg_res <- fit_resamples(object = acci_wf %>% add_model(logreg_spec),
                            resamples = acci_folds,
                            control = control_resamples(save_pred = TRUE))

# evaluate
collect_metrics(logreg_res)

# last fit
logreg_fit <- last_fit(acci_wf %>% add_model(logreg_spec),
                       acci_split)

# predictions of last fit
logreg_results <- logreg_fit$.predictions[[1]] %>% 
  select(heridos, .pred_class, .pred_0, .pred_1) %>% 
  na.omit() 

# confusion matrix
conf_mat(logreg_results,
         truth = heridos,
         estimate = .pred_class) %>% 
  autoplot()

# auc data
logreg_auc <- logreg_results %>% 
  roc_curve(heridos, .pred_0) %>% 
  mutate(algo = "Logistic regression")

# metrics last fit
collect_metrics(logreg_fit)

# feature importance
logreg_fit %>% 
  extract_fit_engine() %>% 
  tidy() %>% 
  na.omit() %>% 
  filter(term != "(Intercept)") %>% 
  group_by(estimate > 0) %>% 
  slice_max(order_by = estimate, n = 5) %>% 
  ungroup() %>%
  ggplot(aes(x = estimate,
             y = fct_reorder(term, estimate),
             fill = estimate > 0)) +
    geom_col(alpha = 0.7) +
    # geom_segment(aes(xend = 0, yend = estimate)) +
    scale_fill_manual(name = "Registro de Heridos",
                      values = c("#03468C", "#970007"),
                      labels = c("Relacionado con el saldo blanco",
                                 "Relacionado con el registro de heridos")) +
    labs(title = "¿Que factores están relacionados con el registro de heridos?",
         subtitle = "Factores reportados en los accidentes de tránsito ocurridos en Irapuato",
         x = "Coeficiente de correlación",
         y = NULL,
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

ggsave("image/ira_factores.jpg", device = "jpeg", dpi = "retina")



bag_auc %>% 
  add_row(logreg_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = algo)) +
  geom_line() +
  geom_abline(linetype = "dashed", color = "grey60") +
  coord_equal()



# clustering folds
spatial_folds <- clustering_cv(acci_train, 
                               v = 5, 
                               vars = c("longitud", "latitud"))



# fit resamples
doParallel::registerDoParallel()
acci_clust_res <- fit_resamples(object = acci_wf, 
                                resamples = spatial_folds)

collect_metrics(acci_clust_res)


## hora
dt01 %>% 
  mutate(timestamp = hm(paste(hora, minutos, sep = ":"))) %>% 
  mutate(timestamp = as.numeric(timestamp) / 3600) %>% 
  ggplot(aes(x = timestamp)) +
  geom_histogram(breaks = seq(0,24),
                 fill = "darkcyan",
                 color = "white") +
  coord_polar()


# conversión a datos circulares
dt01 %>% 
  mutate(edad = cut(edad, 4)) %>% 
  mutate(timestamp = hm(paste(hora, minutos, sep = ":"))) %>% 
  mutate(timestamp = as.numeric(timestamp) / 3600) %>% 
  mutate(ts_circ = circular::circular(timestamp, 
                                      units = "hours", 
                                      template = "clock24")) %>% 
  ggplot(aes(x = ts_circ)) +
  geom_histogram(breaks = seq(0, 24),
                 fill = "#03468C",
                 color = "grey80",
                 alpha = 0.665) +
  coord_polar() +
  scale_x_continuous(limits = c(0, 24),
                     breaks = seq(0, 24)) +
  theme(text = element_text(family = "Encode Sans Condensed"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(face = "bold", size = 16),
        plot.caption = element_markdown(color = "darkgrey", hjust = 0),
        panel.background = element_rect(fill = "#FFFFFF"),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y.left = element_blank(),
        panel.grid = element_line(color = "grey95")) +
  labs(title = "¿A qué hora ocurren los accidentes vehiculares en Irapuato?",
       subtitle = "Hora del día en que se registran los accidentes",
       x = NULL,
       y = NULL,
       caption = "Fuente: INEGI: Accidentes de Tránsito Terrestre 
         en Zonas Urbanas y Suburbanas 2021<br>
         Visualización: Juan L. Bretón, PMP") 

ggsave("image/ira_hora_01.jpg", device = "jpeg", dpi = "retina")
























  

