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


dt01 <- dt00 %>% 
  janitor::clean_names() %>% 
  mutate(fecha = ymd(paste(anio, mes, dia, sep = "-"))) %>%
  mutate(decesos = as.numeric(if_else(totmuertos >= 1, 1, 0)),
         heridos = as.numeric(if_else(totheridos >= 1, 1, 0)))


dt01 %>% 
  summarize(propor_heridos = mean(heridos)) %>% 
  knitr::kable(digits = 2,
               col.names = c("Proporción de Heridos"),
               format.args = list(decimal.mark = "."))

dt01 %>% 
  count(fecha) %>% 
  summarize(media_accidentes_diarios = sum(n) / 365)


dt01 %>% 
  mutate(fecha = floor_date(fecha, unit = "week")) %>% 
  count(fecha, heridos) %>% 
  filter(fecha != last(fecha),
         fecha != first(fecha)) %>% 
  ggplot(aes(x = fecha, y = n, color = as_factor(heridos))) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "¿Cuántos accidentes de tránsito ocurren en México",
       subtitle = "Ocurridos en zonas urbana y semiurbanas del territorio por semana",
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
  labs(title = "¿Qué proporción de los accidentes vehiculares registra personas heridas?",
       subtitle = "En zonas urbana y semiurbanas del territorio por semana",
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
  labs(title = "¿Cuándo ocurren los accidentes de tránsito en México?",
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
  labs(title = "¿Qué causa los accidentes vehiculares en México",
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
  labs(title = "¿A qué hora ocurren los accidentes vehiculares en México?",
       subtitle = "Hora del día en que se registran los accidentes",
       x = NULL,
       y = NULL,
       caption = "Fuente: INEGI: Accidentes de Tránsito Terrestre 
         en Zonas Urbanas y Suburbanas 2021<br>
         Visualización: Juan L. Bretón, PMP") 
