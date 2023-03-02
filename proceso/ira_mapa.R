## librerías
library(tidyverse)
library(lubridate)
library(sf)
library(ggtext)


## adquisición de datos
# data de accidentes
dat_00 <- 
st_read("raw/BASE MUNICIPAL_ACCIDENTES DE TRANSITO GEORREFERENCIADOS_2021.shp")

# visualización de data de Irapuato GTO
dat_00 %>% 
  filter(EDO == 11) %>% 
  filter(MPIO == 17) %>%
  mutate(heridos = as_factor(if_else(TOTHERIDOS > 0, 1, 0))) %>% 
  ggplot(aes(color = heridos)) +
  geom_sf(size = 0.65, alpha = 0.25) +
  scale_color_manual(name = "¿Hubo heridos?",
                     values = c("#02B3DA", "#970007"),
                     labels = c("Sin heridos", 
                                "Se registran heridos")) +
  theme_void()


# municipios de guanajuato
gto_muns <- st_read("conjunto/11mun.shp")

# data de accidentes en el marco de Irapuato GTO
gto_muns %>% 
  filter(NOMGEO == "Irapuato") %>%
  ggplot() +
    geom_sf(color = "grey60", size = 0.65, fill = "#FFFFFF") +
    geom_sf(data = dat_00 %>% 
              filter(EDO == 11) %>% 
              filter(MPIO == 17) %>%
              mutate(heridos = as_factor(if_else(TOTHERIDOS > 0, 1, 0))),
            aes(color = heridos),
            size = 0.35, alpha = 0.25) +
    labs(title = "¿Dónde ocurren los accidentes de tránsito?",
         subtitle = "Área urbana de Irapuato, Guanajuato",
         x = NULL,
         y = NULL,
         caption = "Fuente: INEGI: Accidentes de Tránsito Terrestre 
         en Zonas Urbanas y Suburbanas 2021<br>
         Visualización: Juan L. Bretón, PMP") +
    theme_void() +
    theme(text = element_text(family = "Encode Sans Condensed"),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.title = element_text(face = "bold", size = 16),
          plot.caption = element_markdown(color = "darkgrey", hjust = 0),
          legend.position = "top",
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank()) +
    scale_color_manual(name = "¿Hubo heridos?",
                       values = c("#03468C", "#970007"),
                       labels = c("Sin heridos", 
                                  "Se registran heridos"))

# datos de vialidades del estado de guanajuato
gto_urb <- st_read("conjunto/11a.shp")

# visualización de todas las capas IRAPUATO
gto_urb %>% 
  filter(CVE_MUN == "017") %>% 
  ggplot() +
    geom_sf(size = 0.65, alpha = 0.25) +
    geom_sf(data = dat_00 %>% 
              filter(EDO == 11) %>% 
              filter(MPIO == 17) %>%
              mutate(heridos = as_factor(if_else(TOTHERIDOS > 0, 1, 0))),
            aes(color = heridos),
            size = 0.50, alpha = 0.55) +
    # geom_sf(data = gto_muns %>% filter(NOMGEO == "Irapuato"),
    #         color = "grey60", size = 0.65, fill = "#FFFFFF",
    #         alpha = 0.2) +
    scale_color_manual(name = "¿Hubo heridos?",
                       values = c("#03468C", "#970007"),
                       labels = c("Sin heridos", 
                                  "Se registran heridos")) +
    theme_void() +
    labs(title = "¿Dónde ocurren los accidentes de tránsito?",
         subtitle = "Municipio de Irapuato, Guanajuato; áreas urbana y suburbanas.",
         x = NULL,
         y = NULL,
         caption = "Fuente: INEGI: Accidentes de Tránsito Terrestre 
           en Zonas Urbanas y Suburbanas 2021<br>
           Visualización: Juan L. Bretón, PMP") +
    theme(text = element_text(family = "Encode Sans Condensed"),
          plot.title.position = "plot",
          plot.caption.position = "plot",
          plot.title = element_text(face = "bold", size = 16),
          plot.caption = element_markdown(color = "darkgrey", hjust = 0)) 

ggsave("image/ira_puntos_01.jpg", device = "jpeg", dpi = "retina")  
