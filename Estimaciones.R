# Autor: Ivan Mata
# Elaborado:25 de noviembre de 2025
# Este codigo incluye el calculo de las estimaciones de las mujeres que enfrentaron alguna situacion de acoso y/o violencia sexual en lugares publicos a partir de la Encuesta Nacional de Seguridad Publica Urbana correspondiente al segundo trimestre de 2025

# 1 paqueterias----

library(tidyverse)
library(srvyr)
library(janitor)

# 2 importacion----

#base con informacion del cuestionario

t2_2025  <- read_csv("conjunto_de_datos_ensu_cb_0625.csv")

#limpieza de nombres

t2_2025 <- clean_names(t2_2025)

#base de informacion demografica

t2_2025dem <- read_csv("conjunto_de_datos_ensu_cs_0625.csv")

#limpieza de nombres

t2_2025dem <- clean_names(t2_2025dem)

# 3 base recodificada----

t2_2025_rec <- t2_2025 |> 
  left_join(t2_2025dem |> 
              select(id_per, i_niv, c_act, v_act), by = "id_per") |> 
  mutate(vtotal = case_when(bp4_1_1 == 1 ~ "Violencia sexual total",
                            bp4_1_2 == 1 ~ "Violencia sexual total",
                            bp4_1_3 == 1 ~ "Violencia sexual total",
                            bp4_1_4 == 1 ~ "Violencia sexual total",
                            bp4_1_5 == 1 ~ "Violencia sexual total",
                            bp4_1_6 == 1 ~ "Violencia sexual total",
                            bp4_1_7 == 1 ~ "Violencia sexual total",
                            bp4_1_8 == 1 ~ "Violencia sexual total",
                            bp4_1_9 == 1 ~ "Violencia sexual total",
                            .default = as.character(0)),
         i_niv = as.numeric(i_niv),
         ocu = factor(case_when(c_act %in% c(1, 2) ~ "Trabajó",
                                c_act == 3 & v_act == 4 ~ "Otra",
                                c_act == 3 & v_act %in% c(1:3) ~ "Trabajó",
                                c_act == 4 & v_act == 4 ~ "Solo es estudiante", 
                                c_act == 4 & v_act %in% c(1:3) ~ "Trabajó",
                                c_act == 5 & v_act == 4 ~ "Solo se dedica a los quehaceres del hogar",
                                c_act == 5 &  v_act %in% c(1:3) ~ "Trabajó",
                                c_act == 6 & v_act == 4 ~ "Otra",
                                c_act == 6 &  v_act %in% c(1:3) ~ "Trabajó",
                                c_act == 8 & v_act == 4 ~ "No trabajó",
                                c_act == 8 &  v_act %in% c(1:3) ~ "Trabajó",
                                c_act == 7 ~ "Otra",
         ), levels = c("Trabajó", "Solo es estudiante", "Solo se dedica a los quehaceres del hogar", "No trabajó", "Otra")),
         redad = case_when(between(edad, 18, 24) ~ "18 - 24",
                           between(edad, 25, 29) ~ "25 - 29",
                           between(edad, 30, 34) ~ "30 - 34",
                           between(edad, 35, 39) ~ "35 - 39",
                           between(edad, 40, 44) ~ "40 - 44",
                           between(edad, 45, 49) ~ "45 - 49",
                           between(edad, 50, 54) ~ "50 - 54",
                           between(edad, 55, 59) ~ "55 - 59",
                           edad >= 60 ~ "60 y más"
         )
  ) 

# 5 diseno muestral----

t2_2025dis <- t2_2025_rec |> 
  as_survey_design(weights = fac_sel,
                   strata = est,
                   ids = upm,
                   nest = TRUE)

# 6 calculo de cruces----


t2025_2 <- bind_rows(
  
  #total
  t2_2025dis |> 
    filter(sexo == 2) |> 
    mutate(x = vtotal) |> 
    group_by(x) |> 
    summarise(p2025_2 = survey_prop(vartype = c("se", "cv", "ci"),
                                    level = 0.9,
                                    df= Inf)) |> 
    filter(x != "0"),
  
  
  #por edad
  t2_2025dis |> 
    filter(sexo == 2) |> 
    mutate(x = redad) |> 
    group_by(x, vtotal) |> 
    summarise(p2025_2 = survey_prop(vartype = c("se", "cv", "ci"),
                                    level = 0.9,
                                    df= Inf)) |> 
    filter(vtotal != "0"),
  
  #ocupacion
  
  t2_2025dis |> 
    filter(sexo == 2) |> 
    mutate(x = ocu) |> 
    group_by(x, vtotal) |> 
    summarise(p2025_2 = survey_prop(vartype = c("se", "cv", "ci"),
                                    level = 0.9,
                                    df= Inf)) |> 
    filter(vtotal != "0"),
) |> 
  mutate(p2025_2 = p2025_2*100,
         p2025_2_low = p2025_2_low*100,
         p2025_2_upp = p2025_2_upp*100,
         x = as.factor(x))
