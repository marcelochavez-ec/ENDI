
# Titulo de la Sintaxis: 
# Estado nutricional a partir de mediciones de hemoglobina

# Operacion Estadistica: 
# Encuesta Nacional sobre Desnutricion Infantil - ENDI 2022 - 2023 

# Autor de la Sintaxis:
# Instituto Nacional de Estadistica y Censos (INEC)
# Direccion Tecnica: 
# Direccion de Estadisticas Sociodemograficas (DIES) 
# Gestion Interna: 
# Gestion de Estadisticas Permanentes a Hogares (GEPH) 

# Fecha de elaboracion: 29/08/2023
# Fecha de actualizacion: 29/08/2023

# Version: 1.0
# Software: R 4.2.0 

#==============================================================================#
####                   Instalacion y carga de paquetes                      ####
#==============================================================================#

# Se usa la funcion p_load() desde el paquete pacman, el cual instala  
# el paquete si esta ausente, y carga para el uso si ya esta instalado 

# Se asegura que el paquete "pacman" este instalado

# Configura la ruta de bibliotecas personalizada
# Agregar la carpeta personalizada a las rutas de búsqueda de paquetes
.libPaths("C:/Program Files/R/R-4.3.0/library/")

# Ahora intenta instalar el paquete
if (!require("pacman")) {
    install.packages("pacman")
}

# Paquetes disponibles desde CRAN

pacman::p_load(
  
  # Gestion de proyectos y archivos 
  here,     # construye rutas a los archivos de su proyecto 
  rio,      # importacion / exportacion de varios tipos de datos
  openxlsx, # importacion / exportacion de libros de Excel de varias hojas
  
  # Instalacion y manejo de paquetes 
  pacman, # instalar y cargar paquetes 
  
  # Manejo general de los datos 
  Hmisc, # funciones utiles para el analsis de datos 
  expss, # tablas, etiquetas y funciones de hojas de calculo y estadisticas de 'SPSS'
  tidyverse, # incluye paquetes para ordenar y presentar los datos 
  lubridate, # trabaja con fechas 
  pillar,    # herramientas para manejar columnas de datos
  janitor,   # Limpieza de datos y tablas
  sjlabelled, # para tratar etiquetas 
  epikit, # agregar categorias
  tibble, # paquete para manejo de estructuras tibble
  
  # Estadisticas
  summarytools, # herramientas para resumir datos de forma rapida y ordenada
  
  # Manejo de muestras complejas 
  srvyr,  # estadistica de resumen para datos de encuetas 
  
  # Paquetes para calculos especificos 
  anthro, # calculo de puntuaciones z de antropometria infantil
  openxlsx # Librería para operar con archivos Excel
)

# Limpieza del espacio de trabajo 
rm(list = ls(all = TRUE))

#==============================================================================#
####                                Funciones                               ####
#==============================================================================#

# Funcion para calcular estadisticos para variables dicotomicas 
srvyr_prop <- function(design, x) {
  
  design %>% 
    summarise(
      props = survey_mean({{ x }},
                          proportion = TRUE,   
                          vartype = c("se", "ci", "cv"),
                          na.rm = T) * 100, 
      deff = survey_mean({{ x }},  
                         deff = "replace",
                         na.rm = T),
      Num = sum({{ x }}, na.rm = TRUE),
      Deno = sum(!is.na({{ x }}))) %>%
    mutate(desag = "Nacional") %>%
    select(
      Desag = desag,
      Props = props, 
      EE    = props_se, 
      LI    = props_low, 
      LS    = props_upp, 
      CV    = props_cv,
      Deff  = deff_deff,
      Num   = Num, 
      Deno  = Deno
    )  
  
}

# Funcion para calcular estadisticos para variables dicotomicas 
# por desagregacion 
srvyr_prop_by <- function(design, x, by) {
  
  design %>%
    group_by({{ by }}) %>%
    summarise(
      props = survey_mean({{ x }},
                          proportion = TRUE,   
                          vartype = c("se", "ci", "cv"),
                          na.rm = T) * 100, 
      deff = survey_mean({{ x }},  
                         deff = "replace",
                         na.rm = T),
      Num = sum({{ x }}, na.rm = TRUE),
      Deno = sum(!is.na({{ x }}))) %>%
    mutate(desag = {{ by }}) %>%
    select(
      Desag = desag,
      Props = props, 
      EE    = props_se, 
      LI    = props_low, 
      LS    = props_upp, 
      CV    = props_cv,
      Deff  = deff_deff,
      Num   = Num, 
      Deno  = Deno
    )  
  
}


# Funcion para calcular estadisticos para variables categoricas
# por desagregacion 
srvyr_freq_by <- function(design, x, by) {
  
  design %>%
    filter(!is.na({{ x }})) %>% 
    group_by({{ by }}, {{ x }}) %>%
    summarise(
      props = survey_mean(proportion = T,
                          vartype = c("se", "ci", "cv"),
                          na.rm = T) * 100,
      deff = survey_mean(deff = "replace",
                         na.rm = T),
      n = unweighted(n())) %>%
    select(
      {{ x }},
      Props = props,
      EE    = props_se,
      LI    = props_low, 
      LS    = props_upp, 
      CV    = props_cv,
      Deff  = deff_deff,
      n     = n
    )
  
}

#==============================================================================#
####                          Carga de base de datos                        ####
#==============================================================================#

# Indicaciones:

# 1. Se proporciona la ruta de archivo absoluta o completa en la funcion import 
# entre comilla. 
# 2. Use barras diagonales ( / ). Este no es el valor predeterminado para las 
# rutas de archivos de Windows 

# Ejemplo:
# df_f1_personas <- import("C:/ENDI/Data/f1_personas.rds")

# Sugerencias:

# Es probable que R no reconozca las rutas de archivos que comienzan con barras 
# inclinadas dobles (p. ej., “//…”) y produzcan un error. Considere mover 
# su trabajo a una unidad "con nombre" o "con letras" (p. ej., "C:" o "D:").

# Base Personas  
df_f1_personas <- readRDS("C:/Users/marcelochavez/Documents/TESIS/NOTEBOOKS_PY/BDD/BDD_ENDI_R2_f1_personas.rds")
df_f1_personas <- as_tibble(df_f1_personas) 

# Base MEF  
df_f2_mef <- readRDS("C:/Users/marcelochavez/Documents/TESIS/NOTEBOOKS_PY/BDD/BDD_ENDI_R2_f2_mef.rds")
df_f2_mef <- as_tibble(df_f2_mef) 

# Diccionario de variables 
# Cargar el archivo: Diccionario_ENDI.xlsx con la hoja ya especificada   

# Personas
dicc_f1_per <- read.xlsx("C:/Users/marcelochavez/Documents/TESIS/NOTEBOOKS_PY/BDD/Diccionario_variables_ENDI_R2.xlsx",
                         sheet = "f1_personas", 
                         startRow = 10)

dicc_f1_per <- as_tibble(dicc_f1_per)

# MEF
dicc_f2_mef <- read.xlsx("C:/Users/marcelochavez/Documents/TESIS/NOTEBOOKS_PY/BDD/Diccionario_variables_ENDI_R2.xlsx",
                      sheet = "f2_mef",
                      startRow = 10)

dicc_f2_mef <- as_tibble(dicc_f2_mef)

#------------------------------------------------------------------------------#
# Join
#------------------------------------------------------------------------------#

# Variables compartidas
df_f2_mef <- df_f2_mef %>% 
  select(-c(id_upm, id_viv, id_hogar, fecha_anio, fecha_mes, 
            fecha_dia, fexp, estrato, area, region, prov))

# Join
df_f1_permef <- df_f1_personas %>%  
  left_join(df_f2_mef, by = c("id_mef"))

rm(df_f1_personas, df_f2_mef)

#==============================================================================#
####           Calculo de variables antropométricas necesarias para el indicador            ####
#==============================================================================#

# Estimacion de la edad en dias -----------------------------------------------#

df_f1_permef <- df_f1_permef %>% 
  mutate(dob = paste(f1_s1_4_3, f1_s1_4_2, f1_s1_4_1)) %>%
  mutate(dov = paste(f1_s6_5_3, f1_s6_5_2, f1_s6_5_1)) %>%
  mutate(dob = as_date(dob)) %>%
  mutate(dov = as_date(dov)) %>%
  mutate(edaddias = (dob %--% dov) / days(1))
  mutate(edadmeses = trunc((dob %--% dov) / months(1))) %>% 
  mutate(edadanios = trunc((dob %--% dov) / years(1)))

df_f1_permef %>%
  descr(edaddias, 
        stats = c("common"),
  round.digits = 2)

# Estimadción del peso en Kg:
# Validación de las 3 medidas del peso
df_f1_personas <- df_f1_personas %>% 
    mutate(aux_peso = abs(f1_s5_4_1 - f1_s5_4_2))

df_f1_personas <- df_f1_personas %>%
    mutate(f1_s5_4_3 = case_when(
        aux_peso <= 0.5 & !list.na(f1_s5_4_3) ~ NA_real_,
        TRUE ~ f1_s5_4_3))

# Nivel de hemoglobina de referencia ------------------------------------------#

# Ajustes de las concentraciones de hemoglobina medidas en funcion de la 
# altitud sobre el nivel del mar

df_f1_permef %>%
  descr(altitud, 
        stats = c("common")) 

df_f1_permef %>%
  descr(f1_s6_3, 
        stats = c("common")) 

df_f1_permef <- df_f1_permef %>% 
  mutate(nivel_ajus = case_when(
    altitud < 1000 & !is.na(altitud) ~ f1_s6_3,
    altitud >= 1000 & altitud <= 1499 ~ f1_s6_3 - 0.2,
    altitud >= 1500 & altitud <= 1999 ~ f1_s6_3 - 0.5,    
    altitud >= 2000 & altitud <= 2499 ~ f1_s6_3 - 0.8,  
    altitud >= 2500 & altitud <= 2999 ~ f1_s6_3 - 1.3,
    altitud >= 3000 & altitud <= 3499 ~ f1_s6_3 - 1.9,
    altitud >= 3500 & altitud <= 3999 ~ f1_s6_3 - 2.7,
    altitud >= 4000 & altitud <= 4499 ~ f1_s6_3 - 3.5,
    altitud >= 4500 & altitud <= 4999 ~ f1_s6_3 - 4.5,
    TRUE ~ NA_real_
  ))

df_f1_permef %>%
  descr(nivel_ajus, 
        stats = c("common")) 

#==============================================================================#
####                Construccion de los indicadores de anemia               ####
#==============================================================================#

# Tipo de anemia para ninos/as de 6 a 59 meses de edad ------------------------#

# Indicador 
df_f1_permef <- df_f1_permef %>% 
  mutate(prev_ane = case_when(
    (nivel_ajus < 7 & !is.na(nivel_ajus)) & 
      (edaddias >= 183 & edaddias < 1826) ~ "Anemia grave",
    (nivel_ajus >= 7 & nivel_ajus < 10) &
      (edaddias >= 183 & edaddias < 1826) ~ "Anemia moderada",
    (nivel_ajus >= 10 & nivel_ajus < 11) &
      (edaddias >= 183 & edaddias < 1826) ~ "Anemia leve",
    (nivel_ajus >= 11 & !is.na(nivel_ajus)) &
      (edaddias >= 183 & edaddias < 1826) ~ "No tiene anemia",
    TRUE ~ NA_character_ 
  ))

# Orden de las categorias
df_f1_permef <- df_f1_permef %>% 
  mutate(prev_ane = fct_relevel(prev_ane,
                                c("Anemia grave", 
                                  "Anemia moderada",
                                  "Anemia leve", 
                                  "No tiene anemia")))

df_f1_permef %>% 
  freq(prev_ane, cumul = F, report.nas = F)

# Variable dicotomica  
df_f1_permef <- df_f1_permef %>%
  mutate(prev_ane_new = case_when(
    (nivel_ajus < 11 & !is.na(nivel_ajus)) &
      (edaddias >= 183 & edaddias < 1826) ~ 1,
    (nivel_ajus >= 11 & !is.na(nivel_ajus)) &
      (edaddias >= 183 & edaddias < 1826) ~ 0,
    TRUE ~ NA_real_
  ))

df_f1_permef %>%
  freq(prev_ane_new, cumul = F, report.nas = F)

# Tipo de anemia para mef de 12 a 49 anios de edad ----------------------------#

df_f1_permef %>%
  freq(f2_s2_200, cumul = F, report.nas = F)

# Indicador 
df_f1_permef <- df_f1_permef %>% 
  mutate(prev_ane_muj = case_when(
    (nivel_ajus < 8 & !is.na(nivel_ajus)) & 
      (edaddias >= 4383 & edaddias < 18263) & f2_s2_200 == 2 ~ "Anemia grave",
    (nivel_ajus >= 8 & nivel_ajus < 11) &
      (edaddias >= 4383 & edaddias < 18263) & f2_s2_200 == 2 ~ "Anemia moderada",
    (nivel_ajus >= 11 & nivel_ajus < 12) &
      (edaddias >= 4383 & edaddias < 18263) & f2_s2_200 == 2 ~ "Anemia leve",
    (nivel_ajus >= 12 & !is.na(nivel_ajus)) &
      (edaddias >= 4383 & edaddias < 18263) & f2_s2_200 == 2 ~ "No tiene anemia",
    TRUE ~ NA_character_ 
  ))

df_f1_permef %>%
  freq(prev_ane_muj, cumul = F, report.nas = F)

# Orden de las categorias
df_f1_permef <- df_f1_permef %>% 
  mutate(prev_ane_muj = fct_relevel(prev_ane_muj,
                                    c("Anemia grave", 
                                      "Anemia moderada",
                                      "Anemia leve", 
                                      "No tiene anemia")))

df_f1_permef %>% 
  freq(prev_ane_muj, cumul = F, report.nas = F)

# Variable dicotomica  
df_f1_permef <- df_f1_permef %>%
  mutate(prev_ane_new_muj = case_when(
    (nivel_ajus < 12 & !is.na(nivel_ajus)) &
      (edaddias >= 4383 & edaddias < 18263) & f2_s2_200 == 2 ~ 1,
    (nivel_ajus >= 12 & !is.na(nivel_ajus)) &
      (edaddias >= 4383 & edaddias < 18263) & f2_s2_200 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

df_f1_permef %>%
  freq(prev_ane_new_muj, cumul = F, report.nas = F)

#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores y ser visibles en las funciones 
# Area
df_f1_permef <- df_f1_permef %>%
  mutate(area = as_label(area)) 

df_f1_permef %>%
  freq(area, cumul = F, report.nas = F)

# Region 
df_f1_permef <- df_f1_permef %>%
  mutate(region = as_label(region)) 

df_f1_permef %>%
  freq(region, cumul = F, report.nas = F)

# Provincia 
df_f1_permef <- df_f1_permef %>%
  mutate(prov = as_label(prov)) 

df_f1_permef %>%
  freq(prov, cumul = F, report.nas = F)

# Parroquias priorizadas 
df_f1_permef <- df_f1_permef %>%
  mutate(parr_pri = as_label(parr_pri)) 

df_f1_permef %>%
  freq(parr_pri, cumul = F, report.nas = F)

# Sexo
df_f1_permef <- df_f1_permef %>%
  mutate(f1_s1_2 = as_label(f1_s1_2)) 

df_f1_permef %>%
  freq(f1_s1_2, cumul = F, report.nas = F)

# Auto-Identificacion etnica 
df_f1_permef <- df_f1_permef %>%
  mutate(etnia = as_label(etnia)) 

df_f1_permef %>%
  freq(etnia, cumul = F, report.nas = F)

# Quintil 
df_f1_permef <- df_f1_permef %>%
  mutate(quintil = as_label(quintil)) 

df_f1_permef %>%
  freq(quintil, cumul = F, report.nas = F)

# Pobreza por Ingresos 
df_f1_permef <- df_f1_permef %>%
  mutate(pobreza = as_label(pobreza)) 

df_f1_permef %>%
  freq(pobreza, cumul = F, report.nas = F)

# Pobreza por NBI
df_f1_permef <- df_f1_permef %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f1_permef %>%
  freq(nbi_1, cumul = F, report.nas = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f1_permef %>% as_survey_design(ids = "id_upm",
                                                   strata = "estrato",
                                                   weights = "fexp")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

# Para los resultados ponderados por diferentes desagregaciones reemplazar 
# respectivamente 

# Tipo de anemia para ninos/as de 6 a 59 meses de edad
survey_design %>%
  srvyr_freq(prev_ane)

survey_design %>%
  srvyr_freq_by(prev_ane, area)

survey_design %>%
  srvyr_prop(prev_ane_new)

survey_design %>%
  srvyr_prop_by(prev_ane_new, area)

# Tipo de anemia para mef de 12 a 49 anios de edad 
survey_design %>%
  srvyr_freq(prev_ane_muj)

survey_design %>%
  srvyr_freq_by(prev_ane_muj, area)

survey_design %>%
  srvyr_prop(prev_ane_new_muj)

survey_design %>%
  srvyr_prop_by(prev_ane_new_muj, area)
