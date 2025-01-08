# D10. Sintaxis del Indicador 

# Titulo de la Sintaxis:
# Prevalencia de desnutrición crónica en niñas/os menores de 5 años

# Operación Estadística:
# Encuesta Nacional sobre Desnutrición Infantil (ENDI 2022 - 2023)

# Autor de la Sintaxis:
# Instituto Nacional de Estadística y Censos (INEC)
# Dirección Técnica:
# Dirección de Estadísticas Sociodemográficas (DIES)
# Gestión Interna:
# Gestión de Estadísticas Permanentes a Hogares (GEPH)
# Fecha de elaboración: 15/02/2023
# Fecha de actualización: 12/06/2023

# Versión: 1.0
# Software: R 4.2.0

#==============================================================================#
####                  Instalación y carga de paquetes                       ####
#==============================================================================#

# Se usa la función p_load() desde el paquete pacman, el cual instala
# el paquete si está ausente, y carga para el uso si ya está instalado

# Se asegura que el paquete "pacman" este instalado 
# if(!require("pacman")) install.packages("pacman")

# Paquetes disponibles desde CRAN

rm(list = ls(all = TRUE))

pacman::p_load(
  
  # Gestión de proyectos y archivos
  here,    # construye rutas a los archivos de su proyecto
  rio,     # importación / exportación de varios tipos de datos
  expss, # tablas, etiquetas y funciones de hojas de cálculo y estadísticas de 'SPSS'
  
  # Instalación y manejo de paquetes pacman, # instalar y cargar paquetes
  
  # Manejo general de los datos
  tidyverse, # incluye paquetes para ordenar y presentar los datos lubridate, 
  # trabaja con fechas
  pillar,   # herramientas para manejar columnas de datos janitor,  
  # Limpieza de datos y tablas
  sjlabelled, # para tratar etiquetas epikit, 
  # agregar categorías
  
  # Estadísticas
  summarytools, # herramientas para resumir datos de forma rápida y ordenada
  
  # Manejo de muestras complejas
  srvyr, # estadística de resumen para datos de encuestas
  
  # Paquetes para cálculos específicos
  anthro, # cálculo de puntuaciones z de antropometría infantil
  openxlsx
  
)

# Limpieza del espacio de trabajo rm(list = ls(all = TRUE))

#==============================================================================#
####                               Funciones                                ####
#==============================================================================#

# Función para calcular estadísticos para variables dicotómicas 
srvyr_prop <- function(design, x) {

design %>%
  summarise( 
    props = survey_mean({{ x }}, 
                        proportion = TRUE,
                        vartype = c("se", "ci", "cv"), na.rm = T) * 100,
    deff = survey_mean({{ x }}, deff = "replace", na.rm = T),
    Num = sum({{ x }}, na.rm = TRUE), Deno = sum(!is.na({{ x }}))) %>%
  mutate(desag = "Nacional") %>%
  select(
    Desag = desag, 
    Props = props,
    EE   = props_se, 
    LI    = props_low, 
    LS   = props_upp, 
    CV   = props_cv, 
    Deff = deff_deff, 
    Num  = Num, 
    Deno = Deno
  )

}

# Función para calcular estadísticos para variables dicotómicas
# por desagregación
srvyr_prop_by <- function(design, x, by) {
  
  design %>%
    group_by({{ by }}) %>%
    summarise(
      props = survey_mean({{ x }}, 
                          proportion = TRUE,
                          vartype = c("se", "ci", "cv"), 
                          na.rm = T) * 100,
      deff = survey_mean({{ x }}, 
                         deff = "replace", na.rm = T),
      Num = sum({{ x }}, na.rm = TRUE),
      Deno = sum(!is.na({{ x }}))) %>% 
    mutate(desag = {{ by }}) %>% 
    select(
        Desag = desag, 
        Props = props,
        EE   = props_se, 
        LI    = props_low, 
        LS   = props_upp,
        CV   = props_cv, 
        Deff = deff_deff,
        Num  = Num,
        Deno = Deno
      )
  
}

#==============================================================================#
####                          Carga de base de datos                        ####
#==============================================================================#

# Indicaciones:

# 1. Se proporciona la ruta de archivo absoluta o completa en la función import
# entre comilla.
# 2. Use barras diagonales ( / ). Este no es el valor predeterminado para las
# rutas de archivos de Windows

# Ejemplo:
# df_f1_personas <- import("C:/ENDI/Data/f1_personas.rds")

# Sugerencias:

# Es probable que R no reconozca las rutas de archivos que comienzan con barras
# inclinadas dobles (p. ej., “//…”) y produzcan un error. Considere mover
# su trabajo a una unidad "con nombre" o "con letras" (p. ej., "C:" o "D:").

# Base personas 
# df_f1_personas <- import("")
# df_f1_personas <- as_tibble(df_f1_personas)
# df_f1_personas

# Diccionario de variables
# Cargar el archivo: Diccionario_ENDI.xlsx con la hoja ya especificada 
# dicc_f1_per <- import("",
#                       which = "f1_personas")
# 
# dicc_f1_per <- as_tibble(dicc_f1_per)
# dicc_f1_per

# Base Personas  
df_f1_personas <- readRDS("C:/Users/marcelochavez/Documents/TESIS/PROYECTO_ENDI/BDD/BDD_ENDI_R2_f1_personas.rds")
df_f1_personas <- as_tibble(df_f1_personas) 

dicc_f1_per <- read.xlsx("C:/Users/marcelochavez/Documents/TESIS/PROYECTO_ENDI/BDD/Diccionario_variables_ENDI_R2.xlsx",
                         sheet = "f1_personas", 
                         startRow = 10)
dicc_f1_per <- as_tibble(dicc_f1_per)

# Base MEF  
df_f2_mef <- readRDS("C:/Users/marcelochavez/Documents/TESIS/PROYECTO_ENDI/BDD/BDD_ENDI_R2_f2_mef.rds")
df_f2_mef <- as_tibble(df_f2_mef) 

dicc_f2_mef <- read.xlsx("C:/Users/marcelochavez/Documents/TESIS/PROYECTO_ENDI/BDD/Diccionario_variables_ENDI_R2.xlsx",
                         sheet = "f2_mef",
                         startRow = 10)
dicc_f2_mef <- as_tibble(dicc_f2_mef)

#==============================================================================#
####   Cálculo de variables antropométricas necesarias para el indicador    ####
#==============================================================================#

# Estimación de la edad en días -----------------------------------------------#

df_f1_personas <- df_f1_personas %>%
  mutate(dob = paste(f1_s5_2_3, f1_s5_2_2, f1_s5_2_1)) %>% 
  mutate(dov = paste(f1_s5_3_3, f1_s5_3_2, f1_s5_3_1)) %>% 
  mutate(dob = as_date(dob)) %>%
  mutate(dov = as_date(dov)) %>%
  mutate(edaddias = (dob %--% dov) / days(1)) %>% 
  mutate(edadmeses = trunc((dob %--% dov) / months(1))) %>% 
  mutate(edadanios = trunc((dob %--% dov) / years(1)))

df_f1_personas %>%
  descr(edaddias,
        stats = c("common"), 
        round.digits = 2)

# Estimación del peso (Kg) ----------------------------------------------------#

# Validación de las 3 medidas del peso 
df_f1_personas <- df_f1_personas %>% mutate(aux_peso = abs(f1_s5_4_1 - f1_s5_4_2))

df_f1_personas <- df_f1_personas %>%
  mutate(f1_s5_4_3 = case_when(
    aux_peso <= 0.5 & !is.na(f1_s5_4_3) ~ NA_real_, 
    TRUE ~ f1_s5_4_3
  )) 

# Se calcula el peso en kg
# Distancia entre las tres medidas 
df_f1_personas <- df_f1_personas %>% 
mutate(d1 = abs(f1_s5_4_1 - f1_s5_4_2)) %>% 
mutate(d2 = abs(f1_s5_4_1 - f1_s5_4_3)) %>% 
mutate(d3 = abs(f1_s5_4_2 - f1_s5_4_3))

# Variable identificador
# Distancia entre toma 1 y toma 2 es menor o igual a 0.5 
df_f1_personas <- df_f1_personas %>%
mutate(s = case_when(
  d1 <= 0.5 ~ 1, d1 
  > 0.5 ~ 0, TRUE ~ 
    NA_real_
))

# Promedio simple entre toma 1 y toma 2 
df_f1_personas<-df_f1_personas %>% 
mutate(peso = case_when(
  s == 1 ~ (f1_s5_4_1 + f1_s5_4_2) / 2, 
  TRUE ~ NA_real_
))

df_f1_personas %>%
  descr(peso,
        stats = c("common"), 
        round.digits = 2)

# Caso contrario, promedio de la menor distancia entre las 3 mediciones
# Distancia mínima
df_f1_personas <- df_f1_personas %>%
mutate(dmin = case_when( 
  (d1 <= d2 & d1 <= d3) |
  (!is.na(d1) & is.na(d2) & is.na(d3)) ~ d1, 
  (d2 <= d1 & d2 <= d3) |
  (!is.na(d2) & is.na(d1) & is.na(d3)) ~ d2, 
  (d3 <= d1 & d3 <= d2) |
  (!is.na(d3) & is.na(d1) & is.na(d2)) ~ d3, 
  TRUE ~ NA_real_
  ))

df_f1_personas <- df_f1_personas %>%
  mutate(peso = case_when(
    d3 == dmin ~ (f1_s5_4_2 + f1_s5_4_3) / 2, d2 == dmin ~ 
      (f1_s5_4_1 + f1_s5_4_3) / 2, d1 == dmin ~ (f1_s5_4_1 + f1_s5_4_2) / 2, 
    TRUE ~ peso
  ))

df_f1_personas %>%
  descr(peso,
        stats = c("common"), 
        round.digits = 2)

# Estimación de la talla (cm) -------------------------------------------------#

# Validación de las 3 medidas de la longitud 
df_f1_personas <- df_f1_personas %>% 
  mutate(aux_long = abs(f1_s5_5_1 - f1_s5_5_2)) 
df_f1_personas <- df_f1_personas %>%
  mutate(f1_s5_5_3 = case_when(
    aux_long <= 0.5 & !is.na(f1_s5_5_3) ~ NA_real_, 
    TRUE ~ f1_s5_5_3
  ))

# Validación de las 3 medidas de la talla 

df_f1_personas <- df_f1_personas %>% mutate(aux_tal = abs(f1_s5_6_1 - f1_s5_6_2))

df_f1_personas <- df_f1_personas %>%
  mutate(f1_s5_6_3 = case_when(
    aux_tal <= 0.5 & !is.na(f1_s5_6_3) ~ NA_real_, 
    TRUE ~ f1_s5_6_3
  ))

# Se calcula la talla en cm.
# Consolido las tomas para longitud y talla

# Talla 1
df_f1_personas <- df_f1_personas %>%
  mutate(talla1 = case_when(
    is.na(f1_s5_5_1) & !is.na(f1_s5_6_1) ~ f1_s5_6_1,
    !is.na(f1_s5_5_1) & is.na(f1_s5_6_1) ~ f1_s5_5_1, 
    TRUE ~ NA_real_
  ))

# Talla 2
df_f1_personas <- df_f1_personas %>%
  mutate(talla2 = case_when(
    is.na(f1_s5_5_2) & !is.na(f1_s5_6_2) ~ f1_s5_6_2,
    !is.na(f1_s5_5_2) & is.na(f1_s5_6_2) ~ f1_s5_5_2, TRUE ~ NA_real_
  ))

# Talla 3
df_f1_personas <- df_f1_personas %>%
  mutate(talla3 = case_when(
    is.na(f1_s5_5_3) & !is.na(f1_s5_6_3) ~ f1_s5_6_3,
    !is.na(f1_s5_5_3) & is.na(f1_s5_6_3) ~ f1_s5_5_3, 
    TRUE ~ NA_real_
  ))

# Distancia entre las tres medidas 
df_f1_personas <- df_f1_personas %>% 
  mutate(d1_tal = abs(talla1 - talla2)) %>% 
  mutate(d2_tal = abs(talla1 - talla3)) %>% 
  mutate(d3_tal = abs(talla2 - talla3))

# Variable identificador
# Distancia entre toma 1 y toma 2 es menor o igual a 0.5 
df_f1_personas <- df_f1_personas %>%
mutate(s_tal = case_when(
  d1_tal <= 0.5 ~ 1, 
  d1_tal > 0.5 ~ 0, 
  TRUE ~ NA_real_
))

# Promedio simple entre toma 1 y toma 2 
df_f1_personas <- df_f1_personas %>% 
mutate(talla = case_when(
  s_tal == 1 ~ (talla1 + talla2) / 2, 
  TRUE ~ NA_real_
))

df_f1_personas %>%
  descr(talla,
        stats = c("common"), 
        round.digits = 2)

# Caso contrario, promedio de la menor distancia entre las 3 mediciones
# Distancia mínima
df_f1_personas <- df_f1_personas %>%
  mutate(dmin_tal = case_when(
    (d1_tal <= d2_tal & d1_tal <= d3_tal) |
      (!is.na(d1_tal) & is.na(d2_tal) & is.na(d3_tal)) ~ d1_tal, 
    (d2_tal <= d1_tal & d2_tal <= d3_tal) |
      (!is.na(d2_tal) & is.na(d1_tal) & is.na(d3_tal)) ~ d2_tal,
    (d3_tal <= d1_tal & d3_tal <= d2_tal) |
      (!is.na(d3_tal) & is.na(d1_tal) & is.na(d2_tal)) ~ d3_tal, 
    TRUE ~ NA_real_
  ))

df_f1_personas <- df_f1_personas %>%
  mutate(talla = case_when(
    d3_tal == dmin_tal ~ (talla2 + talla3) / 2, d2_tal == dmin_tal 
    ~ (talla1 + talla3) / 2, d1_tal == dmin_tal ~ (talla1 + talla2) / 2, 
    TRUE ~ talla
  ))

df_f1_personas %>%
  descr(talla,
        stats = c("common"), 
        round.digits = 2)

# Sexo ------------------------------------------------------------------------#

df_f1_personas <- df_f1_personas %>%
  mutate(sexo = unlabel(f1_s1_2))

df_f1_personas %>%
  freq(sexo, cumul = F)

#==============================================================================#
####          Cálculo de puntuaciones z de antropometría infantil           ####
#==============================================================================#

# Valoración de los z-scores 
df_f1_personas <- df_f1_personas %>% 
  mutate(anthro_zscores(
sex = sexo,
age = edaddias, 
weight = peso, 
lenhei = talla
))

#==============================================================================#
####    Construcción de las variables de desnutrición con los Z-Score       ####
#==============================================================================# 
# Desnutrición crónica DCI para menores de 5 años de edad #

# Definición de la edad en días de la población menor a 5 años:
# Días = 365.25 * 5 = 1826.25 ----> 1826 (valor aproximado) (Manual Anthro OMS)

# Indicador
df_f1_personas <- df_f1_personas %>%
  mutate(dcronica = case_when(
    (zlen >= -6 & zlen < -2) & (edaddias < 1826 & !is.na(edaddias)) ~ 1, 
    (zlen >= -2 & zlen <= 6) & (edaddias < 1826 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_
  ))

df_f1_personas %>%
  freq(dcronica, cumul = F, report.nas = F)

#==============================================================================#
####                          Desagregación                                 ####
#==============================================================================#

# Para establecer las etiquetas como valores
# Área
df_f1_personas <- df_f1_personas %>%
  mutate(area = as_label(area))

df_f1_personas %>%
  freq(area, cumul = F, report.nas = F)

# Región
df_f1_personas <- df_f1_personas %>%
  mutate(region = as_label(region))

df_f1_personas %>%
  freq(region, cumul = F, report.nas = F)

# Provincia
df_f1_personas <- df_f1_personas %>%
  mutate(prov = as_label(prov))

df_f1_personas %>%
  freq(prov, cumul = F, report.nas = F)

# Sexo
df_f1_personas <- df_f1_personas %>%
  mutate(f1_s1_2 = as_label(f1_s1_2))

df_f1_personas %>%
  freq(f1_s1_2, cumul = F, report.nas = F)

#==============================================================================#
####                      Declaración de encuesta                           ####
#==============================================================================#

survey_design <- df_f1_personas %>% as_survey_design(ids = "id_upm", 
                                                     strata = "estrato",
                                                     weights = "fexp")
options(survey.lonely.psu = "adjust")

#==============================================================================# 
####                        Resultados ponderados                           ####
#==============================================================================#

# Desnutrición crónica
# Menores de 5 
survey_design %>% 
srvyr_prop(dcronica)

# Área 

survey_design %>% 
srvyr_prop_by(dcronica, area)

# Región 
survey_design %>% 
srvyr_prop_by(dcronica, region)

# Provincia 
survey_design %>% 
srvyr_prop_by(dcronica, prov)

# Sexo 
survey_design %>% 
srvyr_prop_by(dcronica, f1_s1_2)