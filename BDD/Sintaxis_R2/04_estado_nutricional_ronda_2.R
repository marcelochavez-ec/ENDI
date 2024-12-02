
# Titulo de la Sintaxis: 
# Estado nutricional a partir de indicadores antropometricos

# Operacion Estadistica: 
# Encuesta Nacional sobre Desnutricion Infantil - ENDI 2023 - 2024

# Autor de la Sintaxis:
# Instituto Nacional de Estadistica y Censos (INEC)
# Direccion Tecnica: 
# Direccion de Estadisticas Sociodemograficas (DIES) 
# Gestion Interna: 
# Gestion de Estadisticas Permanentes a Hogares (GEPH) 

# Fecha de elaboracion: 16/10/2024
# Fecha de actualizacion: 16/10/2024

# Version: 1.0
# Software: R 4.2.0 

#------------------------------------------------------------------------------#
# >>>>                          Importante                               <<<<  #
#                                                                              #
#       Primero realizar la accion descrita en el documento "00.master.R"      #
#                                                                              #
#------------------------------------------------------------------------------#

variables_eliminar <- setdiff(ls(all = TRUE), variables_mantener)
rm(list = variables_eliminar)

#==============================================================================#
####                   Carga de bases y disenio de encuesta                 ####
#==============================================================================#

# Base personas 
df_f1_personas <- readRDS(here(link_bdd, name_base_personas))
df_f1_personas <- as_tibble(df_f1_personas)

# Diccionario
dicc_f1_per <- import(paste0(link_bdd, name_diccionario), 
                      which = "f1_personas",trust = TRUE)

dicc_f1_per <- as_tibble(dicc_f1_per)   

#==============================================================================#
####    Calculo de variables antropometricas necesarias para el indicador   ####
#==============================================================================#

# Estimacion de la edad en dias -----------------------------------------------#

df_f1_personas <- df_f1_personas %>% 
  mutate(dob = paste(f1_s5_2_3, f1_s5_2_2, f1_s5_2_1)) %>% 
  mutate(dov = paste(f1_s5_3_3, f1_s5_3_2, f1_s5_3_1)) %>%  
  mutate(dob = as_date(dob)) %>%   
  mutate(dov = as_date(dov)) %>%  
  mutate(edaddias = (dob %--% dov) / days(1)) 

df_f1_personas %>%
  descr(edaddias, 
        stats = c("common"),
        round.digits = 2) 

# Estimacion del peso (kg) ----------------------------------------------------#

# Validacion de las 3 medidas del peso
df_f1_personas <- df_f1_personas %>%
  mutate(aux_peso = abs(f1_s5_4_1 - f1_s5_4_2)) 

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
    d1 <= 0.5 ~ 1,
    d1 > 0.5 ~ 0,
    TRUE ~ NA_real_
  )) 

# Promedio simple entre toma 1 y toma 2
df_f1_personas <- df_f1_personas %>% 
  mutate(peso = case_when(
    s == 1 ~ (f1_s5_4_1 + f1_s5_4_2) / 2,
    TRUE ~ NA_real_
  ))

df_f1_personas %>%
  descr(peso, 
        stats = c("common"),
        round.digits = 2) 

# Caso contrario, promedio de la menor distancia entre las 3 mediciones 
# Distancia minima
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
    d3 == dmin ~ (f1_s5_4_2 + f1_s5_4_3) / 2, 
    d2 == dmin ~ (f1_s5_4_1 + f1_s5_4_3) / 2, 
    d1 == dmin ~ (f1_s5_4_1 + f1_s5_4_2) / 2,
    TRUE ~ peso
  )) 

df_f1_personas %>%
  descr(peso, 
        stats = c("common"),
        round.digits = 2) 

# Estimacion de la talla (cm) -------------------------------------------------#

# Validacion de las 3 medidas de la longitud
df_f1_personas <- df_f1_personas %>%
  mutate(aux_long = abs(f1_s5_5_1 - f1_s5_5_2))

df_f1_personas <- df_f1_personas %>%
  mutate(f1_s5_5_3 = case_when(
    aux_long <= 0.5 & !is.na(f1_s5_5_3) ~ NA_real_,
    TRUE ~ f1_s5_5_3
  ))

# Validacion de las 3 medidas de la talla
df_f1_personas <- df_f1_personas %>%
  mutate(aux_tal = abs(f1_s5_6_1 - f1_s5_6_2))

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
    !is.na(f1_s5_5_2) & is.na(f1_s5_6_2) ~ f1_s5_5_2, 
    TRUE ~ NA_real_
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
# Distancia minima
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
    d3_tal == dmin_tal ~ (talla2 + talla3) / 2, 
    d2_tal == dmin_tal ~ (talla1 + talla3) / 2,
    d1_tal == dmin_tal ~ (talla1 + talla2) / 2,
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
  freq(sexo, cumul = F, report.nas = F)

#==============================================================================#
####           Calculo de puntuaciones z de antropometria infantil          ####
#==============================================================================#

# Valoracion de los z-scores 
df_f1_personas <- df_f1_personas %>% 
  mutate(anthro_zscores(
    sex = sexo,
    age = edaddias,
    weight = peso,
    lenhei = talla
  ))

#==============================================================================#
####     Construccion de las variables de desnutricion con los z-score      ####
#==============================================================================#

# Definicion de la edad en dias de la poblacion menor a 2 anios:
# Dias = 365.25 * 2 = 730.5 ----> 731 (valor aproximado) (Manual Anthro OMS)

# Desnutricion cronica para menores de 2 anios de edad ------------------------#

# Revision 
df_f1_personas %>%
  descr(zlen, 
        stats = c("common"))

# Indicador 
df_f1_personas <- df_f1_personas %>%
  mutate(dcronica_2 = case_when(
    (zlen >= -6 & zlen < -2) & (edaddias < 731 & !is.na(edaddias)) ~ 1,
    (zlen >= -2 & zlen <= 6) & (edaddias < 731 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_ 
  ))

df_f1_personas %>%
  freq(dcronica_2, cumul = F, report.nas = F)

# Desnutricion global para menores de 2 anios de edad -------------------------#

# Revision 
df_f1_personas %>%
  descr(zwei, 
        stats = c("common"))

# Indicador
df_f1_personas <- df_f1_personas %>%
  mutate(dglobal_2 = case_when(
    (zwei >= -6 & zwei < -2) & (edaddias < 731 & !is.na(edaddias)) ~ 1,
    (zwei >= -2 & zwei <= 5) & (edaddias < 731 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_ 
  ))

df_f1_personas %>%
  freq(dglobal_2, cumul = F, report.nas = F)

# Desnutricion aguda para menores de 2 anios de edad --------------------------#

# Revision 
df_f1_personas %>%
  descr(zwfl, 
        stats = c("common"))

# Indicador
df_f1_personas <- df_f1_personas %>%
  mutate(daguda_2 = case_when(
    (zwfl >= -5 & zwfl < -2) & (edaddias < 731 & !is.na(edaddias)) ~ 1,
    (zwfl >= -2 & zwfl <= 5) & (edaddias < 731 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_ 
  ))

df_f1_personas %>%
  freq(daguda_2, cumul = F, report.nas = F)

# Definicion de la edad en dias de la poblacion menor a 5 anios:
# Dias = 365.25 * 5 = 1826.25 ----> 1826 (valor aproximado) (Manual Anthro OMS)

# Desnutricion cronica para menores de 5 anios de edad ------------------------#

# Indicador 
df_f1_personas <- df_f1_personas %>%
  mutate(dcronica = case_when(
    (zlen >= -6 & zlen < -2) & (edaddias < 1826 & !is.na(edaddias)) ~ 1,
    (zlen >= -2 & zlen <= 6) & (edaddias < 1826 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_))

df_f1_personas %>%
  freq(dcronica, cumul = F, report.nas = F)

# Desnutricion global para menores de 5 anios de edad -------------------------#

# Indicador 
df_f1_personas <- df_f1_personas %>%
  mutate(dglobal = case_when(
    (zwei >= -6 & zwei < -2) & (edaddias < 1826 & !is.na(edaddias)) ~ 1,
    (zwei >= -2 & zwei <= 5) & (edaddias < 1826 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_ 
  ))

df_f1_personas %>%
  freq(dglobal, cumul = F, report.nas = F)  

# Desnutricion aguda para menores de 5 anios de edad --------------------------#

# Indicador
df_f1_personas <- df_f1_personas %>%
  mutate(daguda = case_when(
    (zwfl >= -5 & zwfl < -2) & (edaddias < 1826 & !is.na(edaddias)) ~ 1,
    (zwfl >= -2 & zwfl <= 5) & (edaddias < 1826 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_ 
  ))

df_f1_personas %>%
  freq(daguda, cumul = F, report.nas = F)

# Desnutricion cronica en ninas/os de 2 a 5 anios -----------------------------#

# Indicador 
df_f1_personas <-  df_f1_personas %>%
  mutate(dcronica2_5 = case_when(
    (zlen >= -6 & zlen < -2) & (edaddias >= 731 & edaddias < 1826) ~ 1,
    (zlen >= -2 & zlen <= 6) & (edaddias >= 731 & edaddias < 1826) ~ 0, 
    TRUE ~ NA_real_
  ))

df_f1_personas %>%
  freq(dcronica2_5, cumul = F, report.nas = F)

# Desnutricion global en ninas/os de 2 a 5 anios ------------------------------#

# Indicador
df_f1_personas <- df_f1_personas %>%
  mutate(dglobal2_5 = case_when(
    (zwei >= -6 & zwei < -2) & (edaddias >= 731 & edaddias < 1826) ~ 1,
    (zwei >= -2 & zwei <= 5) & (edaddias >= 731 & edaddias < 1826) ~ 0,
    TRUE ~ NA_real_
  ))

df_f1_personas %>%
  freq(dglobal2_5, cumul = F, report.nas = F)

# Desnutricion aguda en ninas/os de 2 a 5 anios -------------------------------#

# Indicador
df_f1_personas <- df_f1_personas %>%
  mutate(daguda2_5 = case_when(
    (zwfl >= -5 & zwfl < -2) & (edaddias >= 731 & edaddias < 1826) ~ 1,
    (zwfl >= -2 & zwfl <= 5) & (edaddias >= 731 & edaddias < 1826) ~ 0, 
    TRUE ~ NA_real_
  ))

df_f1_personas %>%
  freq(daguda2_5, cumul = F, report.nas = F)

# Sobrepeso para menores de 5 anios de edad -----------------------------------#

# Revision
df_f1_personas %>%
  descr(zbmi, 
        stats = c("common"))

# Indicador
df_f1_personas <- df_f1_personas %>%
  mutate(sp5 = case_when(
    (zbmi > 2 & zbmi <= 3) & (edaddias < 1826 & !is.na(edaddias)) ~ 1,
    ((zbmi >= -5 & zbmi <= 2) | (zbmi > 3 & zbmi <= 5)) & 
      (edaddias < 1826 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_ 
  ))

df_f1_personas %>%
  freq(sp5, cumul = F, report.nas = F)

# Obesidad para menores de 5 anios de edad ------------------------------------#

# Indicador
df_f1_personas <- df_f1_personas %>%
  mutate(ob5 = case_when(
    (zbmi > 3 & zbmi <= 5) & (edaddias < 1826 & !is.na(edaddias)) ~ 1,
    (zbmi >= -5 & zbmi <= 3) & (edaddias < 1826 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_ 
  ))

df_f1_personas %>%
  freq(ob5, cumul = F, report.nas = F)

# Sobrepeso y obesidad para menores de 5 anios de edad ------------------------#

# Indicador
df_f1_personas <- df_f1_personas %>%
  mutate(spob5 = case_when(
    (zbmi > 2 & zbmi <= 5) & (edaddias < 1826 & !is.na(edaddias)) ~ 1,
    (zbmi >= -5 & zbmi <= 2) & (edaddias < 1826 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_ 
  ))

df_f1_personas %>%
  freq(spob5, cumul = F, report.nas = F)

# Malnutricion en para menores de 5 anios -------------------------------------#

# Indicador 
df_f1_personas <-  df_f1_personas %>%
  mutate(malnutricion = case_when(
    (zwfl >= -5 & zwfl < -2) & (edaddias < 1826 & !is.na(edaddias)) ~ 1, # Desnutricion Aguda
    (zwfl >= -2 & zwfl <= 2) & (edaddias < 1826 & !is.na(edaddias)) ~ 0, 
    (zwfl > 2 & zwfl <= 3) & (edaddias < 1826 & !is.na(edaddias)) ~ 1, # Sobrepeso
    (zwfl > 3 & zwfl <= 5) & (edaddias < 1826 & !is.na(edaddias)) ~ 1, # Obesidad
    TRUE ~ NA_real_
  ))

df_f1_personas %>%
  freq(malnutricion, cumul = F, report.nas = F)

# Doble carga de malnutricion para menores de 5 anios -------------------------#

# Indicador 
df_f1_personas <-  df_f1_personas %>%
  mutate(malnutricion_dc = case_when(
    ((zlen >= -6 & zlen < -2) & (zbmi > 2 & zbmi <= 5)) & 
      (edaddias < 1826 & !is.na(edaddias)) ~ 1, # Desnutricion cronica - Sobrepeso y obesidad
    (zlen >= -2 & zlen <= 6) & (edaddias < 1826 & !is.na(edaddias)) ~ 0,
    (zbmi >= -5 & zbmi <= 2) & (edaddias < 1826 & !is.na(edaddias)) ~ 0, 
    TRUE ~ NA_real_
  ))

df_f1_personas %>%
  freq(malnutricion_dc, cumul = F, report.nas = F)

#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores y ser visibles en las funciones 
# Area
df_f1_personas <- df_f1_personas %>%
  mutate(area = as_label(area)) 

df_f1_personas %>%
  freq(area, cumul = F, report.nas = F)

# Region 
df_f1_personas <- df_f1_personas %>%
  mutate(region = as_label(region)) 

df_f1_personas %>%
  freq(region, cumul = F, report.nas = F)

# Provincia 
df_f1_personas <- df_f1_personas %>%
  mutate(prov = as_label(prov)) 

df_f1_personas %>%
  freq(prov, cumul = F, report.nas = F)

# Parroquias priorizadas 
df_f1_personas <- df_f1_personas %>%
  mutate(parr_pri = as_label(parr_pri)) 

df_f1_personas %>%
  freq(parr_pri, cumul = F, report.nas = F)

# Sexo
df_f1_personas <- df_f1_personas %>%
  mutate(f1_s1_2 = as_label(f1_s1_2)) 

df_f1_personas %>%
  freq(f1_s1_2, cumul = F, report.nas = F)

# Auto-Identificacion etnica 
df_f1_personas <- df_f1_personas %>%
  mutate(etnia = as_label(etnia)) 

df_f1_personas %>%
  freq(etnia, cumul = F, report.nas = F)

# Quintil 
df_f1_personas <- df_f1_personas %>%
  mutate(quintil = as_label(quintil)) 

df_f1_personas %>%
  freq(quintil, cumul = F, report.nas = F)

# Pobreza por Ingresos 
df_f1_personas <- df_f1_personas %>%
  mutate(pobreza = as_label(pobreza)) 

df_f1_personas %>%
  freq(pobreza, cumul = F, report.nas = F)

# Pobreza por NBI
df_f1_personas <- df_f1_personas %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f1_personas %>%
  freq(nbi_1, cumul = F, report.nas = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f1_personas %>% as_survey_design(ids = "id_upm",
                                                     strata = "estrato",
                                                     weights = "fexp")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

# Para los resultados ponderados por diferentes desagregaciones reemplazar 
# respectivamente 

# Desnutricion cronica para menores de 2 anios de edad 
survey_design %>%
  srvyr_prop(dcronica_2)

survey_design %>%
  srvyr_prop_by(dcronica_2, area)

# Desnutricion global para menores de 2 anios de edad 
survey_design %>%
  srvyr_prop(dglobal_2)

survey_design %>%
  srvyr_prop_by(dglobal_2, area)

# Desnutricion aguda para menores de 2 anios de edad 
survey_design %>%
  srvyr_prop(daguda_2)

survey_design %>%
  srvyr_prop_by(daguda_2, area)

# Desnutricion cronica para menores de 5 anios de edad 
survey_design %>%
  srvyr_prop(dcronica)

survey_design %>%
  srvyr_prop_by(dcronica, area)

# Desnutricion global para menores de 5 anios de edad
survey_design %>%
  srvyr_prop(dglobal)

survey_design %>%
  srvyr_prop_by(dglobal, area)

# Desnutricion aguda para menores de 5 anios de edad 
survey_design %>%
  srvyr_prop(daguda)

survey_design %>%
  srvyr_prop_by(daguda, area)

# Desnutricion cronica en ninas/os de 2 a 5 anios
survey_design %>%
  srvyr_prop(dcronica2_5)

survey_design %>%
  srvyr_prop_by(dcronica2_5, area)

# Desnutricion global en ninas/os de 2 a 5 anios
survey_design %>%
  srvyr_prop(dglobal2_5)

survey_design %>%
  srvyr_prop_by(dglobal2_5, area)

# Desnutricion aguda en ninas/os de 2 a 5 anios
survey_design %>%
  srvyr_prop(daguda2_5)

survey_design %>%
  srvyr_prop_by(daguda2_5, area)

# Sobrepeso para menores de 5 anios de edad 
survey_design %>%
  srvyr_prop(sp5)

survey_design %>%
  srvyr_prop_by(sp5, area)

# Obesidad para menores de 5 anios de edad 
survey_design %>%
  srvyr_prop(ob5)

survey_design %>%
  srvyr_prop_by(ob5, area)

# Sobrepeso y obesidad para menores de 5 anios de edad 
survey_design %>%
  srvyr_prop(spob5)

survey_design %>%
  srvyr_prop_by(spob5, area)

# Malnutricion en para menores de 5 anios
survey_design %>%
  srvyr_prop(malnutricion)

survey_design %>%
  srvyr_prop_by(malnutricion, area)

# Doble carga de malnutricion para menores de 5 anios
survey_design %>%
  srvyr_prop(malnutricion_dc)

survey_design %>%
  srvyr_prop_by(malnutricion_dc, area)

#==============================================================================#
####                     Funciones para los tabulados                       ####
#==============================================================================#

tab_fun <- function(design, x, by_1, by_2, by_3, by_4, by_5, 
                    by_6, by_7, by_8, by_9) {  

  # Nacional 
  tab_nac <- design %>% srvyr_prop(.data[[x]])
  
  # Area 
  tab_area <- design %>% srvyr_prop_by(.data[[x]], {{ by_1 }}) 
  
  # Region
  tab_reg <- design %>% srvyr_prop_by(.data[[x]], {{ by_2}})
  
  # Provincia
  tab_prov <- design %>% srvyr_prop_by(.data[[x]], {{ by_3 }})
  
  # Parroquias priorizadas  
  tab_parr <- design %>% srvyr_prop_by(.data[[x]], {{ by_4 }}) 
  
  # Sexo
  tab_sex <- design %>% srvyr_prop_by(.data[[x]], {{ by_5 }})
  
  # Auto-Identificacion etnica
  tab_etnia <- design %>% srvyr_prop_by(.data[[x]], {{ by_6 }})
  
  # Quintiles  
  tab_quintil <- design %>% filter(!is.na({{ by_7}})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_7 }}) 
  
  # Pobreza por Ingresos  
  tab_pobreza <- design %>% filter(!is.na({{ by_8 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_8 }}) 
  
  # Pobreza por NBI  
  tab_pobreza_nbi <- design %>% filter(!is.na({{ by_9 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_9 }}) 
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac,tab_area, tab_reg, 
                         tab_prov, tab_parr, tab_sex, tab_etnia, tab_quintil, 
                         tab_pobreza, tab_pobreza_nbi)
  return(tab_final)
}

#==============================================================================#
####                 Obtencion de tablas en formato lista                   ####
#==============================================================================#

tab_full<- map(c("dcronica_2", "dglobal_2", "daguda_2", "dcronica", 
                 "dglobal", "daguda", "dcronica2_5", "dglobal2_5", 
                 "daguda2_5", "sp5", "ob5", "spob5", "malnutricion", 
                 "malnutricion_dc"), ~tab_fun(survey_design, .x, 
                                              area, region, prov, 
                                              parr_pri, f1_s1_2, etnia, 
                                              quintil, pobreza, nbi_1))

#==============================================================================#
####                     Exportacion de tabulados                           ####
#==============================================================================#

Style_tab(tab_full[[1]],  "T2_i1") # dcronica_2
Style_tab(tab_full[[2]],  "T2_i2") # dglobal_2
Style_tab(tab_full[[3]],  "T2_i3") # daguda_2
Style_tab(tab_full[[4]],  "T2_i4") # dcronica
Style_tab(tab_full[[5]],  "T2_i5") # dglobal
Style_tab(tab_full[[6]],  "T2_i6") # daguda
Style_tab(tab_full[[7]],  "T2_i7") # dcronica2_5
Style_tab(tab_full[[8]],  "T2_i8") # dglobal2_5
Style_tab(tab_full[[9]],  "T2_i9") # daguda2_5
Style_tab(tab_full[[10]],  "T2_i10") # sp5
Style_tab(tab_full[[11]],  "T2_i11") # ob5
Style_tab(tab_full[[12]],  "T2_i12") # spob5
Style_tab(tab_full[[13]],  "T2_i13") # malnutricion
Style_tab(tab_full[[14]],  "T2_i14") # malnutricion_dc

# si requiere importar las tablas por separado reemplazar la funcion "Style_tab"
# por "Style_tab_0"
