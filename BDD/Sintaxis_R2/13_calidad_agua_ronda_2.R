
# Titulo de la Sintaxis: 
# Calidad del Agua

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

# Base hogar 
df_f1_hogar <- readRDS(here(link_bdd, name_base_hogar))
df_f1_hogar <- as_tibble(df_f1_hogar)

# Base personas 
df_f1_personas <- readRDS(here(link_bdd, name_base_personas))
df_f1_personas <- as_tibble(df_f1_personas)

# Diccionario hogar
dicc_f1_hog <- import(paste0(link_bdd, name_diccionario), 
                      which = "f1_hogar",trust = TRUE)

dicc_f1_hog <- as_tibble(dicc_f1_hog)   

# Diccionario personas
dicc_f1_per <- import(paste0(link_bdd, name_diccionario), 
                      which = "f1_personas",trust = TRUE)

dicc_f1_per <- as_tibble(dicc_f1_per)   

#------------------------------------------------------------------------------#
# Join
#------------------------------------------------------------------------------#

# Variables de desagregacion
df_f1_personas <- df_f1_personas %>% 
  select(id_hogar, parr_pri, quintil, pobreza, nbi_1)

df_f1_personas <- df_f1_personas %>% 
  filter(!duplicated(id_hogar))

df_f1_hogar <- df_f1_hogar %>% 
  inner_join(df_f1_personas, by = "id_hogar")

rm(df_f1_personas)

#==============================================================================#
####              Calculo de indicadores sobre calidad del agua             ####
#==============================================================================#

# Prueba de cloro en el punto de consumo (vaso) -----------------------------#

# Seleccion pregunta 1 entre la 1 y 6
df_f1_hogar %>%
  freq(f1_s7_8, cumul = F, report.nas = F)

# Resultado de la prueba 
df_f1_hogar %>%
  descr(f1_s7_9, 
        stats = c("common"))

# Coloracion de la muestra 
df_f1_hogar %>%
  freq(f1_s7_9_a, cumul = F, report.nas = F)

# Validacion 
# Se eliminan valores 999 en el resultado de la prueba. Estos valores se registran 
# por motivos de problemas logisticos para realizar las pruebas 

df_f1_hogar <- df_f1_hogar %>%
  mutate(f1_s7_9_a = case_when(
    f1_s7_9 == 999 ~ NA,
    TRUE ~ f1_s7_9_a
  ))

df_f1_hogar %>%
  freq(f1_s7_9_a, cumul = F) 

df_f1_hogar <- df_f1_hogar %>% 
  mutate(f1_s7_9 = case_when(
    f1_s7_9 == 999 ~ NA_real_,
    TRUE ~ f1_s7_9 
  ))

df_f1_hogar %>%
  descr(f1_s7_9, 
        stats = c("common"))

# Clasificaion del cloro por su presencia (vaso)
df_f1_hogar <- df_f1_hogar %>% 
  mutate(cloro_v = case_when(
    f1_s7_8 == 1 & (f1_s7_9 == 0 | f1_s7_9_a == 2) ~ "Sin presencia",
    f1_s7_8 == 1 & (f1_s7_9 > 0 & f1_s7_9 < 0.3) & f1_s7_9_a == 1 ~ "Baja presencia",
    f1_s7_8 == 1 & (f1_s7_9 >= 0.3 & f1_s7_9 <= 1.5) & f1_s7_9_a == 1 ~ "Presencia ideal",
    f1_s7_8 == 1 & (f1_s7_9 > 1.5 & !is.na(f1_s7_9)) & f1_s7_9_a == 1 ~ "Presencia alta",
    TRUE ~ NA_character_ 
  ))

df_f1_hogar <- df_f1_hogar %>% 
  mutate(cloro_v = fct_relevel(cloro_v, c("Sin presencia", 
                                          "Baja presencia", 
                                          "Presencia ideal", 
                                          "Presencia alta")))

df_f1_hogar %>%
  freq(cloro_v, cumul = F, report.nas = F)

# Sin presencia de cloro en el punto de consumo (vaso) 
df_f1_hogar <- df_f1_hogar %>% 
  mutate(cloro_v_new = case_when(
    f1_s7_8 == 1 &
      ((f1_s7_9_a == 1 & (f1_s7_9 < 0.3 & !is.na(f1_s7_9))) |
         f1_s7_9_a == 2) ~ 1,
    f1_s7_8 == 1 &
      f1_s7_9_a == 1 & (f1_s7_9 >= 0.3 & !is.na(f1_s7_9)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f1_hogar %>%
  freq(cloro_v_new, cumul = F, report.nas = F)

# Prueba de cloro en la fuente ------------------------------------------------#

# Seleccion pregunta 1 entre la 1 y 6
df_f1_hogar %>%
  freq(f1_s7_12, cumul = F, report.nas = F)

# Resultado de la prueba 
df_f1_hogar %>%
  descr(f1_s7_13, 
        stats = c("common"))

# Coloracion de la muestra 
df_f1_hogar %>%
  freq(f1_s7_13_a, cumul = F, report.nas = F)

# Validacion 
# Se eliminan valores 999 en el resultado de la prueba. Estos valores se registran 
# por motivos de problemas logisticos para realizar las pruebas 

df_f1_hogar <- df_f1_hogar %>%
  mutate(f1_s7_13_a = case_when(
    f1_s7_13 == 999 ~ NA,
    TRUE ~ f1_s7_13_a
  ))

df_f1_hogar %>%
  freq(f1_s7_13_a, cumul = F) 

df_f1_hogar <- df_f1_hogar %>% 
  mutate(f1_s7_13 = case_when(
    f1_s7_13 == 999 ~ NA_real_,
    TRUE ~ f1_s7_13 
  ))

df_f1_hogar %>%
  descr(f1_s7_13, 
        stats = c("common"))

# Clasificaion del cloro por su presencia (fuente)
df_f1_hogar <- df_f1_hogar %>% 
  mutate(cloro_f = case_when(
    f1_s7_12 == 1 & (f1_s7_13 == 0 | f1_s7_13_a == 2) ~ "Sin presencia",
    f1_s7_12 == 1 & (f1_s7_13 > 0 & f1_s7_13 < 0.3) & f1_s7_13_a == 1 ~ "Baja presencia",
    f1_s7_12 == 1 & (f1_s7_13 >= 0.3 & f1_s7_13 <= 1.5) & f1_s7_13_a == 1 ~ "Presencia ideal",
    f1_s7_12 == 1 & (f1_s7_13 > 1.5 & !is.na(f1_s7_13)) & f1_s7_13_a == 1 ~ "Presencia alta",
    TRUE ~ NA_character_ 
  ))

df_f1_hogar <- df_f1_hogar %>% 
  mutate(cloro_f = fct_relevel(cloro_f, c("Sin presencia", 
                                          "Baja presencia", 
                                          "Presencia ideal", 
                                          "Presencia alta")))

df_f1_hogar %>%
  freq(cloro_f, cumul = F, report.nas = F)

# Sin presencia de cloro en la fuente 
df_f1_hogar <- df_f1_hogar %>% 
  mutate(cloro_f_new = case_when(
    f1_s7_12 == 1 &
      ((f1_s7_13_a == 1 & (f1_s7_13 < 0.3 & !is.na(f1_s7_13))) |
         f1_s7_13_a == 2) ~ 1,
    f1_s7_12 == 1 &
      f1_s7_13_a == 1 & (f1_s7_13 >= 0.3 & !is.na(f1_s7_13)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f1_hogar %>%
  freq(cloro_f_new, cumul = F, report.nas = F)

# Prueba de E.coli en el punto de consumo (vaso) ------------------------------#

# Obtencion de muestra   
df_f1_hogar %>%
  freq(f1_s7_10, cumul = F, report.nas = F)

# Total de hora de incubacion 
df_f1_hogar %>%
  descr(f1_s7_14_tothoras, 
        stats = c("common"))

# Visualizacion de resultados 
df_f1_hogar %>%
  freq(f1_s7_15, cumul = F, report.nas = F)

# Conteo de colonias 
df_f1_hogar %>%
  descr(f1_s7_16, 
        stats = c("common"))

# Validacion de la prueba 
df_f1_hogar <- df_f1_hogar %>% 
  mutate(validtest_v = case_when(
    (f1_s7_10 == 1 | f1_s7_10 == 3) & 
      (f1_s7_14_tothoras >= 24 & f1_s7_14_tothoras <= 48) & 
      f1_s7_15 == 1 ~ 1,
    (f1_s7_10 == 2 | f1_s7_10 == 4) | 
      f1_s7_15 == 2 ~ 0,
    TRUE ~ NA_real_
  )) 

df_f1_hogar %>%
  freq(validtest_v, cumul = F, report.nas = F)

# Validacion
# Se eliminan valores 999 en el resultado de la prueba. Estos valores se registran 
# por motivos de problemas logisticos para realizar las pruebas 

df_f1_hogar <- df_f1_hogar %>%
  mutate(validtest_v = case_when(
    f1_s7_16 == 999 ~ NA_real_,
    TRUE ~ validtest_v
  ))

df_f1_hogar %>%
  freq(validtest_v, cumul = F) 

df_f1_hogar <- df_f1_hogar %>% 
  mutate(f1_s7_16 = case_when(
    f1_s7_16 == 999 ~ NA_real_,
    TRUE ~ f1_s7_16 
  ))

df_f1_hogar %>%
  descr(f1_s7_16, 
        stats = c("common"))

# Clasificacion de E.coli por su presencia (vaso)
df_f1_hogar <- df_f1_hogar %>% 
  mutate(ecoli_v = case_when(
    validtest_v == 1 & (f1_s7_16 > 100 & !is.na(f1_s7_16)) ~ "Riesgo muy alto", 
    validtest_v == 1 & (f1_s7_16 >= 11 & f1_s7_16 <= 100) ~ "Riesgo alto",   
    validtest_v == 1 & (f1_s7_16 >= 1 & f1_s7_16 <= 10) ~ "Riesgo medio",                    
    validtest_v == 1 & f1_s7_16 == 0 ~ "Riesgo bajo",
    TRUE ~ NA_character_
  ))

df_f1_hogar <- df_f1_hogar %>% 
  mutate(ecoli_v = fct_relevel(ecoli_v, c("Riesgo muy alto", 
                                          "Riesgo alto", 
                                          "Riesgo medio", 
                                          "Riesgo bajo")))

df_f1_hogar %>%
  freq(ecoli_v, cumul = F, report.nas = F)

# Presencia de E.coli en el punto de consumo (vaso) 
df_f1_hogar <- df_f1_hogar %>% 
  mutate(ecoli_v_new = case_when(
    validtest_v == 1 & (f1_s7_16 >= 1 & !is.na(f1_s7_16)) ~ 1, 
    validtest_v == 1 & f1_s7_16 == 0 ~ 0,
    TRUE ~ NA_real_
  ))

df_f1_hogar %>%
  freq(ecoli_v_new, cumul = F, report.nas = F)

# Prueba de E.Coli en la fuente -----------------------------------------------#

# Obtencion de muestra   
df_f1_hogar %>%
  freq(f1_s7_10, cumul = F, report.nas = F)

# Total de hora de incubacion 
df_f1_hogar %>%
  descr(f1_s7_19_tothoras, 
        stats = c("common"))

# Visualizacion de resultados 
df_f1_hogar %>%
  freq(f1_s7_20, cumul = F, report.nas = F)

# Conteo de colonias 
df_f1_hogar %>%
  descr(f1_s7_21, 
        stats = c("common"))

# Validacion de la prueba 
df_f1_hogar <- df_f1_hogar %>% 
  mutate(validtest_f = case_when(
    (f1_s7_10 == 2 | f1_s7_10 == 3) & 
      ((f1_s7_19_tothoras >= 24 & f1_s7_19_tothoras <= 48) & 
         f1_s7_20 == 1) ~ 1,
    (f1_s7_10 == 1 | f1_s7_10 == 4) |
      f1_s7_20 == 2 ~ 0,
    TRUE ~ NA_real_
  )) 

df_f1_hogar %>%
  freq(validtest_f, cumul = F, report.nas = F)

# Validacion
# Se eliminan valores 999 en el resultado de la prueba. Estos valores se registran 
# por motivos de problemas logisticos para realizar las pruebas 

df_f1_hogar <- df_f1_hogar %>%
  mutate(validtest_f = case_when(
    f1_s7_21 == 999 ~ NA_real_,
    TRUE ~ validtest_f
  ))

df_f1_hogar %>%
  freq(validtest_f, cumul = F) 

df_f1_hogar <- df_f1_hogar %>% 
  mutate(f1_s7_21 = case_when(
    f1_s7_21 == 999 ~ NA_real_,
    TRUE ~ f1_s7_21 
  ))

df_f1_hogar %>%
  descr(f1_s7_21, 
        stats = c("common"))

# Clasificacion de E.coli por su presencia (fuente)
df_f1_hogar <- df_f1_hogar %>% 
  mutate(ecoli_f = case_when(
    validtest_f == 1 & (f1_s7_21 > 100 & !is.na(f1_s7_21)) ~ "Riesgo muy alto", 
    validtest_f == 1 & (f1_s7_21 >= 11 & f1_s7_21 <= 100) ~ "Riesgo alto",   
    validtest_f == 1 & (f1_s7_21 >= 1 & f1_s7_21 <= 10) ~ "Riesgo medio",                    
    validtest_f == 1 & f1_s7_21 == 0 ~ "Riesgo bajo",
    TRUE ~ NA_character_
  ))

df_f1_hogar <- df_f1_hogar %>% 
  mutate(ecoli_f = fct_relevel(ecoli_f, c("Riesgo muy alto", 
                                          "Riesgo alto", 
                                          "Riesgo medio", 
                                          "Riesgo bajo")))

df_f1_hogar %>%
  freq(ecoli_f, cumul = F, report.nas = F)

# Presencia de E.coli en la fuente 
df_f1_hogar <- df_f1_hogar %>% 
  mutate(ecoli_f_new = case_when(
    validtest_f == 1 & (f1_s7_21 >= 1 & !is.na(f1_s7_21)) ~ 1, 
    validtest_f == 1 & f1_s7_21 == 0 ~ 0,
    TRUE ~ NA_real_
  )) 

df_f1_hogar %>%
  freq(ecoli_f_new, cumul = F, report.nas = F)

#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores 
# Area
df_f1_hogar <- df_f1_hogar %>%
  mutate(area = as_label(area)) 

df_f1_hogar %>%
  freq(area, cumul = F, report.nas = F)

# Region 
df_f1_hogar <- df_f1_hogar %>%
  mutate(region = as_label(region)) 

df_f1_hogar %>%
  freq(region, cumul = F, report.nas = F)

# Provincia 
df_f1_hogar <- df_f1_hogar %>%
  mutate(prov = as_label(prov)) 

df_f1_hogar %>%
  freq(prov, cumul = F, report.nas = F)

# Parroquias priorizadas
df_f1_hogar <- df_f1_hogar %>%
  mutate(parr_pri = as_label(parr_pri)) 

df_f1_hogar %>%
  freq(parr_pri, cumul = F, report.nas = F)

# Quintiles
df_f1_hogar <- df_f1_hogar %>%
  mutate(quintil = as_label(quintil)) 

df_f1_hogar %>%
  freq(quintil, cumul = F, report.nas = F)

# Pobreza por ingresos
df_f1_hogar <- df_f1_hogar %>%
  mutate(pobreza = as_label(pobreza)) 

df_f1_hogar %>%
  freq(pobreza, cumul = F, report.nas = F)

# Pobreza por necesidades basicas insatisfechas
df_f1_hogar <- df_f1_hogar %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f1_hogar %>%
  freq(nbi_1, cumul = F, report.nas = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f1_hogar %>% as_survey_design(ids = "id_upm",
                                                  strata = "estrato",
                                                  weights = "fexp")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

# Cloro
# Punto de consumo 
survey_design %>%
  srvyr_freq(cloro_v)

survey_design %>%
  srvyr_freq_by(cloro_v, area) 

survey_design %>%
  srvyr_prop(cloro_v_new)

survey_design %>%
  srvyr_prop_by(cloro_v_new, area) 

# Fuente 
survey_design %>%
  srvyr_freq(cloro_f)

survey_design %>%
  srvyr_freq_by(cloro_f, area) 

survey_design %>%
  srvyr_prop(cloro_f_new)

survey_design %>%
  srvyr_prop_by(cloro_f_new, area) 

# E.coli
# Punto de consumo 
survey_design %>%
  srvyr_freq(ecoli_v)

survey_design %>%
  srvyr_freq_by(ecoli_v, area)

survey_design %>%
  srvyr_prop(ecoli_v_new)

survey_design %>%
  srvyr_prop_by(ecoli_v_new, area)

# Fuente
survey_design %>%
  srvyr_freq(ecoli_f)

survey_design %>%
  srvyr_freq_by(ecoli_f, area)

survey_design %>%
  srvyr_prop(ecoli_f_new)

survey_design %>%
  srvyr_prop_by(ecoli_f_new, area)

#==============================================================================#
####                     Funciones para los tabulados                       ####
#==============================================================================#

# Para resultados en clasificacion 
tab_fun <- function(design, x, by_1) {  
  
  # Nacional 
  tab_nac <- design %>% 
    srvyr_freq(.data[[x]])
  
  # Area 
  tab_area <- design %>% 
    srvyr_freq_by(.data[[x]], {{ by_1 }}) 
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac, tab_area)
}


tab_fun_1 <- function(design, x, by_1, by_2, by_3, by_4, by_5, by_6, by_7) {  
  
  # Nacional 
  tab_nac <- design %>% 
    srvyr_prop(.data[[x]])
  
  # Area 
  tab_area <- design %>% 
    srvyr_prop_by(.data[[x]], {{ by_1 }}) 
  
  # Region
  tab_reg <- design %>%
    srvyr_prop_by(.data[[x]], {{ by_2}})
  
  # Provincia
  tab_prov <- design %>%
    srvyr_prop_by(.data[[x]], {{ by_3 }})
  
  # Parroquias priorizadas  
  tab_parr <- design %>%
    filter(!is.na({{ by_4 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_4 }}) 
  
  # Quintiles
  tab_quintil <- design %>%
    filter(!is.na({{ by_5 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_5 }})
  
  # Pobreza por Ingresos  
  tab_pobreza <- design %>%
    filter(!is.na({{ by_6 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_6 }}) 
  
  # Pobreza por NBI  
  tab_pobreza_nbi <- design %>%
    filter(!is.na({{ by_7 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_7 }}) 
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac, 
                         tab_area, tab_reg,
                         tab_prov, tab_parr,
                         tab_quintil, tab_pobreza,
                         tab_pobreza_nbi)
  
  return(tab_final)
}

#==============================================================================#
####                 Obtencion de tablas en formato lista                   ####
#==============================================================================#

tab_full <- map(c("cloro_f_new","cloro_v_new","ecoli_f_new","ecoli_v_new"),
                ~tab_fun_1(.x, design = survey_design, 
                           area, region, prov, parr_pri, 
                           quintil, pobreza, nbi_1)) 

tab_full_1 <- map(c("cloro_f","cloro_v","ecoli_f","ecoli_v"),
                  ~tab_fun(.x, design = survey_design,
                           area))

#==============================================================================#
####                     Exportacion de tabulados                           ####
#==============================================================================#

concat_a <- function(tabla_lista) {
  tab_concat <- tabla_lista %>%
    mutate(Desag = ifelse(is.na(area),
                          paste0("", .[[1]]), 
                          paste0(area, ": ", .[[1]])))
  
  tab_concat <- tab_concat %>%
    select("Desag","Props","EE","LI","LS","CV","Num","Deno","deff","NEFE","RIC",
           "RCRE","GL","Valid")
  
  return(tab_concat)
}

#==============================================================================#
####                     Exportacion de tabulados                           ####
#==============================================================================#

#  concatenar y arreglar la base
tabla <- list()

for (i in 1:length(tab_full_1)) {
  tabla[[i]] <- concat_a(tab_full_1[[i]])
}

Style_tab(tab_full[[1]], "T9_i1") # cloro_f_new
Style_tab(tab_full[[2]], "T9_i2") # cloro_v_new
Style_tab(tab_full[[3]], "T9_i3") # ecoli_f_new
Style_tab(tab_full[[4]], "T9_i4") # ecoli_v_new
Style_tab(tabla[[1]], "T9_i5") # cloro_f
Style_tab(tabla[[2]], "T9_i6") # cloro_v
Style_tab(tabla[[3]], "T9_i7") # ecoli_f
Style_tab(tabla[[4]], "T9_i8") # ecoli_v

# si requiere importar las tablas por separado reemplazar la funcion "Style_tab"
# por "Style_tab_0"

