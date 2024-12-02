
# Titulo de la Sintaxis: 
# Desarrollo Infantil - Lenguaje metodologia MacArthur - Bates

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
# Software: R 4.3.2

#------------------------------------------------------------------------------#
# >>>>                          Importante                               <<<<  #
#                                                                              #
#       Primero realizar la accion descrita en el documento "00.master.R"      #
#                                                                              #
#------------------------------------------------------------------------------#

variables_eliminar <- setdiff(ls(all = TRUE), variables_mantener)
rm(list = variables_eliminar)

#==============================================================================#
####                          Carga de base de datos                        ####
#==============================================================================#

# Base personas 
df_f1_personas <- readRDS(here(link_bdd, name_base_personas))
df_f1_personas <- as_tibble(df_f1_personas)
df_f1_personas

# Base Desarrollo Infantil 
df_f3 <- readRDS(here(link_bdd, name_base_di))
df_f3 <- as_tibble(df_f3)
df_f3

# Diccionario
# Personas 
dicc_f1_per <- import(paste0(link_bdd, name_diccionario), 
                      which = "f1_personas",trust = TRUE)

dicc_f1_per <- as_tibble(dicc_f1_per)  

# Desarrollo Infantil 
dicc_f3 <- import(paste0(link_bdd, name_diccionario_di), 
                  which = "f3_desarrollo_infantil",trust = TRUE)

dicc_f3 <- as_tibble(dicc_f3)  

#------------------------------------------------------------------------------#
# Join 
#------------------------------------------------------------------------------#

# Creacion de identificador de madre para cada hijo 
df_f1_personas %>% 
  select(f1_s1_12, f1_s1_12_1)

df_f1_personas <- df_f1_personas %>% 
  mutate(f1_s1_12_1 = as.character(f1_s1_12_1)) %>% 
  mutate(f1_s1_12_1 = case_when(
    nchar(f1_s1_12_1) == 1 ~ paste0(0, f1_s1_12_1),
    TRUE ~ f1_s1_12_1
  ))

df_f1_personas <- df_f1_personas %>% 
  mutate(id_madre = case_when(
    !is.na(f1_s1_12_1) ~ paste0(id_hogar, f1_s1_12_1),
    TRUE ~ NA
  ))

df_f1_personas %>% 
  select(id_per, f1_s1_12, f1_s1_12_1, id_madre)

# Nivel de instruccion 
df_f1_personas <- df_f1_personas %>% 
  mutate(nivins = case_when(
    (f1_s1_15_1 >= 1 & f1_s1_15_1 <= 4) ~ 1, 
    (f1_s1_15_1 == 5 | f1_s1_15_1 == 6 | 
       (f1_s1_15_1 == 7 & f1_s1_15_2 < 4 & !is.na(f1_s1_15_2))) ~ 1,
    ((f1_s1_15_1 == 7 & f1_s1_15_2 > 3 & !is.na(f1_s1_15_2)) | 
       f1_s1_15_1 == 8) ~ 2,
    (f1_s1_15_1 >= 9 & f1_s1_15_1 <= 13) ~ 3,
    TRUE ~ NA_real_
  ))

df_f1_personas = apply_labels(df_f1_personas,
                              nivins = num_lab(
                                "1 Ninguno/Educación Básica 
                                 2 Educación Media/Bachillerato
                                 3 Superior" 
                              ))
df_f1_personas %>% 
  freq(nivins, cumul = F)

# Variables de desagregacion   
df_f1_madre <- df_f1_personas %>% 
  select(id_per, nivins) %>% 
  rename(id_madre = id_per,
         nivins_madre = nivins)

# Variables necesarias para desagracion del ninio 
df_f1_personas <- df_f1_personas %>% 
  select(id_per, id_madre, f1_s1_2, f1_s1_11, f1_s1_12,
         etnia, quintil, pobreza, nbi_1, 
         dcronica)

# Join 
df_f1_personas_new <- df_f1_personas %>% 
  left_join(df_f1_madre, by = c("id_madre"))

df_f1_personas_new %>% 
  freq(nivins_madre, cumul = F, report.nas = F)

# Join 
df_f3 <- df_f3 %>% 
  inner_join(df_f1_personas_new, by = c("id_per"))

rm(df_f1_personas, df_f1_madre, df_f1_personas_new)

#==============================================================================#
####                         Calculo de indicadores                         ####
#==============================================================================#

#------------------------------------------------------------------------------#
# Edad en dias 
#------------------------------------------------------------------------------#

df_f3 <- df_f3 %>% 
  mutate(dob = paste(f3_s0_1b_anio, f3_s0_1b_mes, f3_s0_1b_dia)) %>%
  mutate(dov = paste(fecha_anio, fecha_mes, fecha_dia)) %>%
  mutate(dob = as_date(dob)) %>%   
  mutate(dov = as_date(dov)) %>%  
  mutate(edaddias_nin = (dob %--% dov) / days(1)) %>% 
  mutate(edadmeses_nin = trunc((dob %--% dov) / months(1))) 

df_f3 %>% 
  descr(edadmeses_nin,
        c("common"))

#------------------------------------------------------------------------------#
# Promedio de palabras que entienden los ninios de 12 a 18 meses
#------------------------------------------------------------------------------#

for (i in 301:350) {
  entiende <- paste0("entiende1_", i)
  df_f3 <- df_f3 %>% 
    mutate(!!entiende := case_when(
      (!!sym(paste0("f3_s3_", i)) == 1 |
         !!sym(paste0("f3_s3_", i)) == 2) ~ 1,
      !!sym(paste0("f3_s3_", i)) == 3 ~ 0,
      !!sym(paste0("f3_s3_", i)) == 88 ~ NA_real_
    ))
}

# Respuestas en missing (NS/NR) para prueba McArthur 12-18 meses - entiende

df_f3 <- df_f3 %>%
  mutate(missing_12_18_entiende = case_when(
    f3_s3_300 >= 1 & f3_s3_300 <= 3 ~ 
      rowSums(is.na(select(., starts_with("entiende1_")))),
    TRUE ~ NA_real_
  )) 

df_f3 %>% 
  freq(missing_12_18_entiende, cumul = F, report.nas = F)

# Promedio de palabras que entienden los ninios de 12 a 18  meses

df_f3 <- df_f3 %>% 
  mutate(num_entiende_1 = case_when(
    f3_s3_300 >= 1 & f3_s3_300 <= 3 ~
      rowSums(select(., starts_with("entiende1_")), na.rm = T),
    TRUE ~ NA_real_
  ))

df_f3 %>%
  descr(num_entiende_1,
        c("common"))

df_f3 <- df_f3 %>% 
  mutate(prom_num_entiende_1 = num_entiende_1,
         prom_num_entiende_1 = case_when(
           missing_12_18_entiende >= 10 ~ NA_real_, 
           TRUE ~ prom_num_entiende_1
         )) 

df_f3 %>%
  descr(prom_num_entiende_1,
        c("common"))

#------------------------------------------------------------------------------#
# Promedio de palabras que dicen los ninios de 12 a 18  meses
#------------------------------------------------------------------------------#

for (i in 301:350) {
  dice <- paste0("dice1_", i)
  df_f3 <- df_f3 %>% 
    mutate(!!dice := case_when(
      !!sym(paste0("f3_s3_", i)) == 1 ~ 1,
      (!!sym(paste0("f3_s3_", i)) == 2 | 
         !!sym(paste0("f3_s3_", i)) == 3) ~ 0,
      !!sym(paste0("f3_s3_", i)) == 88 ~ NA_real_
    ))
}

# Respuestas en missing (NS/NR) para prueba McArthur 12-18 meses - dice

df_f3 <- df_f3 %>%
  mutate(missing_12_18 = case_when(
    f3_s3_300 >= 1 & f3_s3_300 <= 3 ~ 
      rowSums(is.na(select(., starts_with("dice1_")))),
    TRUE ~ NA_real_
  ))  

df_f3 %>% 
  freq(missing_12_18, cumul = F, report.nas = F)

# Promedio de palabras que dicen los ninios de 12 a 18 meses

df_f3 <- df_f3 %>% 
  mutate(num_dice_1 = case_when(
    f3_s3_300 >= 1 & f3_s3_300 <= 3 ~
      rowSums(select(., starts_with("dice1_")), na.rm = T),
    TRUE ~ NA_real_
  ))

df_f3 %>% 
  descr(num_dice_1, 
        c("common"))

df_f3 <- df_f3 %>% 
  mutate(prom_num_dice_1 = num_dice_1,
         prom_num_dice_1 = case_when(
           missing_12_18 >= 10 ~ NA_real_, 
           TRUE ~ prom_num_dice_1
         )) 

df_f3 %>% 
  descr(prom_num_dice_1, 
        c("common"))

#------------------------------------------------------------------------------#
# Promedio de palabras que dicen los ninios de 19 a 30 meses
#------------------------------------------------------------------------------#

for (i in 401:450) {
  dice <- paste0("dice2_", i)
  df_f3 <- df_f3 %>% 
    mutate(!!dice := case_when(
      !!sym(paste0("f3_s4_", i)) == 1 ~ 1,
      !!sym(paste0("f3_s4_", i)) == 2 ~ 0,
      !!sym(paste0("f3_s4_", i)) == 88 ~ NA_real_
    ))
}

# Respuestas en missing (NS/NR) para prueba McArthur 19-30 meses - dice 

df_f3 <- df_f3 %>%
  mutate(missing_19_30 = case_when(
    f3_s4_400 >= 1 & f3_s4_400 <= 3 ~ 
      rowSums(is.na(select(., starts_with("dice2_")))),
    TRUE ~ NA_real_
  )) 

df_f3 %>% 
  freq(missing_19_30, cumul = F, report.nas = F)

# Promedio de palabras que dicen los ninios de 19 a 30 meses

df_f3 <- df_f3 %>% 
  mutate(num_dice_2 = case_when(
    f3_s4_400 >= 1 & f3_s4_400 <= 3 ~
      rowSums(select(., starts_with("dice2_")), na.rm = T),
    TRUE ~ NA_real_
  ))

df_f3 %>%
  descr(num_dice_2, 
        c("common"))

df_f3 <- df_f3 %>% 
  mutate(prom_num_dice_2 = num_dice_2,
         prom_num_dice_2 = case_when(
           missing_19_30 >= 10 ~ NA_real_, 
           TRUE ~ prom_num_dice_2
         )) 

df_f3 %>%
  descr(prom_num_dice_2, 
        c("common"))

#------------------------------------------------------------------------------#
# Promedio de palabras que dicen los ninios de 31 a 42 meses
#------------------------------------------------------------------------------#

for (i in 501:550) {
  dice <- paste0("dice3_", i)
  df_f3 <- df_f3 %>% 
    mutate(!!dice := case_when(
      !!sym(paste0("f3_s5_", i)) == 1 ~ 1,
      !!sym(paste0("f3_s5_", i)) == 2 ~ 0,
      !!sym(paste0("f3_s5_", i)) == 88 ~ NA_real_
    ))
}

# Respuestas en missing (NS/NR) para prueba McArthur 31-42 meses - dice

df_f3 <- df_f3 %>%
  mutate(missing_31_42 = case_when(
    f3_s5_500 >= 1 & f3_s5_500 <= 3 ~ 
      rowSums(is.na(select(., starts_with("dice3_")))),
    TRUE ~ NA_real_
  )) 

df_f3 %>% 
  freq(missing_31_42, cumul = F)

# Promedio de palabras que dicen los ninios de 31 a 42 meses

df_f3 <- df_f3 %>% 
  mutate(num_dice_3 = case_when(
    f3_s5_500 >= 1 & f3_s5_500 <= 3 ~
      rowSums(select(., starts_with("dice3_")), na.rm = T),
    TRUE ~ NA_real_
  ))

df_f3 %>%
  descr(num_dice_3, 
        c("common"))

df_f3 <- df_f3 %>% 
  mutate(prom_num_dice_3 = num_dice_3,
         prom_num_dice_3 = case_when(
           missing_31_42 >= 10 ~ NA_real_, 
           TRUE ~ prom_num_dice_3
         )) 

df_f3 %>%
  descr(prom_num_dice_3, 
        c("common"))

#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores 
# Area
df_f3 <- df_f3 %>%
  mutate(area = as_label(area)) 

df_f3 %>%
  freq(area, cumul = F)

# Region 
df_f3 <- df_f3 %>%
  mutate(region = as_label(region)) 

df_f3 %>%
  freq(region, cumul = F)

# Sexo 
df_f3 <- df_f3 %>%
  mutate(f1_s1_2 = as_label(f1_s1_2)) 

df_f3 %>%
  freq(f1_s1_2, cumul = F)

# Auto-Identificacion etnica
df_f3 <- df_f3 %>%
  mutate(etnia = as_label(etnia)) 

df_f3 %>%
  freq(etnia, cumul = F)

# Quintiles 
df_f3 <- df_f3 %>%
  mutate(quintil = as_label(quintil)) 

df_f3 %>%
  freq(quintil, cumul = F)

# Pobreza por ingreso 
df_f3 <- df_f3 %>%
  mutate(pobreza = as_label(pobreza)) 

df_f3 %>%
  freq(pobreza, cumul = F)

# NBI 
df_f3 <- df_f3 %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f3 %>%
  freq(nbi_1, cumul = F)

# Nivel de instruccion de la madre
df_f3 <- df_f3 %>%
  mutate(nivins_madre = as_label(nivins_madre)) 

df_f3 %>%
  freq(nivins_madre, cumul = F)

# Desnutricion cronica 
df_f3 = apply_labels(df_f3,
                     dcronica = num_lab(
                       "0 Sin desnutrición crónica
                             1 Con desnutrición crónica"
                     ))

df_f3 <- df_f3 %>%
  mutate(dcronica = as_label(dcronica)) 

df_f3 %>%
  freq(dcronica, cumul = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f3 %>% as_survey_design(ids = "id_upm",
                                            strata = "estrato",
                                            weights = "fexp_di")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

# Promedio de palabras que entienden los ninios de 12 a 18 meses
survey_design %>% 
  srvyr_mean(prom_num_entiende_1)

survey_design %>% 
  srvyr_mean_by(prom_num_entiende_1, area)

# Promedio de palabras que dicen los ninios de 12 a 18  meses
survey_design %>% 
  srvyr_mean(prom_num_dice_1)

survey_design %>% 
  srvyr_mean_by(prom_num_dice_1, area)

# Promedio de palabras que dicen los ninios de 19 a 30 meses
survey_design %>% 
  srvyr_mean(prom_num_dice_2)

survey_design %>% 
  srvyr_mean_by(prom_num_dice_2, area)

# Promedio de palabras que dicen los ninios de 31 a 42 meses
survey_design %>% 
  srvyr_mean(prom_num_dice_3)

survey_design %>% 
  srvyr_mean_by(prom_num_dice_3, area)

#==============================================================================#
####                                Funciones                               ####
#==============================================================================#

# Funcion para agregar tablas descriptivas dependiendo los diferentes tipos de 
# desagregacion  

# Para resultados en promedio
tab_fun <- function(design, x, by_1, by_2, by_3, by_4, by_5, 
                    by_6, by_7, by_8, by_9) {  
  
  # Nacional 
  tab_nac <- design %>% 
    srvyr_mean(.data[[x]])
  
  # Area 
  tab_area <- design %>% 
    srvyr_mean_by(.data[[x]], {{ by_1 }}) 
  
  # Region
  tab_reg <- design %>%
    srvyr_mean_by(.data[[x]], {{ by_2 }})
  
  # Sexo
  tab_sex <- design %>%
    srvyr_mean_by(.data[[x]], {{ by_3 }})
  
  # Auto-Identificacion etnica
  tab_etnia <- design %>%
    filter(!is.na({{ by_4 }})) %>% 
    srvyr_mean_by(.data[[x]], {{ by_4 }})
  
  # Quintiles
  tab_quintil <- design %>%
    filter(!is.na({{ by_5 }})) %>% 
    srvyr_mean_by(.data[[x]], {{ by_5 }})
  
  # Pobreza por Ingresos  
  tab_pobreza <- design %>%
    filter(!is.na({{ by_6 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_6 }}) 
  
  # Pobreza por NBI  
  tab_pobreza_nbi <- design %>%
    filter(!is.na({{ by_7 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_7 }}) 
  
  # Nivel de instruccion de la MEF  
  tab_nivins_madre_nin <- design %>%
    filter(!is.na({{ by_8 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_8 }}) 
  
  # Desnutricion cronica  
  tab_dcronica <- design %>%
    filter(!is.na({{ by_9 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_9 }}) 
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac, tab_area, 
                         tab_reg, tab_sex, 
                         tab_etnia, tab_quintil, 
                         tab_pobreza, tab_pobreza_nbi,
                         tab_nivins_madre_nin, tab_dcronica)
}

tab_full <- map(c("prom_num_entiende_1", "prom_num_dice_1", "prom_num_dice_2",
                  "prom_num_dice_3"),
                ~tab_fun(.x, design = survey_design,
                         area, region, f1_s1_2, etnia, 
                         quintil, pobreza, nbi_1, nivins_madre, dcronica))

#==============================================================================#
####               Extraccion de elementos de objetos listas                #### 
#==============================================================================#

Style_tab(tab_full[[1]],"T10_i13")#tab_prom_num_entiende_1
Style_tab(tab_full[[2]],"T10_i14")#tab_prom_num_dice_1
Style_tab(tab_full[[3]],"T10_i15")#tab_prom_num_dice_1
Style_tab(tab_full[[4]],"T10_i16")#tab_prom_num_dice_1

