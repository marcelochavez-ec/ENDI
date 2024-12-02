
# Titulo de la Sintaxis: 
# Desarrollo Infantil - Depresion materna

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
# Depresion materna
#------------------------------------------------------------------------------#

for (i in letters[1:8]) {
  
  phq8 <- paste0("phq8_", i)
  
  df_f3 <- df_f3 %>% 
    mutate(!!sym(paste0("phq8_", i)) := case_when(
      !!sym(paste0("f3_s7_700_", i)) == 1 ~ 0,
      !!sym(paste0("f3_s7_700_", i)) == 2 ~ 1,
      !!sym(paste0("f3_s7_700_", i)) == 3 ~ 2,
      !!sym(paste0("f3_s7_700_", i)) == 4 ~ 3,
      !!sym(paste0("f3_s7_700_", i)) == 5 ~ NA))
  
}

# Consistencia interna de los items

df_f3_test <- df_f3 %>%
  select(starts_with("phq8_"))

alpha(df_f3_test)

rm(df_f3_test)

# Sumar la totalidad de los items 

df_f3 <- df_f3 %>%
  rowwise() %>% 
  mutate(phq8_score = sum(c(phq8_a, phq8_b, phq8_c, phq8_d, 
                            phq8_e, phq8_f, phq8_g, phq8_h), na.rm = T)) %>% 
  ungroup()

# Contar el numero de valores missings : si hay mas de uno lo elimino

df_f3 <- df_f3 %>%
  rowwise() %>% 
  mutate(phq8miss = sum(is.na(c_across(phq8_a:phq8_h)))) %>% 
  ungroup()

df_f3 <- df_f3 %>% 
  mutate(phq8_score = case_when(
    phq8miss > 1 & !is.na(phq8miss) ~ NA_real_,
    T ~ phq8_score))

df_f3 %>% 
  descr(phq8_score,
        c("common"))

# Depresion 

df_f3 <- df_f3 %>% 
  mutate(depresion = case_when(
    phq8_score >= 10 & !is.na(phq8_score) ~ 1,
    phq8_score < 10 & !is.na(phq8_score) ~ 0,
    T ~ NA_real_
  ))

df_f3 %>% 
  freq(depresion, cumul = F, report.nas = F)

# Se define el indicador solamente para las madres

df_f3 %>% 
  freq(relacion_cuidadora, cumul = F)

# Depresion materna  

df_f3 <- df_f3 %>% 
  mutate(depresion_m = case_when(
    relacion_cuidadora == 2 ~ depresion, 
    TRUE ~ NA
  ))


df_f3 %>% 
  freq(depresion_m, cumul = F, report.nas = F)

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

# Depresion
survey_design %>%
  srvyr_prop(depresion_m)

survey_design %>%
  srvyr_prop_by(depresion_m, area)

#==============================================================================#
####                                Funciones                               ####
#==============================================================================#

# Funcion para agregar tablas descriptivas dependiendo los diferentes tipos de 
# desagregacion  

# Para resultados en proporcion
tab_fun <- function(design, x, by_1, by_2, by_3, by_4, by_5, 
                    by_6, by_7, by_8) {  
  
  # Nacional 
  tab_nac <- design %>% 
    srvyr_prop(.data[[x]])
  
  # Area 
  tab_area <- design %>% 
    srvyr_prop_by(.data[[x]], {{ by_1 }}) 
  
  # Region
  tab_reg <- design %>%
    srvyr_prop_by(.data[[x]], {{ by_2 }})
  
  # Auto-Identificacion etnica de la MEF
  tab_etnia <- design %>%
    filter(!is.na({{ by_3 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_3 }})
  
  # Quintiles
  tab_quintil <- design %>%
    filter(!is.na({{ by_4 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_4 }})
  
  # Pobreza por Ingresos  
  tab_pobreza <- design %>%
    filter(!is.na({{ by_5 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_5 }}) 
  
  # Pobreza por NBI  
  tab_pobreza_nbi <- design %>%
    filter(!is.na({{ by_6 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_6 }}) 
  
  # Nivel de instruccion de la MEF  
  tab_nivins_madre_nin <- design %>%
    filter(!is.na({{ by_7 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_7 }}) 
  
  # Desnutricion cronica  
  tab_dcronica <- design %>%
    filter(!is.na({{ by_8 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_8 }}) 
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac, tab_area, 
                         tab_reg, tab_etnia, tab_quintil, 
                         tab_pobreza, tab_pobreza_nbi,
                         tab_nivins_madre_nin, tab_dcronica)
}

tab_full <- map(c("depresion_m"),
                ~tab_fun(.x, design = survey_design,
                         area, region, etnia, 
                         quintil, pobreza, nbi_1, nivins_madre, dcronica))

#==============================================================================#
####               Extraccion de elementos de objetos listas                #### 
#==============================================================================#

# Aplicacion de la funcion para la obtencion de tabulados
Style_tab(tab_full[[1]],"T10_i18") #tab_depresion_m

