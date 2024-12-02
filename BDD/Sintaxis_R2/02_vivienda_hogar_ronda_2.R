
# Titulo de la Sintaxis: 
# Datos de la vivienda y el hogar 

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
  distinct(id_hogar, .keep_all = TRUE)

df_f1_hogar_new <- df_f1_hogar %>% 
  inner_join(df_f1_personas, by = c("id_hogar"))

rm(df_f1_personas, df_f1_hogar)

#==============================================================================#
####                          Calculo de indicadores                        ####
#==============================================================================#

# Hogares sin adecuado sistema de eliminacion de excretas ---------------------#

# El Estado garantiza el derecho a un habitat saludable y a vivir en un 
# ambiente sano. 
# El indicador es una aproximacion que mide el derecho a tener una vivienda con
# condiciones minimas de saneamiento que se correlaciona con un potencial 
# ambiente saludable. Son hogares no privados aquellos que eliminan las excretas 
# por alcantarillado y pozo septico. 
# La forma de eliminacion de excretas a traves de pozo septico no garantiza un 
# ambiente sano.

# Revision 
df_f1_hogar_new %>% 
  freq(f1_s3_11, cumul = F, report.nas = F)

# Indicador 
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(eliminacion = case_when(
    (area == 1 & f1_s3_11 >= 2 & f1_s3_11 <= 7) | 
      (area == 2 & f1_s3_11 >= 3 & f1_s3_11 <= 7) ~ 1,
    !is.na(f1_s3_11) ~ 0, 
    TRUE ~ NA_real_ 
  ))

df_f1_hogar_new %>%
  freq(eliminacion, cumul = F, report.nas = F)

# Hogares que utilizan suministros seguros de agua para beber -----------------# 

# Componentes del indicador:
# 1. Tipo de suministro
# 2. Calidad del agua
# 3. Cercania del suministro
# 4. Suficiencia de agua para beber 

# 1. Tipo de suministro 
# Revision
df_f1_hogar_new %>% 
  freq(f1_s3_10, cumul = F, report.nas = F)

df_f1_hogar_new %>% 
  freq(f1_s7_1, cumul = F, report.nas = F)

df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(f1_s3_10r = case_when(
    (f1_s3_10 == 1 | f1_s3_10 == 2) ~ 1,
    (f1_s3_10 >= 3 & f1_s3_10 <= 5) ~ 2, 
    TRUE ~ NA_real_ 
  ))

df_f1_hogar_new %>%
  freq(f1_s3_10r, cumul = F, report.nas = F)

# Componente 1 
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(agua_cp1 = case_when(
    f1_s7_1 == 1 | f1_s7_1 == 2 | f1_s7_1 == 3 | f1_s7_1 == 7 | f1_s7_1 == 9 | 
      ((f1_s7_1 == 5 | f1_s7_1 == 6) & f1_s3_10r == 1)  ~ 1, # Tipo A
    f1_s7_1 == 4 | f1_s7_1 == 8 | f1_s7_1 == 10 | f1_s7_1 == 11 |
      ((f1_s7_1 == 5 | f1_s7_1 == 6) & f1_s3_10r == 2) ~ 2, # Tipo B 
    f1_s7_1 == 12 | f1_s7_1 == 13  ~ 3, # Tipo C 
    TRUE ~ NA_real_ 
  ))

df_f1_hogar_new %>%
  freq(agua_cp1, cumul = F, report.nas = F)

# 2. Calidad del agua - tratamiento 
# 2.1. Si la beben tal como llega al hogar, no tiene agua segura.
# 2.2. Si la hierven o tiene algun otro tratamiento, tiene calidad.
# 2.3. La respuesta "no sabe" no sera tomada en cuenta 

# Revision 
df_f1_hogar_new %>% 
  freq(f1_s7_23, cumul = F, report.nas = F)

# Componente 2
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(agua_cp2 = case_when(
    f1_s7_23 >= 2 & f1_s7_23 <= 5 ~ 1,
    f1_s7_23 == 1 ~ 2, 
    TRUE ~ NA_real_ 
  ))

df_f1_hogar_new %>%
  freq(agua_cp2, cumul = F, report.nas = F) 

# 3. Cercania del suministro 
# Revision
df_f1_hogar_new %>% 
  freq(f1_s7_1, cumul = F, report.nas = F)

df_f1_hogar_new %>% 
  freq(f1_s7_2, cumul = F, report.nas = F)

df_f1_hogar_new %>% 
  descr(f1_s7_3,
        stats = "common")

# Componente 3
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(agua_cp3 = case_when(
    (f1_s7_2 == 1 | f1_s7_2 == 2) | (f1_s7_1 == 5 | f1_s7_1 == 6) ~ 1,
    f1_s7_2 == 3 & (f1_s7_3 <= 30 & !is.na(f1_s7_3)) ~ 2,
    f1_s7_2 == 3 & (f1_s7_3 > 30 & f1_s7_3 < 888) ~ 3,
    TRUE ~ NA_real_ 
  )) # Carro reartidor, no se contempla en este calculo 

df_f1_hogar_new %>%
  freq(agua_cp3, cumul = F, report.nas = F)

# 4. Suficiencia de agua para beber 
# Revision 
df_f1_hogar_new %>% 
  freq(f1_s7_4, cumul = F, report.nas = F)

# Componente 4
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(agua_cp4 = case_when(
    f1_s7_4 == 1 ~ 1,
    f1_s7_4 == 2 ~ 2,
    TRUE ~ NA_real_ 
  ))# no se contempla el "no sabe"

df_f1_hogar_new %>%
  freq(agua_cp4, cumul = F, report.nas = F)

# Servicios de agua para beber manejado de forma segura 
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(i_agua = case_when(
    agua_cp1 == 1 & agua_cp2 == 1 & agua_cp3 == 1 & agua_cp4 == 1 ~ 1,
    (agua_cp1 == 1 & agua_cp2 == 1 & agua_cp3 == 1 & agua_cp4 == 2) | 
      (agua_cp1 == 1 & agua_cp2 == 1 & agua_cp3 == 2) ~ 2,
    (agua_cp1 == 1 & agua_cp2 == 2 & agua_cp3 == 1) | 
      (agua_cp1 == 1 & agua_cp2 == 2 & agua_cp3 == 2) ~ 3,
    agua_cp1 == 1 & agua_cp3 == 3 ~ 4,
    agua_cp1 == 2 ~ 5,
    agua_cp1 == 3 ~ 6,
    TRUE ~ NA_real_ 
  ))

df_f1_hogar_new = apply_labels(df_f1_hogar_new,
                               i_agua = num_lab(
                                 "1 Seguro
                              2 Básico 1
                              3 Básico 2
                              4 Limitado
                              5 No mejorado
                              6 Superficial"
                               ))

df_f1_hogar_new %>%
  freq(i_agua, cumul = F, report.nas = F)

# Indicador 
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(agua_segura = case_when(
    i_agua == 1  ~ 1,
    !is.na(i_agua) ~ 0,
    TRUE ~ NA_real_ 
  ))

df_f1_hogar_new %>%
  freq(agua_segura, cumul = F, report.nas = F)

#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores y ser visibles en las funciones 
# Area
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(area = as_label(area)) 

df_f1_hogar_new %>%
  freq(area, cumul = F, report.nas = F)

# Region 
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(region = as_label(region)) 

df_f1_hogar_new %>%
  freq(region, cumul = F, report.nas = F)

# Provincia 
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(prov = as_label(prov)) 

df_f1_hogar_new %>%
  freq(prov, cumul = F, report.nas = F)

# Parroquias priorizadas 
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(parr_pri = as_label(parr_pri)) 

df_f1_hogar_new %>%
  freq(parr_pri, cumul = F, report.nas = F)

# Quintil 
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(quintil = as_label(quintil)) 

df_f1_hogar_new %>%
  freq(quintil, cumul = F, report.nas = F)

# Pobreza por Ingresos 
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(pobreza = as_label(pobreza)) 

df_f1_hogar_new %>%
  freq(pobreza, cumul = F, report.nas = F)

# Pobreza por NBI
df_f1_hogar_new <- df_f1_hogar_new %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f1_hogar_new %>%
  freq(nbi_1, cumul = F, report.nas = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f1_hogar_new %>% as_survey_design(ids = "id_upm",
                                                      strata = "estrato",
                                                      weights = "fexp")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

# Para los resultados ponderados por diferentes desagregaciones reemplazar 
# respectivamente 

# Hogares sin adecuado sistema de eliminacion de excretas
survey_design %>%
  srvyr_prop(eliminacion)

survey_design %>%
  srvyr_prop_by(eliminacion, area)

# Hogares que utilizan suministros seguros de agua para beber 
survey_design %>%
  srvyr_prop(agua_segura)

survey_design %>%
  srvyr_prop_by(agua_segura, area)

#==============================================================================#
####                     Funciones para los tabulados                       ####
#==============================================================================#

tab_fun <- function(design, x, by_1, by_2, by_3, by_4, by_5, by_6, by_7) {  
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
  
  # Quintiles  
  tab_quintil <- design %>% filter(!is.na({{ by_5 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_5 }}) 
  
  # Pobreza por Ingresos  
  tab_pobreza <- design %>% filter(!is.na({{ by_6 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_6 }}) 
  
  # Pobreza por NBI  
  tab_pobreza_nbi <- design %>% filter(!is.na({{ by_7 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_7 }}) 
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac, tab_area, tab_reg, tab_prov, tab_parr, 
                         tab_quintil, tab_pobreza, tab_pobreza_nbi)
  return(tab_final)
}

#==============================================================================#
####                 Obtencion de tablas en formato lista                   ####
#==============================================================================#

tab_full<- map(c("eliminacion", "agua_segura"), ~tab_fun(
  survey_design, .x, area, region, prov, parr_pri, quintil, pobreza,
  nbi_1))

#==============================================================================#
####                     Exportacion de tabulados                           ####
#==============================================================================#

Style_tab(tab_full[[1]],"T1_i1") # eliminacion
Style_tab(tab_full[[2]],"T1_i2") # agua_segura

# si requiere importar las tablas por separado reemplazar la funcion "Style_tab"
# por "Style_tab_0"

