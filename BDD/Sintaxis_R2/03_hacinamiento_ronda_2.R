
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

# Variables compartidas
df_f1_hogar <- df_f1_hogar %>% 
  select(-c(id_upm, id_viv, fecha_anio, fecha_mes, 
            fecha_dia, fexp, estrato, area, region, prov))

# Join
df_f1_perhog <- df_f1_personas %>%  
  inner_join(df_f1_hogar, by = c("id_hogar"))

rm(df_f1_personas, df_f1_hogar)

#==============================================================================#
####                          Calculo de indicadores                        ####
#==============================================================================#

# Hacinamiento ----------------------------------------------------------------#

# Revision 
df_f1_perhog %>% 
  descr(f1_s3_16,
        stats = c("common"))

df_f1_perhog <- df_f1_perhog %>%
  mutate(f1_s3_16_dor = case_when(
    f1_s3_16 == 0 ~ 1, 
    TRUE ~ f1_s3_16
  ))

# Conteo de personas por hogar 
df_f1_perhog <- df_f1_perhog %>%
  mutate(pers = 1)

df_f1_perhog <- df_f1_perhog %>%
  group_by(id_hogar) %>% 
  mutate(numpers = sum(pers)) %>% 
  ungroup()

df_f1_perhog %>% 
  descr(numpers,
        stats = c("common"))

# Relacion entre numero de personas y dormitorios
df_f1_perhog <- df_f1_perhog %>%
  mutate(percuart = numpers / f1_s3_16_dor)

# Indicador 
df_f1_perhog <- df_f1_perhog %>%
  mutate(hacm = case_when(
    !is.na(percuart) & percuart > 3 ~ 1, 
    !is.na(percuart) & percuart <= 3 ~ 0,
    TRUE ~ NA_real_
  ))

df_f1_perhog %>%
  freq(hacm, cumul = F, report.nas = F)

#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores y ser visibles en las funciones 
# Area
df_f1_perhog <- df_f1_perhog %>%
  mutate(area = as_label(area)) 

df_f1_perhog %>%
  freq(area, cumul = F, report.nas = F)

# Region 
df_f1_perhog <- df_f1_perhog %>%
  mutate(region = as_label(region)) 

df_f1_perhog %>%
  freq(region, cumul = F, report.nas = F)

# Provincia 
df_f1_perhog <- df_f1_perhog %>%
  mutate(prov = as_label(prov)) 

df_f1_perhog %>%
  freq(prov, cumul = F, report.nas = F)

# Parroquias priorizadas 
df_f1_perhog <- df_f1_perhog %>%
  mutate(parr_pri = as_label(parr_pri)) 

df_f1_perhog %>%
  freq(parr_pri, cumul = F, report.nas = F)

# Quintil 
df_f1_perhog <- df_f1_perhog %>%
  mutate(quintil = as_label(quintil)) 

df_f1_perhog %>%
  freq(quintil, cumul = F, report.nas = F)

# Pobreza por Ingresos 
df_f1_perhog <- df_f1_perhog %>%
  mutate(pobreza = as_label(pobreza)) 

df_f1_perhog %>%
  freq(pobreza, cumul = F, report.nas = F)

# Pobreza por NBI
df_f1_perhog <- df_f1_perhog %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f1_perhog %>%
  freq(nbi_1, cumul = F, report.nas = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f1_perhog %>% as_survey_design(ids = "id_upm",
                                                   strata = "estrato",
                                                   weights = "fexp")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

# Para los resultados ponderados por diferentes desagregaciones reemplazar 
# respectivamente 

# Hacinamiento
survey_design %>% 
  srvyr_prop(hacm)

survey_design %>% 
  srvyr_prop_by(hacm, area)

survey_design %>% 
  srvyr_prop_by(hacm, prov)

#==============================================================================#
####                     Funciones para los tabulados                       ####
#==============================================================================#

tab_fun <- function(design, x, by_1, by_2, by_3, by_4, by_5, by_6) {  
  
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
  
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac,
                         tab_area, tab_reg, 
                         tab_prov, tab_parr,
                         tab_quintil, tab_pobreza)
  
  return(tab_final)
}

#==============================================================================#
####                 Obtencion de tablas en formato lista                   ####
#==============================================================================#

tab_full <- map("hacm",
                ~tab_fun(.x, design = survey_design,
                         area, region, prov, parr_pri, quintil, pobreza))

#==============================================================================#
####                     Exportacion de tabulados                           ####
#==============================================================================#

Style_tab(tab_full[[1]],"T1_i3") # hacm
# Style_tab_0(tab_full[[1]],"T1_i3") # hacm  

# si requiere importar las tablas por separado reemplazar la funcion "Style_tab"
# por "Style_tab_0"

