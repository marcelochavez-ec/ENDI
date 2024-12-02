
# Titulo de la Sintaxis: 
# Estado nutricional a partir de mediciones de hemoglobina

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
####                       Calculo de variable necesarias                   ####
#==============================================================================#

# Estimacion de la edad en dias -----------------------------------------------#

df_f1_personas <- df_f1_personas %>% 
  mutate(dob = paste(f1_s1_4_3, f1_s1_4_2, f1_s1_4_1)) %>%
  mutate(dov = paste(f1_s6_5_3, f1_s6_5_2, f1_s6_5_1)) %>%
  mutate(dob = as_date(dob)) %>%
  mutate(dov = as_date(dov)) %>%
  mutate(edaddias = (dob %--% dov) / days(1)) 

df_f1_personas %>%
  descr(edaddias, 
        stats = c("common")) 

#==============================================================================#
####                           Calculo Anemia                               ####
#==============================================================================#

# Nivel de hemoglobina de referencia ------------------------------------------#

# Ajustes de las concentraciones de hemoglobina medidas en funcion de la 
# altitud sobre el nivel del mar

df_f1_personas %>%
  descr(altitud, 
        stats = c("common")) 

df_f1_personas %>%
  descr(f1_s6_3, 
        stats = c("common")) 

df_f1_personas <- df_f1_personas %>% 
  mutate(nivel_ajus = case_when(
    altitud < 500 & !is.na(altitud) ~ f1_s6_3,
    altitud >= 500 & altitud < 1000 ~ f1_s6_3 - 0.4,
    altitud >= 1000 & altitud < 1500 ~ f1_s6_3 - 0.8,    
    altitud >= 1500 & altitud < 2000 ~ f1_s6_3 - 1.1,  
    altitud >= 2000 & altitud < 2500 ~ f1_s6_3 - 1.4,
    altitud >= 2500 & altitud < 3000 ~ f1_s6_3 - 1.8,
    altitud >= 3000 & altitud < 3500 ~ f1_s6_3 - 2.1,
    altitud >= 3500 & altitud < 4000 ~ f1_s6_3 - 2.5,
    altitud >= 4000 & altitud < 4500 ~ f1_s6_3 - 2.9,
    altitud >= 4500 & altitud < 5000 ~ f1_s6_3 - 3.3,
    TRUE ~ NA_real_
  ))

df_f1_personas %>%
  descr(nivel_ajus, 
        stats = c("common")) 

# Tipo de anemia para ninos/as de 6 a 59 meses de edad ------------------------#

# Indicador 
df_f1_personas <- df_f1_personas %>% 
  mutate(ane6_59 = case_when(
    (nivel_ajus < 7 & !is.na(nivel_ajus)) & 
      (edaddias >= 183 & edaddias < 731) ~ "Anemia grave",
    (nivel_ajus >= 7 & nivel_ajus < 9.5) &
      (edaddias >= 183 & edaddias < 731) ~ "Anemia moderada",
    (nivel_ajus >= 9.5 & nivel_ajus < 10.5) &
      (edaddias >= 183 & edaddias < 731) ~ "Anemia leve",
    (nivel_ajus >= 10.5 & !is.na(nivel_ajus)) &
      (edaddias >= 183 & edaddias < 731) ~ "No tiene anemia",
    (nivel_ajus < 7 & !is.na(nivel_ajus)) & 
      (edaddias >= 731 & edaddias < 1826) ~ "Anemia grave",
    (nivel_ajus >= 7 & nivel_ajus < 10) &
      (edaddias >= 731 & edaddias < 1826) ~ "Anemia moderada",
    (nivel_ajus >= 10 & nivel_ajus < 11) &
      (edaddias >= 731 & edaddias < 1826) ~ "Anemia leve",
    (nivel_ajus >= 11 & !is.na(nivel_ajus)) &
      (edaddias >= 731 & edaddias < 1826) ~ "No tiene anemia",
    TRUE ~ NA_character_ 
  ))

# Orden de las categorias
df_f1_personas <- df_f1_personas %>% 
  mutate(ane6_59 = fct_relevel(ane6_59,
                               c("Anemia grave", 
                                 "Anemia moderada",
                                 "Anemia leve", 
                                 "No tiene anemia")))

df_f1_personas %>% 
  freq(ane6_59, cumul = F, report.nas = F)

# Variable dicotomica  
df_f1_personas <- df_f1_personas %>%
  mutate(ane6_59_new = case_when(
    (nivel_ajus < 10.5 & !is.na(nivel_ajus)) &
      (edaddias >= 183 & edaddias < 731) ~ 1,
    (nivel_ajus >= 10.5 & !is.na(nivel_ajus)) &
      (edaddias >= 183 & edaddias < 731) ~ 0,
    (nivel_ajus < 11 & !is.na(nivel_ajus)) &
      (edaddias >= 731 & edaddias < 1826) ~ 1,
    (nivel_ajus >= 11 & !is.na(nivel_ajus)) &
      (edaddias >= 731 & edaddias < 1826) ~ 0,
    TRUE ~ NA_real_
  ))

df_f1_personas %>%
  freq(ane6_59_new, cumul = F, report.nas = F)

# Tipo de anemia para ninos/as de 6 a 23 meses de edad ------------------------#

# Indicador 
df_f1_personas <- df_f1_personas %>% 
  mutate(ane6_23 = case_when(
    (nivel_ajus < 7 & !is.na(nivel_ajus)) & 
      (edaddias >= 183 & edaddias < 731) ~ "Anemia grave",
    (nivel_ajus >= 7 & nivel_ajus < 9.5) &
      (edaddias >= 183 & edaddias < 731) ~ "Anemia moderada",
    (nivel_ajus >= 9.5 & nivel_ajus < 10.5) &
      (edaddias >= 183 & edaddias < 731) ~ "Anemia leve",
    (nivel_ajus >= 10.5 & !is.na(nivel_ajus)) &
      (edaddias >= 183 & edaddias < 731) ~ "No tiene anemia",
    TRUE ~ NA_character_ 
  ))

# Orden de las categorias
df_f1_personas <- df_f1_personas %>% 
  mutate(ane6_23 = fct_relevel(ane6_23,
                               c("Anemia grave", 
                                 "Anemia moderada",
                                 "Anemia leve", 
                                 "No tiene anemia")))

df_f1_personas %>% 
  freq(ane6_23, cumul = F, report.nas = F)

# Variable dicotomica  
df_f1_personas <- df_f1_personas %>%
  mutate(ane6_23_new = case_when(
    (nivel_ajus < 10.5 & !is.na(nivel_ajus)) &
      (edaddias >= 183 & edaddias < 731) ~ 1,
    (nivel_ajus >= 10.5 & !is.na(nivel_ajus)) &
      (edaddias >= 183 & edaddias < 731) ~ 0,
    TRUE ~ NA_real_
  ))

df_f1_personas %>%
  freq(ane6_23_new, cumul = F, report.nas = F)

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

# Tipo de anemia para ninos/as de 6 a 59 meses de edad 
survey_design %>%
  srvyr_freq(ane6_59)

survey_design %>%
  srvyr_freq_by(ane6_59, area)

survey_design %>%
  srvyr_prop(ane6_59_new)

survey_design %>%
  srvyr_prop_by(ane6_59_new, area)

# Tipo de anemia para ninos/as de 6 a 23 meses de edad 
survey_design %>%
  srvyr_freq(ane6_23)

survey_design %>%
  srvyr_freq_by(ane6_23, area)

survey_design %>%
  srvyr_prop(ane6_23_new)

survey_design %>%
  srvyr_prop_by(ane6_23_new, area)

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

tab_full <- map(c("ane6_59_new", "ane6_23_new"),
                ~tab_fun_1(.x, design = survey_design, 
                           area, region, prov, parr_pri, 
                           quintil, pobreza, nbi_1)) 

tab_full_1 <- map(c("ane6_59", "ane6_23"),
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

Style_tab(tab_full[[1]], "T2_i15") # ane6_59_new
Style_tab(tab_full[[2]], "T2_i16") # ane6_23_new
Style_tab(tabla[[1]], "T2_i17") # ane6_59
Style_tab(tabla[[2]], "T2_i18") # ane6_23

# si requiere importar las tablas por separado reemplazar la funcion "Style_tab"
# por "Style_tab_0"

