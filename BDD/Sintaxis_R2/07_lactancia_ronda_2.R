
# Titulo de la Sintaxis: 
# Lactancia Materna

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

# Base mef 
df_f2_mef <- readRDS(here(link_bdd, name_base_mef))
df_f2_mef <- as_tibble(df_f2_mef)

# Base lactancia
df_f2_lactancia <- readRDS(here(link_bdd, name_base_lactancia))
df_f2_lactancia <- as_tibble(df_f2_lactancia)

# Base personas 
df_f1_personas <- readRDS(here(link_bdd, name_base_personas))
df_f1_personas <- as_tibble(df_f1_personas)

# Diccionario mef
dicc_f2_mef <- import(paste0(link_bdd, name_diccionario), 
                      which = "f2_mef",trust = TRUE)

dicc_f2_mef <- as_tibble(dicc_f2_mef)   

# Diccionario lactancia
dicc_f2_lactancia <- import(paste0(link_bdd, name_diccionario), 
                            which = "f2_lactancia",trust = TRUE)

dicc_f2_lactancia <- as_tibble(dicc_f2_lactancia)   

# Diccionario personas
dicc_f1_per <- import(paste0(link_bdd, name_diccionario), 
                      which = "f1_personas",trust = TRUE)

dicc_f1_per <- as_tibble(dicc_f1_per)   

#==============================================================================#
####           Calculo de variables necesarias para el indicador            ####
#==============================================================================#

#------------------------------------------------------------------------------#
# Se procede a cambiar la base de mef de ancho a largo en funcion del numero 
# de hijos
#------------------------------------------------------------------------------#

# Nueva base con las variables de los hijos de las mefs
df_f2_hijos <- df_f2_mef %>% 
  select(starts_with("id"), starts_with("f2_s2_235_cod_"), 
         starts_with("f2_s2_235_a_")) 

# Cambio de la base de ancho a largo 
df_f2_hijos_long <- df_f2_hijos %>% 
  pivot_longer(
    cols = starts_with("f2_s2_235_"),
    names_to = c(".value", "ord_hijo"),
    names_pattern = "([A-Za-z]+)_([0-9]+)"
  ) 

df_f2_hijos_long <- df_f2_hijos_long %>%
  mutate(cod = as.character(cod)) %>% 
  rename(sexo = a)

rm(df_f2_hijos, df_f2_mef)

# Creacion de variables con dos digitos  
df_f2_hijos_long <- df_f2_hijos_long %>% 
  mutate(cod = case_when(
    nchar(cod) == 1 ~ paste0("0", cod),
    TRUE ~ cod
  )) 

# Creacion de identificadores 
df_f2_hijos_long <- df_f2_hijos_long %>% 
  mutate(id_mef_per = case_when(
    !is.na(cod) ~ paste0(id_mef, cod),
    TRUE ~ NA_character_
  ))

# Mantengo la base de los hijos registrados y que vivan en el hogar 
df_f2_hijos_long <- df_f2_hijos_long %>% 
  filter(!is.na(cod) & cod != "77")

# Mantengo variables a utilizar 
df_f2_hijos_long <- df_f2_hijos_long %>% 
  select(id_mef_per, sexo)

#------------------------------------------------------------------------------#
# Join - Base de hijos seccion 2 y base de salud de la ninez 
#------------------------------------------------------------------------------#

# Join 
df_f2_lactancia_new <- df_f2_lactancia %>% 
  inner_join(df_f2_hijos_long, by = c("id_mef_per")) 

rm(df_f2_hijos_long, df_f2_lactancia)

#------------------------------------------------------------------------------#
# Join 
#------------------------------------------------------------------------------#

# Variables de desagregacion 
df_f1_personas <- df_f1_personas %>% 
  select(id_per, parr_pri, etnia, quintil, pobreza, nbi_1)

df_f2_lactancia_new <- df_f2_lactancia_new %>% 
  left_join(df_f1_personas, by = c("id_per"))

rm(df_f1_personas)

#==============================================================================#
####               Calculo de los indicadores sobre Lactancia               ####
#==============================================================================#

# Estimacion de la edad en dias -----------------------------------------------#

df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(across(c(f2_s3_301_b_anio, f2_s3_301_b_mes, f2_s3_301_b_dia), as.character)) %>% 
  mutate(dob = paste(f2_s3_301_b_anio, f2_s3_301_b_mes, f2_s3_301_b_dia)) %>%
  mutate(dov = paste(fecha_anio, fecha_mes, fecha_dia)) %>%
  mutate(dob = as_date(dob)) %>%   
  mutate(dov = as_date(dov)) %>% 
  mutate(edaddias = (dob %--% dov) / days(1)) 

df_f2_lactancia_new %>%
  descr(edaddias, 
        stats = c("common"),
        round.digits = 2)

# Inicio temprano de la lactancia materna menores de 1 anio -------------------#

# Revision
df_f2_lactancia_new %>% 
  freq(f2_s3_302, cumul = F, report.nas = F)

df_f2_lactancia_new %>% 
  freq(f2_s3_304, cumul = F, report.nas = F)

# Indicador 
df_f2_lactancia_new <- df_f2_lactancia_new %>% 
  mutate(it_lacm_1 = case_when(
    f2_s3_302 == 1 & 
      (f2_s3_304 == 1 | f2_s3_304 == 2) & 
      (edaddias < 365 & !is.na(edaddias)) ~ 1,
    (f2_s3_302 == 2 | (f2_s3_304 == 3 | f2_s3_304 == 4)) & 
      (edaddias < 365 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_ 
  ))

df_f2_lactancia_new %>% 
  freq(it_lacm_1, cumul = F, report.nas = F)

# Inicio temprano de la lactancia materna menores de 2 anio -------------------#

# Indicador 
df_f2_lactancia_new <- df_f2_lactancia_new %>% 
  mutate(it_lacm_2 = case_when(
    f2_s3_302 == 1 & 
      (f2_s3_304 == 1 | f2_s3_304 == 2) & 
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    (f2_s3_302 == 2 | (f2_s3_304 == 3 | f2_s3_304 == 4)) & 
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_ 
  ))

df_f2_lactancia_new %>% 
  freq(it_lacm_2, cumul = F, report.nas = F)

# Lactancia materna exclusiva -------------------------------------------------#

# Revision
df_f2_lactancia_new %>% 
  freq(f2_s3_306, cumul = F, report.nas = F)

df_f2_lactancia_new %>% 
  freq(f2_s3_309, cumul = F, report.nas = F)

df_f2_lactancia_new %>% 
  freq(f2_s3_311, cumul = F, report.nas = F)

# Indicador 
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(lac_exc6 = case_when(
    f2_s3_306 == 1 & f2_s3_309 == 2 & f2_s3_311 == 2 &
      (edaddias < 183 & !is.na(edaddias)) ~ 1,
    !is.na(f2_s3_306) & !is.na(f2_s3_309) & !is.na(f2_s3_311) &
      (edaddias < 183 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  )) 

df_f2_lactancia_new %>%
  freq(lac_exc6, cumul = F, report.nas = F)

# Lactancia materna continua --------------------------------------------------#

# Revision
df_f2_lactancia_new %>% 
  freq(f2_s3_306, cumul = F, report.nas = F)

# Indicador 
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(lac_cont = case_when(
    f2_s3_306 == 1 &
      (edaddias >= 365 & edaddias < 487) ~ 1,
    !is.na(f2_s3_306) &
      (edaddias >= 365 & edaddias < 487) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_lactancia_new %>%
  freq(lac_cont, cumul = F, report.nas = F)

# Diversidad alimentaria ------------------------------------------------------#

# Revision 
df_f2_lactancia_new %>%
  freq(f2_s3_312_a, cumul = F, report.nas = F)

df_f2_lactancia_new %>%
  freq(f2_s3_312_b, cumul = F, report.nas = F)

df_f2_lactancia_new %>%
  freq(f2_s3_312_d, cumul = F, report.nas = F)

# Grupos de alimentos
# Grupo 1: cereales, raices y tuberculos
df_f2_lactancia_new <- df_f2_lactancia_new %>% 
  mutate(grupo1 = case_when(
    f2_s3_312_a == 1 | f2_s3_312_b == 1 | f2_s3_312_d == 1 ~ 1,
    !is.na(f2_s3_312_a) & !is.na(f2_s3_312_b) & !is.na(f2_s3_312_d) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_lactancia_new %>%
  freq(grupo1, cumul = F, report.nas = F)

# Revision 
df_f2_lactancia_new %>%
  freq(f2_s3_312_l, cumul = F, report.nas = F)

# Grupo 2: legumbres y nueces
df_f2_lactancia_new <- df_f2_lactancia_new %>% 
  mutate(grupo2 = case_when(
    f2_s3_312_l == 1 ~ 1,
    !is.na(f2_s3_312_l) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_lactancia_new %>%
  freq(grupo2, cumul = F, report.nas = F)

# Revision
df_f2_lactancia_new %>%
  freq(f2_s3_312_m, cumul = F, report.nas = F)

# Grupo 3: lacteos (leche, yogurt, queso)
df_f2_lactancia_new <- df_f2_lactancia_new %>% 
  mutate(grupo3 = case_when(
    f2_s3_312_m == 1 ~ 1,
    !is.na(f2_s3_312_m) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_lactancia_new %>%
  freq(grupo3, cumul = F, report.nas = F)

# Revision
df_f2_lactancia_new %>%
  freq(f2_s3_312_h, cumul = F, report.nas = F)

df_f2_lactancia_new %>%
  freq(f2_s3_312_i, cumul = F, report.nas = F)

df_f2_lactancia_new %>%
  freq(f2_s3_312_k, cumul = F, report.nas = F)

# Grupo 4: carnes (carne, pescado, aves e higado o carnes provenientes de visceras)
df_f2_lactancia_new <- df_f2_lactancia_new %>% 
  mutate(grupo4 = case_when(
    f2_s3_312_h == 1 | f2_s3_312_i == 1 | f2_s3_312_k == 1 ~ 1,
    !is.na(f2_s3_312_h) & !is.na(f2_s3_312_i) & !is.na(f2_s3_312_k) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_lactancia_new %>%
  freq(grupo4, cumul = F, report.nas = F)

# Revision 
df_f2_lactancia_new %>%
  freq(f2_s3_312_j, cumul = F, report.nas = F)

# Grupo 5: Huevos
df_f2_lactancia_new <- df_f2_lactancia_new %>% 
  mutate(grupo5 = case_when(
    f2_s3_312_j == 1 ~ 1,
    !is.na(f2_s3_312_j) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_lactancia_new %>%
  freq(grupo5, cumul = F, report.nas = F)

# Revision 
df_f2_lactancia_new %>%
  freq(f2_s3_312_c, cumul = F, report.nas = F)

df_f2_lactancia_new %>%
  freq(f2_s3_312_e, cumul = F, report.nas = F)

df_f2_lactancia_new %>%
  freq(f2_s3_312_f, cumul = F, report.nas = F)

# Grupo 6: frutas y verduras ricas en vitamina A
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(grupo6 = case_when(
    f2_s3_312_c == 1 | f2_s3_312_e == 1 | f2_s3_312_f == 1 ~ 1,
    !is.na(f2_s3_312_c) & !is.na(f2_s3_312_e) & !is.na(f2_s3_312_f) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_lactancia_new %>%
  freq(grupo6, cumul = F, report.nas = F)

# Revision 
df_f2_lactancia_new %>%
  freq(f2_s3_312_g, cumul = F, report.nas = F)

# Grupo 7: otras frutas y verduras
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(grupo7 = case_when(
    f2_s3_312_g == 1 ~ 1,
    !is.na(f2_s3_312_g) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_lactancia_new %>%
  freq(grupo7, cumul = F, report.nas = F)

# Suma de la diversidad de alimentos
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  rowwise() %>% 
  mutate(div_grupos = sum(c(grupo1, grupo2, grupo3, grupo4, grupo5, grupo6, grupo7) == "1")) %>%
  ungroup()

df_f2_lactancia_new %>%
  freq(div_grupos, cumul = F, report.nas = F)

# Ninos amamantados de 6 a 23 meses de edad que recibieron alimentos
# de 4 y mas grupos alimentarios durante el dia anterior
# Indicador 
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(div_alim = case_when(
    div_grupos >= 4 & !is.na(div_grupos) &
      (edaddias >= 183 & edaddias <= 730) & f2_s3_306 == 1 ~ 1,
    div_grupos < 4 & !is.na(div_grupos) &
      (edaddias >= 183 & edaddias <= 730) & f2_s3_306 == 1 ~ 0, 
    TRUE ~ NA_real_
  ))

df_f2_lactancia_new %>%
  freq(div_alim, cumul = F, report.nas = F)

#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores y ser visibles en las funciones 
# Area
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(area = as_label(area)) 

df_f2_lactancia_new %>%
  freq(area, cumul = F, report.nas = F)

# Region 
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(region = as_label(region)) 

df_f2_lactancia_new %>%
  freq(region, cumul = F, report.nas = F)

# Provincia 
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(prov = as_label(prov)) 

df_f2_lactancia_new %>%
  freq(prov, cumul = F, report.nas = F)

# Parroquias priorizadas 
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(parr_pri = as_label(parr_pri)) 

df_f2_lactancia_new %>%
  freq(parr_pri, cumul = F, report.nas = F)

# Sexo
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(sexo = as_label(sexo)) 

df_f2_lactancia_new %>%
  freq(sexo, cumul = F, report.nas = F)

# Auto-Identificacion etnica 
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(etnia = as_label(etnia)) 

df_f2_lactancia_new %>%
  freq(etnia, cumul = F, report.nas = F)

# Quintil 
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(quintil = as_label(quintil)) 

df_f2_lactancia_new %>%
  freq(quintil, cumul = F, report.nas = F)

# Pobreza por Ingresos 
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(pobreza = as_label(pobreza)) 

df_f2_lactancia_new %>%
  freq(pobreza, cumul = F, report.nas = F)

# Pobreza por NBI
df_f2_lactancia_new <- df_f2_lactancia_new %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f2_lactancia_new %>%
  freq(nbi_1, cumul = F, report.nas = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f2_lactancia_new %>% as_survey_design(ids = "id_upm",
                                                          strata = "estrato",
                                                          weights = "fexp_lm")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

# Para los resultados ponderados por diferentes desagregaciones reemplazar 
# respectivamente 

# Inicio temprano de la lactancia materna menores de 1 anio 
survey_design %>%
  srvyr_prop(it_lacm_1)

survey_design %>%
  srvyr_prop_by(it_lacm_1, area)

# Inicio temprano de la lactancia materna menores de 2 anio
survey_design %>%
  srvyr_prop(it_lacm_2)

survey_design %>%
  srvyr_prop_by(it_lacm_2, area)

# Lactancia materna exclusiva 
survey_design %>%
  srvyr_prop(lac_exc6)

survey_design %>%
  srvyr_prop_by(lac_exc6, area)

# Lactancia materna continua
survey_design %>%
  srvyr_prop(lac_cont)

survey_design %>%
  srvyr_prop_by(lac_cont, area)

# Diversidad alimentaria 
survey_design %>%
  srvyr_prop(div_alim)

survey_design %>%
  srvyr_prop_by(div_alim, area)

#==============================================================================#
####                     Funciones para los tabulados                       ####
#==============================================================================#

tab_fun <- function(design, x, by_1, by_2, by_3, by_4, by_5, 
                    by_6, by_7, by_8, by_9) {  
  
  # Nacional 
  tab_nac <- design %>% 
    srvyr_prop(.data[[x]])
  
  # Area 
  tab_area <- design %>% 
    srvyr_prop_by(.data[[x]], {{ by_1 }}) 
  
  # Region
  tab_reg <- design %>%
    srvyr_prop_by(.data[[x]], {{ by_2 }})
  
  # Provincia
  tab_prov <- design %>%
    srvyr_prop_by(.data[[x]], {{ by_3 }})
  
  # Parroquias priorizadas  
  tab_parr <- design %>%
    filter(!is.na({{ by_4 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_4 }}) 
  
  # Sexo
  tab_sex <- design %>%
    srvyr_prop_by(.data[[x]], {{ by_5 }})
  
  # Auto-Identificacion etnica
  tab_etnia <- design %>%
    filter(!is.na({{ by_6 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_6 }})
  
  # Quintiles
  tab_quintil <- design %>%
    filter(!is.na({{ by_7 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_7 }})
  
  # Pobreza por Ingresos  
  tab_pobreza <- design %>%
    filter(!is.na({{ by_8 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_8 }}) 
  
  # Pobreza por NBI  
  tab_pobreza_nbi <- design %>%
    filter(!is.na({{ by_9 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_9 }}) 
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac,tab_area, 
                         tab_reg, tab_prov, 
                         tab_parr, tab_sex, 
                         tab_etnia, tab_quintil,
                         tab_pobreza, tab_pobreza_nbi)
  return(tab_final)
}

#==============================================================================#
####                 Obtencion de tablas en formato lista                   ####
#==============================================================================#

tab_full<- map(c("it_lacm_1", "it_lacm_2", "lac_exc6", "lac_cont", "div_alim"),
               ~tab_fun(.x, design = survey_design,
                        area, region, prov, parr_pri, sexo, 
                        etnia,quintil, pobreza, nbi_1))

#==============================================================================#
####                     Exportacion de tabulados                           ####
#==============================================================================#

Style_tab(tab_full[[1]],"T4_i1") # it_lacm_1
Style_tab(tab_full[[2]],"T4_i2") # it_lacm_2
Style_tab(tab_full[[3]],"T4_i3") # lac_exc6
Style_tab(tab_full[[4]],"T4_i4") # lac_cont
Style_tab(tab_full[[5]],"T4_i5") # div_alim

# si requiere importar las tablas por separado reemplazar la funcion "Style_tab"
# por "Style_tab_0"

