
# Titulo de la Sintaxis: 
# Vacunacion 

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

# Base salud ninez 
df_f2_salud_ninez <- readRDS(here(link_bdd, name_base_ninez))
df_f2_salud_ninez <- as_tibble(df_f2_salud_ninez)

# Base personas 
df_f1_personas <- readRDS(here(link_bdd, name_base_personas))
df_f1_personas <- as_tibble(df_f1_personas)

# Diccionario mef
dicc_f2_mef <- import(paste0(link_bdd, name_diccionario), 
                      which = "f2_mef",trust = TRUE)

dicc_f2_mef <- as_tibble(dicc_f2_mef)   


# Diccionario salud ninez
dicc_f2_salud_ninez <- import(paste0(link_bdd, name_diccionario), 
                              which = "f2_salud_ninez",trust = TRUE)

dicc_f2_salud_ninez <- as_tibble(dicc_f2_salud_ninez)   

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
         starts_with("f2_s2_235_a_"), starts_with("f2_s2_235_b_dia_"), 
         starts_with("f2_s2_235_b_mes_"), starts_with("f2_s2_235_b_anio_")) 

# Cambio de la base de ancho a largo 
df_f2_hijos_long <- df_f2_hijos %>% 
  pivot_longer(
    cols = starts_with("f2_s2_235_"),
    names_to = c(".value", "ord_hijo"),
    names_pattern = "([A-Za-z]+)_([0-9]+)"
  )

df_f2_hijos_long <- df_f2_hijos_long %>% 
  mutate(across(c(cod, dia, mes, anio), as.character))

df_f2_hijos_long <- df_f2_hijos_long %>% 
  rename(sexo = a)

rm(df_f2_hijos, df_f2_mef)

# Creacion de variables con dos digitos  
df_f2_hijos_long <- df_f2_hijos_long %>% 
  mutate(cod = case_when(
    nchar(cod) == 1 ~ paste0("0", cod),
    TRUE ~ cod
  )) %>% 
  mutate(ord_hijo = case_when(
    nchar(ord_hijo) == 1 ~ paste0("0", ord_hijo),
    TRUE ~ ord_hijo
  ))

# Creacion de identificadores 
df_f2_hijos_long <- df_f2_hijos_long %>% 
  mutate(id_hijo_ord = case_when(
    !is.na(cod) ~ paste0(id_mef, cod, ord_hijo),
    TRUE ~ NA_character_
  ))

# Mantengo la base de los hijos registrados 
df_f2_hijos_long <- df_f2_hijos_long %>% 
  filter(!is.na(id_hijo_ord))

# Mantengo variables a utilizar 
df_f2_hijos_long <- df_f2_hijos_long %>% 
  select(id_hijo_ord, dia, mes, anio, sexo)

#------------------------------------------------------------------------------#
# Se procede a cambiar la base de hijos de la seccion 4 de ancho a largo en 
# funcion de las vacunas  
#------------------------------------------------------------------------------#

# Cambio de la base de ancho a largo 
df_f2_hijos_vac_long <- df_f2_salud_ninez %>% 
  pivot_longer(
    cols = starts_with(c("f2_s4j_501_")),
    names_to = c(".value", ".value", "ord_vac", ".value"),
    names_pattern = "([a-z])([a-z_])([a-z])(.*)"   
  ) 

rm(df_f2_salud_ninez)

# Cambio de nombres 
df_f2_hijos_vac_long <- df_f2_hijos_vac_long %>%
  rename(
    vacuna = a_,
    dia_vac = a__dia, 
    mes_vac = a__mes,
    anio_vac = a__anio,
    madre = b_ 
  )

#------------------------------------------------------------------------------#
# Join -  Base de vacunas por hijos y base de hijos
#------------------------------------------------------------------------------#

# Join 
df_f2_hijos_long_long <- df_f2_hijos_vac_long %>% 
  left_join(df_f2_hijos_long, by = c("id_hijo_ord")) 
# Se utiliza left join para mantener la estructura de la base 
# df_f2_hijos_vac_long

rm(df_f2_hijos_long, df_f2_hijos_vac_long)

# Estimacion de la edad de vacunación en dias 
df_f2_hijos_long_long <- df_f2_hijos_long_long %>%
  mutate(across(ends_with("_vac"), as.character)) %>% 
  mutate(dob = paste(anio, mes, dia)) %>%
  mutate(dov = paste(anio_vac, mes_vac, dia_vac)) %>%
  mutate(dob = as_date(dob)) %>%   
  mutate(dov = as_date(dov)) %>%  
  mutate(edaddias = (dob %--% dov) / days(1)) 

df_f2_hijos_long_long %>%
  descr(edaddias, 
        stats = c("common"),
        round.digits = 2) 

#------------------------------------------------------------------------------#
# Se procede a cambiar la base de vacunacion de los hijos de largo a ancho 
# en funcion de los hijos 
#------------------------------------------------------------------------------#

# Cambio de la base de largo a ancho
df_f2_hijos_wide <- df_f2_hijos_long_long %>% 
  pivot_wider(
    names_from = ord_vac,
    names_sep = "_",
    values_from = c(vacuna, dia_vac, mes_vac, anio_vac, madre, 
                    dob, dov, edaddias)
  )

rm(df_f2_hijos_long_long)

#------------------------------------------------------------------------------#
# Join 
#------------------------------------------------------------------------------#

# Variables de desagregacion a partir del identificador de personas  
df_f1_personas_new <- df_f1_personas %>% 
  select(id_per, parr_pri, etnia, quintil, pobreza, nbi_1)

# Variables de desagregacion a partir del identificador de mef  
df_f1_mef <- df_f1_personas %>% 
  select(id_mef, parr_pri, etnia, quintil, pobreza, nbi_1) %>% 
  rename(parr_pri_mef = parr_pri,
         etnia_mef = etnia,
         quintil_mef = quintil,
         pobreza_mef = pobreza,
         nbi_1_mef = nbi_1)

# Join 
df_f2_hijos_wide <- df_f2_hijos_wide %>% 
  left_join(df_f1_personas_new, by = c("id_per"))

df_f2_hijos_wide <- df_f2_hijos_wide %>% 
  left_join(df_f1_mef, by = c("id_mef"))

# Recuperacion de informacion de las hijas/os por medio de la informacion de la 
# madre, esto para las hijas/os que no son miembros de hogar o fallecidas/os

df_f2_hijos_wide <- df_f2_hijos_wide %>% 
  mutate(parr_pri = coalesce(parr_pri, parr_pri_mef),
         etnia = coalesce(etnia, etnia_mef),
         quintil = coalesce(quintil, quintil_mef),
         pobreza = coalesce(pobreza, pobreza_mef),
         nbi_1 = coalesce(nbi_1, nbi_1_mef))

rm(df_f1_personas, df_f1_personas_new, df_f1_mef)

#==============================================================================#
####           Construccion de los indicadores de vacunacion                ####
#==============================================================================#

# Estimacion de la edad en dias -----------------------------------------------#

df_f2_hijos_wide <- df_f2_hijos_wide %>% 
  mutate(dob = paste(anio, mes, dia)) %>%
  mutate(dov = paste(fecha_anio, fecha_mes, fecha_dia)) %>%
  mutate(dob = as_date(dob)) %>%   
  mutate(dov = as_date(dov)) %>%  
  mutate(edaddias_nin = (dob %--% dov) / days(1)) 

df_f2_hijos_wide %>%
  descr(edaddias_nin, 
        stats = c("common"),
        round.digits = 2) 

# Vacunacion BCG --------------------------------------------------------------#

# BCG
df_f2_hijos_wide %>%
  freq(vacuna_a, cumul = F, report.nas = F)

# Indicador 
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(bcg = case_when(
    vacuna_a == 1 &
      (edaddias_a < 365 & !is.na(edaddias_a)) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 1,
    !is.na(vacuna_a) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_hijos_wide %>%
  freq(bcg, cumul = F, report.nas = F)

# Vacunacion Hepatitis B ------------------------------------------------------#

# Hepatitis B
df_f2_hijos_wide %>%
  freq(vacuna_b, cumul = F, report.nas = F)

# Indicador 
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(hepatitis = case_when(
    vacuna_b == 1 &
      (edaddias_b < 365 & !is.na(edaddias_b)) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 1,
    !is.na(vacuna_b) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_hijos_wide %>%
  freq(hepatitis, cumul = F, report.nas = F)

# Vacunacion de rotavirus -----------------------------------------------------# 

# Rotavirus 1 
df_f2_hijos_wide %>%
  freq(vacuna_c, cumul = F, report.nas = F) 

# Rotavirus 2
df_f2_hijos_wide %>%
  freq(vacuna_d, cumul = F, report.nas = F) 

# Indicador 
df_f2_hijos_wide <- df_f2_hijos_wide %>% 
  mutate(rota_total = case_when(
    vacuna_c == 1 & vacuna_d == 1 &
      (edaddias_c < 365 & !is.na(edaddias_c)) &
      (edaddias_d < 365 & !is.na(edaddias_d)) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 1,
    !is.na(vacuna_c) & !is.na(vacuna_d) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_hijos_wide %>%
  freq(rota_total, cumul = F, report.nas = F)

# Vacunacion Pentavalente -----------------------------------------------------#

# Pentavalente 1 
df_f2_hijos_wide %>%
  freq(vacuna_e, cumul = F, report.nas = F)

# Pentavalente 2 
df_f2_hijos_wide %>%
  freq(vacuna_f, cumul = F, report.nas = F)

# Pentavalente 3 
df_f2_hijos_wide %>%
  freq(vacuna_g, cumul = F, report.nas = F)

# Indicador 
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(penta_total = case_when(
    vacuna_e == 1 & vacuna_f == 1 & vacuna_g == 1 &
      (edaddias_e < 365 & !is.na(edaddias_e)) &
      (edaddias_f < 365 & !is.na(edaddias_f)) &
      (edaddias_g < 365 & !is.na(edaddias_g)) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 1,
    !is.na(vacuna_e) & !is.na(vacuna_f) & !is.na(vacuna_g) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_hijos_wide %>%
  freq(penta_total, cumul = F, report.nas = F)

# Vacunacion Antipoliomielitica -----------------------------------------------#

# Antipoliomielitica 1 
df_f2_hijos_wide %>%
  freq(vacuna_h, cumul = F, report.nas = F)

# Antipoliomielitica 2
df_f2_hijos_wide %>%
  freq(vacuna_i, cumul = F, report.nas = F)

# Antipoliomielitica 3
df_f2_hijos_wide %>%
  freq(vacuna_j, cumul = F, report.nas = F)

# Indicador 
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(antip_total = case_when(
    vacuna_h == 1 & vacuna_i == 1 & vacuna_j == 1 &
      (edaddias_h < 365 & !is.na(edaddias_h)) &
      (edaddias_i < 365 & !is.na(edaddias_i)) &
      (edaddias_j < 365 & !is.na(edaddias_j)) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 1,
    !is.na(vacuna_h) & !is.na(vacuna_i) & !is.na(vacuna_j) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_hijos_wide %>%
  freq(antip_total, cumul = F, report.nas = F)

# Vacunacion neumococo --------------------------------------------------------# 

# Neumococo 1
df_f2_hijos_wide %>%
  freq(vacuna_m, cumul = F, report.nas = F) 

# Neumococo 2
df_f2_hijos_wide %>%
  freq(vacuna_n, cumul = F, report.nas = F) 

# Neumococo 3
df_f2_hijos_wide %>%
  freq(vacuna_o, cumul = F, report.nas = F) 

# Indicador 
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(neumo_total = case_when(
    vacuna_m == 1 & vacuna_n == 1 & vacuna_o == 1 &
      (edaddias_m < 365 & !is.na(edaddias_m)) &
      (edaddias_n < 365 & !is.na(edaddias_n)) &
      (edaddias_o < 365 & !is.na(edaddias_o)) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 1,
    !is.na(vacuna_m) & !is.na(vacuna_n) & !is.na(vacuna_o) &
      (edaddias_nin >= 365 & !is.na(edaddias_nin)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_hijos_wide %>%
  freq(neumo_total, cumul = F, report.nas = F)

#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores y ser visibles en las funciones 
# Area
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(area = as_label(area)) 

df_f2_hijos_wide %>%
  freq(area, cumul = F, report.nas = F)

# Region 
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(region = as_label(region)) 

df_f2_hijos_wide %>%
  freq(region, cumul = F, report.nas = F)

# Provincia 
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(prov = as_label(prov)) 

df_f2_hijos_wide %>%
  freq(prov, cumul = F, report.nas = F)

# Parroquias priorizadas 
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(parr_pri = as_label(parr_pri)) 

df_f2_hijos_wide %>%
  freq(parr_pri, cumul = F, report.nas = F)

# Sexo
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(sexo = as_label(sexo)) 

df_f2_hijos_wide %>%
  freq(sexo, cumul = F, report.nas = F)

# Auto-Identificacion etnica 
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(etnia = as_label(etnia)) 

df_f2_hijos_wide %>%
  freq(etnia, cumul = F, report.nas = F)

# Quintil 
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(quintil = as_label(quintil)) 

df_f2_hijos_wide %>%
  freq(quintil, cumul = F, report.nas = F)

# Pobreza por Ingresos 
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(pobreza = as_label(pobreza)) 

df_f2_hijos_wide %>%
  freq(pobreza, cumul = F, report.nas = F)

# Pobreza por NBI
df_f2_hijos_wide <- df_f2_hijos_wide %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f2_hijos_wide %>%
  freq(nbi_1, cumul = F, report.nas = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f2_hijos_wide %>% as_survey_design(ids = "id_upm",
                                                       strata = "estrato",
                                                       weights = "fexp")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

# Para los resultados ponderados por diferentes desagregaciones reemplazar 
# respectivamente 

# Vacunacion BCG
survey_design %>%
  srvyr_prop(bcg)

survey_design %>%
  srvyr_prop_by(bcg, area)

# Vacunacion Hepatitis B 
survey_design %>%
  srvyr_prop(hepatitis)

survey_design %>%
  srvyr_prop_by(hepatitis, area)

# Vacunacion de rotavirus
survey_design %>%
  srvyr_prop(rota_total)

survey_design %>%
  srvyr_prop_by(rota_total, area)

# Vacunacion Pentavalente
survey_design %>%
  srvyr_prop(penta_total)

survey_design %>%
  srvyr_prop_by(penta_total, area)

# Vacunacion Antipoliomielitica
survey_design %>%
  srvyr_prop(antip_total)

survey_design %>%
  srvyr_prop_by(antip_total, area)

# Vacunacion neumococo
survey_design %>%
  srvyr_prop(neumo_total)

survey_design %>%
  srvyr_prop_by(neumo_total, area)

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
  tab_reg <- design %>% srvyr_prop_by(.data[[x]], {{ by_2 }})
  
  # Provincia
  tab_prov <- design %>% srvyr_prop_by(.data[[x]], {{ by_3 }})
  
  # Parroquias priorizadas  
  tab_parr <- design %>% filter(!is.na({{ by_4 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_4 }}) 
  
  # Sexo
  tab_sex <- design %>% srvyr_prop_by(.data[[x]], {{ by_5 }})
  
  # Auto-Identificacion etnica
  tab_etnia <- design %>% filter(!is.na({{ by_6 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_6 }})
  
  # Quintiles
  tab_quintil <- design %>% filter(!is.na({{ by_7 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_7 }})
  
  # Pobreza por Ingresos  
  tab_pobreza <- design %>% filter(!is.na({{ by_8 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_8 }}) 
  
  # Pobreza por NBI  
  tab_pobreza_nbi <- design %>% filter(!is.na({{ by_9 }})) %>% 
    srvyr_prop_by(.data[[x]], {{ by_9 }}) 
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac,tab_area,tab_reg, tab_prov, tab_parr, tab_sex, 
                         tab_etnia, tab_quintil,tab_pobreza, tab_pobreza_nbi)
  
  return(tab_final)
}

#==============================================================================#
####                 Obtencion de tablas en formato lista                   ####
#==============================================================================#

tab_full <- map(c("bcg","hepatitis","rota_total","penta_total",
                  "antip_total","neumo_total"), 
                ~tab_fun(design = survey_design, .x, area, region, 
                         prov, parr_pri, sexo, etnia, quintil,
                         pobreza, nbi_1))


#==============================================================================#
####                     Exportacion de tabulados                           ####
#==============================================================================#

Style_tab(tab_full[[1]],"T3_i1") # bcg
Style_tab(tab_full[[2]],"T3_i2") # hepatitis
Style_tab(tab_full[[3]],"T3_i3") # rota_total
Style_tab(tab_full[[4]],"T3_i4") # penta_total
Style_tab(tab_full[[5]],"T3_i5") # antip_total
Style_tab(tab_full[[6]],"T3_i6") # neumo_total

# si requiere importar las tablas por separado reemplazar la funcion "Style_tab"
# por "Style_tab_0"

