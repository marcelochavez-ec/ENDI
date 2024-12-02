
# Titulo de la Sintaxis: 
# Control prenatal

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
# Join - Base de hijos seccion 2 y base de salud de la ninez 
#------------------------------------------------------------------------------#

# Join 
df_f2_salud_ninez_new <- df_f2_salud_ninez %>% 
  inner_join(df_f2_hijos_long, by = c("id_hijo_ord")) 

rm(df_f2_hijos_long, df_f2_salud_ninez)

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
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  left_join(df_f1_personas_new, by = c("id_per"))

df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  left_join(df_f1_mef, by = c("id_mef"))

# Recuperacion de informacion de las hijas/os por medio de la informacion de la 
# madre, esto para las hijas/os que no son miembros de hogar o fallecidas/os

df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(parr_pri = coalesce(parr_pri, parr_pri_mef),
         etnia = coalesce(etnia, etnia_mef),
         quintil = coalesce(quintil, quintil_mef),
         pobreza = coalesce(pobreza, pobreza_mef),
         nbi_1 = coalesce(nbi_1, nbi_1_mef))

rm(df_f1_personas, df_f1_personas_new, df_f1_mef)

#==============================================================================#
####         Calculo de los indicadores sobre controles prenatales          ####
#==============================================================================#

# Estimacion de la edad en dias -----------------------------------------------#

df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(dob = paste(anio, mes, dia)) %>%
  mutate(dov = paste(fecha_anio, fecha_mes, fecha_dia)) %>%
  mutate(dob = as_date(dob)) %>%   
  mutate(dov = as_date(dov)) %>%  
  mutate(edaddias = (dob %--% dov) / days(1)) 

df_f2_salud_ninez_new %>%
  descr(edaddias, 
        stats = c("common")) 

# Controles prenatales --------------------------------------------------------#

# Revision 
df_f2_salud_ninez_new %>%
  descr(f2_s4b_406, 
        stats = c("common")) 

# Indicador (menores de 5 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(c_prenatales = f2_s4b_406)

df_f2_salud_ninez_new %>%
  descr(c_prenatales,
        stats = c("common"))

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(c_prenatales2 = case_when( 
    edaddias < 731 & !is.na(edaddias) ~ f2_s4b_406,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>%
  descr(c_prenatales2,
        stats = c("common"))

# Al menos 5 controles prenatales ----------------------------------------------#

df_f2_salud_ninez_new %>% 
  descr(f2_s4b_406, 
        stats = c("common"))

# Indicador (menores de 5 anios) 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(control5_5 = case_when(
    f2_s4b_406 >= 5 & !is.na(f2_s4b_406) ~ 1,
    f2_s4b_406 < 5 & !is.na(f2_s4b_406) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(control5_5, cumul = F, report.nas = F)

# Indicador (menores de 2 anios) 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(control5_2 = case_when(
    f2_s4b_406 >= 5 & !is.na(f2_s4b_406) &
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    f2_s4b_406 < 5 & !is.na(f2_s4b_406) &
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(control5_2, cumul = F, report.nas = F)

# Consumo de hierro y acido folico (durante el embarazo) ----------------------#

# Acido folico  
df_f2_salud_ninez_new %>% 
  freq(f2_s4b_409_a, cumul = F, report.nas = F)

# Hierro
df_f2_salud_ninez_new %>% 
  freq(f2_s4b_410_a, cumul = F, report.nas = F)

# Indicador (menores de 5 anios) 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(consumo_ha = case_when(
    f2_s4b_409_a == 1 & f2_s4b_410_a == 1 ~ 1,
    !is.na(f2_s4b_409_a) & !is.na(f2_s4b_410_a) ~ 0,
    TRUE ~ NA_real_
  )) 

df_f2_salud_ninez_new %>% 
  freq(consumo_ha, cumul = F, report.nas = F)

# Indicador (menores de 2 anios) 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(consumo_ha_2 = case_when(
    f2_s4b_409_a == 1 & f2_s4b_410_a == 1 & 
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    !is.na(f2_s4b_409_a) & !is.na(f2_s4b_410_a) &
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  )) 

df_f2_salud_ninez_new %>% 
  freq(consumo_ha_2, cumul = F, report.nas = F)

# Examen de VIH (antes de la semana 20) ---------------------------------------#

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s4b_415, cumul = F, report.nas = F)

# Indicador (menores de 5 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(vih_ant = case_when(
    f2_s4b_415 == 1 ~ 1,
    f2_s4b_415 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(vih_ant, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(vih_ant_2 = case_when(
    f2_s4b_415 == 1 &
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    f2_s4b_415 == 2 &
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(vih_ant_2, cumul = F, report.nas = F)

# Examen de VIH (despues de la semana 20) -------------------------------------#

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s4b_416, cumul = F, report.nas = F)

# Indicador (menores de 5 anios) 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(vih_des = case_when(
    f2_s4b_416 == 1 ~ 1,
    f2_s4b_416 == 2 ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(vih_des, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(vih_des_2 = case_when(
    f2_s4b_416 == 1 &
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    f2_s4b_416 == 2 &
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(vih_des_2, cumul = F, report.nas = F)

# Examenes de VIH (durante el embarazo) ---------------------------------------#

# Indicador (menores de 5 anios) 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(vih_dur = case_when(
    (f2_s4b_415 == 1 | f2_s4b_416 == 1) ~ 1,
    (f2_s4b_415 == 2 | f2_s4b_416 == 2) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(vih_dur, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(vih_dur_2 = case_when(
    (f2_s4b_415 == 1 | f2_s4b_416 == 1) &
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    (f2_s4b_415 == 2 | f2_s4b_416 == 2) &
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(vih_dur_2, cumul = F, report.nas = F)

# Examenes de orina (antes de la semana 20) -----------------------------------#

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s4b_420_a, cumul = F, report.nas = F)

# Indicador (menores de 5 anios) 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(ori_ant = case_when(
    f2_s4b_420_a == 1 ~ 1,
    f2_s4b_420_a == 2 ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(ori_ant, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(ori_ant_2 = case_when(
    f2_s4b_420_a == 1 &
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    f2_s4b_420_a == 2 &
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(ori_ant_2, cumul = F, report.nas = F)

# Examenes de orina (despues de la semana 20) ---------------------------------#

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s4b_421_a, cumul = F, report.nas = F)

# Indicador (menores de 5 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(ori_des = case_when(
    f2_s4b_421_a == 1 ~ 1,
    f2_s4b_421_a == 2 ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(ori_des, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(ori_des_2 = case_when(
    f2_s4b_421_a == 1 &
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    f2_s4b_421_a == 2 &
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(ori_des_2, cumul = F, report.nas = F)

# Examenes de orina (durante el embarazo) -------------------------------------#

# Indicador (menores de 5 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(ori_dur = case_when(
    (f2_s4b_420_a == 1 | f2_s4b_421_a == 1) ~ 1,
    (f2_s4b_420_a == 2 | f2_s4b_421_a == 2) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(ori_dur, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(ori_dur_2 = case_when(
    (f2_s4b_420_a == 1 | f2_s4b_421_a == 1) &
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    (f2_s4b_420_a == 2 | f2_s4b_421_a == 2) & 
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(ori_dur_2, cumul = F, report.nas = F)

# Examenes de TORCHs (antes de la semana 20) ----------------------------------#

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s4b_422_a, cumul = F, report.nas = F)

# Indicador
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(torchs_ant = case_when(
    f2_s4b_422_a == 1 ~ 1,
    f2_s4b_422_a == 2 ~ 0,
    TRUE ~ NA_real_ 
  ))

df_f2_salud_ninez_new %>% 
  freq(torchs_ant, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(torchs_ant_2 = case_when(
    f2_s4b_422_a == 1 &
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    f2_s4b_422_a == 2 &
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(torchs_ant_2, cumul = F, report.nas = F)

# Examenes de TORCHs (despues de la semana 20) --------------------------------#

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s4b_423_a, cumul = F, report.nas = F)

# Indicador
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(torchs_des = case_when(
    f2_s4b_423_a == 1 ~ 1,
    f2_s4b_423_a == 2 ~ 0,
    TRUE ~ NA_real_ 
  ))

df_f2_salud_ninez_new %>% 
  freq(torchs_des, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(torchs_des_2 = case_when(
    f2_s4b_423_a == 1 &
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    f2_s4b_423_a == 2 &
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(torchs_des_2, cumul = F, report.nas = F)

# Examenes de TORCHs (durante el embarazo) ------------------------------------#

# Indicador
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(torchs_dur = case_when(
    (f2_s4b_422_a == 1 |  f2_s4b_423_a == 1) ~ 1,
    (f2_s4b_422_a == 2 |  f2_s4b_423_a == 2) ~ 0,
    TRUE ~ NA_real_ 
  ))

df_f2_salud_ninez_new %>% 
  freq(torchs_dur, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(torchs_dur_2 = case_when(
    (f2_s4b_422_a == 1 |  f2_s4b_423_a == 1) &
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    (f2_s4b_422_a == 2 |  f2_s4b_423_a == 2) &
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_ 
  ))

df_f2_salud_ninez_new %>% 
  freq(torchs_dur_2, cumul = F, report.nas = F)

# Examenes de VIH, Orina y TORCHs (durante el embarazo) -----------------------#

# Indicador (menores de 5 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(vot_dur = case_when(
    (f2_s4b_415 == 1 | f2_s4b_416 == 1) &
      (f2_s4b_420_a == 1 | f2_s4b_421_a == 1) &
      (f2_s4b_422_a == 1 |  f2_s4b_423_a == 1) ~ 1,
    (!is.na(f2_s4b_415) & f2_s4b_415 != 88) |
      (!is.na(f2_s4b_416) & f2_s4b_416 != 88) |
      (!is.na(f2_s4b_420_a) & f2_s4b_420_a != 88) |
      (!is.na(f2_s4b_421_a) & f2_s4b_421_a != 88) |
      (!is.na(f2_s4b_422_a) & f2_s4b_422_a != 88) |
      (!is.na(f2_s4b_423_a) & f2_s4b_423_a != 88) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(vot_dur, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(vot_dur_2 = case_when(
    (f2_s4b_415 == 1 | f2_s4b_416 == 1) &
      (f2_s4b_420_a == 1 | f2_s4b_421_a == 1) &
      (f2_s4b_422_a == 1 |  f2_s4b_423_a == 1) &
      (edaddias < 731 & !is.na(edaddias)) ~ 1,
    ((!is.na(f2_s4b_415) & f2_s4b_415 != 88) |
       (!is.na(f2_s4b_416) & f2_s4b_416 != 88) |
       (!is.na(f2_s4b_420_a) & f2_s4b_420_a != 88) |
       (!is.na(f2_s4b_421_a) & f2_s4b_421_a != 88) |
       (!is.na(f2_s4b_422_a) & f2_s4b_422_a != 88) |
       (!is.na(f2_s4b_423_a) & f2_s4b_423_a != 88)) &
      (edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(vot_dur_2, cumul = F, report.nas = F)

# Vacuna contra tetanos y difteria --------------------------------------------#

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s4b_424_a, cumul = F, report.nas = F)

# Indicador (menores de 5 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(tet_dif = case_when(
    f2_s4b_424_a == 1 ~ 1,
    f2_s4b_424_a == 2 ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(tet_dif, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(tet_dif2 = case_when(
    f2_s4b_424_a == 1 & (edaddias < 731 & !is.na(edaddias)) ~ 1,
    f2_s4b_424_a == 2 & (edaddias < 731 & !is.na(edaddias))~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(tet_dif2, cumul = F, report.nas = F)

# Ecos Obtetricos -------------------------------------------------------------#

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s4b_426_a, cumul = F, report.nas = F)

# Indicador (menores de 5 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(ecos_obs = case_when(
    f2_s4b_426_a == 1 ~ 1,
    f2_s4b_426_a == 2 ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(ecos_obs, cumul = F, report.nas = F)

# Indicador (menores de 2 anios)
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(ecos_obs2 = case_when(
    f2_s4b_426_a == 1 &(edaddias < 731 & !is.na(edaddias)) ~ 1,
    f2_s4b_426_a == 2 &(edaddias < 731 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(ecos_obs2, cumul = F, report.nas = F)

#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores y ser visibles en las funciones 
# Area
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>%
  mutate(area = as_label(area)) 

df_f2_salud_ninez_new %>%
  freq(area, cumul = F, report.nas = F)

# Region 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>%
  mutate(region = as_label(region)) 

df_f2_salud_ninez_new %>%
  freq(region, cumul = F, report.nas = F)

# Provincia 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>%
  mutate(prov = as_label(prov)) 

df_f2_salud_ninez_new %>%
  freq(prov, cumul = F, report.nas = F)

# Parroquias priorizadas 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>%
  mutate(parr_pri = as_label(parr_pri)) 

df_f2_salud_ninez_new %>%
  freq(parr_pri, cumul = F, report.nas = F)

# Sexo
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>%
  mutate(sexo = as_label(sexo)) 

df_f2_salud_ninez_new %>%
  freq(sexo, cumul = F, report.nas = F)

# Auto-Identificacion etnica 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>%
  mutate(etnia = as_label(etnia)) 

df_f2_salud_ninez_new %>%
  freq(etnia, cumul = F, report.nas = F)

# Quintil 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>%
  mutate(quintil = as_label(quintil)) 

df_f2_salud_ninez_new %>%
  freq(quintil, cumul = F, report.nas = F)

# Pobreza por Ingresos 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>%
  mutate(pobreza = as_label(pobreza)) 

df_f2_salud_ninez_new %>%
  freq(pobreza, cumul = F, report.nas = F)

# Pobreza por NBI
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f2_salud_ninez_new %>%
  freq(nbi_1, cumul = F, report.nas = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f2_salud_ninez_new %>% as_survey_design(ids = "id_upm",
                                                            strata = "estrato",
                                                            weights = "fexp")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

# Para los resultados ponderados por diferentes desagregaciones reemplazar 
# respectivamente 

# Controles prenatales
# Menores de 5 anios
survey_design %>%
  srvyr_mean(c_prenatales)

survey_design %>%
  srvyr_mean_by(c_prenatales, area)

# Menores de 2 anios
survey_design %>%
  srvyr_mean(c_prenatales2)

survey_design %>%
  srvyr_mean_by(c_prenatales2, area)

# Al menos 5 controles prenatales
# Menores de 5 anios
survey_design %>%
  srvyr_prop(control5_5)

survey_design %>%
  srvyr_prop_by(control5_5, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(control5_2)

survey_design %>%
  srvyr_prop_by(control5_2, area)

# Consumo de hierro y acido folico (durante el embarazo) 
# Menores de 5 anios
survey_design %>%
  srvyr_prop(consumo_ha)

survey_design %>%
  srvyr_prop_by(consumo_ha, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(consumo_ha_2)

survey_design %>%
  srvyr_prop_by(consumo_ha_2, area)

# Examen de VIH (antes de la semana 20)
# Menores de 5 anios
survey_design %>%
  srvyr_prop(vih_ant)

survey_design %>%
  srvyr_prop_by(vih_ant, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(vih_ant_2)

survey_design %>%
  srvyr_prop_by(vih_ant_2, area)

# Examen de VIH (despues de la semana 20) 
# Menores de 5 anios
survey_design %>%
  srvyr_prop(vih_des)

survey_design %>%
  srvyr_prop_by(vih_des, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(vih_des_2)

survey_design %>%
  srvyr_prop_by(vih_des_2, area)

# Examenes de VIH (durante el embarazo)
# Menores de 5 anios
survey_design %>%
  srvyr_prop(vih_dur)

survey_design %>%
  srvyr_prop_by(vih_dur, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(vih_dur_2)

survey_design %>%
  srvyr_prop_by(vih_dur_2, area)

# Examenes de orina (antes de la semana 20)
# Menores de 5 anios
survey_design %>%
  srvyr_prop(ori_ant)

survey_design %>%
  srvyr_prop_by(ori_ant, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(ori_ant_2)

survey_design %>%
  srvyr_prop_by(ori_ant_2, area)

# Examenes de orina (despues de la semana 20)
# Menores de 5 anios
survey_design %>%
  srvyr_prop(ori_des)

survey_design %>%
  srvyr_prop_by(ori_des, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(ori_des_2)

survey_design %>%
  srvyr_prop_by(ori_des_2, area)

# Examenes de orina (durante el embarazo)
# Menores de 5 anios
survey_design %>%
  srvyr_prop(ori_dur)

survey_design %>%
  srvyr_prop_by(ori_dur, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(ori_dur_2)

survey_design %>%
  srvyr_prop_by(ori_dur_2, area)

# Examenes de TORCHs (antes de la semana 20)
# Menores de 5 anios
survey_design %>%
  srvyr_prop(torchs_ant)

survey_design %>%
  srvyr_prop_by(torchs_ant, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(torchs_ant_2)

survey_design %>%
  srvyr_prop_by(torchs_ant_2, area)

# Examenes de TORCHs (despues de la semana 20)
# Menores de 5 anios
survey_design %>%
  srvyr_prop(torchs_des)

survey_design %>%
  srvyr_prop_by(torchs_des, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(torchs_des_2)

survey_design %>%
  srvyr_prop_by(torchs_des_2, area)

# Examenes de TORCHs (durante el embarazo) 
# Menores de 5 anios
survey_design %>%
  srvyr_prop(torchs_dur)

survey_design %>%
  srvyr_prop_by(torchs_dur, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(torchs_dur_2)

survey_design %>%
  srvyr_prop_by(torchs_dur_2, area)

# Examenes de VIH, Orina y TORCHs (durante el embarazo) 
# Menores de 5 anios
survey_design %>%
  srvyr_prop(vot_dur)

survey_design %>%
  srvyr_prop_by(vot_dur, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(vot_dur_2)

survey_design %>%
  srvyr_prop_by(vot_dur_2, area)

# Vacuna contra tetanos y difteria
# Menores de 5 anios
survey_design %>%
  srvyr_prop(tet_dif)

survey_design %>%
  srvyr_prop_by(tet_dif, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(tet_dif2)

survey_design %>%
  srvyr_prop_by(tet_dif2, area)

# Ecos Obtetricos
# Menores de 5 anios
survey_design %>%
  srvyr_prop(ecos_obs)

survey_design %>%
  srvyr_prop_by(ecos_obs, area)

# Menores de 2 anios
survey_design %>%
  srvyr_prop(ecos_obs2)

survey_design %>%
  srvyr_prop_by(ecos_obs2, area)

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

tab_fun1 <- function(design, x, by_1, by_2, by_3, by_4, by_5, 
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
  
  # Provincia
  tab_prov <- design %>%
    srvyr_mean_by(.data[[x]], {{ by_3 }})
  
  # Parroquias priorizadas  
  tab_parr <- design %>%
    filter(!is.na({{ by_4 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_4 }}) 
  
  # Sexo
  tab_sex <- design %>%
    srvyr_mean_by(.data[[x]], {{ by_5 }})
  
  # Auto-Identificacion etnica
  tab_etnia <- design %>%
    filter(!is.na({{ by_6 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_6 }})
  
  # Quintiles
  tab_quintil <- design %>%
    filter(!is.na({{ by_7 }})) %>% 
    srvyr_mean_by(.data[[x]], {{ by_7 }})
  
  # Pobreza por Ingresos  
  tab_pobreza <- design %>%
    filter(!is.na({{ by_8 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_8 }}) 
  
  # Pobreza por NBI  
  tab_pobreza_nbi <- design %>%
    filter(!is.na({{ by_9 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_9 }}) 
  
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

tab_full <- map(c("control5_2", "consumo_ha_2", "vih_ant_2","vih_des_2",
                  "vih_dur_2", "ori_ant_2","ori_des_2","ori_dur_2",
                  "torchs_ant_2", "torchs_des_2","torchs_dur_2", "vot_dur_2", 
                  "tet_dif2", "ecos_obs2", "control5_5", "consumo_ha","vih_ant",
                  "vih_des","vih_dur","ori_ant","ori_des","ori_dur","torchs_ant",
                  "torchs_des","torchs_dur","vot_dur", "tet_dif", "ecos_obs"),
                ~tab_fun(.x, design = survey_design,
                         area, region, prov, parr_pri, sexo, etnia, 
                         quintil, pobreza, nbi_1))


tab_full1 <- map(c("c_prenatales","c_prenatales2"),
                 ~tab_fun1(.x, design = survey_design,
                           area, region, prov, parr_pri, sexo, etnia, 
                           quintil, pobreza, nbi_1))

#==============================================================================#
####                     Exportacion de tabulados                           ####
#==============================================================================#

Style_tab(tab_full1[[2]], "T5_i1") # c_prenatales2
Style_tab(tab_full[[1]], "T5_i2") # control5_2
Style_tab(tab_full[[2]], "T5_i3") # consumo_ha_2
Style_tab(tab_full[[3]], "T5_i4") # vih_ant_2
Style_tab(tab_full[[4]], "T5_i5") # vih_des_2
Style_tab(tab_full[[5]], "T5_i6") # vih_dur_2
Style_tab(tab_full[[6]], "T5_i7") # ori_ant_2
Style_tab(tab_full[[7]], "T5_i8") # ori_des_2
Style_tab(tab_full[[8]], "T5_i9") # ori_dur_2
Style_tab(tab_full[[9]], "T5_i10") # torchs_ant_2
Style_tab(tab_full[[10]], "T5_i11") # torchs_des_2
Style_tab(tab_full[[11]], "T5_i12") # torchs_dur_2
Style_tab(tab_full[[12]], "T5_i13") # vot_dur_2
Style_tab(tab_full[[13]], "T5_i14") # tet_dif2
Style_tab(tab_full[[14]], "T5_i15") # ecos_obs2
Style_tab(tab_full1[[1]], "T5_i16") # c_prenatales
Style_tab(tab_full[[15]], "T5_i17") # control5_5
Style_tab(tab_full[[16]], "T5_i18") # consumo_ha
Style_tab(tab_full[[17]], "T5_i19") # vih_ant
Style_tab(tab_full[[18]], "T5_i20") # vih_des
Style_tab(tab_full[[19]], "T5_i21") # vih_dur
Style_tab(tab_full[[20]], "T5_i22") # ori_ant
Style_tab(tab_full[[21]], "T5_i23") # ori_des
Style_tab(tab_full[[22]], "T5_i24") # ori_dur
Style_tab(tab_full[[23]], "T5_i25") # torchs_ant
Style_tab(tab_full[[24]], "T5_i26") # torchs_des
Style_tab(tab_full[[25]], "T5_i27") # torchs_dur
Style_tab(tab_full[[26]], "T5_i28") # vot_dur
Style_tab(tab_full[[27]], "T5_i29") # tet_dif
Style_tab(tab_full[[28]], "T5_i30") # ecos_obs

# si requiere importar las tablas por separado reemplazar la funcion "Style_tab"
# por "Style_tab_0"

