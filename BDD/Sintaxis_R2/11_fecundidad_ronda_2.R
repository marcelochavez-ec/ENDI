
# Titulo de la Sintaxis: 
# Caracteristicas de fecundidad de las madres

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

# Base personas 
df_f1_personas <- readRDS(here(link_bdd, name_base_personas))
df_f1_personas <- as_tibble(df_f1_personas)

# Diccionario mef
dicc_f2_mef <- import(paste0(link_bdd, name_diccionario), 
                      which = "f2_mef",trust = TRUE)

dicc_f2_mef <- as_tibble(dicc_f2_mef)   

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
  select(starts_with("id"), area, prov, region, 
         fecha_anio, fecha_mes, fecha_dia,
         f2_s1_100_1, f2_s1_100_2, f2_s1_100_3,
         fexp, estrato,
         starts_with("f2_s2_235_")) 

# Cambio de la base de ancho a largo 
df_f2_hijos_long <- df_f2_hijos %>% 
  pivot_longer(
    cols = starts_with("f2_s2_235_"),
    names_to = c(".value", "ord_hijo"),
    names_pattern = "(.*)_(.*)"
  )

df_f2_hijos_long <- df_f2_hijos_long %>% 
  rename(cod = f2_s2_235_cod,
         dia = f2_s2_235_b_dia, 
         mes = f2_s2_235_b_mes,
         anio = f2_s2_235_b_anio
  )

df_f2_hijos_long <- df_f2_hijos_long %>% 
  mutate(cod = as.character(cod))

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
  )) %>% 
  mutate(id_per = case_when(
    !is.na(cod) ~ paste0(id_hogar, cod),
    TRUE ~ NA_character_
  ))

# Mantengo la base de los hijos registrados 
df_f2_hijos_long <- df_f2_hijos_long %>% 
  filter(!is.na(id_hijo_ord))

#------------------------------------------------------------------------------#
# Join 
#------------------------------------------------------------------------------#

# Variables de desagregacion para identificador mef 
df_f1_personas <- df_f1_personas %>% 
  select(id_mef, parr_pri, etnia, quintil, pobreza, nbi_1)

df_f2_hijos_long_new <- df_f2_hijos_long %>% 
  left_join(df_f1_personas, by = c("id_mef"))

rm(df_f2_hijos_long, df_f1_personas)

#==============================================================================#
####              Calculo de los indicadores sobre fecundidad               ####
#==============================================================================#

# Fecha de entrevista (CMC) ---------------------------------------------------#

df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(fecha_dia = as.double(fecha_dia),
         fecha_anio = as.double(fecha_anio),
         fecha_mes = as.double(fecha_mes))

df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(d_am_entr = fecha_dia / 30) %>% 
  mutate(d_am_entr = case_when(
    d_am_entr > 1 & !is.na(d_am_entr) ~ 1, 
    TRUE ~ d_am_entr 
  ))

df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(amd_entr = (fecha_anio - 1900) * 12 + fecha_mes + d_am_entr)

df_f2_hijos_long_new %>% 
  descr(amd_entr, 
        stats = "common")

# Imputacion ------------------------------------------------------------------#

# Imputacion fecha de nacimiento 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(dia = case_when(
    dia == 99 ~ 15,
    TRUE ~ dia 
  )) %>% 
  mutate(mes = case_when(
    mes == 99 ~ 6,
    TRUE ~ mes
  )) %>% 
  mutate(anio = na_if(anio, 9999))

# Imputacion de valores no sabe / no responde 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(f2_s2_235_e_dia = na_if(f2_s2_235_e_dia, 99)) %>% 
  mutate(f2_s2_235_e_mes = na_if(f2_s2_235_e_mes, 99)) %>% 
  mutate(f2_s2_235_e_anio = na_if(f2_s2_235_e_anio, 9999)) %>% 
  mutate(f2_s2_235_f_dias = na_if(f2_s2_235_f_dias, 99)) %>% 
  mutate(f2_s2_235_f_meses = na_if(f2_s2_235_f_meses, 99)) %>% 
  mutate(f2_s2_235_f_anios = na_if(f2_s2_235_f_anios, 99))

# Recuperacion ----------------------------------------------------------------#

# Recuperacion de anio de nacimiento
df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(anio_nac = f2_s2_235_e_anio - f2_s2_235_f_anios) %>% 
  mutate(anio = case_when(
    !is.na(anio_nac) & is.na(anio) ~ anio_nac,
    TRUE ~ anio 
  ))

# Recuperacion de anio de nacimiento (a partir de edad actual - vivo) 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(f2_s2_235_d_anios = na_if(f2_s2_235_d_anios, 99)) 

df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(anio_nac_new = fecha_anio - f2_s2_235_d_anios) %>% 
  mutate(anio = case_when(
    !is.na(anio_nac_new) & is.na(anio) ~ anio_nac_new, 
    TRUE ~ anio 
  ))

# Fecha de nacimiento de los hijos (CMC) --------------------------------------#

df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(d_am_b = dia / 30) %>% 
  mutate(d_am_b = case_when(
    d_am_b > 1 & !is.na(d_am_b) ~ 1, 
    TRUE ~ d_am_b 
  ))

df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(nacim = (anio - 1900) * 12 + mes + d_am_b)

df_f2_hijos_long_new %>% 
  descr(nacim, 
        stats = "common")

# Recuperacion con datos anteriores y posteriores 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  arrange(id_mef, desc(ord_hijo))

df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  group_by(id_mef) %>% 
  mutate(nacim_1 = lag(nacim)) %>% 
  mutate(nacim_2 = lead(nacim)) %>% 
  mutate(nacim_3 = (nacim_1 + nacim_2) / 2) %>% 
  ungroup()

df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(nacim = case_when(
    is.na(nacim) & !is.na(nacim_3) ~ nacim_3,
    TRUE ~ nacim 
  ))

df_f2_hijos_long_new %>% 
  descr(nacim, 
        stats = "common")

df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  group_by(id_mef) %>%
  arrange(id_mef, desc(ord_hijo)) %>%
  ungroup()

df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  group_by(id_mef) %>% 
  mutate(nacim_t =  nacim - lag(nacim)) %>%
  ungroup()

df_f2_hijos_long_new %>% 
  descr(nacim_t, 
        stats = "common")

df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(nacim_t = case_when(
    nacim_t < 0 & !is.na(nacim_t) ~ NA_real_,
    TRUE ~ nacim_t
  ))
# Nota: se excluyen casos debido a que por la imputacion de perdidos
# en fechas, las edad en meses no son consistentes entre un hijo mayor y 
# otro menor

df_f2_hijos_long_new %>% 
  descr(nacim_t, 
        stats = "common")

# Edad ninios 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(dov = paste(fecha_anio, fecha_mes, fecha_dia),
         dob = paste(anio, mes, dia),
         dov = as_date(dov),
         dob = as_date(dob),
         edaddias_nin = (dob %--% dov) / days(1),
         edadmeses_nin = trunc((dob %--% dov) / months(1)),
         edadanios_nin = trunc((dob %--% dov) / years(1))) 

df_f2_hijos_long_new %>% 
  descr(edaddias_nin, 
        stats = "common")

# Espaciamiento entre nacimientos ---------------------------------------------#

# Se quedan solo los casos de menor a 5 anios 
# Indicador
df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(nacim_t_5 = case_when(
    edaddias_nin < 1826 & !is.na(edaddias_nin) ~ nacim_t, 
    TRUE ~ NA_real_ 
  ))

df_f2_hijos_long_new %>% 
  descr(nacim_t_5, 
        stats = "common")

# Edad promedio al primer nacimiento ------------------------------------------#

# Edad madres 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(dobm = paste(f2_s1_100_3, f2_s1_100_2, f2_s1_100_1),
         dobm = as_date(dobm),
         edaddias_mad = (dobm %--% dov) / days(1),
         edadmeses_mad = trunc((dobm %--% dov) / months(1)),
         edadanios_mad = trunc((dobm %--% dov) / years(1)))

# Identificacion del hijo mayor 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  group_by(id_mef) %>% 
  mutate(hijo_mayor = case_when(
    rank(-edadanios_nin, na.last = "keep") == 1 ~ 1,
    TRUE ~ 0
  )) %>% 
  ungroup()

# Indicador 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>% 
  mutate(edad_primerh = case_when(
    hijo_mayor == 1 ~ (edadanios_mad - edadanios_nin),
    TRUE ~ NA_real_))

df_f2_hijos_long_new %>% 
  descr(edad_primerh, 
        stats = "common")

#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores y ser visibles en las funciones 
# Area
df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(area = as_label(area)) 

df_f2_hijos_long_new %>%
  freq(area, cumul = F, report.nas = F)

# Region 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(region = as_label(region)) 

df_f2_hijos_long_new %>%
  freq(region, cumul = F, report.nas = F)

# Provincia 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(prov = as_label(prov)) 

df_f2_hijos_long_new %>%
  freq(prov, cumul = F, report.nas = F)

# Parroquias priorizadas 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(parr_pri = as_label(parr_pri)) 

df_f2_hijos_long_new %>%
  freq(parr_pri, cumul = F, report.nas = F)

# Auto-Identificacion etnica 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(etnia = as_label(etnia)) 

df_f2_hijos_long_new %>%
  freq(etnia, cumul = F, report.nas = F)

# Quintil 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(quintil = as_label(quintil)) 

df_f2_hijos_long_new %>%
  freq(quintil, cumul = F, report.nas = F)

# Pobreza por Ingresos 
df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(pobreza = as_label(pobreza)) 

df_f2_hijos_long_new %>%
  freq(pobreza, cumul = F, report.nas = F)

# Pobreza por NBI
df_f2_hijos_long_new <- df_f2_hijos_long_new %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f2_hijos_long_new %>%
  freq(nbi_1, cumul = F, report.nas = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f2_hijos_long_new %>% as_survey_design(ids = "id_upm",
                                                           strata = "estrato",
                                                           weights = "fexp")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

# Para los resultados ponderados por diferentes desagregaciones reemplazar 
# respectivamente 

# Espaciamiento entre nacimientos
survey_design %>% 
  srvyr_mean(nacim_t_5)

survey_design %>% 
  srvyr_mean_by(nacim_t_5, area)

# Edad promedio al primer nacimiento
survey_design %>% 
  srvyr_mean(edad_primerh)

survey_design %>% 
  srvyr_mean_by(edad_primerh, area)

#==============================================================================#
####                     Funciones para los tabulados                       ####
#==============================================================================#
tab_fun <- function(design, x, by_1, by_2, by_3, by_4, by_5, by_6,
                    by_7, by_8) {  
  
  # Nacional 
  tab_nac <- design %>% 
    srvyr_mean(.data[[x]])
  
  # Area 
  tab_area <- design %>% 
    srvyr_mean_by(.data[[x]], {{ by_1 }}) 
  
  # Region
  tab_reg <- design %>%
    srvyr_mean_by(.data[[x]], {{by_2}})
  
  # Provincia
  tab_prov <- design %>%
    srvyr_mean_by(.data[[x]], {{ by_3 }})
  
  # Parroquias priorizadas  
  tab_parr <- design %>%
    filter(!is.na({{ by_4 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_4 }}) 
  
  # Auto-Identificacion etnica
  tab_etnia <- design %>%
    filter(!is.na({{ by_5 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_5 }})
  
  # Quintiles  
  tab_quintil <- design %>%
    filter(!is.na({{ by_6 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_6 }}) 
  
  # Pobreza por Ingresos  
  tab_pobreza <- design %>%
    filter(!is.na({{ by_7 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_7 }}) 
  
  # Pobreza por NBI  
  tab_pobreza_nbi <- design %>%
    filter(!is.na({{ by_8 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_8 }}) 
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac,tab_area, 
                         tab_reg, tab_prov, 
                         tab_parr, 
                         tab_etnia, tab_quintil,
                         tab_pobreza, tab_pobreza_nbi)
  
  return(tab_final)
}

#==============================================================================#
####                 Obtencion de tablas en formato lista                   ####
#==============================================================================#

tab_full <- map(c("edad_primerh","nacim_t"),
                ~tab_fun(.x, design = survey_design,
                         area, region, prov, parr_pri, etnia, 
                         quintil, pobreza, nbi_1))

#==============================================================================#
####                     Exportacion de tabulados                           ####
#==============================================================================#

Style_tab(tab_full[[1]], "T8_i1") # edad_primerh
Style_tab(tab_full[[2]], "T8_i2") # nacim_t

# si requiere importar las tablas por separado reemplazar la funcion "Style_tab"
# por "Style_tab_0"

