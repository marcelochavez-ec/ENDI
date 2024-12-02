
# Titulo de la Sintaxis: 
# Programas de primera infancia 

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
####          Calculo de los indicadores sobre salud en la ninez            ####
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

# Asistencia a un Centro de Desarrollo Infantil (CDI) o centro de educacion ---#
# inicial ---------------------------------------------------------------------# 

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s5_500, cumul = F) 

# Indicador
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(cdi = case_when(
    f2_s5_500 == 1 ~ 1,
    !is.na(f2_s5_500) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(cdi, cumul = F, report.nas = F)

# Visita por educadoras de Creciendo con Nuestros Hijos (CNH) -----------------# 
# para menores de 3 anios -----------------------------------------------------#

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s5_506, cumul = F)

# Indicador 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(cnh = case_when(
    f2_s5_506 == 1  & (edaddias < 1096 & !is.na(edaddias)) ~ 1,
    !is.na(f2_s5_506) & (edaddias < 1096 & !is.na(edaddias)) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(cnh, cumul = F, report.nas = F)

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

# CDI
survey_design %>%
  srvyr_prop(cdi)

survey_design %>%
  srvyr_prop_by(cdi, area)

# CNH 
survey_design %>%
  srvyr_prop(cnh)

survey_design %>%
  srvyr_prop_by(cnh, area)

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
    filter(!is.na({{ by_5 }})) %>% 
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

tab_full <- map(c("cdi", "cnh"),
                ~tab_fun(.x, design = survey_design,
                         area, region, prov, parr_pri, sexo, etnia, 
                         quintil, pobreza, nbi_1))

#==============================================================================#
####                     Exportacion de tabulados                           ####
#==============================================================================#

Style_tab(tab_full[[1]], "T7_i1") # cdi
Style_tab(tab_full[[2]], "T7_i2") # cnh

# si requiere importar las tablas por separado reemplazar la funcion "Style_tab"
# por "Style_tab_0"

