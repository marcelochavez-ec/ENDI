
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
  select(id_per, parr_pri, etnia, quintil, pobreza, nbi_1, f1_s1_2)

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
####              Calculo de los indicadores sobre fecundidad               ####
#==============================================================================#

# Embarazo deseado planeado ---------------------------------------------------#

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s4a_403, cumul = F, report.nas = F)

# Indicador 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(emb_plan = case_when(
    f2_s4a_403 == 1 ~ 1,
    f2_s4a_403 != 1 ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(emb_plan, cumul = F, report.nas = F)

# Embarazo deseado no previsto ------------------------------------------------#

# Revision 
df_f2_salud_ninez_new %>% 
  freq(f2_s4a_403, cumul = F, report.nas = F)

# Indicador 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(emb_noplan = case_when(
    f2_s4a_403 == 2 ~ 1,
    f2_s4a_403 != 2 ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(emb_noplan, cumul = F, report.nas = F)

# Embarazo no previsto --------------------------------------------------------#

# Revision
df_f2_salud_ninez_new %>% 
  freq(f2_s4a_403, cumul = F, report.nas = F)

# Indicador 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(emb_noprev = case_when(
    f2_s4a_403 == 3 | f2_s4a_403 == 4 ~ 1,
    f2_s4a_403 != 3 | f2_s4a_403 != 4 ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(emb_noprev, cumul = F, report.nas = F)

# Nacimientos institucionales -------------------------------------------------#

# Revision
df_f2_salud_ninez_new %>% 
  freq(f2_s4c_429_a, cumul = F, report.nas = F)

# Indicador 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(nac_inst = case_when(
    f2_s4c_429_a >= 1 & f2_s4c_429_a <= 8 ~ 1, 
    f2_s4c_429_a > 8 ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(nac_inst, cumul = F, report.nas = F)

# Nacimientos atendidos por personal de salud calificado ----------------------#

# Revision
df_f2_salud_ninez_new %>% 
  freq(f2_s4c_430_a, cumul = F, report.nas = F)

# Indicador 
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(nac_per_cal = case_when(
    f2_s4c_430_a >= 1 & f2_s4c_430_a <= 3 ~ 1,
    f2_s4c_430_a > 3 ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(nac_per_cal, cumul = F, report.nas = F)

# Cesaria ---------------------------------------------------------------------#

# Revision
df_f2_salud_ninez_new %>% 
  freq(f2_s4c_431, cumul = F, report.nas = F)

# Indicador
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(cesaria = case_when(
    f2_s4c_431 == 2 ~ 1, 
    !is.na(f2_s4c_431) ~ 0,
    TRUE ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(cesaria, cumul = F, report.nas = F)

# Control post parto antes de los siete dias ----------------------------------#

# Revision
df_f2_salud_ninez_new %>% 
  freq(f2_s4e_447, cumul = F, report.nas = F)

df_f2_salud_ninez_new %>% 
  descr(f2_s4e_448_a,
        stats = c("common"))

df_f2_salud_ninez_new %>% 
  descr(f2_s4e_448_b,
        stats = c("common"))

df_f2_salud_ninez_new %>% 
  descr(f2_s4e_448_c,
        stats = c("common"))

df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(sum_pos = f2_s4e_448_a + (f2_s4e_448_b * 7) + (f2_s4e_448_c * 30.44))

df_f2_salud_ninez_new %>% 
  select(f2_s4e_448_a, f2_s4e_448_b, f2_s4e_448_c, sum_pos)

df_f2_salud_ninez_new %>% 
  descr(sum_pos,
        stats = "common")

# Indicador
df_f2_salud_ninez_new <- df_f2_salud_ninez_new %>% 
  mutate(control_post = case_when(
    sum_pos < 7 & !is.na(sum_pos) ~ 1,
    sum_pos > 6 & !is.na(sum_pos) ~ 0,
    T ~ NA_real_
  ))

df_f2_salud_ninez_new %>% 
  freq(control_post, cumul = F)

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

# Embarazo deseado planeado
survey_design %>%
  srvyr_prop(emb_plan)

survey_design %>%
  srvyr_prop_by(emb_plan, area)

# Embarazo deseado no previsto 
survey_design %>%
  srvyr_prop(emb_noplan)

survey_design %>%
  srvyr_prop_by(emb_noplan, area)

# Embarazo no previsto 
survey_design %>%
  srvyr_prop(emb_noprev)

survey_design %>%
  srvyr_prop_by(emb_noprev, area)

# Nacimientos institucionales
survey_design %>%
  srvyr_prop(nac_inst)

survey_design %>%
  srvyr_prop_by(nac_inst, area)

# Nacimientos atendidos por personal de salud calificado
survey_design %>%
  srvyr_prop(nac_per_cal)

survey_design %>%
  srvyr_prop_by(nac_per_cal, area)

# Cesaria
survey_design %>%
  srvyr_prop(cesaria)

survey_design %>%
  srvyr_prop_by(cesaria, area)

# Control post parto antes de los siete dias
survey_design %>%
  srvyr_prop(control_post)

survey_design %>%
  srvyr_prop_by(control_post, area)

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

tab_full <- map(c("emb_plan","emb_noplan","emb_noprev",
                  "nac_inst","nac_per_cal","cesaria", "control_post"),
                ~tab_fun(.x, design = survey_design,
                         area, region, prov, parr_pri, sexo, etnia, 
                         quintil, pobreza, nbi_1))

#==============================================================================#
####                     Exportacion de tabulados                           ####
#==============================================================================#

Style_tab(tab_full[[1]],"T8_i3") # emb_plan
Style_tab(tab_full[[2]],"T8_i4") # emb_noplan
Style_tab(tab_full[[3]],"T8_i5") # emb_noprev
Style_tab(tab_full[[4]],"T8_i6") # nac_inst
Style_tab(tab_full[[5]],"T8_i7") # nac_per_cal
Style_tab(tab_full[[6]],"T8_i8") # cesaria
Style_tab(tab_full[[7]],"T8_i9") # control_post

# si requiere importar las tablas por separado reemplazar la funcion "Style_tab"
# por "Style_tab_0"

