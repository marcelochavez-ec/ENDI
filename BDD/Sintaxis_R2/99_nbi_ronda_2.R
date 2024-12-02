
# Titulo de la Sintaxis: 
# Necesidades Basicas Insatisfechas (NBI)

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
####                          Carga de base de datos                        ####
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
####               Necesidades Basicas Insatisfechas (NBI)                  ####
#==============================================================================#

# El metodo abarca cinco dimensiones y dentro de cada dimension existe 
# indicadores que miden privaciones

# Capacidad economica ---------------------------------------------------------#

# El hogar se considera privado de esta dimension si: 
# i) Los anios de escolarida del jefe(a) del hogar es menor o igual a 2 anios y; 
# ii) Existen mas de tres personas por cada persona ocupada del hogar 

# Escolaridad del jefe de hogar 
df_f1_perhog %>% 
  freq(f1_s1_15_1, cumul = F)

df_f1_perhog %>% 
  descr(f1_s1_15_2,
        stats = c("common"))

df_f1_perhog <- df_f1_perhog %>%
  mutate(escol = case_when(
    f1_s1_15_1 == 1 & f1_s1_1 == 1 ~ 0, # ninguno    
    f1_s1_15_1 == 4 & (f1_s1_15_2 >= 0 & f1_s1_15_2 <= 3) & 
      f1_s1_1 == 1 ~ 2 * f1_s1_15_2, # Alfabetizacion
    f1_s1_15_1 == 4 & (f1_s1_15_2 >= 4 & f1_s1_15_2 <= 10) &
      f1_s1_1 == 1 ~ 3 + f1_s1_15_2, # Alfabetizacion
    f1_s1_15_1 == 5 & f1_s1_1 == 1 ~ 1 + f1_s1_15_2, # Primaria 
    f1_s1_15_1 == 6 & f1_s1_1 == 1 ~ f1_s1_15_2, # Educacion Basica   
    f1_s1_15_1 == 7 & f1_s1_1 == 1 ~ 7 + f1_s1_15_2, # Secundaria 
    f1_s1_15_1 == 8 & f1_s1_1 == 1 ~ 10 + f1_s1_15_2, # Bachiller  
    f1_s1_15_1 == 9 & f1_s1_1 == 1 ~ 13 + f1_s1_15_2, # Ciclo postbachillerato (no enemdu)
    f1_s1_15_1 == 10 & f1_s1_1 == 1 ~ 13 + f1_s1_15_2, # Educacion tecnica (diferente categoria en enemdu)
    f1_s1_15_1 == 11 & f1_s1_1 == 1 ~ 13 + f1_s1_15_2, # Educacion superior (diferente categoria en enemdu)
    f1_s1_15_1 == 12 & f1_s1_1 == 1 ~ 18 + f1_s1_15_2, # Maestria (diferente categoria en enemdu)
    f1_s1_15_1 == 13 & f1_s1_1 == 1 ~ 18 + f1_s1_15_2, # PHD (diferente categoria en enemdu) 
    TRUE ~ NA_real_
  ))

df_f1_perhog %>% 
  descr(escol,
        stats = c("common"))

df_f1_perhog <- df_f1_perhog %>%
  mutate(escjefe = case_when(
    (escol < 3 & !is.na(escol)) ~ 1, 
    (escol >= 3 & !is.na(escol)) ~ 0,    
    TRUE ~ NA_real_ 
  ))

df_f1_perhog %>%
  freq(escjefe, cumul = F, report.nas = F)

fmax = function(x, na.rm = TRUE) {
  if(all(is.na(x))) return(x[1])
  return(max(x, na.rm = na.rm))
}

# Resultado reflejado para todos los miembros del hogar 
df_f1_perhog <- df_f1_perhog %>%
  group_by(id_hogar) %>%
  mutate(escjefe_1 = fmax(escjefe)) %>% 
  ungroup()

df_f1_perhog %>%
  freq(escjefe_1, cumul = F, report.nas = F)

# Ocupacion 
df_f1_perhog <- df_f1_perhog %>%
  mutate(ocup = case_when(
    f1_s1_3_1 >= 15 & (f1_s2_1 >= 1 & f1_s2_1 <= 6) ~ 1,
    TRUE ~ 0
  )) 

# Numeros de ocupados en el hogar 
df_f1_perhog <- df_f1_perhog %>%
  group_by(id_hogar) %>% 
  mutate(numper_ocu = sum(ocup)) %>% 
  ungroup()

df_f1_perhog %>%
  descr(numper_ocu, 
        stats = c("common"))

# Numero de personas en el hogar 
df_f1_perhog <- df_f1_perhog %>%
  mutate(pers = 1)

df_f1_perhog <- df_f1_perhog %>%
  group_by(id_hogar) %>% 
  mutate(numpers = sum(pers)) %>% 
  ungroup()

df_f1_perhog %>%
  descr(numpers, 
        stats = c("common"))

# Dependencia economica 
df_f1_perhog <- df_f1_perhog %>%
  mutate(depec = case_when(
    (numpers / numper_ocu) > 3 & numper_ocu > 0 & escjefe_1 == 1 ~ 1, 
    numper_ocu == 0 & escjefe_1 == 1 ~ 1, 
    TRUE ~ 0 
  ))

df_f1_perhog %>%
  freq(depec, cumul = F, report.nas = F)

# Acceso a educacion basica ---------------------------------------------------#

# El hogar se considera privado en esta dimension si: existe en el hogar ninos 
# de 6 a 12 anios de edad que no asisten a clases

df_f1_perhog %>%
  descr(f1_s1_3_1, 
        stats = c("common"))

df_f1_perhog %>%
  freq(f1_s1_13, cumul = F, report.nas = F)

df_f1_perhog <- df_f1_perhog %>%
  mutate(ninastesc = case_when(
    (f1_s1_3_1 > 5 & f1_s1_3_1 < 13) & f1_s1_13 == 2 ~ 1,
    (f1_s1_3_1 > 5 & f1_s1_3_1 < 13) & f1_s1_13 == 1 ~ 0,
    TRUE ~ NA_real_
  ))

df_f1_perhog %>%
  freq(ninastesc, cumul = F, report.nas = F)

# Resultado reflejado para todos los miembros del hogar 
df_f1_perhog <- df_f1_perhog %>%
  group_by(id_hogar) %>%
  mutate(ninastesc_1 = fmax(ninastesc)) %>% 
  ungroup()  

df_f1_perhog %>%
  freq(ninastesc_1, cumul = F, report.nas = F) 

df_f1_perhog <- df_f1_perhog %>%
  mutate(ninastesc_1 = case_when(
    is.na(ninastesc_1) ~ 0,
    TRUE ~ ninastesc_1
  ))

df_f1_perhog %>%
  freq(ninastesc_1, cumul = F, report.nas = F)

# Acceso a vivienda -----------------------------------------------------------#

# El hogar esta privado si:
# i) El material del piso es de tierra u de otros materiales
# ii) El material de las paredes son de cana, estera u otros

df_f1_perhog %>%
  freq(f1_s3_7, cumul = F, report.nas = F)

df_f1_perhog %>%
  freq(f1_s3_5, cumul = F, report.nas = F)

df_f1_perhog <- df_f1_perhog %>%
  mutate(matviv_def = case_when(
    (f1_s3_7 == 7 | f1_s3_7 == 8) | (f1_s3_5 == 7 | f1_s3_5 == 8) ~ 1, 
    TRUE ~ 0 
  ))

df_f1_perhog %>%
  freq(matviv_def, cumul = F, report.nas = F)

# Acceso a servicios basicos --------------------------------------------------#

# La dimension considera las condiciones sanitarias de la vivienda.
# El hogar es pobre si:
# i) La vivienda no tiene servicio higienico o si lo tiene es 
#    por pozo ciego o letrina 
# ii) Si el agua que obtiene la vivienda no es 
#     por red publica o por otra fuente de tuberia 

df_f1_perhog %>%
  freq(f1_s3_11, cumul = F, report.nas = F)

df_f1_perhog %>%
  freq(f1_s3_10, cumul = F, report.nas = F)

df_f1_perhog <- df_f1_perhog %>%
  mutate(ser_viv = case_when(
    (f1_s3_11 >= 4 & f1_s3_11 <= 7) | 
      (f1_s3_10 >= 3 & f1_s3_10 <= 5) ~ 1, 
    TRUE ~ 0 
  ))

df_f1_perhog %>%
  freq(ser_viv, cumul = F, report.nas = F)

# Hacinamiento ----------------------------------------------------------------#

# El hogar se considera pobre si la relación de personas por dormitorio es 
# mayor a tres

df_f1_perhog %>% 
  descr(f1_s3_16,
        stats = c("common"))

df_f1_perhog <- df_f1_perhog %>%
  mutate(f1_s3_16_dor = case_when(
    f1_s3_16 == 0 ~ 1, 
    TRUE ~ f1_s3_16
  ))

# Relacion entre numero de personas y dormitorios
df_f1_perhog <- df_f1_perhog %>%
  mutate(percuart = numpers / f1_s3_16_dor)

df_f1_perhog <- df_f1_perhog %>%
  mutate(hacm = case_when(
    !is.na(percuart) & percuart > 3 ~ 1, 
    !is.na(percuart) & percuart <= 3 ~ 0,
    TRUE ~ NA_real_
  ))

df_f1_perhog %>%
  freq(hacm, cumul = F, report.nas = F)

# Calculo del indicador de NBI ------------------------------------------------#

df_f1_perhog <- df_f1_perhog %>%
  rowwise() %>% 
  mutate(nbi = sum(c(depec, ninastesc_1, matviv_def, ser_viv, hacm) == "1")) %>%
  ungroup()

# Pobreza por nbi 
df_f1_perhog <- df_f1_perhog %>%
  mutate(nbi_1 = case_when(
    !is.na(nbi) & nbi >= 1 ~ 1,
    TRUE ~ 0 
  ))

df_f1_perhog %>%
  freq(nbi_1, cumul = F, report.nas = F)

# Pobreza extrema por nbi 
df_f1_perhog <- df_f1_perhog %>%
  mutate(nbi_2 = case_when(
    !is.na(nbi) & nbi >= 2 ~ 1,
    TRUE ~ 0 
  ))

df_f1_perhog %>%
  freq(nbi_2, cumul = F, report.nas = F)

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

# Pobreza por NBI 
survey_design %>% 
  srvyr_prop(nbi_1)

# Pobreza extrema por NBI 
survey_design %>% 
  srvyr_prop(nbi_2)

