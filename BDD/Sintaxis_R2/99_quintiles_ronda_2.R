
# Titulo de la Sintaxis: 
# Quintiles 

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

# Base personas 
df_f1_personas <- readRDS(here(link_bdd, name_base_personas))
df_f1_personas <- as_tibble(df_f1_personas)

# Diccionario
dicc_f1_per <- import(paste0(link_bdd, name_diccionario), 
                      which = "f1_personas",trust = TRUE)

dicc_f1_per <- as_tibble(dicc_f1_per)   

#==============================================================================#
####                               Quintiles                                ####
#==============================================================================#

# Tratamiento 9s --------------------------------------------------------------# 

# Creacion de vector de variable involucradas 
var <- c("f1_s2_9", "f1_s2_10_2", "f1_s2_11", 
         "f1_s2_12", "f1_s2_13","f1_s2_14_2", 
         "f1_s2_15", "f1_s2_16_2", "f1_s2_17_2", 
         "f1_s2_18_2", "f1_s2_19_2", "f1_s2_20_2", 
         "f1_s2_22")

# Formato numerico para variables 
df_f1_personas <- df_f1_personas %>% 
  mutate(across(c(all_of(var)), as.numeric)) 

# Homologacion de 9s a una cantidad fija
for (i in var) {
  df_f1_personas[[i]] <- case_when(
    df_f1_personas[[i]] == 9999999 ~ 999999,
    df_f1_personas[[i]] == 99999999 ~ 999999,
    TRUE ~ df_f1_personas[[i]])
}

# Creacion de variable de control para diferentes escenarios
# x = 1:
# Si el individuo en su actividad laboral principal, no informa (no conoce) de 
# sus ingresos como dependiente o independiente, segun sea el caso. (Un 
# individuo solo puede tener ingresos como dependiente o independiente ya que 
# las categorias son excluyentes). Si el individuo no informa de ambas fuentes 
# de la actividad secundaria (monetario y especies) y no posee ingresos no 
# laborales.
# x = 2:
# Si el individuo no informa de ambas fuentes de la actividad secundaria 
# (monetario y especies) pero si posee ingresos no laborales, entonces el 
# ingreso individual total es el ingreso no laboral (esto ultimo se considera 
# mas adelante).

df_f1_personas <- df_f1_personas %>%
  mutate(x = case_when(
    (f1_s2_9 == 999999 | f1_s2_12 == 999999 ) |
      (f1_s2_15 == 999999 & f1_s2_16_2 == 999999 &
         is.na(f1_s2_17_2) & is.na(f1_s2_18_2) & is.na(f1_s2_19_2) & 
         is.na(f1_s2_20_2) & is.na(f1_s2_22)) ~ 1,
    (f1_s2_15 == 999999 & f1_s2_16_2 == 999999) & 
      (!is.na(f1_s2_17_2) | !is.na(f1_s2_18_2) | !is.na(f1_s2_19_2) |
         !is.na(f1_s2_20_2) | !is.na(f1_s2_22)) ~ 2,
    TRUE ~ 0))

# Control de 9s 
for (i in var) {
  df_f1_personas[[i]] <- case_when(
    df_f1_personas[[i]] == 999999 ~ NA_real_, 
    TRUE ~ df_f1_personas[[i]])
}

for (i in var) {
  df_f1_personas <- df_f1_personas %>%
    mutate(x = case_when(
      (!!sym(i) == 999 | !!sym(i) == 9999 | !!sym(i) == 99999) ~ 1, 
      TRUE ~ x))
}

# Ingreso laboral -------------------------------------------------------------#															   

# Actividad principal - Asalariados e Independientes
df_f1_personas <- df_f1_personas %>%
  mutate(f1_s2_11 = -f1_s2_11)

df_f1_personas <- df_f1_personas %>%
  rowwise() %>%
  mutate(ind = sum(c(f1_s2_9, f1_s2_10_2, f1_s2_11), na.rm = T)) %>%
  mutate(asal = sum(c(f1_s2_12, f1_s2_13, f1_s2_14_2), na.rm = T)) %>% 
  ungroup()

df_f1_personas <- df_f1_personas %>%
  rowwise() %>% 
  mutate(ila1 = sum(c(ind, asal), na.rm = T)) %>% 
  ungroup()

# Actividad secundaria - Asalariados e Independientes
df_f1_personas <- df_f1_personas %>%
  rowwise() %>% 
  mutate(ila2 = sum(c(f1_s2_15, f1_s2_16_2), na.rm = T)) %>% 
  ungroup()

# Ingreso Laboral
df_f1_personas <- df_f1_personas %>% 
  rowwise() %>% 
  mutate(ila = sum(c(ila1, ila2), na.rm = T)) %>% 
  ungroup()

df_f1_personas <- df_f1_personas %>%
  mutate(ila = case_when(
    ila1 < 0 ~ ila2,
    TRUE ~ ila))

# "ineg" es una variable que identifica a las personas 
# que gastan mas de lo que ganan monetariamente.

df_f1_personas <- df_f1_personas %>% 
  mutate(ineg = case_when(
    ila1 < 0 & ila == NA_real_ ~ 1,
    x == 1 ~ NA_real_))

# Ingreso no laboral ----------------------------------------------------------# 

# Rentas del capital, Propiedad, Transferencias y regalos 

# Ingresos de capital
df_f1_personas <- df_f1_personas %>%  
  mutate(icap = f1_s2_17_2)

# Ingresos por transferencias
df_f1_personas <- df_f1_personas %>% 
  mutate(ipens = f1_s2_18_2,
         ilocal = f1_s2_19_2,
         iextr = f1_s2_20_2,
         isocial = f1_s2_22)

df_f1_personas <- df_f1_personas %>% 
  rowwise() %>% 
  mutate(itrans = sum(c(ipens, ilocal, iextr, isocial), na.rm = T)) %>% 
  ungroup()

# Ingresos no laborales
df_f1_personas <- df_f1_personas%>%
  rowwise() %>% 
  mutate(inla = sum(c(icap, itrans), na.rm = T)) %>% 
  ungroup()

# Si es incoherente se hace missing todas las fuentes del ingreso individual
var1 <- c("ind", "asal", "ila", "icap", "ipens", 
          "ilocal", "iextr", "isocial", "itrans", 
          "inla")

for (i in var1) {
  df_f1_personas <- df_f1_personas %>% 
    mutate(!!sym(i) := case_when(
      x == 1 ~ NA_real_, 
      TRUE ~ !!sym(i))
    )
}

# Ingreso individual-----------------------------------------------------------#												 		   

df_f1_personas <- df_f1_personas %>% 
  rowwise() %>% 
  mutate(ii = sum(c(ila, inla), na.rm = T)) %>% 
  ungroup()

# Si es incoherente se hace missing el ingreso individual 
df_f1_personas <- df_f1_personas %>% 
  mutate(ii = case_when(
    x == 1 ~ NA_real_, 
    TRUE ~ ii))

# Si no informa de ambas fuentes de la actividad secundaria pero si posee
# ingresos no laborales, el ingreso individual total es el ingreso no laboral
df_f1_personas <- df_f1_personas %>% 
  mutate(ii = case_when(
    x == 2 ~ inla, 
    TRUE ~ ii))

# Si existen ingresos individuales "cero" se los reemplaza por NAs
df_f1_personas <- df_f1_personas %>% 
  mutate(ii = case_when(
    ii == 0 ~ NA_real_, 
    TRUE ~ ii))

# Ingreso familiar-------------------------------------------------------------#	

df_f1_personas <- df_f1_personas %>% 
  arrange(id_hogar) 

df_f1_personas <- df_f1_personas %>% 
  group_by(id_hogar) %>% 
  mutate(ilaf = sum(ila),
         icapf = sum(icap),
         ipensf = sum(ipens),
         ilocalf = sum(ilocal),
         iextrf = sum(iextr),
         isocialf = sum(isocial),
         itransf = sum(itrans),
         inlaf = sum(inla)) %>% 
  ungroup()

var2 <- c("ilaf", "icapf", "ipensf", "ilocalf", 
          "iextrf", "isocialf", "itransf", "inlaf")

for (i in var2) {
  df_f1_personas <- df_f1_personas %>% 
    mutate(!!sym(i) := case_when(
      !!sym(i) == 0 ~ NA_real_, 
      TRUE ~ !!sym(i))
    )
}

# Ingreso familiar
df_f1_personas <- df_f1_personas %>% 
  group_by(id_hogar) %>% 
  mutate(ih = sum((ii), na.rm = T)) %>% 
  ungroup()

df_f1_personas <- df_f1_personas %>% 
  mutate(ih = case_when(
    ih == 0 ~ NA_real_, 
    TRUE ~ ih))

# Ingreso percapita familiar---------------------------------------------------# 	

df_f1_personas <- df_f1_personas %>% 
  mutate(nump = 1)

# Ingreso percapita familiar
df_f1_personas <- df_f1_personas %>% 
  group_by(id_hogar) %>% 
  mutate(hsize = sum(nump)) %>% # Miembros por hogar   
  mutate(ipcf = ih / hsize) %>%  # Ingreso per capita familiar
  ungroup()

# Logaritmo del ingreso per capita familiar
df_f1_personas <- df_f1_personas %>% 
  mutate(lipcf = log(ipcf))

# Calculo de quintiles
df_f1_personas <-df_f1_personas%>%
  mutate(quintil = as.numeric(
    cut(lipcf, breaks = wtd.quantile(
      lipcf, w = fexp, probs = seq(0,1, length = 6), na.rm = TRUE),
      include.lowest = TRUE)))

df_f1_personas$quintil <- factor(df_f1_personas$quintil,
                                 levels = c("1","2","3","4","5"),
                                 labels = c("Quintil 1", "Quintil 2", 
                                            "Quintil 3", "Quintil 4", 
                                            "Quintil 5"))

df_f1_personas %>% 
  freq(quintil, cumul = F)

df_f1_personas <- df_f1_personas %>% 
  mutate(ipcf_new = ipcf)

#==============================================================================#
####                 Linea de pobreza y extrema pobre                       ####			  
#==============================================================================#

df_f1_personas <- df_f1_personas %>% 
  mutate(ipc_ = as.numeric(case_when(
    fecha_anio == "2023" & 
      fecha_mes == "07" ~ 111.181541477615, 
    fecha_anio == "2023" & 
      fecha_mes == "08" ~ 111.780858152468, 
    fecha_anio == "2023" & 
      fecha_mes == "09" ~ 112.343081099914, 
    fecha_anio == "2023" & 
      fecha_mes == "10" ~ 112.385606673683, 
    fecha_anio == "2023" & 
      fecha_mes == "11" ~ 112.188911619948,
    fecha_anio == "2023" & 
      fecha_mes == "12" ~ 111.741053926941,
    fecha_anio == "2024" & 
      fecha_mes == "01" ~ 111.715101416755,
    fecha_anio == "2024" & 
      fecha_mes == "02" ~ 111.855131486751,
    fecha_anio == "2024" & 
      fecha_mes == "03" ~ 111.958952108953,
    fecha_anio == "2024" & 
      fecha_mes == "04" ~ 112.281439749787,
    fecha_anio == "2024" & 
      fecha_mes == "05" ~ 113.710894638438,
    fecha_anio == "2024" & 
      fecha_mes == "06" ~ 113.575358026089,
    fecha_anio == "2024" & 
      fecha_mes == "07" ~ 112.494039825892,
    fecha_anio == "2024" & 
      fecha_mes == "08" ~ 113.540676380544,
  )))

df_f1_personas <- df_f1_personas %>% 
  rename(ingtot_pc = ipcf)

# Linea de pobreza
df_f1_personas <- df_f1_personas %>% 
  mutate(lpobre = 56.64 * (ipc_ / 70.262819184092)) 

df_f1_personas <- df_f1_personas %>% 
  mutate(pobreza = case_when(
    ingtot_pc < lpobre ~ 1,
    !is.na(ingtot_pc) ~ 0))

df_f1_personas %>% 
  freq(pobreza, cumul = F)

# Linea de extrema pobreza
df_f1_personas <- df_f1_personas %>% 
  mutate(lepobre = 31.92 * (ipc_ / 70.262819184092))

df_f1_personas <- df_f1_personas %>% 
  mutate(epobreza = case_when(
    ingtot_pc < lepobre ~ 1,
    !is.na(ingtot_pc) ~ 0))

df_f1_personas %>% 
  freq(epobreza, cumul = F)

rm(i, var, var1, var2)

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

# Quintiles 
survey_design %>% 
  srvyr_freq(quintil)

# Pobreza
survey_design %>% 
  srvyr_prop(pobreza)

# Pobreza extrema 
survey_design %>% 
  srvyr_prop(epobreza)

