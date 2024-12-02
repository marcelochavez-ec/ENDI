
# Titulo de la Sintaxis: 
# Desarrollo Infantil 

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
####                          Carga de base de datos                        ####
#==============================================================================#

# Base personas 
df_f1_personas <- readRDS(here(link_bdd, name_base_personas))
df_f1_personas <- as_tibble(df_f1_personas)
df_f1_personas

# Base Desarrollo Infantil 
df_f3 <- readRDS(here(link_bdd, name_base_di))
df_f3 <- as_tibble(df_f3)
df_f3

# Diccionario
# Personas 
dicc_f1_per <- import(paste0(link_bdd, name_diccionario), 
                      which = "f1_personas",trust = TRUE)

dicc_f1_per <- as_tibble(dicc_f1_per)  

# Desarrollo Infantil 
dicc_f3 <- import(paste0(link_bdd, name_diccionario_di), 
                  which = "f3_desarrollo_infantil",trust = TRUE)

dicc_f3 <- as_tibble(dicc_f3)  

#------------------------------------------------------------------------------#
# Join 
#------------------------------------------------------------------------------#

# Creacion de identificador de madre para cada hijo 
df_f1_personas %>% 
  select(f1_s1_12, f1_s1_12_1)

df_f1_personas <- df_f1_personas %>% 
  mutate(f1_s1_12_1 = as.character(f1_s1_12_1)) %>% 
  mutate(f1_s1_12_1 = case_when(
    nchar(f1_s1_12_1) == 1 ~ paste0(0, f1_s1_12_1),
    TRUE ~ f1_s1_12_1
  ))

df_f1_personas <- df_f1_personas %>% 
  mutate(id_madre = case_when(
    !is.na(f1_s1_12_1) ~ paste0(id_hogar, f1_s1_12_1),
    TRUE ~ NA
  ))

df_f1_personas %>% 
  select(id_per, f1_s1_12, f1_s1_12_1, id_madre)

# Nivel de instruccion 
df_f1_personas <- df_f1_personas %>% 
  mutate(nivins = case_when(
    (f1_s1_15_1 >= 1 & f1_s1_15_1 <= 4) ~ 1, 
    (f1_s1_15_1 == 5 | f1_s1_15_1 == 6 | 
       (f1_s1_15_1 == 7 & f1_s1_15_2 < 4 & !is.na(f1_s1_15_2))) ~ 1,
    ((f1_s1_15_1 == 7 & f1_s1_15_2 > 3 & !is.na(f1_s1_15_2)) | 
       f1_s1_15_1 == 8) ~ 2,
    (f1_s1_15_1 >= 9 & f1_s1_15_1 <= 13) ~ 3,
    TRUE ~ NA_real_
  ))

df_f1_personas = apply_labels(df_f1_personas,
                              nivins = num_lab(
                                "1 Ninguno/Educación Básica 
                                 2 Educación Media/Bachillerato
                                 3 Superior" 
                              ))
df_f1_personas %>% 
  freq(nivins, cumul = F)

# Variables de desagregacion   
df_f1_madre <- df_f1_personas %>% 
  select(id_per, nivins) %>% 
  rename(id_madre = id_per,
         nivins_madre = nivins)

# Variables necesarias para desagracion del ninio 
df_f1_personas <- df_f1_personas %>% 
  select(id_per, id_madre, f1_s1_2, f1_s1_11, f1_s1_12,
         etnia, quintil, pobreza, nbi_1, 
         dcronica)

# Join 
df_f1_personas_new <- df_f1_personas %>% 
  left_join(df_f1_madre, by = c("id_madre"))

df_f1_personas_new %>% 
  freq(nivins_madre, cumul = F, report.nas = F)

# Join 
df_f3 <- df_f3 %>% 
  inner_join(df_f1_personas_new, by = c("id_per"))

rm(df_f1_personas, df_f1_madre, df_f1_personas_new)

#==============================================================================#
####                         Calculo de indicadores                         ####
#==============================================================================#

#------------------------------------------------------------------------------#
# Edad en dias 
#------------------------------------------------------------------------------#

df_f3 <- df_f3 %>% 
  mutate(dob = paste(f3_s0_1b_anio, f3_s0_1b_mes, f3_s0_1b_dia)) %>%
  mutate(dov = paste(fecha_anio, fecha_mes, fecha_dia)) %>%
  mutate(dob = as_date(dob)) %>%   
  mutate(dov = as_date(dov)) %>%  
  mutate(edaddias_nin = (dob %--% dov) / days(1)) %>% 
  mutate(edadmeses_nin = trunc((dob %--% dov) / months(1))) 

df_f3 %>% 
  descr(edadmeses_nin,
        c("common"))

#------------------------------------------------------------------------------#
# Promocion de desarrollo en el hogar 
#------------------------------------------------------------------------------#

# Variedad de actividades de juego --------------------------------------------# 

for (i in letters[1:7]) {
  act <- paste0("act", i)
  df_f3 <- df_f3 %>% 
    mutate(!!act := case_when(
      !!sym(paste0("f3_s1_105_", i)) == 2 ~ 0,
      TRUE ~ 1
    ))
} 

df_f3 <- df_f3 %>% 
  rowwise() %>% 
  mutate(mh_activ = sum(c(acta, actb, actc, actd, acte, actf, actg), 
                        na.rm = T)) %>% 
  ungroup()

df_f3 %>%
  descr(mh_activ,
        c("common"))

df_f3 <- df_f3 %>% 
  select(-starts_with("act"))

# Ninias/os que realizan cuatro o mas actividades de estimulacion 
# con cualquier cuidador en los ultimos 3 dias

df_f3 <- df_f3 %>% 
  mutate(mh_act = case_when(
    mh_activ >= 4 & !is.na(mh_activ) ~ 1,
    !is.na(mh_activ) ~ 0))

df_f3 %>% 
  freq(mh_act, cumul = F)

# Variedad de actividades con la madre ----------------------------------------#

for (i in letters[1:7]) {
  act <- paste0("act_", i)
  df_f3 <- df_f3 %>% 
    mutate(!!act := case_when(
      !!sym(paste0("f3_s1_105_", i)) == 1 &
        !!sym(paste0("f3_s1_105_", i, "1_1")) == 1 ~ 1,
      !!sym(paste0("f3_s1_105_", i)) == 2 ~ 0,
      TRUE ~ NA_real_
    ))
}

df_f3 <- df_f3 %>% 
  rowwise() %>%
  mutate(actividades_m = sum(c(act_a, act_b, act_c, act_d, 
                               act_e, act_f, act_g), na.rm = T)) %>% 
  ungroup()

df_f3 %>%
  descr(actividades_m,
        c("common"))

df_f3 <- df_f3 %>% 
  select(-starts_with("act_"))

# Ninias/os que realizan cuatro o mas actividades de estimulacion 
# con la madre en los ultimos 3 dias

df_f3 <- df_f3 %>% 
  mutate(actividades_m = case_when(
    f1_s1_12 == 2 ~ NA_real_,
    TRUE ~ actividades_m))

df_f3 <- df_f3 %>% 
  mutate(madre_act = case_when(
    actividades_m >= 4 & !is.na(actividades_m) ~ 1,
    !is.na(actividades_m) ~ 0))

df_f3 %>% 
  freq(madre_act, cumul = F)

# Variedad de actividades con el padre ----------------------------------------#

for (i in letters[1:7]) {
  act <- paste0("act_", i)
  df_f3 <- df_f3 %>% 
    mutate(!!act := case_when(
      !!sym(paste0("f3_s1_105_", i)) == 1 &
        !!sym(paste0("f3_s1_105_", i, "1_2")) == 1 ~ 1,
      !!sym(paste0("f3_s1_105_", i)) == 2 ~ 0,
      TRUE ~ NA_real_
    ))
}

df_f3 <- df_f3 %>% 
  rowwise() %>%
  mutate(actividades_p = sum(c(act_a, act_b, act_c, act_d, 
                               act_e, act_f, act_g), na.rm = T)) %>% 
  ungroup()

df_f3 %>%
  descr(actividades_p,
        c("common"))

df_f3 <- df_f3 %>% 
  select(-starts_with("act_"))

# Ninias/os que realizan cuatro o mas actividades de estimulacion 
# con en el padre en los ultimos 3 dias

df_f3 <- df_f3 %>% 
  mutate(actividades_p = case_when(
    f1_s1_11 == 2 ~ NA_real_,
    TRUE ~ actividades_p))

df_f3 <- df_f3 %>% 
  mutate(padre_act = case_when(
    actividades_p >= 4 & !is.na(actividades_p) ~ 1,
    !is.na(actividades_p) ~ 0))

df_f3 %>% 
  freq(padre_act, cumul = F)

# Libros infantiles -----------------------------------------------------------#

# Promedio de libros infantiles para ninios en el hogar

df_f3 %>% 
  descr(f3_s1_102,
        c("common"))

df_f3 <- df_f3 %>% 
  mutate(f3_s1_102 = as.double(f3_s1_102))

df_f3 <- df_f3 %>% 
  mutate(libros = f3_s1_102,
         libros = case_when(
           libros == 88 ~ NA_real_,
           TRUE ~ libros
         ))

# Ninias/os con al menos 1 libro en el hogar 

df_f3 <- df_f3 %>% 
  mutate(libro_1 = case_when(
    libros >= 1 & !is.na(libros) ~ 1,
    !is.na(libros) ~ 0,
    TRUE ~ NA_real_
  ))

df_f3 %>% 
  freq(libro_1, cumul = F)

# Ninias/os con al menos 3 libros en el hogar 

df_f3 <- df_f3 %>% 
  mutate(libro_3 = case_when(
    libros >= 3 & !is.na(libros) ~ 1,
    !is.na(libros) ~ 0
  ))

df_f3 %>% 
  freq(libro_3, cumul = F)

#------------------------------------------------------------------------------#
# Actividades promotoras de juego 
#------------------------------------------------------------------------------#

# Juguetes en el hogar --------------------------------------------------------# 

for (i in letters[4:7]) {
  
  juego <- paste0("juego_", i)
  
  df_f3 <- df_f3 %>% 
    mutate(!!sym(paste0("f3_s1_100_", i)) := 
             as.double(!!sym(paste0("f3_s1_100_", i))))
  
  df_f3 <- df_f3 %>% 
    mutate(!!sym(paste0("f3_s1_100_", i)) := ifelse(
      !!sym(paste0("f3_s1_100_", i)) == 8, NA_real_,
      !!sym(paste0("f3_s1_100_", i))
    ))
  
  df_f3 <- df_f3 %>% 
    mutate(!!juego := ifelse(
      !is.na(!!sym(paste0("f3_s1_100_", i))), 0,
      !!sym(paste0("f3_s1_100_", i))))
  
  df_f3 <- df_f3 %>% 
    mutate(!!sym(paste0("juego_", i)) := ifelse(
      !!sym(paste0("f3_s1_100_", i)) == 1, 1,
      !!sym(paste0("juego_", i))))
  
}

df_f3 <- df_f3 %>% 
  rowwise() %>% 
  mutate(tipo_juguete = sum(c(juego_d, juego_e, juego_f, 
                              juego_g, libro_1), na.rm = T)) %>% 
  ungroup()

df_f3 %>% 
  descr(tipo_juguete, 
        c("common"))

# Ninias/os con al menos tres juguetes en el hogar 

df_f3 <- df_f3 %>% 
  mutate(juguete_3 = case_when(
    tipo_juguete >= 3 & !is.na(tipo_juguete) ~ 1,
    !is.na(tipo_juguete) ~ 0
  ))

df_f3 %>% 
  freq(juguete_3, cumul = F)

df_f3 <- df_f3 %>% 
  select(-starts_with("juego_"))

#------------------------------------------------------------------------------#
# Practicas de disciplina - libre
#------------------------------------------------------------------------------#

# Ninias/os entre 1 a menores de 5 anios que estan libres de maltrato fisico en el ultimo mes             

df_f3 <- df_f3 %>% 
  mutate(lmaltrato_f5 = case_when(
    (f3_s2a_200_c == 2 & f3_s2a_200_f == 2 & f3_s2a_200_g == 2 & 
       f3_s2a_200_i == 2 & f3_s2a_200_j == 2 & f3_s2a_200_k == 2 & 
       f3_s2a_200_m == 2) ~ 1, 
    (f3_s2a_200_c == 1 | f3_s2a_200_f == 1 | f3_s2a_200_g == 1 | f3_s2a_200_i == 1 | 
       f3_s2a_200_j == 1 | f3_s2a_200_k == 1 | f3_s2a_200_m == 1) ~ 0
  )
  )

df_f3 %>% 
  freq(lmaltrato_f5, cumul = F, report.nas = F)

# Ninas/os entre 1 a menores de 5 anios que estan libres de maltrato psicologico en el ultimo mes              

df_f3 <- df_f3 %>% 
  mutate(lmaltrato_p5 = case_when(
    (f3_s2a_200_d == 2 & f3_s2a_200_h == 2) ~ 1,
    (f3_s2a_200_d == 1 | f3_s2a_200_h == 1) ~ 0,
  ))

df_f3 %>% 
  freq(lmaltrato_p5, cumul = F, report.nas = F)

# Ninias/os menores de 1 anio que estan libres de maltrato fisico en el ultimo mes  

df_f3 <- df_f3 %>% 
  mutate(lmaltrato_f1 = case_when(
    (f3_s2b_201_f == 2 & f3_s2b_201_h == 2) ~ 1,
    (f3_s2b_201_f == 1 | f3_s2b_201_h == 1) ~ 0,
    TRUE ~ NA_real_
  ))

df_f3 %>% 
  freq(lmaltrato_f1, cumul = F, report.nas = F)

# Ninias/os menores de 1 anio que estan libres de maltrato psicologico en el ultimo mes                 

df_f3 <- df_f3 %>% 
  mutate(lmaltrato_p1 = case_when(
    (f3_s2b_201_b == 2 & f3_s2b_201_d == 2 & f3_s2b_201_i == 2) ~ 1,
    (f3_s2b_201_b == 1 | f3_s2b_201_d == 1 | f3_s2b_201_i == 1) ~ 0,
    TRUE ~ NA_real_
  ))

df_f3 %>% 
  freq(lmaltrato_p1, cumul = F, report.nas = F)

#------------------------------------------------------------------------------#
# Negligencia 
#------------------------------------------------------------------------------#

# Ninas/os que quedaron al cuidado de un menor de 10 anios por mas 
# de una hora en los ultimos 7 dias

df_f3 %>% 
  descr(f3_s1_103,
        c("common"))

df_f3 <- df_f3 %>% 
  mutate(f3_s1_103 = ifelse(
    f3_s1_103 == 88, NA_real_, f3_s1_103
  ))

df_f3 <- df_f3 %>% 
  mutate(f3_s1_103 = as.double(f3_s1_103),
         dias_solo = case_when(
           f3_s1_103 <= 7 ~ f3_s1_103,
           TRUE ~ NA_real_
         ))

df_f3 <- df_f3 %>% 
  mutate(neglig_10 = case_when(
    dias_solo >= 1 & !is.na(dias_solo) ~ 1,
    !is.na(dias_solo) ~ 0,
    TRUE ~ NA_real_
  ))

df_f3 %>% 
  freq(neglig_10, cumul = F, report.nas = F)

# Ninias/os menores de 5 anios que en los ultimos 7 dias que se 
# quedaron solos por mas de una hora 

df_f3 %>% 
  descr(f3_s1_104,
        c("common"))

df_f3 <- df_f3 %>% 
  mutate(f3_s1_104 = ifelse(
    f3_s1_104 == 88, NA_real_, f3_s1_104
  ))

df_f3 <- df_f3 %>% 
  mutate(f3_s1_104 = as.double(f3_s1_104),
         dias_solo1 = case_when(
           f3_s1_104 <= 7 ~ f3_s1_104,
           TRUE ~ NA_real_
         ))

df_f3 <- df_f3 %>% 
  mutate(neg_1hora = case_when(
    dias_solo1 >= 1 & !is.na(dias_solo1) ~ 1,
    !is.na(dias_solo1) ~ 0,
    TRUE ~ NA_real_
  ))

df_f3 %>% 
  freq(neg_1hora, cumul = F, report.nas = F)


#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores 
# Area
df_f3 <- df_f3 %>%
  mutate(area = as_label(area)) 

df_f3 %>%
  freq(area, cumul = F)

# Region 
df_f3 <- df_f3 %>%
  mutate(region = as_label(region)) 

df_f3 %>%
  freq(region, cumul = F)

# Sexo 
df_f3 <- df_f3 %>%
  mutate(f1_s1_2 = as_label(f1_s1_2)) 

df_f3 %>%
  freq(f1_s1_2, cumul = F)

# Auto-Identificacion etnica
df_f3 <- df_f3 %>%
  mutate(etnia = as_label(etnia)) 

df_f3 %>%
  freq(etnia, cumul = F)

# Quintiles 
df_f3 <- df_f3 %>%
  mutate(quintil = as_label(quintil)) 

df_f3 %>%
  freq(quintil, cumul = F)

# Pobreza por ingreso 
df_f3 <- df_f3 %>%
  mutate(pobreza = as_label(pobreza)) 

df_f3 %>%
  freq(pobreza, cumul = F)

# NBI 
df_f3 <- df_f3 %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f3 %>%
  freq(nbi_1, cumul = F)

# Nivel de instruccion de la madre
df_f3 <- df_f3 %>%
  mutate(nivins_madre = as_label(nivins_madre)) 

df_f3 %>%
  freq(nivins_madre, cumul = F)

# Desnutricion cronica 
df_f3 = apply_labels(df_f3,
                     dcronica = num_lab(
                       "0 Sin desnutrición crónica
                             1 Con desnutrición crónica"
                     ))

df_f3 <- df_f3 %>%
  mutate(dcronica = as_label(dcronica)) 

df_f3 %>%
  freq(dcronica, cumul = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f3 %>% as_survey_design(ids = "id_upm",
                                            strata = "estrato",
                                            weights = "fexp_di")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

# Ninias/os que realizan cuatro o mas actividades de estimulacion 
# con cualquier cuidador en los ultimos 3 dias
survey_design %>% 
  srvyr_prop(mh_act)

survey_design %>% 
  srvyr_prop_by(mh_act, area)

# Ninias/os que realizan cuatro o mas actividades de estimulacion 
# con la madre en los ultimos 3 dias
survey_design %>% 
  srvyr_prop(madre_act)

survey_design %>% 
  srvyr_prop_by(madre_act, area)

# Ninias/os que realizan cuatro o mas actividades de estimulacion 
# con en el padre en los ultimos 3 dias
survey_design %>% 
  srvyr_prop(padre_act)

survey_design %>% 
  srvyr_prop_by(padre_act, area)

# Ninias/os con al menos 1 libro en el hogar 
survey_design %>% 
  srvyr_prop(libro_1)

survey_design %>% 
  srvyr_prop_by(libro_1, area)

# Ninias/os con al menos 3 libros en el hogar 
survey_design %>% 
  srvyr_prop(libro_3)

survey_design %>% 
  srvyr_prop_by(libro_3, area)

# Ninias/os con al menos tres juguetes en el hogar 
survey_design %>% 
  srvyr_prop(juguete_3)

survey_design %>% 
  srvyr_prop_by(juguete_3, area)

# Ninias/os entre 1 a menores de 5 anios que estan libres de maltrato fisico en el ultimo mes
survey_design %>% 
  srvyr_prop(lmaltrato_f5)

survey_design %>% 
  srvyr_prop_by(lmaltrato_f5, area)

# Ninas/os entre 1 a menores de 5 anios que estan libres de maltrato psicologico en el ultimo mes              
survey_design %>% 
  srvyr_prop(lmaltrato_p5)

survey_design %>% 
  srvyr_prop_by(lmaltrato_p5, area)

# Ninias/os menores de 1 anio que estan libres de maltrato fisico en el ultimo mes
survey_design %>% 
  srvyr_prop(lmaltrato_f1)

survey_design %>% 
  srvyr_prop_by(lmaltrato_f1, area)

# Ninias/os menores de 1 anio que estan libres de maltrato psicologico en el ultimo mes
survey_design %>% 
  srvyr_prop(lmaltrato_p1)

survey_design %>% 
  srvyr_prop_by(lmaltrato_p1, area)

# Ninas/os que quedaron al cuidado de un menor de 10 anios por mas 
# de una hora en los ultimos 7 dias
survey_design %>% 
  srvyr_prop(neglig_10)

survey_design %>% 
  srvyr_prop_by(neglig_10, area)

# Ninias/os menores de 5 anios que en los ultimos 7 dias que se 
# quedaron solos por mas de una hora 
survey_design %>% 
  srvyr_prop(neg_1hora)

survey_design %>% 
  srvyr_prop_by(neg_1hora, area)


#==============================================================================#
####                                Funciones                               ####
#==============================================================================#

# Funcion para agregar tablas descriptivas dependiendo los diferentes tipos de 
# desagregacion  

# Para resultados en proporcion
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
  
  # Sexo
  tab_sex <- design %>%
    srvyr_prop_by(.data[[x]], {{ by_3 }})
  
  # Auto-Identificacion etnica
  tab_etnia <- design %>%
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
  
  # Nivel de instruccion de la MEF  
  tab_nivins_madre_nin <- design %>%
    filter(!is.na({{ by_8 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_8 }}) 
  
  # Desnutricion cronica  
  tab_dcronica <- design %>%
    filter(!is.na({{ by_9 }})) %>%
    srvyr_prop_by(.data[[x]], {{ by_9 }}) 
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac, tab_area, 
                         tab_reg, tab_sex, 
                         tab_etnia, tab_quintil, 
                         tab_pobreza, tab_pobreza_nbi,
                         tab_nivins_madre_nin, tab_dcronica)
}

tab_full <- map(c("mh_act", "madre_act", "padre_act", "libro_1", "libro_3",
                  "juguete_3", "lmaltrato_f5", "lmaltrato_p5", "lmaltrato_f1",
                  "lmaltrato_p1", "neglig_10", "neg_1hora"),
                ~tab_fun(.x, design = survey_design,
                         area, region, f1_s1_2, etnia, 
                         quintil, pobreza, nbi_1, nivins_madre, dcronica))

#==============================================================================#
####               Extraccion de elementos de objetos listas                #### 
#==============================================================================#

Style_tab(tab_full[[1]],"T10_i1") # tab_mh_act (actividades de estimulacion temprana con miembros del hogar)
Style_tab(tab_full[[2]],"T10_i2") # tab_madre_act
Style_tab(tab_full[[3]],"T10_i3") # tab_padre_act
Style_tab(tab_full[[4]],"T10_i4") # tab_libro_1 (tienen uno o mas libros, cuentos, revistas o libros con dibujos)
Style_tab(tab_full[[5]],"T10_i5") # tab_libro_3
Style_tab(tab_full[[6]],"T10_i6") # tab_juguete_3
Style_tab(tab_full[[7]],"T10_i7") # tab_lmaltrato_f5 (no recibieron maltrato fisico en el ultimo mes)
Style_tab(tab_full[[8]],"T10_i8") # tab_lmaltrato_p5
Style_tab(tab_full[[9]],"T10_i9") # tab_lmaltrato_f1
Style_tab(tab_full[[10]],"T10_i10") # tab_lmaltrato_p1
Style_tab(tab_full[[11]],"T10_i11") # tab_neglig_10
Style_tab(tab_full[[12]],"T10_i12") # tab_neg_1hora 


