
# Titulo de la Sintaxis: 
# Desarrollo Infantil - TVIP

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
####          Calculo del indicador Test Peabody (43 a 59 meses)            ####
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

# Idioma con que se comunica 
df_f3 %>% 
  freq(f3_s6_600, cumul = F, report.nas = F)

# Se puede trabajar con el menor 
df_f3 %>% 
  freq(f3_s6_601, cumul = F, report.nas = F)

# Razon por la que no se pudo trabajar
df_f3 %>%
  freq(f3_s6_601_a, report.nas = F)

# Acceso a TVIP
df_f3 <- df_f3 %>% 
  mutate(acc_tvip = case_when(
    f3_s6_601 == 1 & edadmeses_nin >= 43 & !is.na(edadmeses_nin) ~ 1,
    edadmeses_nin >= 43 & !is.na(edadmeses_nin) ~ 0,
    TRUE ~ NA_real_
  ))

df_f3 %>% 
  freq(acc_tvip, cumul = F, report.nas = F)

#------------------------------------------------------------------------------#
# Genero variable de error/acierto
#------------------------------------------------------------------------------#

# 0 = acierto
# 1 = error 

resp <- c(2, 4, 1, 2, 1, 4, 1, 1, 4, 2, 3, 3, 3, 3, 2, 2, 1, 1, 2, 2, 4, 3,
          2, 4, 3, 3, 3, 4, 2, 4, 3, 2, 4, 4, 1, 4, 1, 2, 3, 3, 3, 2, 1, 2, 
          1, 4, 2, 2, 1, 3, 4, 2, 3, 3, 4, 4, 1, 3, 1, 1, 4, 1, 4, 2, 3, 3, 
          1, 2, 4, 2, 3, 1, 4, 1, 3, 2, 3, 2, 3, 4, 2, 3, 2, 1, 3, 2, 2, 2, 
          4, 4, 4, 3, 4, 4, 4, 4, 2, 2, 3, 3, 1, 4, 1, 1, 1, 2, 2, 3, 3, 3, 
          4, 2, 1, 2, 3, 3, 2, 3, 1, 3, 2, 3, 1, 4, 4)

for (i in 1:125) {
  df_f3 <- df_f3 %>%
    mutate(
      !!paste0("resp_", i) := case_when(
        !!sym(paste0("f3_s6_605_", i)) == resp[i] ~ 0, 
        !!sym(paste0("f3_s6_605_", i)) != resp[i] & 
          !is.na(!!sym(paste0("f3_s6_605_", i))) ~ 1,
        TRUE ~ NA_real_))
  
  df_f3 <- df_f3 %>% 
    mutate(
      !!sym(paste0("resp_m_", i)) := case_when(
        !!sym(paste0("resp_", i)) == 0 ~ 1,
        !!sym(paste0("resp_", i)) == 1 ~ 0,
        TRUE ~ NA_real_
      ))
}

# Flag: los que no tienen ni una respuesta y presentaron la prueba
df_f3 <- df_f3 %>%
  mutate(temp = case_when(
    edadmeses_nin >= 43 &
      edadmeses_nin <= 59 ~ rowSums(is.na(select(., starts_with("resp_m_")))),
    TRUE ~ NA_real_))

df_f3 %>% 
  filter(acc_tvip == 1) %>% 
  descr(temp, 
        c("common")) # Ningun valor registra sumatoria de missing igual a 125

#------------------------------------------------------------------------------#
# Se procede a cambiar la base de ninios/as de ancho a largo en funcion de 
# las respuestas
#------------------------------------------------------------------------------#

df_f3_copia <- as_tibble(df_f3)

df_f3 <- df_f3 %>% 
  select(id_per, starts_with("resp_"), edadmeses_nin) %>% 
  filter(edadmeses_nin >= 43 & edadmeses_nin <= 59)

df_f3_long <- df_f3 %>% 
  pivot_longer(
    cols = starts_with("resp_"),
    names_to = c(".value", "j"),
    names_pattern = "([A-Za-z]+)_([0-9]+)") %>% 
  rename(resp_m = m)

rm(df_f3)

df_f3_long <- df_f3_long %>% 
  mutate(j = as.numeric(j))

df_f3_long <- df_f3_long %>%
  arrange(id_per, j)

df_f3_long <- df_f3_long %>% 
  mutate(m = case_when(
    is.na(resp) ~ 1, 
    TRUE ~ NA_real_
  ))

df_f3_long <- df_f3_long %>% 
  group_by(id_per) %>% 
  mutate(ms = sum(m, na.rm = T)) %>% 
  ungroup()

df_f3_long %>% 
  descr(ms, 
        c("common"))

df_f3_long <- df_f3_long %>% 
  filter(ms != 125)  # se filtra los que al menos tienen una respuesta

#------------------------------------------------------------------------------#
# Obtengo cantidad maxima de aciertos inenterrumpidos
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>%
  group_by(id_per) %>%
  mutate(temp = row_number()) %>% 
  ungroup()

df_f3_long <- df_f3_long %>% 
  mutate(temp = case_when(
    resp_m == 0 | is.na(resp_m) ~ NA_integer_,
    TRUE ~ temp
  ))

# En temp1 se guarda identificador de bloque
df_f3_long <- df_f3_long %>%
  group_by(id_per) %>%
  mutate(temp1 = case_when(
    is.na(temp) ~ row_number(),
    TRUE ~ NA_integer_
  )) %>% 
  ungroup()

df_f3_long <- df_f3_long %>%
  group_by(id_per) %>%
  fill(temp1, .direction = "up") %>%
  ungroup()

df_f3_long <- df_f3_long %>%
  group_by(id_per, temp1) %>%
  mutate(temp2 = ifelse(all(is.na(temp)), NA, min(temp, na.rm = TRUE))) %>% 
  ungroup()

df_f3_long <- df_f3_long %>% 
  mutate(temp = temp - temp2 + 1)  

# En variable temp3 se guarda cantidad maxima de aciertos inenterrumpidos
df_f3_long <- df_f3_long %>%
  group_by(id_per, temp1) %>%
  mutate(temp3 = ifelse(all(is.na(temp)), NA, max(temp, na.rm = TRUE))) %>% 
  ungroup()

df_f3_long <- df_f3_long %>% 
  select(-c(temp, temp2, m, ms))

df_f3_long <- df_f3_long %>% 
  select(id_per, edadmeses_nin, j, resp, resp_m, temp1, temp3)

df_f3_long <- df_f3_long %>% 
  mutate(temp3 = case_when(
    is.na(resp_m) | resp_m == 0 ~ NA_real_,
    TRUE ~ temp3
  ))

#------------------------------------------------------------------------------#
# Obtengo variable que identifique ultimo bloque con al menos 8 aciertos
#------------------------------------------------------------------------------#

# Variable identificadora de ninio (mas facil de visualizar)
df_f3_long <- df_f3_long %>%
  mutate(id_nin = dense_rank(id_per))

# Identificador de bloque con al menos 8 aciertos consecutivos
df_f3_long <- df_f3_long %>%
  arrange(id_nin, j)

df_f3_long <- df_f3_long %>% 
  mutate(temp4 = case_when(
    temp3 >= 8 & !is.na(temp3) ~ 1,
    TRUE ~ NA_real_
  ))

# Idenficador de total bloques acertados
df_f3_long <- df_f3_long %>%
  group_by(id_per) %>%
  mutate(temp5 = cumsum(ifelse(is.na(temp4), 0, 1))) %>% 
  ungroup()

# Maxima suma por bloque y por ninio
df_f3_long <- df_f3_long %>%
  group_by(id_per, temp1) %>%
  mutate(temp6 = case_when(
    !is.na(temp4) ~ max(temp5, na.rm = T),
    TRUE ~ NA_real_))

# Maxima suma por nino
df_f3_long <- df_f3_long %>%
  group_by(id_per) %>%
  mutate(temp7 = max(temp6, na.rm = TRUE)) %>%
  ungroup() 

df_f3_long <- df_f3_long %>% 
  mutate(piso = case_when(
    temp4 == 1 & temp6 == temp7 ~ 1,
    TRUE ~ NA_real_
  ))

# Me quedo solo con información necesaria
df_f3_long <- df_f3_long %>% 
  select(id_per, id_nin, j, starts_with("resp"), 
         temp1, temp3, piso, temp4, temp6, temp7, edadmeses_nin)

#------------------------------------------------------------------------------#
# Creacion de techo teorico 
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>%
  arrange(id_nin, desc(j))

df_f3_long <- df_f3_long %>%
  select(-starts_with("posible_bloque_"), 
         -starts_with("bloque_error_"))

for (i in 1:118) {
  max <- i + 7
  df_f3_long <- df_f3_long %>%
    mutate(!!paste0("posible_bloque_", i) := ifelse(
      j >= i & j <= max, i, NA)) 
}  

df_f3_long <- df_f3_long %>%
  mutate_all(~replace(., is.infinite(.) | is.na(.), NA))

for (i in 1:118) {  
  df_f3_long <- df_f3_long %>%
    group_by(id_nin, !!sym(paste0("posible_bloque_", i))) %>%
    mutate(!!paste0("bloque_error_", i) := ifelse(
      !!sym(paste0("posible_bloque_", i)) == i,
      sum(resp, na.rm = TRUE), NA),
      !!paste0("posible_bloque_", i) := ifelse(
        !!sym(paste0("bloque_error_", i)) < 6,
        NA, !!sym(paste0("posible_bloque_", i))
      )) %>% 
    ungroup()
}

df_f3_long <- df_f3_long %>%
  mutate_all(~replace(., is.infinite(.) | is.na(.), NA))

df_f3_long <- df_f3_long %>%
  arrange(id_nin, desc(j))

#------------------------------------------------------------------------------#
# Identificador de j para techo teorico
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>% 
  mutate(id_bloque_techo = 125)

df_f3_long <- df_f3_long %>%
  group_by(id_nin) %>%
  mutate(n_0001 = min(!!!syms(paste0("posible_bloque_", 1:118)), na.rm = T)) %>%
  ungroup() %>% 
  mutate(n_0001 = ifelse(is.infinite(n_0001), NA, n_0001))

df_f3_long <- df_f3_long %>%
  mutate(id_bloque_techo = ifelse(
    n_0001 < id_bloque_techo &
      !is.na(n_0001), n_0001, id_bloque_techo)) 

df_f3_long <- df_f3_long %>% 
  select(-c(n_0001, starts_with("posible_bloque_"), 
            starts_with("bloque_error_")))

#------------------------------------------------------------------------------#
# Genero suma de errores solo en bloque teorico
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>% 
  arrange(id_nin, j)

df_f3_long <- df_f3_long %>%
  group_by(id_nin) %>%
  mutate(max_techo = cumsum(ifelse(
    !is.na(resp) & j >= id_bloque_techo & j <= (id_bloque_techo + 7), resp, 0))) %>% 
  mutate(max_techo = ifelse(
    max_techo == 0 & 
      j < id_bloque_techo, NA, max_techo)) %>% 
  mutate(max_techo = case_when(
    j > (id_bloque_techo + 7) ~ NA,
    TRUE ~ max_techo)) %>% 
  ungroup() 

#------------------------------------------------------------------------------#
# Identifico pregunta en la que teoricamente debio parar
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>% 
  group_by(id_nin) %>% 
  mutate(n_0001 = case_when(
    max_techo == 6 & !is.na(resp) ~ j,
    TRUE ~ NA)) %>% 
  ungroup()

df_f3_long <- df_f3_long %>%
  group_by(id_nin) %>% 
  mutate(max_techo_j = min(n_0001, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(max_techo_j = ifelse(is.infinite(max_techo_j), NA, max_techo_j))

#------------------------------------------------------------------------------#
# Identifico item de piso
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>% 
  select(-n_0001)

df_f3_long <- df_f3_long %>%
  group_by(id_nin) %>%
  mutate(n_0001 = ifelse(piso == 1, 
                         max(j[piso == 1], 
                             na.rm = TRUE), NA)) %>% 
  ungroup()

df_f3_long <- df_f3_long %>%
  group_by(id_nin) %>% 
  mutate(piso_j = min(n_0001, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(piso_j = ifelse(is.infinite(piso_j), NA, piso_j))

df_f3_long <- df_f3_long %>% 
  mutate(piso_j = case_when(
    is.na(piso_j) ~ 1,
    TRUE ~ piso_j))

# Flag : ninios con piso automatico 
df_f3_long <- df_f3_long %>%
  mutate(flag1 = case_when(
    piso_j > max_techo_j ~ 1, 
    TRUE ~ 0
  ))

df_f3_long %>% 
  freq(flag1, cumul = F)

#------------------------------------------------------------------------------#
# Identifico items en los que el piso es mayor al techo y arreglo
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>% 
  mutate(temp6 = case_when(
    j > max_techo_j ~ NA,
    TRUE ~ temp6)) %>%
  mutate(temp6 = ifelse(temp6 == 0, NA, temp6))

df_f3_long <- df_f3_long %>% 
  group_by(id_nin) %>% 
  mutate(temp7_1 = max(temp6, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(temp7_1 = ifelse(-is.infinite(temp7_1), NA, temp7_1))

df_f3_long <- df_f3_long %>% 
  mutate(piso_teor = case_when(
    temp4 == 1 & (temp6 == temp7_1) & !is.na(temp7_1) ~ 1,
    TRUE ~ NA))

df_f3_long <- df_f3_long %>%
  group_by(id_nin) %>%
  mutate(n_0002 = ifelse(piso_teor == 1, 
                         max(j[piso_teor == 1], 
                             na.rm = TRUE), NA)) %>% 
  ungroup()

df_f3_long <- df_f3_long %>%
  group_by(id_nin) %>% 
  mutate(piso_j_teor = min(n_0002, na.rm = T)) %>% 
  ungroup()%>% 
  mutate(piso_j_teor = ifelse(is.infinite(piso_j_teor), NA, piso_j_teor))

df_f3_long <- df_f3_long %>% 
  mutate(piso_j_teor = case_when(
    is.na(piso_j_teor) ~ 1,
    TRUE ~ piso_j_teor))	

df_f3_long <- df_f3_long %>% 
  mutate(diff_p = case_when(
    piso_j_teor != piso_j ~ 1, 
    TRUE ~ 0
  ))

df_f3_long %>% 
  freq(diff_p, cumul = F)

#------------------------------------------------------------------------------#
# Conteo teorico de errores
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>%
  group_by(id_nin) %>%
  mutate(n_errores_teor = cumsum(replace_na(ifelse(j >= piso_j_teor & (is.na(max_techo_j) | j <= max_techo_j), resp, 0), 0))) %>%
  ungroup()

df_f3_long <- df_f3_long %>% 
  group_by(id_nin) %>% 
  mutate(n_errores_teor_t = max(n_errores_teor, na.rm = T)) %>% 
  ungroup()

#------------------------------------------------------------------------------#
# Variable que identifique maximo j de respuesta
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>%
  group_by(id_nin) %>%
  mutate(max_resp = ifelse(is.na(resp), NA, max(j[!is.na(resp)]))) %>% 
  ungroup()

df_f3_long <- df_f3_long %>% 
  mutate(n_error_noParar = case_when(
    max_resp > max_techo_j & !is.na(max_resp) ~ 1,
    TRUE ~ NA)) %>% 
  mutate(n_error_noParar_N = case_when(
    n_error_noParar == 1 ~ max_resp - max_techo_j,
    TRUE ~ NA))

df_f3_long %>% 
  freq(n_error_noParar, cumul = F)

#------------------------------------------------------------------------------#
# Missings en medio
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>% 
  select(-temp4)

df_f3_long <- df_f3_long %>% 
  mutate(miss_resp = case_when(is.na(resp) ~ 1,
                               TRUE ~ NA))

df_f3_long <- df_f3_long %>% 
  group_by(id_nin) %>% 
  mutate(temp4 = max(max_resp, na.rm = T)) %>% 
  ungroup()

df_f3_long <- df_f3_long %>%
  group_by(id_nin) %>%
  mutate(miss_en_medio = sum(ifelse(j >= 1 & j <= temp4, miss_resp, NA), na.rm = T)) %>%
  ungroup()

df_f3_long %>% 
  descr(miss_en_medio,
        c("common"))

#------------------------------------------------------------------------------#
# Se reduce la base a nivel de individuo 
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>%
  group_by(id_per) %>%
  summarise(item_piso = max(piso_j, na.rm = T),
            item_piso_teor = max(piso_j_teor, na.rm = T),
            item_techo = max(max_techo_j, na.rm = T),
            max_resp = max(max_resp, na.rm = T),
            n_errores_teor = max(n_errores_teor,  na.rm = T),
            n_error_noParar = max(n_error_noParar, na.rm = T),
            n_error_noParar_N = max(n_error_noParar_N, na.rm = T),
            miss_en_medio = max(miss_en_medio, na.rm = T),
            flag1 = max(flag1, na.rm = T),
            edadmeses_nin = max(edadmeses_nin, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(n_error_noParar = ifelse(
    -is.infinite(n_error_noParar), NA, n_error_noParar),
    n_error_noParar_N = ifelse(
      -is.infinite(n_error_noParar_N), NA, n_error_noParar_N),
    item_techo = ifelse(
      -is.infinite(item_techo), NA, item_techo)) %>% 
  mutate(item_techo = ifelse(is.na(item_techo) & max_resp == 125, max_resp, item_techo))

#------------------------------------------------------------------------------#
# Flags 
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>% 
  mutate(flag2 = ifelse(item_piso_teor == 1, 1, 0),
         flag3 = ifelse(item_techo == 125, 1, 0),
         flag4 = ifelse(item_techo < max_resp & 
                          !is.na(item_techo) & 
                          !is.na(max_resp), 1, 0),
         flag5 = ifelse(is.na(item_techo), 1, 0))

# Flag 1: Ninos con piso mayor que techo
df_f3_long %>% 
  freq(flag1, cumul = F)

# Flag 2: Ninos con piso automatico
df_f3_long %>% 
  freq(flag2, cumul = F)

# Flag 3: Ninos con techo automatico
df_f3_long %>% 
  freq(flag3, cumul = F)

# Flag 4: Ninos que pararon despues 
df_f3_long %>% 
  freq(flag4, cumul = F)

# Flag 5: Ninos que pararon antes (tienen menos de seis errores en el ultimo bloque)
df_f3_long %>% 
  freq(flag5, cumul = F)

#------------------------------------------------------------------------------#
# Genero score teorico del TVIP
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>% 
  mutate(score_teor = item_techo - n_errores_teor)

df_f3_long %>% 
  descr(score_teor, 
        c("common"))

df_f3_long %>% 
  filter(flag2 == 1) %>% 
  descr(score_teor, 
        c("common"))

df_f3_long %>% 
  filter(flag3 == 1) %>% 
  descr(score_teor, 
        c("common"))

#-------------------------------------------------------------------------------
# Grupo etario
#-------------------------------------------------------------------------------

df_f3_long <- df_f3_long %>%
  mutate(eta1 = case_when(
    edadmeses_nin >= 0 & edadmeses_nin <= 42 ~ NA_real_,          
    edadmeses_nin >= 43 & edadmeses_nin <= 48 ~ 1,                
    edadmeses_nin >= 49 & edadmeses_nin <= 54 ~ 2,                
    edadmeses_nin >= 55 & edadmeses_nin <= 59 ~ 3,                
    is.na(edadmeses_nin) ~ NA_real_,                          
    TRUE ~ NA_real_                               
  ))

df_f3_long = apply_labels(df_f3_long,
                          eta1 = num_lab(
                            "1 43 a 48 meses
                             2 49 a 54 meses
                             3 55 a 59 meses"))

df_f3_long %>% 
  freq(eta1, cumul = F)

#------------------------------------------------------------------------------#
# Join 
#------------------------------------------------------------------------------#

df_f3_long <- df_f3_long %>%
  right_join(df_f3_copia, by = "id_per")

rm(df_f3_copia)

#-------------------------------------------------------------------------------
# Puntaje TVIP promedio para ninos de 43 a 59 meses
#-------------------------------------------------------------------------------

df_f3_long %>% 
  filter(!is.na(eta1)) %>% 
  group_by(eta1) %>% 
  descr(score_teor, 
        c("common"))

# Comprobacion entre el puntaje reportado y el calculado 
df_f3_long %>%
  descr(f3_s6_608,
        c("common"))

df_f3_long %>%
  descr(score_teor,
        c("common"))

df_f3_long <- df_f3_long %>%
  mutate(diff_score = case_when(
    score_teor != f3_s6_608 ~ 1,
    TRUE ~ 0
  ))

df_f3_long %>%
  freq(diff_score, cumul = F)

#==============================================================================#
####                           Desagregacion                                ####
#==============================================================================#

# Para establecer las etiquetas como valores 
# Area
df_f3_long <- df_f3_long %>%
  mutate(area = as_label(area)) 

df_f3_long %>%
  freq(area, cumul = F)

# Region 
df_f3_long <- df_f3_long %>%
  mutate(region = as_label(region)) 

df_f3_long %>%
  freq(region, cumul = F)

# Sexo 
df_f3_long <- df_f3_long %>%
  mutate(f1_s1_2 = as_label(f1_s1_2)) 

df_f3_long %>%
  freq(f1_s1_2, cumul = F)

# Auto-Identificacion etnica
df_f3_long <- df_f3_long %>%
  mutate(etnia = as_label(etnia)) 

df_f3_long %>%
  freq(etnia, cumul = F)

# Quintiles 
df_f3_long <- df_f3_long %>%
  mutate(quintil = as_label(quintil)) 

df_f3_long %>%
  freq(quintil, cumul = F)

# Pobreza por ingreso 
df_f3_long <- df_f3_long %>%
  mutate(pobreza = as_label(pobreza)) 

df_f3_long %>%
  freq(pobreza, cumul = F)

# NBI 
df_f3_long <- df_f3_long %>%
  mutate(nbi_1 = as_label(nbi_1)) 

df_f3_long %>%
  freq(nbi_1, cumul = F)

# Nivel de instruccion de la madre
df_f3_long <- df_f3_long %>%
  mutate(nivins_madre = as_label(nivins_madre)) 

df_f3_long %>%
  freq(nivins_madre, cumul = F)

# Desnutricion cronica 
df_f3_long = apply_labels(df_f3_long,
                          dcronica = num_lab(
                            "0 Sin desnutrición crónica
                             1 Con desnutrición crónica"
                          ))

df_f3_long <- df_f3_long %>%
  mutate(dcronica = as_label(dcronica)) 

df_f3_long %>%
  freq(dcronica, cumul = F)

#==============================================================================#
####                       Declaracion de encuesta                          ####
#==============================================================================#

survey_design <- df_f3_long %>% as_survey_design(ids = "id_upm",
                                                 strata = "estrato",
                                                 weights = "fexp_di")
options(survey.lonely.psu = "adjust")

#==============================================================================#
####                         Resultados ponderados                          ####
#==============================================================================#

survey_design %>% 
  srvyr_mean(score_teor)

survey_design %>% 
  srvyr_mean_by(score_teor, area)

#==============================================================================#
####                                Funciones                               ####
#==============================================================================#

# Funcion para agregar tablas descriptivas dependiendo los diferentes tipos de 
# desagregacion  

# Para resultados en promedio
tab_fun <- function(design, x, by_1, by_2, by_3, by_4, by_5, 
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
  
  # Sexo
  tab_sex <- design %>%
    srvyr_mean_by(.data[[x]], {{ by_3 }})
  
  # Auto-Identificacion etnica
  tab_etnia <- design %>%
    filter(!is.na({{ by_4 }})) %>% 
    srvyr_mean_by(.data[[x]], {{ by_4 }})
  
  # Quintiles
  tab_quintil <- design %>%
    filter(!is.na({{ by_5 }})) %>% 
    srvyr_mean_by(.data[[x]], {{ by_5 }})
  
  # Pobreza por Ingresos  
  tab_pobreza <- design %>%
    filter(!is.na({{ by_6 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_6 }}) 
  
  # Pobreza por NBI  
  tab_pobreza_nbi <- design %>%
    filter(!is.na({{ by_7 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_7 }}) 
  
  # Nivel de instruccion de la MEF  
  tab_nivins_madre_nin <- design %>%
    filter(!is.na({{ by_8 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_8 }}) 
  
  # Desnutricion cronica  
  tab_dcronica <- design %>%
    filter(!is.na({{ by_9 }})) %>%
    srvyr_mean_by(.data[[x]], {{ by_9 }}) 
  
  # Union de tabulados 
  tab_final <- bind_rows(tab_nac, tab_area, 
                         tab_reg, tab_sex, 
                         tab_etnia, tab_quintil, 
                         tab_pobreza, tab_pobreza_nbi,
                         tab_nivins_madre_nin, tab_dcronica)
}

tab_full <- map(c("score_teor"),
                ~tab_fun(.x, design = survey_design,
                         area, region, f1_s1_2, etnia, 
                         quintil, pobreza, nbi_1, nivins_madre, dcronica))

#==============================================================================#
####               Extraccion de elementos de objetos listas                #### 
#==============================================================================#

# Aplicacion de la funcion para la obtencion de tabulados
Style_tab(tab_full[[1]],"T10_i17") # tab score_teor (Lenguaje-Test de vocabulario en imágenes PEABODY-TVIP)

