
# Titulo de la Sintaxis: 
# Inicializacion  

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

# Este documento carga las librerias y la gran mayoria de funciones que se 
# requieren en el resto de documentos para poder ser ejecutados.

#==============================================================================#
####                   Instalacion y carga de paquetes                      ####
#==============================================================================#

# Se usa la funcion p_load() desde el paquete pacman, el cual instala  
# el paquete si esta ausente, y carga para el uso si ya esta instalado 

# Se asegura que el paquete "pacman" este instalado
if(!require("pacman")) install.packages("pacman") 

# Paquetes disponibles desde CRAN

pacman::p_load(
  
  # Gestion de proyectos y archivos 
  here,     # construye rutas a los archivos de su proyecto 
  rio,      # importacion / exportacion de varios tipos de datos
  openxlsx, # importacion / exportacion de libros de Excel de varias hojas
  
  # Instalacion y manejo de paquetes 
  pacman, # instalar y cargar paquetes 
  
  # Manejo general de los datos 
  Hmisc, # funciones utiles para el analsis de datos 
  expss, # tablas, etiquetas y funciones de hojas de calculo y estadisticas de 'SPSS'
  tidyverse, # incluye paquetes para ordenar y presentar los datos 
  lubridate, # trabaja con fechas 
  pillar,    # herramientas para manejar columnas de datos
  janitor,   # Limpieza de datos y tablas
  sjlabelled, # para tratar etiquetas 
  epikit, # agregar categorias
  
  # Estadisticas
  summarytools, # herramientas para resumir datos de forma rapida y ordenada
  
  # Manejo de muestras complejas 
  srvyr,  # estadistica de resumen para datos de encuentas 
  
  # Paquetes para calculos especificos 
  anthro, # calculo de puntuaciones z de antropometria infantil
  psych, # procedimientos para la investigacion psicologica, psicometrica y de personalidad
  
  # Para exportaciones
  readr, # herramienta para exportar o importar archivos csv y tsv 
  readxl, # Leer archivos excel
  
  # Para graficacion 
  grid, # graficas
  ggforce # graficas
  
)

#====================== variables a no eliminar ===============================#

variables_mantener <- c(
  
  "srvyr_prop", "srvyr_prop_by", "srvyr_prop_by_new", "srvyr_freq",
  "srvyr_freq_by", "srvyr_mean", "srvyr_mean_by", "srvyr_mean_by_new",
  "link_scripts", "link_bdd", "ruta_archivo",  "name_base_personas", 
  "name_base_hogar", "name_base_di", "name_base_mef", 
  "name_base_ninez", "name_base_lactancia", "df_title", "variables_mantener",
  "name_diccionario", "name_diccionario_di",  "exportar", "wb", "Style_tab",
  "ruta_archivo_formato", "a", "b", "c", "d", "Style_tab_0",
  "ruta_archivo_formato_2", "link_tabulados"
)

variables_eliminar <- setdiff(ls(all = TRUE), variables_mantener)
rm(list = variables_eliminar)

#==============================================================================#
####                         links y bases a usar                           ####
#==============================================================================#

# ----- Nombre de las bases a usar

name_base_personas <- "/BDD_ENDI_R2_f1_personas.rds"
name_base_hogar <- "/BDD_ENDI_R2_f1_hogar.rds"
name_base_mef <- "/BDD_ENDI_R2_f2_mef.rds"
name_base_ninez <- "/BDD_ENDI_R2_f2_salud_ninez.rds"
name_base_lactancia <- "/BDD_ENDI_R2_f2_lactancia.rds"
name_base_di <-  "/BDD_ENDI_R2_f3_desarrollo_inf.rds"
name_diccionario <- "/Diccionario_ENDI.xlsx"
name_diccionario_di <- "/Diccionario_ENDI_di.xlsx" 

#==============================================================================#
####                Funciones - Para disenio de las Encuestas               ####
#==============================================================================#

#  -----Funcion para calcular estadisticos para variables dicotomicas ----------

a <- "Suprimir"
b <- "Revisar"
c <- "Estimación precisa"
d <- "Sin casos"

# Nacional 
srvyr_prop <- function(design, x) {
  
  design %>% 
    summarise(
      props = survey_mean({{ x }},
                          proportion = TRUE,   
                          vartype = c("se", "ci", "cv"),
                          na.rm = T), 
      deff = survey_mean({{ x }},  
                         deff = "replace", 
                         na.rm = T),
      n_upm = n_distinct(id_upm[!is.na({{ x }})]),
      n_estratos = n_distinct(estrato[!is.na({{ x }})]),
      Num = sum({{ x }} , na.rm = TRUE),
      Deno = sum(!is.na( {{ x }} ))) %>%
    mutate(
      desag = "Nacional",
      rangoic = props_upp - props_low,
      rangoicrel = rangoic/props,
      grados = n_upm - n_estratos,
      nefec = ifelse(deff_deff < 1, Deno, Deno / deff_deff), 
      validez = case_when(Num == 0 ~ d,
                          nefec < 30 ~ a,
                          rangoic >= 0.3 ~ a,
                          rangoic > 0.05 & rangoicrel > 1.3 ~ a,
                          grados < 8 ~ b,
                          rangoic <= 0.05 & props > 0 & grados >= 8 ~ c,
                          rangoic > 0.05 & rangoicrel <= 1.3 & props > 0 & grados >= 8 ~ c,
                          T ~ "algo esta mal"),
      props = props * 100,
      props_se = props_se * 100,
      props_low = props_low * 100,
      props_upp = props_upp * 100,
      props_cv = props_cv * 100,
      rangoic = rangoic * 100,
      rangoicrel = rangoicrel * 100
    ) %>% 
    select(
      Desag = desag,
      Props = props, 
      EE    = props_se, 
      LI    = props_low, 
      LS    = props_upp,
      CV    = props_cv,
      Num   = Num, 
      Deno  = Deno,
      deff = deff_deff,
      NEFE  = nefec,
      RIC   = rangoic,
      RCRE  = rangoicrel,
      GL    = grados,
      Valid = validez
    )
}

# Por desagregacion 
srvyr_prop_by <- function(design, x, by) {
  
  design %>%
    group_by({{ by }}) %>%
    summarise(
      props = survey_mean({{ x }} ,
                          proportion = TRUE,   
                          vartype = c("se", "ci", "cv"),
                          na.rm = T) , 
      deff = survey_mean({{ x }} ,  
                         deff = "replace", 
                         na.rm = T),
      n_upm = n_distinct(id_upm[!is.na({{ x }})]),
      n_estratos = n_distinct(estrato[!is.na({{ x }})]),
      Num = sum({{ x }} , na.rm = TRUE),
      Deno = sum(!is.na( {{ x }} ))) %>%
    mutate(
      desag = {{ by }},
      rangoic = props_upp - props_low,
      rangoicrel = rangoic/props,
      grados = n_upm - n_estratos,
      nefec = ifelse(deff_deff < 1, Deno, Deno / deff_deff), 
      validez = case_when(Num == 0 ~ d, 
                          nefec < 30 ~ a,
                          rangoic >= 0.3 ~ a,
                          rangoic > 0.05 & rangoicrel > 1.3 ~ a,
                          grados < 8 ~ b,
                          rangoic <= 0.05 & props > 0 & grados >= 8 ~ c,
                          rangoic > 0.05 & rangoicrel <= 1.3 & props > 0 & grados >= 8 ~ c,
                          T ~ "algo esta mal"),
      props = props * 100,
      props_se = props_se * 100,
      props_low = props_low * 100,
      props_upp = props_upp * 100,
      props_cv = props_cv * 100,
      rangoic = rangoic * 100,
      rangoicrel = rangoicrel * 100
    ) %>% 
    select(
      Desag = desag,
      Props = props, 
      EE    = props_se, 
      LI    = props_low, 
      LS    = props_upp,
      CV    = props_cv,
      Num   = Num, 
      Deno  = Deno,
      deff = deff_deff,
      NEFE  = nefec,
      RIC   = rangoic,
      RCRE  = rangoicrel,
      GL    = grados,
      Valid = validez
    )
}

# Por desagregacion cruzada
srvyr_prop_by_new <- function(design, x, by_1, by_2) {
  
  design %>%
    group_by({{ by_1 }}, {{ by_2 }}) %>%
    summarise(
      props = survey_mean({{ x }},
                          proportion = TRUE,   
                          vartype = c("se", "ci", "cv"),
                          na.rm = T), 
      deff = survey_mean({{ x }},  
                         deff = "replace", 
                         na.rm = T),
      n_upm = n_distinct(id_upm[!is.na({{ x }})]),
      n_estratos = n_distinct(estrato[!is.na({{ x }})]),
      Num = sum({{ x }}, na.rm = TRUE),
      Deno = sum(!is.na({{ x }}))
    ) %>% 
    mutate(
      desag_2 = {{ by_2 }},
      rangoic = props_upp - props_low,
      rangoicrel = rangoic/props,
      grados = n_upm - n_estratos,
      nefec = ifelse(deff_deff < 1, Deno, Deno / deff_deff), 
      validez = case_when(Num == 0 ~ d, 
                          nefec < 30 ~ a,
                          rangoic >= 0.3 ~ a,
                          rangoic > 0.05 & rangoicrel > 1.3 ~ a,
                          grados < 8 ~ b,
                          rangoic <= 0.05 & props > 0 & grados >= 8 ~ c,
                          rangoic > 0.05 & rangoicrel <= 1.3 & props > 0 & grados >= 8 ~ c,
                          T ~ "algo esta mal"),
      props = props * 100,
      props_se = props_se * 100,
      props_low = props_low * 100,
      props_upp = props_upp * 100,
      props_cv = props_cv * 100,
      rangoic = rangoic * 100,
      rangoicrel = rangoicrel * 100
    ) %>% 
    select(
      Desag_2 = desag_2,
      Props = props, 
      EE    = props_se, 
      LI    = props_low, 
      LS    = props_upp,
      CV    = props_cv,
      Num   = Num, 
      Deno  = Deno,
      deff = deff_deff,
      NEFE  = nefec,
      RIC   = rangoic,
      RCRE  = rangoicrel,
      GL    = grados,
      Valid = validez
    )
  
}

#-------Funcion para calcular estadisticos para variables categoricas ----------

# Nacional
srvyr_freq <- function(design, x) {
  
  design %>%
    filter(!is.na({{ x }})) %>% 
    group_by({{ x }}) %>%
    summarise(
      props = survey_mean(proportion = T,
                          vartype = c("se", "ci", "cv"),
                          na.rm = T),
      deff = survey_mean(deff = "replace",
                         na.rm = T),
      n_upm = n_distinct(id_upm),
      n_estratos = n_distinct(estrato),
      Num = unweighted(n())) %>% 
    ungroup() %>% 
    mutate(
      desag = "Nacional",
      Deno = sum(Num),
      rangoic = props_upp - props_low,
      rangoicrel = rangoic/props,
      grados = n_upm - n_estratos,
      nefec = ifelse(deff_deff < 1, Deno, Deno / deff_deff), 
      validez = case_when(Num == 0 ~ d, 
                          nefec < 30 ~ a,
                          rangoic >= 0.3 ~ a,
                          rangoic > 0.05 & rangoicrel > 1.3 ~ a,
                          grados < 8 ~ b,
                          rangoic <= 0.05 & props > 0 & grados >= 8 ~ c,
                          rangoic > 0.05 & rangoicrel <= 1.3 & props > 0 & grados >= 8 ~ c,
                          T ~ "algo esta mal"),
      props = props * 100,
      props_se = props_se * 100,
      props_low = props_low * 100,
      props_upp = props_upp * 100,
      props_cv = props_cv * 100,
      rangoic = rangoic * 100,
      rangoicrel = rangoicrel * 100
    ) %>% 
    select(
      {{ x }},
      Props = props, 
      EE    = props_se, 
      LI    = props_low, 
      LS    = props_upp,
      CV    = props_cv,
      Num   = Num, 
      Deno  = Deno,
      deff = deff_deff,
      NEFE  = nefec,
      RIC   = rangoic,
      RCRE  = rangoicrel,
      GL    = grados,
      Valid = validez
    )
}

# Por desagregacion 
srvyr_freq_by <- function(design, x, by) {
  
  design %>%
    filter(!is.na({{ x }})) %>% 
    group_by({{ by }}, {{ x }}) %>%
    summarise(
      props = survey_mean(proportion = T,
                          vartype = c("se", "ci", "cv"),
                          na.rm = T),
      deff = survey_mean(deff = "replace",
                         na.rm = T),
      n_upm = n_distinct(id_upm),
      n_estratos = n_distinct(estrato),
      Num = unweighted(n())) %>% 
    ungroup() %>% 
    group_by({{ by }}) %>% 
    mutate(Deno = sum(Num)) %>% 
    ungroup() %>% 
    mutate(
      rangoic = props_upp - props_low,
      rangoicrel = rangoic/props,
      grados = n_upm - n_estratos,
      nefec = ifelse(deff_deff < 1, Deno, Deno / deff_deff), 
      validez = case_when(Num == 0 ~ d, 
                          nefec < 30 ~ a,
                          rangoic >= 0.3 ~ a,
                          rangoic > 0.05 & rangoicrel > 1.3 ~ a,
                          grados < 8 ~ b,
                          rangoic <= 0.05 & props > 0 & grados >= 8 ~ c,
                          rangoic > 0.05 & rangoicrel <= 1.3 & props > 0 & grados >= 8 ~ c,
                          T ~ "algo esta mal"),
      props = props * 100,
      props_se = props_se * 100,
      props_low = props_low * 100,
      props_upp = props_upp * 100,
      props_cv = props_cv * 100,
      rangoic = rangoic * 100,
      rangoicrel = rangoicrel * 100
    ) %>% 
    select(
      {{ by }},
      {{ x }},
      Props = props, 
      EE    = props_se, 
      LI    = props_low, 
      LS    = props_upp,
      CV    = props_cv,
      Num   = Num, 
      Deno  = Deno,
      deff = deff_deff,
      NEFE  = nefec,
      RIC   = rangoic,
      RCRE  = rangoicrel,
      GL    = grados,
      Valid = validez
    )
  
}

#------------ Funcion para calcular promedios con sus estadisticos  ------------

# Nacional
srvyr_mean <- function(design, x) {
  
  design %>% 
    summarise(
      mean = survey_mean({{ x }},
                         vartype = c("se", "ci"),
                         na.rm = T),
      cv = survey_mean({{ x }},
                       vartype = c("cv"),
                       na.rm = T),
      deff = survey_mean({{ x }},  
                         deff = "replace", 
                         na.rm = T),
      n_upm = n_distinct(id_upm[!is.na({{ x }})]),
      n_estratos = n_distinct(estrato[!is.na({{ x }})]),
      Num = sum({{ x }} , na.rm = TRUE),
      Deno = sum(!is.na( {{ x }} ))) %>%
    mutate(
      desag = "Nacional",
      rangoic = (mean_upp - mean_low),
      rangoicrel = rangoic/mean,
      grados = n_upm - n_estratos,
      nefec = ifelse(deff_deff < 1, Deno, Deno / deff_deff), 
      validez = case_when(
        Num == 0 ~ d, 
        Deno < 100 ~ a,
        cv_cv > 0.15 ~ b, 
        cv_cv <= 0.15 & Deno >= 100 ~ c,
        T ~ "algo esta mal")
    ) %>% 
    mutate(
      cv_cv = cv_cv * 100,
      rangoicrel = rangoicrel * 100
    ) %>% 
    select(
      Desag = desag,
      Props = mean, 
      EE    = mean_se, 
      LI    = mean_low, 
      LS    = mean_upp,
      CV    = cv_cv,
      Num   = Num, 
      Deno  = Deno, 
      deff  = deff_deff,
      NEFE  = nefec,
      RIC   = rangoic,
      RCRE  = rangoicrel,
      GL    = grados,
      Valid = validez
    )
}

# Por desagregacion 
srvyr_mean_by <- function(design, x, by) {
  
  design %>% 
    group_by({{ by }}) %>%
    summarise(
      mean = survey_mean({{ x }},
                         vartype = c("se", "ci"),
                         na.rm = T),
      cv = survey_mean({{ x }},
                       vartype = c("cv"),
                       na.rm = T),
      deff = survey_mean({{ x }},  
                         deff = "replace", 
                         na.rm = T),
      n_upm = n_distinct(id_upm[!is.na({{ x }})]),
      n_estratos = n_distinct(estrato[!is.na({{ x }})]),
      Num = sum({{ x }}, na.rm = TRUE),
      Deno = sum(!is.na( {{ x }} ))) %>%
    mutate(
      desag = "Nacional",
      rangoic = (mean_upp - mean_low),
      rangoicrel = rangoic/mean,
      grados = n_upm - n_estratos,
      nefec = ifelse(deff_deff < 1, Deno, Deno / deff_deff), 
      validez = case_when(
        Num == 0 ~ d, 
        Deno < 100 ~ a,
        cv_cv > 0.15 ~ b, 
        cv_cv <= 0.15 & Deno >= 100 ~ c,
        T ~ "algo esta mal")
    ) %>% 
    mutate(
      cv_cv = cv_cv * 100,
      rangoicrel = rangoicrel * 100
    ) %>% 
    select(
      Desag = {{ by }},
      Props = mean, 
      EE    = mean_se, 
      LI    = mean_low, 
      LS    = mean_upp,
      CV    = cv_cv,
      Num   = Num, 
      Deno  = Deno,
      deff = deff_deff,
      NEFE  = nefec,
      RIC   = rangoic,
      RCRE  = rangoicrel,
      GL    = grados,
      Valid = validez
    )
}

# Por desagregacion cruzada
srvyr_mean_by_new <- function(design, x, by_1, by_2) {
  
  design %>% 
    group_by({{ by_1 }}, {{ by_2 }}) %>%
    summarise(
      mean = survey_mean({{ x }},
                         vartype = c("se", "ci"),
                         na.rm = T),
      cv = survey_mean({{ x }},
                       vartype = c("cv"),
                       na.rm = T),
      deff = survey_mean({{ x }},  
                         deff = "replace", 
                         na.rm = T),
      n_upm = n_distinct(id_upm[!is.na({{ x }})]),
      n_estratos = n_distinct(estrato[!is.na({{ x }})]),
      Num = sum({{ x }} , na.rm = TRUE),
      Deno = sum(!is.na( {{ x }} ))) %>%
    mutate(
      desag = "Nacional",
      rangoic = (mean_upp - mean_low),
      rangoicrel = rangoic/mean,
      grados = n_upm - n_estratos,
      nefec = ifelse(deff_deff < 1, Deno, Deno / deff_deff), 
      validez = case_when(
        Num == 0 ~ d, 
        Deno < 100 ~ a,
        cv_cv > 0.15 ~ b, 
        cv_cv <= 0.15 & Deno >= 100  ~ c,
        T ~ "algo esta mal")
    ) %>% 
    mutate(
      cv_cv = cv_cv * 100,
      rangoicrel = rangoicrel * 100
    ) %>% 
    select(
      Desag = {{ by_2 }},
      Props = mean, 
      EE    = mean_se, 
      LI    = mean_low, 
      LS    = mean_upp,
      CV    = cv_cv,
      Num   = Num, 
      Deno  = Deno,
      deff = deff_deff,
      NEFE  = nefec,
      RIC   = rangoic,
      RCRE  = rangoicrel,
      GL    = grados,
      Valid = validez
    )
}

#==============================================================================#
####                Funcion exportacion tabulados comparación               ####
#==============================================================================#

Style_tab <- function(z, x) {
  
  # a <- "Suprimir"
  # b <- "Revisar"
  # c <- "Estimación precisa"
  # d <- "Sin casos"
  
  # Encontrar el indicador
  indice <- suppressWarnings(which(df_title[, 1] == x))
  
  if (length(indice) > 0) {
    yz <- df_title[indice, 2] # Nombre del indicador
  } else {
    warning("\t>>No se encontró el indicador")
    yz <- NA
  }
  
  y = nrow(z) #obtener el tamaño de tabla
  z <- as.data.frame(z)
  # guardar el vector donde se encuentren el criterio de analisis
  vect <- z %>% 
    mutate( condicional = case_when(
      Valid == a ~ 1,
      Valid == d ~ 2,
      Valid == b ~ 3,
      TRUE ~ NA_real_ )) %>% 
    select(c(condicional))  
  
  # filtrar la tabla por variables
  z <- z %>% 
    select(-c(RIC, RCRE,Valid)) 
  
  # revision de la tabla sobre numeradores igual a cero
  pos_fila <- which(z$Num == 0)
  # Verificar si hay filas encontradas
  
  
  # Revisar numeradores igual a cero -----------------------
  
  if (length(pos_fila) > 0) {
    for (fila in pos_fila) {
      z[fila, 9:11] <- NA
    }
  }
  
  # Determinar matrices con numerador y denominador iguales
  verf_fila <- which(z$Num == z$Deno)
  
  if(length(verf_fila) > 0){
    for (fila in verf_fila) {
      z[fila, 9:10] <- NA
    }
  }
  
  # Verificar matriz para insertar formatos
  if (grepl("romedio", yz)) {
    z <- z[, -7]  # Elimina la septima columna del dataframe z
  }
  
  # reconocer afro y poner dos asteriscos
  z[,1] <- sapply(z[,1], function(x) gsub("Afroecuatoriana/o", "Afroecuatoriana/o **", x))
  
  wb <- loadWorkbook(ruta_archivo_formato)
  
  columna <- ncol(z)
  #------------------------- Hipervinculo ----------------------
  headSty <- createStyle(fontSize = 9.5,
                         fontName = "Century Gothic",
                         halign   = "center")
  
  addStyle(wb,
           sheet      = x,
           style      = headSty,
           rows       = 2,
           cols       = columna+2,
           gridExpand = TRUE
  )
  
  writeFormula(wb, x,
               startRow = 2,
               startCol = columna+2,
               x = makeHyperlinkString(
                 sheet  = "INDICE", 
                 row = 1, 
                 col = 1,
                 text   = "Índice"
               )
  )
  
  # Configurar columnas
  setColWidths(wb, 
               sheet = x, 
               cols = 1, 
               widths = 1.5)
  
  # Hallar el índice de la tabla a exportar
  indice <- which(df_title[, 1] == x)
  
  if (length(indice) > 0) {} else {
    
    warning(sprintf("No se encontró el indicador <<%s>>, revisar.", x))
    
  }
  
  # --------------------- Formato a los tabulados-----------------------#
  
  # Encabezado de la tabla descriptiva 
  headSty <- createStyle(fgFill         = "#836FFF", # color celdas
                         fontColour     = "#363636", #color de fuente
                         fontSize       = 10.5, #tamanio de letra
                         fontName       = "Century Gothic", #fuente
                         halign         = "center", 
                         valign         = "center",
                         border         = "TopBottomLeftRight",
                         borderColour   = "#104E8B", # color filos
                         textDecoration = "bold",
                         wrapText       = T
  ) 
  
  
  
  if (grepl("romedio", yz)){
    df <- data.frame(matrix(ncol = 11, nrow = 0))
    colnames(df) <- c("Desagregación","", 
                      "Indicador", "Error     Estándar",
                      "Límite     Inferior", "Límite Superior",
                      "Coeficiente de Variación",  
                      "Número de casos efectivos",  "Efecto de Diseño",
                      "Muestra     Efectiva", "Grados de      libertad"
    )
    
    #---- insertar imagen
    
    # img <- file.path("Info", "Imagen", "endi_imagen.png")
    # 
    # insertImage(wb,
    #             sheet = x,
    #             file = img,
    #             startRow = 1,
    #             startCol = "B",
    #             width = 35.2,
    #             height = 2.30,
    #             units = "cm")
    # 
    # setRowHeights(wb,sheet = x,rows = 1,heights = 70)
    # 
    # setColWidths(wb, sheet = x,cols = 1, widths = 1.5)
    
    
  } else{
    df <- data.frame(matrix(ncol = 12, nrow = 0))
    colnames(df) <- c("Desagregación","", 
                      "Indicador", "Error     Estándar",
                      "Límite     Inferior", "Límite Superior",
                      "Coeficiente de Variación",  "Numerador Muestral", 
                      "Denominador Muestral",  "Efecto de Diseño",
                      "Muestra     Efectiva", "Grados de      libertad"
    )
    
    # ---- insertar imagen
    
    # img <- file.path("Info", "Imagen", "endi_imagen.png")
    # 
    # insertImage(wb,
    #             sheet = x,
    #             file = img,
    #             startRow = 1,
    #             startCol = "B",
    #             width = 37.5,
    #             height = 2.30,
    #             units = "cm")
    # 
    # setRowHeights(wb,sheet = x,rows = 1,heights = 70)
    # 
    # setColWidths(wb, sheet = x,cols = 1, widths = 1.5)
    
  }
  
  writeData(wb,
            sheet       = x,
            x           = df,
            xy          = c("B", 8),
            colNames    = TRUE,
            headerStyle = headSty,
  )
  
  setColWidths(wb, 
               sheet  = x, 
               cols   = 2, 
               widths = 17.5
  ) 
  
  setColWidths(wb, 
               sheet  = x, 
               cols   = 3, 
               widths = 27
  ) 
  
  mergeCells(wb,
             sheet = x,
             cols  = 2:3,
             rows  = 8
  )
  
  if (grepl("romedio", yz)) {
    setColWidths(wb, 
                 sheet  = x, 
                 cols   = c(4:9,10:12), 
                 widths = 11.50
    ) 
    
    tabSty <- createStyle(fontSize       = 10.5, #tamanio de letra
                          fontName       = "Century Gothic", #fuente
                          halign         = "right",
                          border         = "TopBottomLeftRight",
                          borderColour   = "#104E8B", # color filos
                          numFmt         = "0.0" 
    )
    
    addStyle(wb,
             sheet      = x,
             style      = tabSty,
             rows       = 9:(y+8),
             cols       = c(4:8, 10),
             gridExpand = TRUE
    )
    
    
    tabSty <- createStyle(fontSize       = 10.5, #tamanio de letra
                          fontName       = "Century Gothic", #fuente
                          halign         = "right",
                          border         = "TopBottomLeftRight",
                          borderColour   = "#104E8B", # color filos
                          numFmt         = "#,##0"
    )
    
    
    addStyle(wb,
             sheet      = x,
             style      = tabSty,
             rows       = 9:(y+8),
             cols       = c(9,11:12),
             gridExpand = TRUE
    )
    
  } else{
    setColWidths(wb, 
                 sheet  = x, 
                 cols   = c(4:9,11:13), 
                 widths = 11.50
    ) 
    tabSty <- createStyle(fontSize       = 10.5, #tamanio de letra
                          fontName       = "Century Gothic", #fuente
                          halign         = "right",
                          border         = "TopBottomLeftRight",
                          borderColour   = "#104E8B", # color filos
                          numFmt         = "0.0" 
    )
    
    addStyle(wb,
             sheet      = x,
             style      = tabSty,
             rows       = 9:(y+8),
             cols       = c(4:8, 11),
             gridExpand = TRUE
    )
    
    
    tabSty <- createStyle(fontSize       = 10.5, #tamanio de letra
                          fontName       = "Century Gothic", #fuente
                          halign         = "right",
                          border         = "TopBottomLeftRight",
                          borderColour   = "#104E8B", # color filos
                          numFmt         = "#,##0"
    )
    
    
    addStyle(wb,
             sheet      = x,
             style      = tabSty,
             rows       = 9:(y+8),
             cols       = c(9:10,12:13),
             gridExpand = TRUE
    )
    
  }
  
  
  
  setColWidths(wb, 
               sheet  = x, 
               cols   = 10, 
               widths = 13
  ) 
  
  setColWidths(wb, 
               sheet  = x, 
               cols   = 9, 
               widths = 14.5
  ) 
  
  
  setColWidths(wb, 
               sheet  = x, 
               cols   = c(8), 
               widths = 15
  ) 
  
  setColWidths(wb, 
               sheet  = x, 
               cols   = c(10), 
               widths = 15
  ) 
  
  # Tabla descriptiva de resultados
  writeData(wb,
            sheet       = x, 
            x           = z,
            xy          = c("C", 9),
            colNames    = F
  )
  
  
  # Especificacion de desagregacion 
  tabSty <- createStyle(fontSize       = 10.5, #tamanio de letra
                        fontName       = "Century Gothic", #fuente
                        halign         = "center",
                        valign         = "center",
                        border         = "TopBottomLeftRight",
                        borderColour   = "#104E8B", # color filos
                        wrapText = T
  )
  
  addStyle(wb,
           sheet      = x,
           style      = tabSty,
           rows       = 9:(y+8),
           cols       = 2:3,
           gridExpand = TRUE
  )
  
  
  terminos <- c("Nacional",
                "Urbano", "Sierra", 
                "Azuay", "Priorizadas", 
                "Hombre", "Indígena", 
                "No pobreza por ingresos", "No pobreza por NBI", 
                "Quintil 1",
                "Anemia grave", "Urbano: Anemia grave",
                "Sin presencia","Urbano: Sin presencia",
                "Riesgo muy alto","Urbano: Riesgo muy alto",
                "Sin desnutrición crónica","Ninguno/Educación Básica")
  
  # Etiquetas correspondientes a cada término
  etiquetas <- c("Nacional *",
                 "Área", "Región Natural", 
                 "Provincia", "Parroquias", 
                 "Sexo", "Auto-Identificación Étnica", 
                 "Pobreza por Ingresos", "Pobreza por NBI", 
                 "Quintiles",
                 "Nacional *","Área",
                 "Nacional *","Área",
                 "Nacional *","Área",
                 "Desnutrición Crónica","Nivel Educativo")
  
  # Rango de filas a fusionar para cada término
  rangos_fusion <- list(1,
                        1,2,
                        22,1,
                        1,4,
                        1,1,
                        4,
                        3,7,
                        3,7,
                        3,7,
                        1,2
  )
  desagregaciones <- ""
  
  # Iterar sobre los términos
  for (i in seq_along(terminos)) {
    
    fila_area <- which(z$Desag == terminos[i])
    
    if (!is.na(fila_area)&& length(fila_area) > 0) { # Si se encontró la posición
      
      fila_area <- fila_area + 8 # Ajuste en la fila
      
      # Escribir en la hoja
      writeData(wb, 
                sheet = x, 
                x = etiquetas[i], 
                xy = c("B", fila_area))
      
      # obtener las desagregaciones -----------------------
      desagregaciones <- paste(desagregaciones,etiquetas[i],sep = " / ")
      
      
      # -----------
      
      # Fusionar celdas según el término
      if (terminos[i] != "Nacional") {
        
        mergeCells(wb, 
                   sheet = x, 
                   rows = fila_area:(fila_area + as.numeric(rangos_fusion[[i]])), 
                   cols = 2)
      } else {
        
        mergeCells(wb, 
                   sheet = x, 
                   rows = fila_area, 
                   cols = 2:3)
        
      }
    }
  }
  # -------   Imprimiendo las desagregaciones
  desagregaciones <- substr(desagregaciones, 3, nchar(desagregaciones))
  
  writeData(wb,sheet = x,
            x = desagregaciones,
            startCol = 3,
            startRow = 4,
            colNames = FALSE
  )
  
  # ----------------- Formato condicional -----------------
  
  # Copiando los valores de vect a una columna en específico
  
  writeData(wb,
            sheet       = x,
            x           = vect,
            xy          = c("ZA", 8),
            colNames    = TRUE
  )
  
  # Estilo para "Datos insuficientes"
  posStyle1 <- createStyle(fontColour = "#FF3232")
  
  for (i in 1:y+8) {
    conditionalFormatting(wb,
                          sheet = x,
                          rows  = i,
                          cols  = "D",
                          rule  = paste0("ZA", i, " == 1 "),
                          style = posStyle1
    )
  }
  
  # Estilo para "Desagregacion sin casos"
  posStyle2 <- createStyle(fontColour = "#006100",
                           bgFill     = "#EBE600"
  )
  
  for (i in 1:y+8) {
    conditionalFormatting(wb,
                          sheet = x,
                          rows  = i,
                          cols  = "D",
                          rule  = paste0("ZA", i, " == 2 "),
                          style = posStyle2
    )
  }
  
  # Estilo para "errores estandar cercanos a cero"
  posStyle3 <- createStyle(fontColour = "#000000",
                           bgFill     = "#FFC000"
  )
  
  for (i in 1:y+8) {
    conditionalFormatting(wb,
                          sheet = x,
                          rows  = i,
                          cols  = "D",
                          rule  = paste0("I", i, " == J",i),
                          style = posStyle3
    )
  }
  
  # Estilo para "grados de libertad menor a 8"
  posStyle4 <- createStyle(fontColour = "#16D089")
  
  for (i in 1:y+8) {
    conditionalFormatting(wb,
                          sheet = x,
                          rows  = i,
                          cols  = "D",
                          rule  = paste0("ZA", i, " == 3 "),
                          style = posStyle4
    )
  }
  
  # ----------   Figura y pie de página
  cir_1 <- ggplot() +
    coord_fixed(ratio = 1) +
    geom_circle(aes(x0 = 1, y0 = 1, r = 1),
                fill = "#FF3232", color = "#FF3232") +
    labs(x = NULL, y = NULL) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.ticks.length = unit(0, "pt"),
          axis.title = element_blank(),
          plot.background = element_blank(),
          plot.margin = grid::unit(c(0,0,0,0),"cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  print(cir_1) # plot needs to be showing
  insertPlot(wb, 
             x,
             xy = c("B", y+11),
             width = 0.15, 
             height = 0.15, 
             fileType = "png", 
             units = "in")
  
  #condicion 2
  cir_1 <- ggplot() +
    coord_fixed(ratio = 1) +
    geom_circle(aes(x0 = 1, y0 = 1, r = 1),
                fill = "#EBE600", color = "#EBE600") +
    labs(x = NULL, y = NULL) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.ticks.length = unit(0, "pt"),
          axis.title = element_blank(),
          plot.background = element_blank(),
          plot.margin = grid::unit(c(0,0,0,0),"cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  print(cir_1) # plot needs to be showing
  
  
  if (grepl("romedio", yz)){
    
    nota <- data.frame(text = c(
      # El espacio empieza en la posicion de fila: fil_link
      "      Los valores con fuente en rojo, presentan al menos una de las siguientes características:"
      ,"         - Cuando el tamaño de la muestra es menor a 100"
      ,"         - Cuando el coeficiente de variación es menor a 15"
      ,"       El parámetro calculado no presenta casos, quiere decir que el numerador es igual a cero."
      ,"*  La estimación nacional no incluye Galápagos"
      ,"** Afroecuatoriana/o: Esta categoría esta compuesta por afrodescendientes, negras/os, mulatas/os."
    ), stringsAsFactors = FALSE)
    
    insertPlot(wb, 
               x,
               xy = c("B", y+14),
               width = 0.15, 
               height = 0.15, 
               fileType = "png", 
               units = "in")
    
    fil_link <- 100
  }else{ 
    
    verf_fila <- which(z$Num == z$Deno)
    
    if(length(verf_fila) > 0){
      nota <- data.frame(text = c(
        # El espacio empieza en la posicion de fila: fil_link
        "      Los valores con fuente en rojo, presentan al menos una de las siguientes características:"
        ,"         - Muestras efectivas menor a 30"
        ,"         - El rango del intervalo de confianza superior a 30 puntos porcentuales"
        ,"         - El rango del intervalo de confianza superior a 5 puntos porcentuales y el rango relativo superior a 130%"
        ,"      El parámetro calculado no presenta casos, quiere decir que el numerador es igual a cero."
        ,"       El estimador del indicador es del 100% y no presenta varianza , por ende el efecto de diseño es un número grande con tendencia al infinito."
        ,"        Estimación con grados de libertad menor a 8, usar con precaución."
        ,"Para más información dirigirse al siguiente link:"
        ,""
        ,"*  La estimación nacional no incluye Galápagos"
        ,"** Afroecuatoriana/o: Esta categoría esta compuesta por afrodescendientes, negras/os, mulatas/os."
      ), stringsAsFactors = FALSE)
      
      fil_link <- 19
      
      insertPlot(wb, 
                 x,
                 xy = c("B", y+15),
                 width = 0.15, 
                 height = 0.15, 
                 fileType = "png", 
                 units = "in")
      
      
      #condicion 3 Adicional----------------------
      cir_1 <- ggplot() +
        coord_fixed(ratio = 1) +
        geom_circle(aes(x0 = 1, y0 = 1, r = 1),
                    fill = "#FFC000", color = "#FFC000") +
        labs(x = NULL, y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.ticks.length = unit(0, "pt"),
              axis.title = element_blank(),
              plot.background = element_blank(),
              plot.margin = grid::unit(c(0,0,0,0),"cm"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
      
      print(cir_1) # plot needs to be showing
      
      insertPlot(wb, 
                 x,
                 xy = c("B", y+16),
                 width = 0.15, 
                 height = 0.15, 
                 fileType = "png", 
                 units = "in")
      
      #condicion 4 Adicional----------------------
      cir_1 <- ggplot() +
        coord_fixed(ratio = 1) +
        geom_circle(aes(x0 = 1, y0 = 1, r = 1),
                    fill = "#16D089", color = "#16D089") +
        labs(x = NULL, y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.ticks.length = unit(0, "pt"),
              axis.title = element_blank(),
              plot.background = element_blank(),
              plot.margin = grid::unit(c(0,0,0,0),"cm"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
      
      print(cir_1) # plot needs to be showing
      
      insertPlot(wb, 
                 x,
                 xy = c("B", y+17),
                 width = 0.15, 
                 height = 0.15, 
                 fileType = "png", 
                 units = "in")
      
    }else{
      nota <- data.frame(text = c(
        # El espacio empieza en la posicion de fila: fil_link
        "      Los valores con fuente en rojo, presentan al menos una de las siguientes características:"
        ,"         - Muestras efectivas menor a 30"
        ,"         - El rango del intervalo de confianza superior a 30 puntos porcentuales"
        ,"         - El rango del intervalo de confianza superior a 5 puntos porcentuales y el rango relativo superior a 130%"
        ,"      El parámetro calculado no presenta casos, quiere decir que el numerador es igual a cero."
        ,"       Estimación con grados de libertad menor a 8, usar con precaución."
        ,"Para más información dirigirse al siguiente link:"
        ,""
        ,"*  La estimación nacional no incluye Galápagos"
        ,"** Afroecuatoriana/o: Esta categoría esta compuesta por afrodescendientes, negras/os, mulatas/os."
      ), stringsAsFactors = FALSE)
      
      fil_link <- 18
      
      insertPlot(wb, 
                 x,
                 xy = c("B", y+15),
                 width = 0.15, 
                 height = 0.15, 
                 fileType = "png", 
                 units = "in")
      
      #condicion 3 Adicional----------------------
      cir_1 <- ggplot() +
        coord_fixed(ratio = 1) +
        geom_circle(aes(x0 = 1, y0 = 1, r = 1),
                    fill = "#16D089", color = "#16D089") +
        labs(x = NULL, y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.ticks.length = unit(0, "pt"),
              axis.title = element_blank(),
              plot.background = element_blank(),
              plot.margin = grid::unit(c(0,0,0,0),"cm"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
      
      print(cir_1) # plot needs to be showing
      
      insertPlot(wb, 
                 x,
                 xy = c("B", y+16),
                 width = 0.15, 
                 height = 0.15, 
                 fileType = "png", 
                 units = "in")
      
    }
  }
  
  # colocar la Nota seleccionada
  
  indice <- suppressWarnings(which(df_title[, 1] == x))
  
  if (length(indice) > 0) {
    notas <- df_title[indice, 5] # Notas ingresadas al estimador
  } else {  }
  
  nota_2 <- data.frame(nota= notas, stringsAsFactors = FALSE)
  
  # Ingreso de datos para la nota
  writeData(wb,
            sheet       = x, 
            x           = nota,
            xy          = c("B", (y+11)),
            colNames = FALSE
  )
  
  # Ingreso y arreglo de la nota 2 en excel
  fila_nota2 <- y+12+nrow(nota)
  
  writeData(wb,
            sheet       = x, 
            x           = nota_2,
            xy          = c("B", fila_nota2),
            colNames = FALSE
  )
  
  tabSty <- createStyle(fontSize = 9,
                        fontName = "Century Gothic",
                        halign         = "left",
                        valign         = "top",
  )
  
  mergeCells(wb, 
             sheet = x, 
             rows = fila_nota2, 
             cols = 2:9)
  
  setRowHeights(wb, 
                sheet   = x, 
                rows    = fila_nota2, 
                heights = 90)
  
  # Dar un mismo formato a todas las notas
  addStyle(wb,
           sheet      = x,
           style      = tabSty,
           rows       = (y+10):(y+30),
           cols       = c(2:4),  
           gridExpand = TRUE
  )
  # A la nota 2 justificarla
  tabSty <- createStyle(fontSize = 9,
                        fontName = "Century Gothic",
                        halign         = "left",
                        valign         = "top",
                        wrapText = TRUE
  )
  
  
  #------------------------- Hipervinculo ----------------------
  
  url <- "https://www.cdc.gov/nchs/data/series/sr_02/sr02_175.pdf"
  display_text <- "Link to Vital and Health Statistics"
  
  
  writeFormula(wb, x,
               startRow = y + fil_link,
               startCol = 3,
               x = sprintf('HYPERLINK("%s", "%s")', url, display_text)
  )
  
  
  addStyle(wb,
           sheet      = x,
           style      = tabSty,
           rows       = fila_nota2,
           cols       = 2,  
           gridExpand = TRUE
  )
  
  # Dar formato al hiperviculo 
  tabSty2 <- createStyle(fontSize = 9,
                         fontName = "Century Gothic",
                         halign   = "center",
                         fontColour = "#2F51F9",
                         textDecoration = "underline"
  )
  
  addStyle(wb,
           sheet      = x,
           style      = tabSty2,
           rows       = y + fil_link,
           cols       = 3,  
           gridExpand = TRUE,
  )
  
  # -------------- ocultar columna base Validez 
  
  tabSty <- createStyle(fontSize       = 6, #tamanio de letra
                        fontColour     = "#FFFFFF"
  )
  
  addStyle(wb,
           sheet      = x,
           style      = tabSty,
           rows       = 8:(y+8),
           cols       = c("ZA"),
           gridExpand = TRUE
  )
  
  writeData(wb,
            sheet       = x,
            x           = vect,
            xy          = c("ZA", 8),
            colNames    = TRUE
  )
  
  
  if(is.na(nota_2)){
    addStyle(wb,
             sheet      = x,
             style      = tabSty,
             rows       = (y+11),
             cols       = c("B"),
             gridExpand = TRUE
    )
  }
  
  # Arreglo de tabla
  
  
  # ---- manejar un error de permisos-----
  result <- tryCatch({
    saveWorkbook(wb, ruta_archivo_formato, overwrite = TRUE)
    TRUE  # Indicate success
  }, warning = function(w) {
    if (grepl("Permission denied", w$message)) {
      return(FALSE)  # Indicate failure due to permission issue
    }
    warning(w)  # Pass other warnings to the default warning handler
    return(TRUE)  # Indicate success for other warnings
  }, error = function(e) {
    stop(e)  # Pass errors to the default error handler
  })
  
  if (result) {
    warning("\nSe ha exportado el tabulado: ",x)
  } else {
    cat(sprintf("\t<<NO SE PUDO EXPORTAR>>\n Cerrar el documento si está abierto.\n En la ruta:%s\n", 
                ruta_archivo_formato))
  }
  
}

#-------------------------------------------------------------------------------
# Para exportar con tabulados de hojas unicas por separado
#-------------------------------------------------------------------------------

Style_tab_0 <- function(z, x) { 
  
  z <- as.data.frame(z)
  
  ruta_archivo_formato_2 <- paste0(link_tabulados,"/ENDI_tab.xlsx")
  
  result <- suppressWarnings(file.exists(ruta_archivo_formato_2))
  
  if (result) {
    wb <- loadWorkbook(ruta_archivo_formato_2)
  }else{
    wb <- createWorkbook()
  }
  # buscar y reemplazar valor en z o tabla
  
  # guardar el vector donde se encuentren el criterio de analisis
  
  vect <- z %>% 
    mutate( condicional = case_when(
      Valid == a ~ 1,
      Valid == d ~ 2,
      Valid == b ~ 3,
      TRUE ~ NA_real_ )) %>% 
    select(c(condicional)) 
  
  # filtrar la tabla por variables
  z <- z %>% 
    select(-c(RIC, RCRE)) 
  
  # revision de la tabla sobre numeradores igual a cero
  pos_fila <- which(z$Num == 0)
  # Verificar si hay filas encontradas
  
  # Verificar si hay filas encontradas
  if (length(pos_fila) > 0) {
    for (fila in pos_fila) {
      z[fila, 9:11] <- NA
    }
  }
  
  verf_fila <- which(z$Num == z$Deno)
  
  if(length(verf_fila) > 0){
    for (fila in verf_fila) {
      z[fila, 9:10] <- NA
    }
  }
  
  # Hallar el índice de la tabla a exportar
  indice <- suppressWarnings(which(df_title[, 1] == x))
  
  if (length(indice) > 0) {
    xz <- df_title[indice, 1] # Codigo nombre
    yz <- df_title[indice, 2] # Nombre del indicador
    notas <- df_title[indice, 5] # Notas ingresadas al estimador
  } else {
    warning("\t>>No se encontró el indicador")
    xz <- x
    yz <- NA
    notas <- NA
  }
  
  # Verificar matriz para insertar formatos
  if (grepl("romedio", yz)) {
    z <- z[, -7]  # Elimina la septima columna del dataframe z
  }
  columna <- ncol(z)
  # buscar y reemplazar valor en z o tabla
  z[,1] <- sapply(z[,1], function(x) gsub("Afroecuatoriana/o", "Afroecuatoriana/o **", x))
  
  
  addWorksheet(wb, sheetName = x)
  
  # a <- "Suprimir"
  # b <- "Revisar"
  # c <- "Estimación precisa"
  # d <- "Sin casos"
  
  y = nrow(z) #obtener el tamaño de tabla
  col = ncol(z)
  
  #----------- formato tabulado
  
  # Ingresar subtitulos para el encabezado
  df_eti <- data.frame(Etiqueta = c("Tabla N°:", "Indicador/variable:", "Población:",
                                    "Fuente:", "Elaboración:"),
                       Especificacion = c(xz, yz,
                                          "", 
                                          "Encuesta Nacional sobre Desnutrición Infantil - ENDI 2023 -2024",
                                          "Instituto Nacional de Estadística y Censos (INEC)")
  )
  
  writeData(wb,sheet = x,x = df_eti,startCol = 2,startRow = 2,
            colNames = FALSE)
  tabSty <- createStyle(fontSize = 10.5, fontName = "Century Gothic", 
                        halign = "left", textDecoration = "bold")
  tabSty1 <- createStyle(fontSize = 10.5, fontName = "Century Gothic", 
                         halign = "left")
  addStyle(wb, sheet = x, style = tabSty, rows = 2:7, cols = 2, 
           gridExpand = TRUE)
  addStyle(wb, sheet = x, style = tabSty1, rows = 2:7, cols = 3, 
           gridExpand = TRUE)
  
  # --------------------- Formato a los tabulados-----------------------#
  
  # Encabezado de la tabla descriptiva 
  headSty <- createStyle(fgFill         = "#836FFF", # color celdas
                         fontColour     = "#363636", #color de fuente
                         fontSize       = 10.5, #tamanio de letra
                         fontName       = "Century Gothic", #fuente
                         halign         = "center", 
                         valign         = "center",
                         border         = "TopBottomLeftRight",
                         borderColour   = "#104E8B", # color filos
                         textDecoration = "bold",
                         wrapText       = T
  ) 
  
  df <- data.frame(matrix(ncol = 12, nrow = 0))
  
  if (grepl("romedio", yz)){
    df <- data.frame(matrix(ncol = 11, nrow = 0))
    colnames(df) <- c("Desagregación","", 
                      "Indicador", "Error     Estándar",
                      "Límite     Inferior", "Límite Superior",
                      "Coeficiente de Variación",  
                      "Número de casos efectivos",  "Efecto de Diseño",
                      "Muestra     Efectiva", "Grados de      libertad"
    )
    
    # ---- insertar imagen
    
    # img <- file.path("Info", "Imagen", "endi_imagen.png")
    # 
    # insertImage(wb,
    #             sheet = x,
    #             file = img,
    #             startRow = 1,
    #             startCol = "B",
    #             width = 35.2,
    #             height = 2.30,
    #             units = "cm")
    
  } else{
    df <- data.frame(matrix(ncol = 12, nrow = 0))
    colnames(df) <- c("Desagregación","", 
                      "Indicador", "Error     Estándar",
                      "Límite     Inferior", "Límite Superior",
                      "Coeficiente de Variación",  "Numerador Muestral", 
                      "Denominador Muestral",  "Efecto de Diseño",
                      "Muestra     Efectiva", "Grados de      libertad"
    )
    
    # ---- insertar imagen
    
    # img <- file.path("Info", "Imagen", "endi_imagen.png")
    # 
    # insertImage(wb,
    #             sheet = x,
    #             file = img,
    #             startRow = 1,
    #             startCol = "B",
    #             width = 37.5,
    #             height = 2.30,
    #             units = "cm")
    
    
  }
  setRowHeights(wb,sheet = x,rows = 1,heights = 70)
  setColWidths(wb, sheet = x,cols = 1, widths = 1.5)
  
  writeData(wb,
            sheet       = x,
            x           = df,
            xy          = c("B", 8),
            colNames    = TRUE,
            headerStyle = headSty,
  )
  
  setColWidths(wb, 
               sheet  = x, 
               cols   = 2, 
               widths = 17.5
  ) 
  
  setColWidths(wb, 
               sheet  = x, 
               cols   = 3, 
               widths = 31
  ) 
  
  mergeCells(wb,
             sheet = x,
             cols  = 2:3,
             rows  = 8
  )
  
  if (grepl("romedio", yz)) {
    setColWidths(wb, 
                 sheet  = x, 
                 cols   = c(4:9,10:12), 
                 widths = 11.50
    ) 
    
    tabSty <- createStyle(fontSize       = 10.5, #tamanio de letra
                          fontName       = "Century Gothic", #fuente
                          halign         = "right",
                          border         = "TopBottomLeftRight",
                          borderColour   = "#104E8B", # color filos
                          numFmt         = "0.0" 
    )
    
    addStyle(wb,
             sheet      = x,
             style      = tabSty,
             rows       = 9:(y+8),
             cols       = c(4:8, 10),
             gridExpand = TRUE
    )
    
    
    tabSty <- createStyle(fontSize       = 10.5, #tamanio de letra
                          fontName       = "Century Gothic", #fuente
                          halign         = "right",
                          border         = "TopBottomLeftRight",
                          borderColour   = "#104E8B", # color filos
                          numFmt         = "#,##0"
    )
    
    
    addStyle(wb,
             sheet      = x,
             style      = tabSty,
             rows       = 9:(y+8),
             cols       = c(9,11:12),
             gridExpand = TRUE
    )
    
  } else{
    setColWidths(wb, 
                 sheet  = x, 
                 cols   = c(4:9,11:13), 
                 widths = 11.50
    ) 
    tabSty <- createStyle(fontSize       = 10.5, #tamanio de letra
                          fontName       = "Century Gothic", #fuente
                          halign         = "right",
                          border         = "TopBottomLeftRight",
                          borderColour   = "#104E8B", # color filos
                          numFmt         = "0.0" 
    )
    
    addStyle(wb,
             sheet      = x,
             style      = tabSty,
             rows       = 9:(y+8),
             cols       = c(4:8, 11),
             gridExpand = TRUE
    )
    
    
    tabSty <- createStyle(fontSize       = 10.5, #tamanio de letra
                          fontName       = "Century Gothic", #fuente
                          halign         = "right",
                          border         = "TopBottomLeftRight",
                          borderColour   = "#104E8B", # color filos
                          numFmt         = "#,##0"
    )
    
    
    addStyle(wb,
             sheet      = x,
             style      = tabSty,
             rows       = 9:(y+8),
             cols       = c(9:10,12:13),
             gridExpand = TRUE
    )
    
  }
  
  setColWidths(wb, 
               sheet  = x, 
               cols   = 10, 
               widths = 13
  ) 
  
  
  # Tabla descriptiva de resultados
  
  writeData(wb,
            sheet       = x, 
            x           = z,
            xy          = c("C", 9),
            colNames    = F
  )
  
  
  # Especificacion de desagregacion 
  tabSty <- createStyle(fontSize       = 10.5, #tamanio de letra
                        fontName       = "Century Gothic", #fuente
                        halign         = "center",
                        valign         = "center",
                        border         = "TopBottomLeftRight",
                        borderColour   = "#104E8B", # color filos
                        wrapText = T
  )
  
  addStyle(wb,
           sheet      = x,
           style      = tabSty,
           rows       = 9:(y+8),
           cols       = 2:3,
           gridExpand = TRUE
  )
  
  # --------- combinaciones de columna
  terminos <- c("Nacional",
                "Urbano", "Sierra", 
                "Azuay", "Priorizadas", 
                "Hombre", "Indígena", 
                "No pobreza por ingresos", "No pobreza por NBI", 
                "Quintil 1",
                "Anemia grave", "Urbano: Anemia grave",
                "Sin presencia","Urbano: Sin presencia",
                "Riesgo muy alto","Urbano: Riesgo muy alto",
                "Sin desnutrición crónica","Ninguno/Educación Básica")
  
  # Etiquetas correspondientes a cada término
  etiquetas <- c("Nacional *",
                 "Área", "Región Natural", 
                 "Provincia", "Parroquias", 
                 "Sexo", "Auto-Identificación Étnica", 
                 "Pobreza por Ingresos", "Pobreza por NBI", 
                 "Quintiles",
                 "Nacional *","Área",
                 "Nacional *","Área",
                 "Nacional *","Área",
                 "Desnutrición Crónica","Nivel Educativo")
  
  # Rango de filas a fusionar para cada término
  rangos_fusion <- list(1,
                        1,2,
                        22,1,
                        1,4,
                        1,1,
                        4,
                        3,7,
                        3,7,
                        3,7,
                        1,2
  )
  
  desagregaciones <- ""
  
  # Iterar sobre los términos
  for (i in seq_along(terminos)) {
    
    fila_area <- which(z$Desag == terminos[i])
    
    if (!is.na(fila_area) && length(fila_area) > 0) { n
      
      fila_area <- fila_area + 8 # Ajuste en la fila
      
      # Escribir en la hoja
      writeData(wb, 
                sheet = x, 
                x = etiquetas[i], 
                xy = c("B", fila_area))
      
      # obtener las desagregaciones 
      
      desagregaciones <- paste(desagregaciones,etiquetas[i],sep = " / ")
      
      # Fusionar celdas segun el termino
      if (terminos[i] != "Nacional") {
        
        mergeCells(wb, 
                   sheet = x, 
                   rows = fila_area:(fila_area + 
                                       as.numeric(rangos_fusion[[i]])), 
                   cols = 2)
      } else {
        
        mergeCells(wb, 
                   sheet = x, 
                   rows = fila_area, 
                   cols = 2:3)
        
      }
    }
  }
  
  # ----------------- Formato condicional -----------------
  
  # Imprimiendo las desagregaciones
  desagregaciones <- substr(desagregaciones, 3, nchar(desagregaciones))
  
  writeData(wb,sheet = x,
            x = desagregaciones,
            startCol = 3,
            startRow = 4,
            colNames = FALSE
  )
  
  
  # Copiando los valores de vect a una columna en específico
  
  writeData(wb,
            sheet       = x,
            x           = vect,
            xy          = c("ZA", 8),
            colNames    = TRUE
  )
  
  
  # Estilo para "Datos insuficientes"
  posStyle1 <- createStyle(fontColour = "#FF3232")
  
  for (i in 1:y+8) {
    conditionalFormatting(wb,
                          sheet = x,
                          rows  = i,
                          cols  = "D",
                          rule  = paste0("ZA", i, " == 1 "),
                          style = posStyle1
    )
  }
  
  # Estilo para "Desagregacion sin casos"
  posStyle2 <- createStyle(fontColour = "#006100",
                           bgFill     = "#EBE600"
  )
  
  for (i in 1:y+8) {
    conditionalFormatting(wb,
                          sheet = x,
                          rows  = i,
                          cols  = "D",
                          rule  = paste0("ZA", i, " == 2 "),
                          style = posStyle2
    )
  }
  
  # Estilo para "errores estandar cercanos a cero"
  posStyle3 <- createStyle(fontColour = "#000000",
                           bgFill     = "#FFC000"
  )
  
  for (i in 1:y+8) {
    conditionalFormatting(wb,
                          sheet = x,
                          rows  = i,
                          cols  = "D",
                          rule  = paste0("I", i, " == J",i),
                          style = posStyle3
    )
  }
  
  
  # Estilo para "grados de libertad menor a 8"
  posStyle3 <- createStyle(fontColour = "#16D089")
  
  for (i in 1:y+8) {
    conditionalFormatting(wb,
                          sheet = x,
                          rows  = i,
                          cols  = "D",
                          rule  = paste0("ZA", i, " == 3"),
                          style = posStyle3
    )
  }
  
  
  #-------- stile para desgregación
  
  headSty <- createStyle(
    fontSize       = 10.5, #tamanio de letra
    fontName       = "Century Gothic", #fuente
    halign         = "center", 
    valign         = "center",
    border         = "TopBottomLeftRight",
    borderColour   = "#104E8B", # color filos
    wrapText       = T
  ) 
  
  addStyle(wb,
           sheet      = x,
           style      = headSty,
           rows       = 9:(y+8),
           cols       = c(2:3),
           gridExpand = TRUE
  )
  
  
  # ----------   Figura y pie de página
  cir_1 <- ggplot() +
    coord_fixed(ratio = 1) +
    geom_circle(aes(x0 = 1, y0 = 1, r = 1),
                fill = "#FF3232", color = "#FF3232") +
    labs(x = NULL, y = NULL) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.ticks.length = unit(0, "pt"),
          axis.title = element_blank(),
          plot.background = element_blank(),
          plot.margin = grid::unit(c(0,0,0,0),"cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  print(cir_1) # plot needs to be showing
  
  insertPlot(wb, 
             x,
             xy = c("B", y+11),
             width = 0.15, 
             height = 0.15, 
             fileType = "png", 
             units = "in")
  
  #condicion 2
  cir_1 <- ggplot() +
    coord_fixed(ratio = 1) +
    geom_circle(aes(x0 = 1, y0 = 1, r = 1),
                fill = "#EBE600", color = "#EBE600") +
    labs(x = NULL, y = NULL) +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.ticks.length = unit(0, "pt"),
          axis.title = element_blank(),
          plot.background = element_blank(),
          plot.margin = grid::unit(c(0,0,0,0),"cm"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank())
  
  
  print(cir_1) # plot needs to be showing
  
  if (grepl("romedio", yz)){
    
    nota <- data.frame(text = c(
      # El espacio empieza en la posicion de fila: fil_link
      "      Los valores con fuente en rojo, presentan al menos una de las siguientes características:"
      ,"         - Cuando el tamaño de la muestra es menor a 100"
      ,"         - Cuando el coeficiente de variación es menor a 15"
      ,"       El parámetro calculado no presenta casos, quiere decir que el numerador es igual a cero."
      ,"*  La estimación nacional no incluye Galápagos"
      ,"** Afroecuatoriana/o: Esta categoría esta compuesta por afrodescendientes, negras/os, mulatas/os."
    ), stringsAsFactors = FALSE)
    
    insertPlot(wb, 
               x,
               xy = c("B", y+14),
               width = 0.15, 
               height = 0.15, 
               fileType = "png", 
               units = "in")
    
    fil_link <- 100
  }else{ 
    
    verf_fila <- which(z$Num == z$Deno)
    
    if(length(verf_fila) > 0){
      nota <- data.frame(text = c(
        # El espacio empieza en la posicion de fila: fil_link
        "      Los valores con fuente en rojo, presentan al menos una de las siguientes características:"
        ,"         - Muestras efectivas menor a 30"
        ,"         - El rango del intervalo de confianza superior a 30 puntos porcentuales"
        ,"         - El rango del intervalo de confianza superior a 5 puntos porcentuales y el rango relativo superior a 130%"
        ,"      El parámetro calculado no presenta casos, quiere decir que el numerador es igual a cero."
        ,"       El estimador del indicador es del 100% y no presenta varianza , por ende el efecto de diseño es un número grande con tendencia al infinito."
        ,"        Estimación con grados de libertad menor a 8, usar con precaución."
        ,"Para más información dirigirse al siguiente link:"
        ,""
        ,"*  La estimación nacional no incluye Galápagos"
        ,"** Afroecuatoriana/o: Esta categoría esta compuesta por afrodescendientes, negras/os, mulatas/os."
      ), stringsAsFactors = FALSE)
      
      fil_link <- 19
      
      insertPlot(wb, 
                 x,
                 xy = c("B", y+15),
                 width = 0.15, 
                 height = 0.15, 
                 fileType = "png", 
                 units = "in")
      
      
      #condicion 3 Adicional----------------------
      cir_1 <- ggplot() +
        coord_fixed(ratio = 1) +
        geom_circle(aes(x0 = 1, y0 = 1, r = 1),
                    fill = "#FFC000", color = "#FFC000") +
        labs(x = NULL, y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.ticks.length = unit(0, "pt"),
              axis.title = element_blank(),
              plot.background = element_blank(),
              plot.margin = grid::unit(c(0,0,0,0),"cm"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
      
      print(cir_1) # plot needs to be showing
      
      insertPlot(wb, 
                 x,
                 xy = c("B", y+16),
                 width = 0.15, 
                 height = 0.15, 
                 fileType = "png", 
                 units = "in")
      
      #condicion 4 Adicional----------------------
      cir_1 <- ggplot() +
        coord_fixed(ratio = 1) +
        geom_circle(aes(x0 = 1, y0 = 1, r = 1),
                    fill = "#16D089", color = "#16D089") +
        labs(x = NULL, y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.ticks.length = unit(0, "pt"),
              axis.title = element_blank(),
              plot.background = element_blank(),
              plot.margin = grid::unit(c(0,0,0,0),"cm"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
      
      print(cir_1) # plot needs to be showing
      
      insertPlot(wb, 
                 x,
                 xy = c("B", y+17),
                 width = 0.15, 
                 height = 0.15, 
                 fileType = "png", 
                 units = "in")
      
    }else{
      nota <- data.frame(text = c(
        # El espacio empieza en la posicion de fila: fil_link
        "      Los valores con fuente en rojo, presentan al menos una de las siguientes características:"
        ,"         - Muestras efectivas menor a 30"
        ,"         - El rango del intervalo de confianza superior a 30 puntos porcentuales"
        ,"         - El rango del intervalo de confianza superior a 5 puntos porcentuales y el rango relativo superior a 130%"
        ,"      El parámetro calculado no presenta casos, quiere decir que el numerador es igual a cero."
        ,"       Estimación con grados de libertad menor a 8, usar con precaución."
        ,"Para más información dirigirse al siguiente link:"
        ,""
        ,"*  La estimación nacional no incluye Galápagos"
        ,"** Afroecuatoriana/o: Esta categoría esta compuesta por afrodescendientes, negras/os, mulatas/os."
      ), stringsAsFactors = FALSE)
      
      fil_link <- 18
      
      insertPlot(wb, 
                 x,
                 xy = c("B", y+15),
                 width = 0.15, 
                 height = 0.15, 
                 fileType = "png", 
                 units = "in")
      
      #condicion 3 Adicional----------------------
      cir_1 <- ggplot() +
        coord_fixed(ratio = 1) +
        geom_circle(aes(x0 = 1, y0 = 1, r = 1),
                    fill = "#16D089", color = "#16D089") +
        labs(x = NULL, y = NULL) +
        theme(axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.ticks.length = unit(0, "pt"),
              axis.title = element_blank(),
              plot.background = element_blank(),
              plot.margin = grid::unit(c(0,0,0,0),"cm"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.background = element_blank())
      
      print(cir_1) # plot needs to be showing
      
      insertPlot(wb, 
                 x,
                 xy = c("B", y+16),
                 width = 0.15, 
                 height = 0.15, 
                 fileType = "png", 
                 units = "in")
      
    }
  }
  
  # colcar la Nota seleccionada
  
  indice <- suppressWarnings(which(df_title[, 1] == x))
  
  if (length(indice) > 0) {
    notas <- df_title[indice, 5] # Notas ingresadas al estimador
  } else {  }
  
  nota_2 <- data.frame(nota= notas, stringsAsFactors = FALSE)
  
  # Ingreso de datos para la nota
  writeData(wb,
            sheet       = x, 
            x           = nota,
            xy          = c("B", (y+11)),
            colNames = FALSE
  )
  
  # Ingreso y arreglo de la nota 2 en excel
  fila_nota2 <- y+12+nrow(nota)
  
  writeData(wb,
            sheet       = x, 
            x           = nota_2,
            xy          = c("B", fila_nota2),
            colNames = FALSE
  )
  
  tabSty <- createStyle(fontSize = 9,
                        fontName = "Century Gothic",
                        halign         = "left",
                        valign         = "top",
  )
  
  mergeCells(wb, 
             sheet = x, 
             rows = fila_nota2, 
             cols = 2:9)
  
  setRowHeights(wb, 
                sheet   = x, 
                rows    = fila_nota2, 
                heights = 90)
  
  # Dar un mismo formato a todas las notas
  addStyle(wb,
           sheet      = x,
           style      = tabSty,
           rows       = (y+10):(y+30),
           cols       = c(2:4),  
           gridExpand = TRUE
  )
  # A la nota 2 justificarla
  tabSty <- createStyle(fontSize = 9,
                        fontName = "Century Gothic",
                        halign         = "left",
                        valign         = "top",
                        wrapText = TRUE
  )
  
  addStyle(wb,
           sheet      = x,
           style      = tabSty,
           rows       = fila_nota2,
           cols       = 2,  
           gridExpand = TRUE
  )
  
  #------------------------- Hipervinculo ----------------------
  
  url <- "https://www.cdc.gov/nchs/data/series/sr_02/sr02_175.pdf"
  display_text <- "Link to Vital and Health Statistics"
  
  
  writeFormula(wb, x,
               startRow = y + fil_link,
               startCol = 3,
               x = sprintf('HYPERLINK("%s", "%s")', url, display_text)
  )
  
  
  addStyle(wb,
           sheet      = x,
           style      = tabSty,
           rows       = fila_nota2,
           cols       = 2,  
           gridExpand = TRUE
  )
  
  # Dar formato al hiperviculo 
  tabSty2 <- createStyle(fontSize = 9,
                         fontName = "Century Gothic",
                         halign   = "center",
                         fontColour = "#2F51F9",
                         textDecoration = "underline"
  )
  
  addStyle(wb,
           sheet      = x,
           style      = tabSty2,
           rows       = y + fil_link,
           cols       = 3,  
           gridExpand = TRUE,
  )
  
  # -----------------ocultar columna base Validez
  
  tabSty <- createStyle(fontSize       = 6, #tamanio de letra
                        fontColour     = "#FFFFFF", #fuente
  )
  
  addStyle(wb,
           sheet      = x,
           style      = tabSty,
           rows       = 8:(y+8),
           cols       = c("ZA"),
           gridExpand = TRUE
  )
  
  writeData(wb,
            sheet       = x,
            x           = vect,
            xy          = c("ZA", 8),
            colNames    = TRUE
  )
  
  if (grepl("romedio", yz)){
    
    
    addStyle(wb,
             sheet      = x,
             style      = tabSty,
             rows       = 8:(y+8),
             cols       = c("M"),
             gridExpand = TRUE)
    
    
  }else{
    
    
    addStyle(wb,
             sheet      = x,
             style      = tabSty,
             rows       = 8:(y+8),
             cols       = c("N"),
             gridExpand = TRUE
    )
    
    
    
  }
  
  
  # ---- manejar un error de permisos-----
  
  result <- tryCatch({
    saveWorkbook(wb, ruta_archivo_formato_2, overwrite = TRUE)
    TRUE  
  }, warning = function(w) {
    if (grepl("Permission denied", w$message)) {
      return(FALSE) 
    }
    warning(w)  
    return(TRUE) 
  }, error = function(e) {
    stop(e) 
  })
  
  if (result) {
    cat(sprintf("Se ha exportado exitosamente el tabulado '%s' con formato; en la ruta:\n\t%s\n", 
                x, ruta_archivo_formato_2))
  } else {
    cat("'\t<<NO SE PUDO EXPORTAR>>\n Cerrar el documento si está abierto\n")
  }
  
}

#----------------------- Data frame de los parámetros por tematica
df_title <- data.frame(
  nombre = c(
    'T1_i1',
    'T1_i2',
    'T1_i3',
    'T2_i1',
    'T2_i2',
    'T2_i3',
    'T2_i4',
    'T2_i5',
    'T2_i6',
    'T2_i7',
    'T2_i8',
    'T2_i9',
    'T2_i10',
    'T2_i11',
    'T2_i12',
    'T2_i13',
    'T2_i14',
    'T2_i15',
    'T2_i16',
    'T2_i17',
    'T2_i18',
    'T3_i1',
    'T3_i2',
    'T3_i3',
    'T3_i4',
    'T3_i5',
    'T3_i6',
    'T4_i1',
    'T4_i2',
    'T4_i3',
    'T4_i4',
    'T4_i5',
    'T5_i1',
    'T5_i2',
    'T5_i3',
    'T5_i4',
    'T5_i5',
    'T5_i6',
    'T5_i7',
    'T5_i8',
    'T5_i9',
    'T5_i10',
    'T5_i11',
    'T5_i12',
    'T5_i13',
    'T5_i14',
    'T5_i15',
    'T5_i16',
    'T5_i17',
    'T5_i18',
    'T5_i19',
    'T5_i20',
    'T5_i21',
    'T5_i22',
    'T5_i23',
    'T5_i24',
    'T5_i25',
    'T5_i26',
    'T5_i27',
    'T5_i28',
    'T5_i29',
    'T5_i30',
    'T6_i1',
    'T6_i2',
    'T6_i3',
    'T6_i4',
    'T6_i5',
    'T6_i6',
    'T6_i7',
    'T7_i1',
    'T7_i2',
    'T8_i1',
    'T8_i2',
    'T8_i3',
    'T8_i4',
    'T8_i5',
    'T8_i6',
    'T8_i7',
    'T8_i8',
    'T8_i9',
    'T9_i1',
    'T9_i2',
    'T9_i3',
    'T9_i4',
    'T9_i5',
    'T9_i6',
    'T9_i7',
    'T9_i8',
    'T10_i1',
    'T10_i2',
    'T10_i3',
    'T10_i4',
    'T10_i5',
    'T10_i6',
    'T10_i7',
    'T10_i8',
    'T10_i9',
    'T10_i10',
    'T10_i11',
    'T10_i12',
    'T10_i13',
    'T10_i14',
    'T10_i15',
    'T10_i16',
    'T10_i17',
    'T10_i18'
  ),
  indicador = c(
    'Porcentaje de hogares con niñas/os menores de 5 años sin adecuado sistema de eliminacion de excretas',
    'Porcentaje de hogares con niñas/os menores de 5 años que utiliza suministros seguros de agua para beber',
    'Porcentaje de hogares con menores de 5 años, que están en condición de hacinamiento',
    'Prevalencia de desnutrición crónica en niñas/os menores de 2 años',
    'Prevalencia de desnutrición global en niñas/os menores de 2 años',
    'Prevalencia de desnutrición aguda en niñas/os menores de 2 años',
    'Prevalencia de desnutrición crónica en niñas/os menores de 5 años',
    'Prevalencia de desnutrición global en niñas/os menores de 5 años',
    'Prevalencia de desnutrición aguda en niñas/os menores de 5 años',
    'Prevalencia de desnutrición crónica en niñas/os de 2 años a menores de 5 años',
    'Prevalencia de desnutrición global en niñas/os de 2 años a menores de 5 años',
    'Prevalencia de desnutrición aguda en niñas/os de 2 años a menores de 5 años',
    'Prevalencia de sobrepeso en niñas/os menores de 5 años',
    'Prevalencia de obesidad en niñas/os menores de 5 años',
    'Prevalencia de sobrepeso y obesidad en niñas/os menores de 5 años',
    'Prevalencia de malnutrición en niñas/os menores de 5 años',
    'Prevalencia doble carga de malnutrición en niñas/os menores de 5 años',
    'Prevalencia de anemia en niñas/os de 6 a 59 meses de edad',
    'Prevalencia de anemia en niñas/os de 6 a 23 meses de edad',
    'Tipo de anemia en niñas/os de 6 a 59 meses de edad',
    'Tipo de anemia en niñas/os de 6 a 23 meses de edad',
    'Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con BCG antes de cumplir el primer año',
    'Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con hepatitis B antes de cumplir el primer año',
    'Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con las dos dosis de rotavirus antes de cumplir el primer año',
    'Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con las tres dosis de pentavalente antes de cumplir el primer año',
    'Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con las tres dosis de antipoliomielítica antes de cumplir el primer año',
    'Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con las tres dosis de neumococo antes de cumplir el primer año',
    'Porcentaje de niñas/os menores de 1 año con inicio temprano de la lactancia materna',
    'Porcentaje de niñas/os menores de 2 años con inicio temprano de la lactancia materna',
    'Porcentaje de niñas/os menores de 6 meses con lactancia materna exclusiva',
    'Porcentaje de niñas/os de 12 a 15 meses con lactancia materna continua',
    'Porcentaje de niñas/os de 6 a 23 meses de edad que recibieron alimentos de cuatro y más grupos alimentarios durante el día anterior',
    'Promedio de controles prenatales durante la gestación de las niñas/os menores de 2 años',
    'Porcentaje de niñas/os menores de 2 años cuyas madres recibieron al menos 5 controles prenatales durante el embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres consumieron hierro y ácido fólico durante el embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de VIH antes de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de VIH a partir de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de VIH durante el embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de orina antes de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de orina a partir de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de orina durante el embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de TORCHs antes de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de TORCHs a partir de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de TORCHs durante el embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de VIH, orina y TORCHs durante el embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres recibieron la vacuna del tétanos y difteria durante el embarazo',
    'Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron ecos obstétricos durante el embarazo',
    'Promedio de controles prenatales durante la gestación de las niñas/os menores de 5 años',
    'Porcentaje de niñas/os menores de 5 años cuyas madres recibieron al menos 5 controles prenatales durante el embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres consumieron hierro y ácido fólico durante el embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de VIH antes de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de VIH a partir de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de VIH durante el embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de orina antes de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de orina a partir de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de orina durante el embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de TORCHs antes de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de TORCHs a partir de la semana 20 de embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de TORCHs durante el embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de VIH, orina y TORCHs durante el embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres recibieron la vacuna del tétanos y difteria durante el embarazo',
    'Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron ecos obstétricos durante el embarazo',
    'Promedio de consejerías sobre lactancia materna exclusiva que han recibido las madres de las niñas/os menores de 6 meses, durante el embarazo, después del parto o durante el crecimiento',
    'Porcentaje de niñas/os menores de 5 años con bajo peso al nacer',
    'Porcentaje de niñas/os menores de 5 años con inscripción de nacidos vivos en el Registro Civil',
    'Porcentaje de niñas/os de 24 a 59 meses que acudieron al menos a 13 controles de niño sano durante sus primeros 23 meses de vida',
    'Porcentaje de niñas/os menores de 5 años con enfermedad diarreica aguda (EDA)',
    'Porcentaje de niñas/os menores de 5 años con infección respiratoria aguda (IRA)',
    'Porcentaje de niñas/os entre 6 y 23 meses de edad que consumieron hierro, multivitaminas y minerales en polvo al menos una vez en los últimos 7 días',
    'Porcentaje de niñas/os menores de 5 años que asisten o participan al Centro de Desarrollo Infantil (CDI) o centro de educación inicial',
    'Porcentaje de niñas/os menores de 3 años que reciben atención por parte de educadoras del programa Creciendo con Nuestros Hijos (CNH)',
    'Edad promedio de las madres al momento del nacimiento de su primer hijo',
    'Promedio de espaciamiento en meses entre nacimientos de las hijas/os de las madres',
    'Porcentaje de niñas/os menores de 5 años concebidos a través de un embarazo deseado y planeado',
    'Porcentaje de niñas/os menores de 5 años concebidos a través de un embarazo deseado y no previsto',
    'Porcentaje de niñas/os menores de 5 años concebidos a través de un embarazo no deseado y no previsto',
    'Porcentaje de niñas/os menores de 5 años que nacieron en establecimientos de salud',
    'Porcentaje de niñas/os menores de 5 años que su nacimiento fue atendido por personal de salud calificado',
    'Porcentaje de niñas/os menores de 5 años cuyas madres tuvieron su parto por cesárea',
    'Porcentaje de niñas/os menores de 5 años cuyas madres recibieron control post parto antes de los siete días',
    'Porcentaje de hogares con niñas/os menores de 5 años sin presencia de cloro residual en el agua para beber tomada desde la fuente o suministro',
    'Porcentaje de hogares con niñas/os menores de 5 años sin presencia de cloro residual en el agua para beber tomada del punto de consumo',
    'Porcentaje de hogares con niñas/os menores de 5 años con presencia de la bacteria E-coli en el agua para beber tomada desde la fuente o suministro',
    'Porcentaje de hogares con niñas/os menores de 5 años con presencia de la bacteria E-coli en el agua para beber tomada del punto de consumo',
    'Porcentaje de hogares con niñas/os menores de 5 años por clasificación de cloro residual en el agua para beber tomada desde la fuente o suministro',
    'Porcentaje de hogares con niñas/os menores de 5 años por clasificación de cloro residual en el agua para beber tomada del punto de consumo',
    'Porcentaje de hogares con niñas/os menores de 5 años por clasificación de riesgo de presencia de la bacteria E-coli en el agua para beber tomada desde la fuente o suministro',
    'Porcentaje de hogares con niñas/os menores de 5 años por clasificación de riesgo de presencia de la bacteria E-coli en el agua para beber tomada del punto de consumo',
    'Porcentaje de niñas/os menores de 5 años que realizan actividades de estimulación temprana con miembros del hogar',
    'Porcentaje de niñas/os menores de 5 años que realizan actividades de estimulación temprana con la madre',
    'Porcentaje de niñas/os menores de 5 años que realizan actividades de estimulación temprana con el padre',
    'Porcentaje de niñas/os menores de 5 años que tienen uno o más libros, cuentos, revistas o libros con dibujos',
    'Porcentaje de niñas/os menores de 5 años que tienen tres o más libros, cuentos, revistas o libros con dibujos',
    'Porcentaje de niñas/os menores de 5 años que tienen tres o más cosas para jugar en casa',
    'Porcentaje de niñas/os entre 1 a menores de 5 años que están libres de maltrato físico en el último mes',
    'Porcentaje de niñas/os entre 1 a menores de 5 años que están libres de maltrato psicológico en el último mes',
    'Porcentaje de niñas/os menores de 1 año que están libres de maltrato físico en el último mes',
    'Porcentaje de niñas/os menores de 1 año que están libres de maltrato psicológico en el último mes',
    'Porcentaje de niñas/os menores de 5 años que en los últimos 7 días estuvieron al cuidado de otra niña/o menor de 10 años, por más de una hora',
    'Porcentaje de niñas/os menores de 5 años que en los últimos 7 días se quedaron solas/os por más de una hora',
    'Promedio de palabras que entienden los niñas/os de 12 a 18 meses (Desarrollo de habilidades comunicativas de MacArthur-Bates)',
    'Promedio de palabras que dicen niñas/os de 12 a 18 meses (Desarrollo de habilidades comunicativas de MacArthur-Bates)',
    'Promedio de palabras que dicen niñas/os de 19 a 30 meses (Desarrollo de habilidades comunicativas de MacArthur-Bates)',
    'Promedio de palabras que dicen niñas/os de 31 a 42 meses (Desarrollo de habilidades comunicativas de MacArthur-Bates)',
    'Promedio de puntaje del Test de vocabulario en imágenes Peabody (TVIP) para niñas/os de 43 a 59 meses',
    'Porcentaje de niñas/os menores de 5 años cuyas madres presentan un estado psicológico de depresión'
    ),
  temas = c(
    'Datos de la vivienda y el hogar',
    'Datos de la vivienda y el hogar',
    'Datos de la vivienda y el hogar',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Estado nutricional',
    'Vacunación',
    'Vacunación',
    'Vacunación',
    'Vacunación',
    'Vacunación',
    'Vacunación',
    'Lactancia materna',
    'Lactancia materna',
    'Lactancia materna',
    'Lactancia materna',
    'Lactancia materna',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Controles prenatales',
    'Salud en la niñez',
    'Salud en la niñez',
    'Salud en la niñez',
    'Salud en la niñez',
    'Salud en la niñez',
    'Salud en la niñez',
    'Salud en la niñez',
    'Programas de primera infancia',
    'Programas de primera infancia',
    'Características de fecundidad de las madres',
    'Características de fecundidad de las madres',
    'Características de fecundidad de las madres',
    'Características de fecundidad de las madres',
    'Características de fecundidad de las madres',
    'Características de fecundidad de las madres',
    'Características de fecundidad de las madres',
    'Características de fecundidad de las madres',
    'Características de fecundidad de las madres',
    'Calidad del agua',
    'Calidad del agua',
    'Calidad del agua',
    'Calidad del agua',
    'Calidad del agua',
    'Calidad del agua',
    'Calidad del agua',
    'Calidad del agua',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil',
    'Desarrollo Infantil'

    ),
  indicador_arreglo = c(
    'a. Porcentaje de hogares con niñas/os menores de 5 años sin adecuado sistema de eliminacion de excretas',
    'b. Porcentaje de hogares con niñas/os menores de 5 años que utiliza suministros seguros de agua para beber',
    'c.  Porcentaje de hogares con menores de 5 años, que están en condición de hacinamiento ',
    'a. Prevalencia de desnutrición crónica en niñas/os menores de 2 años',
    'b. Prevalencia de desnutrición global en niñas/os menores de 2 años',
    'c.  Prevalencia de desnutrición aguda en niñas/os menores de 2 años',
    'd. Prevalencia de desnutrición crónica en niñas/os menores de 5 años',
    'e. Prevalencia de desnutrición global en niñas/os menores de 5 años',
    'f. Prevalencia de desnutrición aguda en niñas/os menores de 5 años',
    'g. Prevalencia de desnutrición crónica en niñas/os de 2 años a menores de 5 años',
    'h. Prevalencia de desnutrición global en niñas/os de 2 años a menores de 5 años',
    'i. Prevalencia de desnutrición aguda en niñas/os de 2 años a menores de 5 años',
    'j. Prevalencia de sobrepeso en niñas/os menores de 5 años',
    'k. Prevalencia de obesidad en niñas/os menores de 5 años',
    'l. Prevalencia de sobrepeso y obesidad en niñas/os menores de 5 años',
    'm. Prevalencia de malnutrición en niñas/os menores de 5 años',
    'n. Prevalencia doble carga de malnutrición en niñas/os menores de 5 años',
    'o. Prevalencia de anemia en niñas/os de 6 a 59 meses de edad',
    'p. Prevalencia de anemia en niñas/os de 6 a 23 meses de edad',
    'q. Tipo de anemia en niñas/os de 6 a 59 meses de edad',
    'r. Tipo de anemia en niñas/os de 6 a 23 meses de edad',
    'a. Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con BCG antes de cumplir el primer año',
    'b. Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con hepatitis B antes de cumplir el primer año',
    'c.  Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con las dos dosis de rotavirus antes de cumplir el primer año',
    'd. Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con las tres dosis de pentavalente antes de cumplir el primer año',
    'e. Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con las tres dosis de antipoliomielítica antes de cumplir el primer año',
    'f. Porcentaje de niñas/os de 12 a 59 meses que fueron vacunados con las tres dosis de neumococo antes de cumplir el primer año',
    'a. Porcentaje de niñas/os menores de 1 año con inicio temprano de la lactancia materna',
    'b. Porcentaje de niñas/os menores de 2 años con inicio temprano de la lactancia materna',
    'c.  Porcentaje de niñas/os menores de 6 meses con lactancia materna exclusiva',
    'd. Porcentaje de niñas/os de 12 a 15 meses con lactancia materna continua',
    'e. Porcentaje de niñas/os de 6 a 23 meses de edad que recibieron alimentos de cuatro y más grupos alimentarios durante el día anterior',
    'a. Promedio de controles prenatales durante la gestación de las niñas/os menores de 2 años',
    'b. Porcentaje de niñas/os menores de 2 años cuyas madres recibieron al menos 5 controles prenatales durante el embarazo',
    'c.  Porcentaje de niñas/os menores de 2 años cuyas madres consumieron hierro y ácido fólico durante el embarazo',
    'd. Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de VIH antes de la semana 20 de embarazo',
    'e. Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de VIH a partir de la semana 20 de embarazo',
    'f. Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de VIH durante el embarazo',
    'g. Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de orina antes de la semana 20 de embarazo',
    'h. Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de orina a partir de la semana 20 de embarazo',
    'i. Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de orina durante el embarazo',
    'j. Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de TORCHs antes de la semana 20 de embarazo',
    'k. Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de TORCHs a partir de la semana 20 de embarazo',
    'l. Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de TORCHs durante el embarazo',
    'm. Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron al menos un examen de VIH, orina y TORCHs durante el embarazo',
    'n. Porcentaje de niñas/os menores de 2 años cuyas madres recibieron la vacuna del tétanos y difteria durante el embarazo',
    'o. Porcentaje de niñas/os menores de 2 años cuyas madres se realizaron ecos obstétricos durante el embarazo',
    'p. Promedio de controles prenatales durante la gestación de las niñas/os menores de 5 años',
    'q. Porcentaje de niñas/os menores de 5 años cuyas madres recibieron al menos 5 controles prenatales durante el embarazo',
    'r. Porcentaje de niñas/os menores de 5 años cuyas madres consumieron hierro y ácido fólico durante el embarazo',
    's. Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de VIH antes de la semana 20 de embarazo',
    't. Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de VIH a partir de la semana 20 de embarazo',
    'u. Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de VIH durante el embarazo',
    'v. Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de orina antes de la semana 20 de embarazo',
    'w. Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de orina a partir de la semana 20 de embarazo',
    'x. Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de orina durante el embarazo',
    'y. Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de TORCHs antes de la semana 20 de embarazo',
    'z. Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de TORCHs a partir de la semana 20 de embarazo',
    'za. Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de TORCHs durante el embarazo',
    'zb. Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron al menos un examen de VIH, orina y TORCHs durante el embarazo',
    'zc. Porcentaje de niñas/os menores de 5 años cuyas madres recibieron la vacuna del tétanos y difteria durante el embarazo',
    'zd. Porcentaje de niñas/os menores de 5 años cuyas madres se realizaron ecos obstétricos durante el embarazo',
    'a. Promedio de consejerías sobre lactancia materna exclusiva que han recibido las madres de las niñas/os menores de 6 meses, durante el embarazo, después del parto o durante el crecimiento',
    'b. Porcentaje de niñas/os menores de 5 años con bajo peso al nacer',
    'c.  Porcentaje de niñas/os menores de 5 años con inscripción de nacidos vivos en el Registro Civil',
    'd. Porcentaje de niñas/os de 24 a 59 meses que acudieron al menos a 13 controles de niño sano durante sus primeros 23 meses de vida',
    'e. Porcentaje de niñas/os menores de 5 años con enfermedad diarreica aguda (EDA)',
    'f. Porcentaje de niñas/os menores de 5 años con infección respiratoria aguda (IRA)',
    'g. Porcentaje de niñas/os entre 6 y 23 meses de edad que consumieron hierro, multivitaminas y minerales en polvo al menos una vez en los últimos 7 días',
    'a. Porcentaje de niñas/os menores de 5 años que asisten o participan al Centro de Desarrollo Infantil (CDI) o centro de educación inicial',
    'b. Porcentaje de niñas/os menores de 3 años que reciben atención por parte de educadoras del programa Creciendo con Nuestros Hijos (CNH)',
    'a. Edad promedio de las madres al momento del nacimiento de su primer hijo',
    'b. Promedio de espaciamiento en meses entre nacimientos de las hijas/os de las madres',
    'c.  Porcentaje de niñas/os menores de 5 años concebidos a través de un embarazo deseado y planeado',
    'd. Porcentaje de niñas/os menores de 5 años concebidos a través de un embarazo deseado y no previsto',
    'e. Porcentaje de niñas/os menores de 5 años concebidos a través de un embarazo no deseado y no previsto',
    'f. Porcentaje de niñas/os menores de 5 años que nacieron en establecimientos de salud',
    'g. Porcentaje de niñas/os menores de 5 años que su nacimiento fue atendido por personal de salud calificado',
    'h. Porcentaje de niñas/os menores de 5 años cuyas madres tuvieron su parto por cesárea',
    'i. Porcentaje de niñas/os menores de 5 años cuyas madres recibieron control post parto antes de los siete días',
    'a. Porcentaje de hogares con niñas/os menores de 5 años sin presencia de cloro residual en el agua para beber tomada desde la fuente o suministro',
    'b. Porcentaje de hogares con niñas/os menores de 5 años sin presencia de cloro residual en el agua para beber tomada del punto de consumo',
    'c.  Porcentaje de hogares con niñas/os menores de 5 años con presencia de la bacteria E-coli en el agua para beber tomada desde la fuente o suministro',
    'd. Porcentaje de hogares con niñas/os menores de 5 años con presencia de la bacteria E-coli en el agua para beber tomada del punto de consumo',
    'e. Porcentaje de hogares con niñas/os menores de 5 años por clasificación de cloro residual en el agua para beber tomada desde la fuente o suministro',
    'f. Porcentaje de hogares con niñas/os menores de 5 años por clasificación de cloro residual en el agua para beber tomada del punto de consumo',
    'g. Porcentaje de hogares con niñas/os menores de 5 años por clasificación de riesgo de presencia de la bacteria E-coli en el agua para beber tomada desde la fuente o suministro',
    'h. Porcentaje de hogares con niñas/os menores de 5 años por clasificación de riesgo de presencia de la bacteria E-coli en el agua para beber tomada del punto de consumo',
    'a. Porcentaje de niñas/os menores de 5 años que realizan actividades de estimulación temprana con miembros del hogar',
    'b. Porcentaje de niñas/os menores de 5 años que realizan actividades de estimulación temprana con la madre',
    'c.  Porcentaje de niñas/os menores de 5 años que realizan actividades de estimulación temprana con el padre',
    'd. Porcentaje de niñas/os menores de 5 años que tienen uno o más libros, cuentos, revistas o libros con dibujos',
    'e. Porcentaje de niñas/os menores de 5 años que tienen tres o más libros, cuentos, revistas o libros con dibujos',
    'f. Porcentaje de niñas/os menores de 5 años que tienen tres o más cosas para jugar en casa',
    'g. Porcentaje de niñas/os entre 1 a menores de 5 años que están libres de maltrato físico en el último mes',
    'h. Porcentaje de niñas/os entre 1 a menores de 5 años que están libres de maltrato psicológico en el último mes',
    'i. Porcentaje de niñas/os menores de 1 año que están libres de maltrato físico en el último mes',
    'j. Porcentaje de niñas/os menores de 1 año que están libres de maltrato psicológico en el último mes',
    'k. Porcentaje de niñas/os menores de 5 años que en los últimos 7 días estuvieron al cuidado de otra niña/o menor de 10 años, por más de una hora',
    'l. Porcentaje de niñas/os menores de 5 años que en los últimos 7 días se quedaron solas/os por más de una hora',
    'n. Promedio de palabras que entienden los niñas/os de 12 a 18 meses (Desarrollo de habilidades comunicativas de MacArthur-Bates)',
    'o. Promedio de palabras que dicen niñas/os de 12 a 18 meses (Desarrollo de habilidades comunicativas de MacArthur-Bates)',
    'p. Promedio de palabras que dicen niñas/os de 19 a 30 meses (Desarrollo de habilidades comunicativas de MacArthur-Bates)',
    'q. Promedio de palabras que dicen niñas/os de 31 a 42 meses (Desarrollo de habilidades comunicativas de MacArthur-Bates)',
    'r. Promedio de puntaje del Test de vocabulario en imágenes Peabody (TVIP) para niñas/os de 43 a 59 meses',
    's. Porcentaje de niñas/os menores de 5 años cuyas madres presentan un estado psicológico de depresión'
   ),
  notas=c(
    'Nota: Hogares sin adecuado sistema de eliminación de excretas son aquellos con el inodoro o escusado conectado a pozo séptico, a biodigestor, a pozo ciego, con descarga directa al mar, río, lago, quebrada o letrina; o a su vez cuando no tiene.',
    'Nota: Los componentes que permiten establecer si un hogar utiliza suministros seguros de agua para beber son: tipo de suministro, tratamiento que da al agua antes de beberla, cercanía del suministro y suficiencia de agua para beber.',
    'Nota: El hacinamiento se basa en la relación entre la cantidad de personas y el número de dormitorios disponibles. Se considera que hay hacinamiento cuando esta relación es mayor a tres. La estimación está a nivel de persona.',
    'Nota: Desnutrición crónica es el retardo en talla para la edad del niño/a.',
    'Nota: Desnutrición global es el peso bajo para la edad del niño/a.',
    'Nota: Desnutrición aguda es cuando el peso es muy bajo en relación con la talla del niño/a, y puede acompañarse de delgadez.',
    'Nota: Desnutrición crónica es el retardo en talla para la edad del niño/a.',
    'Nota: Desnutrición global es el peso bajo para la edad del niño/a.',
    'Nota: Desnutrición aguda es cuando el peso es muy bajo en relación con la talla del niño/a, y puede acompañarse de delgadez.',
    'Nota: Desnutrición crónica es el retardo en talla para la edad del niño/a.',
    'Nota: Desnutrición global es el peso bajo para la edad del niño/a.',
    'Nota: Desnutrición aguda es cuando el peso es muy bajo en relación con la talla del niño/a, y puede acompañarse de delgadez.',
    'Nota: El sobrepeso es el índice de masa corporal (IMC) para la edad, con un valor entre más de dos a tres desviaciones estándar por encima de la mediana establecida en los patrones de crecimiento infantil de la OMS en niñas/os menores de 5 años.',
    'Nota: La obesidad es el índice de masa corporal (IMC) para la edad, con un valor entre más de tres a cinco desviaciones estándar por encima de la mediana establecida en los patrones de crecimiento infantil de la OMS en niñas/os menores de 5 años.',
    'Nota: El sobrepeso y la obesidad ocurren cuando el niño/a es demasiado pesado para su estatura. Se manifiestan como una acumulación anormal o excesiva de grasa que puede ser perjudicial para la salud de las niñas/os.',
    'Nota: Malnutrición se da por una alimentación desequilibrada, ya sea por exceso o por escasez de diversos componentes o nutrientes que el organismo necesita, como vitaminas, minerales u otros.',
    'Nota: La doble carga de malnutrición se da cuando se presenta sobrepeso u obesidad en coexistencia con retardo en la talla del niño/a.',
    'Nota: Los ajustes de la concentración de hemoglobina en la sangre y de los umbrales, para establecer la condición de anemia, se encuentran alineados según la directriz de la Organización Mundial de la Salud (OMS), documento actualizado el año 2024. (https://www.ncbi.nlm.nih.gov/books/NBK602198/pdf/Bookshelf_NBK602198.pdf)',
    'Nota: Los ajustes de la concentración de hemoglobina en la sangre y de los umbrales, para establecer la condición de anemia, se encuentran alineados según la directriz de la Organización Mundial de la Salud (OMS), documento actualizado el año 2024. (https://www.ncbi.nlm.nih.gov/books/NBK602198/pdf/Bookshelf_NBK602198.pdf)',
    'Nota: Los ajustes de la concentración de hemoglobina en la sangre y de los umbrales, para establecer la condición de anemia, se encuentran alineados según la directriz de la Organización Mundial de la Salud (OMS), documento actualizado el año 2024. (https://www.ncbi.nlm.nih.gov/books/NBK602198/pdf/Bookshelf_NBK602198.pdf)',
    'Nota: Los ajustes de la concentración de hemoglobina en la sangre y de los umbrales, para establecer la condición de anemia, se encuentran alineados según la directriz de la Organización Mundial de la Salud (OMS), documento actualizado el año 2024. (https://www.ncbi.nlm.nih.gov/books/NBK602198/pdf/Bookshelf_NBK602198.pdf)',
    '',
    '',
    '',
    '',
    '',
    '',
    'Nota: Se considera inicio temprano cuando les dieron el seno al nacer inmediatamente después del parto hasta antes de transcurrida 1 hora',
    'Nota: Se considera inicio temprano cuando les dieron el seno al nacer inmediatamente después del parto hasta antes de transcurrida 1 hora',
    'Nota: Niños/as que el día/noche anterior fueron alimentados solo con leche materna, y no consumieron ningún otro líquido ni alimento sólido o semisólido',
    'Nota: Niños/as que el día/noche anterior fueron alimentados con leche materna',
    'Nota: Los grupos alimentarios que se elaboran para el cálculo de este indicador son: Grupo 1: cereales, raíces y tubérculos, Grupo 2: legumbres y nueces, Grupo 3: lácteos (leche, yogurt, queso),Grupo 4: carnes (carne, pescado, aves e hígado o carnes provenientes de vísceras),Grupo 5: huevos, Grupo 6: frutas y verduras ricas en vitamina A, Grupo 7: otras frutas y verduras.',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre reporta haberse realizado al menos un control prenatal de los estipulados dentro del paquete priorizado para atención de gestantes y niñas/os',
    'Nota: Para niños/as cuya madre declaró sí haber recibido al menos una consejería, asesoría o charla durante su gestación, en el posparto, o durante su crecimiento',
    'Nota: Para niños/as con carné que sí registran peso al nacer',
    '',
    'Nota: Corresponde al total de controles reportados por la madre en tres períodos: de 0 a 7 meses de edad, de 8 a 11 meses de edad, y de 12 a 23 meses de edad',
    'Nota: Tuvieron diarrea en las últimas dos semanas',
    'Nota: Presentaron síntomas de Infección Respiratoria Aguda como tos, moquera, dificultad para respirar, dolor de garganta y/o gripe en las últimas dos semanas, incluyendo el día de la encuesta',
    'Nota: Micronutrientes recibidos en los últimos 12 meses por parte del personal de salud tales como sobres de hierro, multivitaminas y minerales en polvo (Chispaz); hierro recibido en jarabe o gotas; o hierro comprado. Adicional se encuentra alineado el rango de edad según la directriz de la Organización Mundial de la Salud (OMS), documento actualizado el año 2024. (https://www.ncbi.nlm.nih.gov/books/NBK602198/pdf/Bookshelf_NBK602198.pdf)',
    'Nota: Centro de Desarrollo Infantil (MIES): unidades de atención públicas (directa-convenio), atienden a niños/as de 1 a 3 años; unidades de atención fiscomisionales y privadas que atienden a niños/as desde los 45 días de nacido a los 3 años a través de la atención y cuidado diario, con actividades de juego y aprendizaje, acciones de salud y nutrición, entrega de alimentación, articulación intersectorial. Centro de educación inicial: Este espacio se concibe como un entorno que acompaña el desarrollo integral de los niños y niñas menores de 5 años, que potencia su aprendizaje y promueve su bienestar, sin desconocer la responsabilidad formativa de la familia y la comunidad',
    'Nota: Creciendo con Nuestros Hijos es un servicio extramural de atención directa a niñas, niños de 0 a 36 meses con sus familias y mujeres gestantes, ejecutado por una o un educador familiar en consejería individual (hogar), consejería grupal (espacios de la comunidad) y seguimiento familiar',
    'Nota: Las mujeres contempladas en este indicador tienen al menos una niña/o menores de 5 años.',
    'Nota: Las mujeres contempladas en este indicador tienen al menos una niña/o menores de 5 años. El espaciamiento presentado se enfoca en los hijos menores de 5 años',
    'Nota: La madre del menor de 5 años responde que, cuando se quedó embarazada de él/ella quería tener ese hijo/a',
    'Nota: La madre del menor de 5 años responde que, cuando se quedó embarazada de él/ella quería esperar más tiempo',
    'Nota: La madre del menor de 5 años responde que, cuando se quedó embarazada de él/ella, no quería más hijos o no quería tener hijos',
    'Nota: Establecimientos incluye los del MSP, Hospital/Clínica/Dispensario del IESS, Seguro Social Campesino, Hospital FF.AA/ Policía, Junta de Beneficencia, Consejo Provincial/Unidad Municipal de Salud, Fundación/ ONG, Clínica/Consultorio privado. No es establecimiento cuando el parto ocurrió en casa o en otro lugar',
    'Nota: Se considera personal de salud calificado cuando atendió un Médico, Obstetriz, o Enfermera. No se considera personal calificado cuando atendió una Comadrona o partera, Familiar, Ella misma, Otro o No sabe',
    '',
    'Para niños/as cuya madre sí tuvo algún control después de su parto',
    'Nota: La muestra de la fuente se determina con base en las respuestas proporcionadas por el hogar y se la recoge directamente con una funda esterilizada. El cloro, en general, es el desinfectante más económico y más común. Desde el punto de vista de salud es un producto bactericida eficaz en la mayoría de las situaciones. El cloro residual presente en el agua desinfectada también ayuda a proteger el sistema de distribución contra la recontaminación microbiana e impide el crecimiento bacteriano. Se realizó un ajuste en sintaxis para el cálculo, el cual excluye los valores registrados con 999. Finalmente, para el cálculo del indicador se considera todo registro de cloro menor a  0.3 mg/L y donde las muestras no se colorearon de rosa.',
    'Nota: La muestra del punto de consumo es el recipiente en el que el informante vierte el agua proveniente de la fuente, para su consumo. El cloro, en general, es el desinfectante más económico y más común. Desde el punto de vista de salud es un producto bactericida eficaz en la mayoría de las situaciones. El cloro residual presente en el agua desinfectada también ayuda a proteger el sistema de distribución contra la recontaminación microbiana e impide el crecimiento bacteriano. Se realizó un ajuste en sintaxis para el cálculo, el cual excluye los valores registrados con 999. Finalmente, para el cálculo del indicador se considera todo registro de cloro menor a  0.3 mg/L y donde las muestras no se colorearon de rosa.',
    'Nota: La muestra de la fuente se determina con base en las respuestas proporcionadas por el hogar y se la recoge directamente con una funda esterilizada. E. coli es una bacteria que se encuentra normalmente en el intestino del ser humano y de los animales de sangre caliente, se transmite principalmente por el consumo de alimentos contaminados, como productos de carne picada cruda o poco cocida y leche cruda, la contaminación fecal del agua y de otros alimentos, así como la contaminación cruzada durante la preparación de estos. Se realizó un ajuste en la sintaxis para el cálculo, el cual excluye los valores registrados con 999. ',
    'Nota: La muestra del punto de consumo es el recipiente en el que el informante vierte el agua proveniente de la fuente, para su consumo. E. coli es una bacteria que se encuentra normalmente en el intestino del ser humano y de los animales de sangre caliente, se transmite principalmente por el consumo de alimentos contaminados, como productos de carne picada cruda o poco cocida y leche cruda, la contaminación fecal del agua y de otros alimentos, así como la contaminación cruzada durante la preparación de estos. Se realizó un ajuste en la sintaxis  para el cálculo, que excluye los valores registrados con 999.',
    'Nota: La muestra de la fuente se determina con base en las respuestas proporcionadas por el hogar y se la recoge directamente con una funda esterilizada. El cloro, en general, es el desinfectante más económico y más común. Desde el punto de vista de salud es un producto bactericida eficaz en la mayoría de las situaciones. El cloro residual presente en el agua desinfectada también ayuda a proteger el sistema de distribución contra la recontaminación microbiana e impide el crecimiento bacteriano. Se realizó un ajuste en la sintaxis para el cálculo, el cual excluye los valores registrados con 999.',
    'Nota: La muestra del punto de consumo es el recipiente en el que el informante vierte el agua proveniente de la fuente, para su consumo. El cloro, en general, es el desinfectante más económico y más común. Desde el punto de vista de salud es un producto bactericida eficaz en la mayoría de las situaciones. El cloro residual presente en el agua desinfectada también ayuda a proteger el sistema de distribución contra la recontaminación microbiana e impide el crecimiento bacteriano. Se realizó un ajuste en la sintaxis para el cálculo, el cual excluye los valores registrados con 999.',
    'Nota: La muestra de la fuente se determina con base en las respuestas proporcionadas por el hogar y se la recoge directamente con una funda esterilizada. E. coli es una bacteria que se encuentra normalmente en el intestino del ser humano y de los animales de sangre caliente, se transmite principalmente por el consumo de alimentos contaminados, como productos de carne picada cruda o poco cocida y leche cruda, la contaminación fecal del agua y de otros alimentos, así como la contaminación cruzada durante la preparación de estos. Se realizó un ajuste en la sintaxis para el cálculo, el cual excluye los valores registrados con 999.',
    'Nota: La muestra del punto de consumo es el recipiente en el que el informante vierte el agua proveniente de la fuente, para su consumo. E. coli es una bacteria que se encuentra normalmente en el intestino del ser humano y de los animales de sangre caliente, se transmite principalmente por el consumo de alimentos contaminados, como productos de carne picada cruda o poco cocida y leche cruda, la contaminación fecal del agua y de otros alimentos, así como la contaminación cruzada durante la preparación de estos. Se realizó un ajuste en la sintaxis para el cálculo, el cual excluye los valores registrados con 999.',
    'Nota: Actividades con miembros del hogar mayores de 15 años (madre, padre, hermano/a, abuelo/a, otro)',
    'Nota: Actividades realizadas con la madre en los últimos tres días (leyó libros, contó cuentos, jugaron, dibujaron, le llevó a pasear, entre otros)',
    'Nota: Actividades realizadas con el padre en los últimos tres días (leyó libros, contó cuentos, jugaron, dibujaron, le llevó a pasear, entre otros)',
    'Nota: No incluye textos escolares ni libros electrónicos',
    'Nota: No incluye textos escolares ni libros electrónicos',
    'Nota: Incluye juguetes para armar y/o construir como legos, rompecabezas, juguetes, artículos o elementos que le permitan aprender texturas, formas y/o colores como triángulos, cuadrados, etc., muñecos/as y objetos de roles o fantasías (tazas y platos de juguetes, disfraces, etc., para jugar al doctor, a la comidita, al maestro, etc.), y también libros, cuentos, revistas o libros con dibujos para niños/as',
    'Nota: El/la cuidadora niega explícitamente haber sacudido, dado nalgadas con la mano, golpeado en el trasero u otra parte del cuerpo con algún objeto duro, dado una palmada en la cara, cabeza, orejas, mano, brazo, pierna, dado una paliza, bañado en agua fría, metido al tanque/tina o pegado con ortiga',
    'Nota: El/la cuidadora niega explícitamente haber gritado al niño/a o llamado tonto/a, perezoso/a, malcriado/a o alguna otra cosa parecida',
    'Nota: El/la cuidadora niega explícitamente haber sacudido para tratar de detener el llanto o pegado al niño/a',
    'Nota: El/la cuidadora niega explícitamente haber ignorado hasta que deje de llorar o de quejarse, haber dicho que deje de llorar, hablado con él severamente, o haber gritado al niño/a',
    '',
    '',
    'Nota: Palabras del Inventario de desarrollo de habilidades comunicativas MacArthur-Bates para su grupo de edad. Responde su madre o su cuidador principal en ausencia de la misma. Considera solo a niños/as que se comunican principalmente en español',
    'Nota: Palabras del Inventario de desarrollo de habilidades comunicativas MacArthur-Bates para su grupo de edad. Responde su madre o su cuidador principal en ausencia de la misma. Considera solo a niños/as que se comunican principalmente en español',
    'Nota: Palabras del Inventario de desarrollo de habilidades comunicativas MacArthur-Bates para su grupo de edad. Responde su madre o su cuidador principal en ausencia de la misma. Considera solo a niños/as que se comunican principalmente en español',
    'Nota: Palabras del Inventario de desarrollo de habilidades comunicativas MacArthur-Bates para su grupo de edad. Responde su madre o su cuidador principal en ausencia de la misma. Considera solo a niños/as que se comunican principalmente en español',
    'Nota: El puntaje obtenido en el TVIP es un indicador que evalúa las habilidades lingüísticas directamente con el niño/a, presentándole una serie de imágenes. Considera solo a niños/as que se comunican principalmente en español',
    'Nota: Se considera a madres cuidadoras que en los últimos 7 días presentaron uno o más síntomas de depresión, tales como poco interés en hacer las cosas, decaimiento, falta de esperanza, afectaciones a los hábitos de sueño y de apetito, cansancio, sentimientos de fracaso, falta de concentración, entre otros'
    
  )
)
