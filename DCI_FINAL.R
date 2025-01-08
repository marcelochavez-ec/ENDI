#==============================================================================#
####              Sintaxis Optimizada del Indicador DCI                     ####
#==============================================================================#

# Prevalencia de desnutrición crónica en niñas/os menores de 5 años
# Operación Estadística:
# Encuesta Nacional sobre Desnutrición Infantil (ENDI 2022 - 2023)

# Autor de la sintaxis optimizada:
# Ing. Marcelo Chávez 
# Maestría en Estadística Aplicada
# Facultad de Ciencias Naturales y Matemática
# ESPOL
# Fecha de última actualización por INEC: 12/06/2023
# Fecha de actualización por Marcelo Chávez: 25/12/2024

# Versión: 2.0
# Software: R 4.2.0

#==============================================================================#
####                  Instalación y carga de paquetes                       ####
#==============================================================================#

# Se usa la función p_load() desde el paquete pacman, el cual instala
# el paquete si está ausente, y carga para el uso si ya está instalado

# Se asegura que el paquete "pacman" este instalado 
# if(!require("pacman")) install.packages("pacman")

# Paquetes disponibles desde CRAN

# Limpieza del espacio de trabajo:

rm(list = ls(all = TRUE))

pacman::p_load(
    
    # Gestión de proyectos y archivos
    here,    # construye rutas a los archivos de su proyecto
    rio,     # importación / exportación de varios tipos de datos
    expss, # tablas, etiquetas y funciones de hojas de cálculo y estadísticas de 'SPSS'
    
    # Instalación y manejo de paquetes pacman, # instalar y cargar paquetes
    
    # Manejo general de los datos
    tidyverse, # incluye paquetes para ordenar y presentar los datos lubridate, 
    # trabaja con fechas
    pillar,   # herramientas para manejar columnas de datos janitor,  
    # Limpieza de datos y tablas
    sjlabelled, # para tratar etiquetas epikit, 
    # agregar categorías
    
    # Estadísticas
    summarytools, # herramientas para resumir datos de forma rápida y ordenada
    
    # Manejo de muestras complejas
    srvyr, # estadística de resumen para datos de encuestas
    
    # Paquetes para cálculos específicos
    anthro, # cálculo de puntuaciones z de antropometría infantil
    openxlsx,
    highcharter)

#==============================================================================#
####                             Funciones                                  ####
#==============================================================================#

# Función para calcular estadísticos para variables dicotómicas 
srvyr_prop <- function(design, x) {
    
    design %>%
        summarise( 
            props = survey_mean({{ x }}, 
                                proportion = TRUE,
                                vartype = c("se", "ci", "cv"), na.rm = T) * 100,
            deff = survey_mean({{ x }}, deff = "replace", na.rm = T),
            Num = sum({{ x }}, na.rm = TRUE), Deno = sum(!is.na({{ x }}))) %>%
        mutate(desag = "Nacional") %>%
        select(
            Desag = desag, 
            Props = props,
            EE   = props_se, 
            LI    = props_low, 
            LS   = props_upp, 
            CV   = props_cv, 
            Deff = deff_deff, 
            Num  = Num, 
            Deno = Deno)}

# Función para calcular estadísticos para variables dicotómicas
# por desagregación
srvyr_prop_by <- function(design, x, by) {
    
    design %>%
        group_by({{ by }}) %>%
        summarise(
            props = survey_mean({{ x }}, 
                                proportion = TRUE,
                                vartype = c("se", "ci", "cv"), 
                                na.rm = T) * 100,
            deff = survey_mean({{ x }}, 
                               deff = "replace", na.rm = T),
            Num = sum({{ x }}, na.rm = TRUE),
            Deno = sum(!is.na({{ x }}))) %>% 
        mutate(desag = {{ by }}) %>% 
        select(
            Desag = desag, 
            Props = props,
            EE   = props_se, 
            LI    = props_low, 
            LS   = props_upp,
            CV   = props_cv, 
            Deff = deff_deff,
            Num  = Num,
            Deno = Deno
        )}

#==============================================================================#
####                       Carga de base de datos                           ####
#==============================================================================#

# Indicaciones:

# 1. Se proporciona la ruta de archivo absoluta o completa en la función import
# entre comilla.
# 2. Use barras diagonales ( / ). Este no es el valor predeterminado para las
# rutas de archivos de Windows

# Ejemplo:
# df_f1_personas <- import("C:/ENDI/Data/f1_personas.rds")

# Sugerencias:

# Es probable que R no reconozca las rutas de archivos que comienzan con barras
# inclinadas dobles (p. ej., “//…”) y produzcan un error. Considere mover
# su trabajo a una unidad "con nombre" o "con letras" (p. ej., "C:" o "D:").

# Base personas 
# df_f1_personas <- import("")
# df_f1_personas <- as_tibble(df_f1_personas)
# df_f1_personas

# Diccionario de variables
# Cargar el archivo: Diccionario_ENDI.xlsx con la hoja ya especificada 
# dicc_f1_per <- import("",
#                       which = "f1_personas")
# 
# dicc_f1_per <- as_tibble(dicc_f1_per)
# dicc_f1_per

# Base Personas  
df_f1_personas <- readRDS("C:/Users/marcelochavez/Documents/TESIS/PROYECTO_ENDI/BDD/BDD_ENDI_R2_f1_personas.rds")
df_f1_personas <- as_tibble(df_f1_personas) 

dicc_f1_per <- read.xlsx("C:/Users/marcelochavez/Documents/TESIS/PROYECTO_ENDI/BDD/Diccionario_variables_ENDI_R2.xlsx",
                         sheet = "f1_personas", 
                         startRow = 10)
dicc_f1_per <- as_tibble(dicc_f1_per)

# Base MEF  
df_f2_mef <- readRDS("C:/Users/marcelochavez/Documents/TESIS/PROYECTO_ENDI/BDD/BDD_ENDI_R2_f2_mef.rds")
df_f2_mef <- as_tibble(df_f2_mef) 

dicc_f2_mef <- read.xlsx("C:/Users/marcelochavez/Documents/TESIS/PROYECTO_ENDI/BDD/Diccionario_variables_ENDI_R2.xlsx",
                         sheet = "f2_mef",
                         startRow = 10)
dicc_f2_mef <- as_tibble(dicc_f2_mef)

#==============================================================================#
####   Cálculo de variables antropométricas necesarias para el indicador    ####
#==============================================================================#

# Estimación de la edad en días -----------------------------------------------#

df_f1_personas <- df_f1_personas %>%
    mutate(
        dob = paste(f1_s5_2_3, f1_s5_2_2, f1_s5_2_1, sep = "-") %>% as_date(format = "%Y-%m-%d"),
        dov = paste(f1_s5_3_3, f1_s5_3_2, f1_s5_3_1, sep = "-") %>% as_date(format = "%Y-%m-%d")
    ) %>%
    mutate(
        edaddias = as.numeric(dob %--% dov / days(1)),
        edadmeses = trunc(as.numeric(dob %--% dov / months(1))),
        edadanios = trunc(as.numeric(dob %--% dov / years(1))))

# Estadísticas descriptivas para edaddias
descr(df_f1_personas$edaddias,
      stats = c("common"), 
      round.digits = 2,
      na.rm = TRUE)

# Generar un gráfico de boxplot
highchart() %>%
    hc_chart(type = "boxplot") %>%
    hc_add_series(
        name = "Edad en días",
        data = list(
            boxplot.stats(df_f1_personas$edaddias)$stats %>%
                t() %>%
                as.list()
        )
    ) %>%
    hc_title(text = "Distribución de Edad en Días") %>%
    hc_xAxis(categories = c("Edad en días")) %>%
    hc_yAxis(
        title = list(text = "Días"),
        labels = list(format = "{value} días")
    ) %>%
    hc_tooltip(
        headerFormat = "<b>{point.key}</b><br>",
        pointFormat = "Valor: {point.y}"
    ) %>%
    hc_plotOptions(
        boxplot = list(
            fillColor = "#004e98",
            lineWidth = 2,
            medianColor = "#f45b5b",
            medianWidth = 3,
            whiskerLength = "90%",
            whiskerWidth = 2,
            whiskerColor = "#2b908f"
        )
    ) %>%
    hc_add_theme(hc_theme_flat())

# ******************************************************************************

# Filtrar valores válidos (sin NA)
df_valid <- df_f1_personas %>% 
    filter(!is.na(edaddias))

# Calcular estadísticas descriptivas
stats <- df_valid %>%
    summarise(
        mean = mean(edaddias, na.rm = TRUE),
        median = median(edaddias, na.rm = TRUE),
        min = min(edaddias, na.rm = TRUE),
        max = max(edaddias, na.rm = TRUE),
        pct_valid = (n() / nrow(df_f1_personas)) * 100
    )

# Crear el histograma
hc <- hchart(df_valid$edaddias, 
             type = "column", 
             name = "Frecuencia",
             color = "#1f78b4") %>%
    hc_add_series(data = list(
        list(x = stats$mean, color = "red", name = "Media"),
        list(x = stats$median, color = "blue", name = "Mediana")
    ),
    type = "scatter",
    marker = list(symbol = "line")) %>%
    hc_title(text = "Distribución de Edaddias con Estadísticas") %>%
    hc_xAxis(title = list(text = "Edaddias")) %>%
    hc_yAxis(title = list(text = "Frecuencia")) %>%
    hc_tooltip(pointFormat = "{point.name}: {point.x}")

hc

# Gráfico de porcentaje de valores válidos
hc_valid <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_add_series(
        name = "Porcentaje de datos válidos",
        data = list(list(y = stats$pct_valid, color = "#28a745")),
        showInLegend = FALSE
    ) %>%
    hc_xAxis(categories = c("Edaddias")) %>%
    hc_yAxis(
        title = list(text = "Porcentaje"),
        max = 100
    ) %>%
    hc_title(text = "Porcentaje de Datos Válidos de Edaddias") %>%
    hc_tooltip(pointFormat = "<b>{point.y:.2f}%</b> de datos válidos")

hc_valid

library(highcharter)
library(dplyr)

# Filtrar valores válidos
df_valid <- df_f1_personas %>% 
    filter(!is.na(edaddias))

# Calcular estadísticas para el boxplot
box_stats <- df_valid %>%
    summarise(
        min = min(edaddias, na.rm = TRUE),
        q1 = quantile(edaddias, 0.25, na.rm = TRUE),
        median = median(edaddias, na.rm = TRUE),
        q3 = quantile(edaddias, 0.75, na.rm = TRUE),
        max = max(edaddias, na.rm = TRUE)
    )

# Crear el boxplot con highcharter
hc_boxplot <- highchart() %>%
    hc_chart(type = "boxplot") %>%
    hc_add_series(
        data = list(c(box_stats$min, box_stats$q1, box_stats$median, box_stats$q3, box_stats$max)),
        name = "Edaddias",
        color = "#1f78b4"
    ) %>%
    hc_title(text = "Boxplot de Edaddias") %>%
    hc_xAxis(categories = c("Edaddias"), title = list(text = "Variable")) %>%
    hc_yAxis(title = list(text = "Valores")) %>%
    hc_tooltip(pointFormat = "Mín: {point.low}<br>Q1: {point.q1}<br>Mediana: {point.median}<br>Q3: {point.q3}<br>Máx: {point.high}")

hc_boxplot













