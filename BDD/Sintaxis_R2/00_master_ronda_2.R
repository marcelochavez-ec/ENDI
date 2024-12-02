
# Titulo de la Sintaxis: 
# Master

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

#==============================================================================#
#                                                                              #
####                          MASTER DE DOCUMENTOS                          ####
#                                                                              #
#==============================================================================#

# Este documento permite crear directamente los tabulados y tambien es necesario
# para ejecutar archivos de las diferentes sintaxis

#------------------------------------------------------------------------------#
#                              >>>>Importante<<<<
#
# Para la manipulacion de los diferentes archivos siempre debe  
# ejecutar toda la seccion 1
#------------------------------------------------------------------------------#

# Nota: Si se requiere exportar tablas individuales puede reemplazar la funcion
# "Style_tab" por "Style_tab_0", esto exportara un documento llamado:
# "ENDI_tab.xlsx"

rm(list = ls(all = TRUE))

if(!require("pacman")) install.packages("pacman") 

pacman::p_load(
  here # construye rutas a los archivos de su proyecto
) 

#==============================================================================#
####                               Seccion 1                                ####
#==============================================================================#

#  ===> Direcciones

# Aqui se ingresara la ubicacion de las bases ".rds", de los documentos 
# scripts ".R" y donde se quiere exportar cualquier tabulado. 

# 1. Se debe proporcionar la ruta del archivo absoluta o completa entre  
# comillas. 
# 2. Use barras diagonales ( / ). Este no es el valor predeterminado para las 
# rutas de archivos de Windows, ejmp: "C:/ENDI/Data"
# 3. El diccionario de variables debe estar en la misma carpeta donde se
# encuentra las bases de datos ".rds"

# Es probable que R no reconozca las rutas de archivos que comienzan con  
# barras inclinadas dobles (p. ej., “//…”) y produzcan un error. Considere 
# mover  su trabajo a una unidad "con nombre" o "con letras" dentro de los 
# discos locales "C:" o "D:".


#----> Manipular solo estas tres líneas (editar/actualizar/cambiar/modificar)---

# Ubicacion de las bases de datos ".rds" y del diccionario
link_bdd <- ""

# Ubicacion donde se encuentra los scripts ".R"
link_scripts <- ""

# Ubicacion donde se quiere que se exporte los tabulados
link_tabulados <- ""

#------------------------------------------------------------------------------#

# Documento necesario para ejecutar todos los scripts
ruta_archivo_formato <- paste0(link_tabulados, "/Tabulados_ENDI_R2.xlsx")

source(paste0(link_scripts,"/01_inicializacion_ronda_2.R"), encoding = "UTF-8") 
# Ejecuta las funciones

#==============================================================================#
####                               Seccion 2                                ####
#==============================================================================#

# Tener precaucion con la siguiente linea, sobreescribe el archivo ".xlsx", si 
# ya anteriormente se ha creado.
source(paste0(link_scripts,"/01a_archivo_excel_tabulados_ronda_2.R"),encoding = "UTF-8") 
# Crea el documento excel con sus diferentes hojas por parametro

#================================ TEMATICAS ===================================#

# =====> Sintaxis por tematicas

source(paste0(link_scripts,"/02_vivienda_hogar_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/03_hacinamiento_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/04_estado_nutricional_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/05_anemia_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/06_vacunacion_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/07_lactancia_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/08_control_prenatal_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/09_salud_ninez_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/10_primera_infancia_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/11_fecundidad_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/12_fecundidad_2_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/13_calidad_agua_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/14_desarrollo_infantil_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/15_desarrollo_infantil_Macarthur_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/16_desarrollo_infantil_TVIP_ronda_2.R"), encoding = "UTF-8")
source(paste0(link_scripts,"/17_depresion_materna_ronda_2.R"), encoding = "UTF-8")

