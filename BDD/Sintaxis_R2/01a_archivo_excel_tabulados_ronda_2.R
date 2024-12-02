
# Titulo de la Sintaxis: 
# Archivos Tabulados

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

#-------------------------------------------------------------------------------
# >>>>                          Importante                               <<<<  #
#                                                                              #
#       Primero realizar la accion descrita en el documento "00.master.R"      #
#                                                                              #
#-------------------------------------------------------------------------------


# Este documento crea el archivo de excel donde se importaran los tabulados, 
# su principal caracteristica que guarda los indices y sus diferentes 
# hiperviculos

if(!require("pacman")) install.packages("pacman") 

pacman::p_load(
  openxlsx # importacion / exportacion de libros de Excel de varias hojas
)

#==============================================================================#
####                              Funciones                                 ####
#==============================================================================#

# Titulos   
Style_titulos <- function(yy,zz) {
  
  # En Indice 
  # Agregar hojas
  addWorksheet(wb, sheetName = "INDICE", gridLines = FALSE)
  
  # Insertar imagen
  # img <- file.path("Info","Imagen","endi_imagen.png")
  # 
  # insertImage(wb,
  #             "INDICE",
  #             img,
  #             startRow = 1,
  #             startCol = "A",
  #             width = 32.5,
  #             height = 2.30,
  #             units = "cm"
  # )
  
  # Titulo   
  setRowHeights(wb,
                sheet = "INDICE",
                rows = 1,
                heights = 70)
  
  tabSty <- createStyle(fontSize = 15.7,
                        fontName = "Century Gothic",
                        fontColour = "#595959",
                        textDecoration = "bold",
                        halign = "center")
  
  addStyle(wb,
           sheet      = "INDICE",
           style      = tabSty,
           rows       = 3,
           cols       = "B",  
           gridExpand = TRUE
  )
  
  setColWidths(wb, 
               sheet  = "INDICE", 
               cols   = "B", 
               widths = 147
  )
  
  setColWidths(wb, 
               sheet  = "INDICE", 
               cols   = "A", 
               widths = 5
  ) 
  
  writeData(wb,
            sheet       = "INDICE",
            x           = yy,
            xy          = c("B", 3)
  )
  
  # Subtitulo
  tabSty <- createStyle(fontSize = 14.7,
                        fontName = "Century Gothic",
                        fontColour = "#595959",
                        textDecoration = "bold",
                        halign = "left")
  
  addStyle(wb,
           sheet      = "INDICE",
           style      = tabSty,
           rows       = 5,
           cols       = "B",  
           gridExpand = TRUE
  )
  
  writeData(wb,
            sheet       = "INDICE", 
            x           = zz,
            xy          = c("B", 5)
  )
  
}



# ----------------  Subtitulos´


Style_subtitulos <- function(y,z,xz,yy) {
  
  headSty <- createStyle(fontSize = 12,
                         fontName = "Century Gothic",
                         fontColour = "#363636",
                         fgFill = "#836FFF",
                         halign = "center",
                         valign = "center",
                         textDecoration = "bold")
  
  addStyle(wb,
           sheet      = y,
           style      = headSty,
           rows       = yy,
           cols       = "A", 
           gridExpand = TRUE
  )
  
  writeData(wb,
            sheet = y,
            x = z,
            xy = c("A",yy)
  )
  
  tabSty <- createStyle(fontSize = 12,
                        fontName = "Century Gothic",
                        fontColour = "#363636",
                        fgFill = "#836FFF",
                        halign = "left",
                        valign = "center",
                        textDecoration = "bold")
  
  addStyle(wb,
           sheet      = y,
           style      = tabSty,
           rows       = yy,
           cols       = "B",  
           gridExpand = TRUE
  )
  
  writeData(wb,
            sheet       = y, 
            x           =  xz,
            xy          = c("B", yy)
  )
  
} 



# ---------------------Indicador


Style_tabulados <- function(y,xz,yy,zz) {
  
  # y  = codigo de la hoja
  # xz = nombre del estimador
  # yy = fila de la hoja indice
  # zz = contador i
  # x  = nota 
  
  # Hallar el indice del data frame de los estimadores
  
  indice <- which(df_title[, 1] == y)
  
  compar_1 <- df_title[indice, 3]
  
  if(indice == 1){compar_2 = "primera_fila"} else {compar_2 = df_title[indice-1, 3]}
  
  if ( compar_1 != compar_2 ) {
    
    fila_ind <- fila_ind + 2 # espacio despues de colocar el tema
    
    y_sub = "INDICE"
    z_sub <- substr(df_title[zz,1], 0, 2)
    xz_sub <- df_title[indice, 3]
    yy_sub <- fila_ind 
    
    Style_subtitulos (y_sub,z_sub,xz_sub,yy_sub)
    fila_ind <- fila_ind + 2 # espacio antes de finalizar el tema
    
  } else {
    
    fila_ind <- fila_ind + 1 # el contador interno para cada estimador
  }
  # reasigno al contador para luego retornar mi contador de filas de Indice
  
  yy <- fila_ind
  
  headSty <- createStyle(fontSize = 11,
                         fontName = "Century Gothic",
                         halign = "center")
  
  addStyle(wb,
           sheet      = "INDICE",
           style      = headSty,
           rows       = yy,
           cols       = "A", 
           gridExpand = TRUE
  )
  
  writeFormula(wb, 
               "INDICE",
               startRow = yy,
               startCol = 1,
               x = makeHyperlinkString(
                 sheet = y, row = 1, col = 1,
                 text = y
               )
  )
  
  tabSty <- createStyle(fontSize = 11,
                        fontName = "Century Gothic",
                        halign = "left")
  
  addStyle(wb,
           sheet      = "INDICE",
           style      = tabSty,
           rows       = yy,
           cols       = "B",  
           gridExpand = TRUE
  )
  
  writeData(wb,
            sheet       = "INDICE", 
            x           =  xz,
            xy          = c("B", yy)
  )
  
  # Agregar hojas
  addWorksheet(wb, sheetName = y, gridLines = FALSE)
  
  # Insertar imagen 
  setRowHeights(wb,
                sheet = y,
                rows = 1,
                heights = 70
  )
  
  # img <- file.path("Info","Imagen","endi_imagen.png")
  # 
  # insertImage(wb,
  #             y,
  #             img,
  #             startRow = 1,
  #             startCol = "B",
  #             width = 36,
  #             height = 2.30,
  #             units = "cm"
  # )
  
  # Identificacion de la tabla
  setColWidths(wb, 
               sheet  = y, 
               cols   = "A", 
               widths = 1.25
  )
  
  df_eti <- data.frame(Etiqueta = c("Tabla N°:", "Indicador/variable:", "Población:",
                                    "Fuente:", "Elaboración:"),
                       Especificacion = c(zz, xz,"",
                                          "Encuesta Nacional sobre Desnutrición Infantil - ENDI 2023 - 2024",
                                          "Instituto Nacional de Estadística y Censos (INEC)"))
  
  writeData(wb,
            sheet       = y, 
            x           = df_eti,
            xy          = c("B", 2),
            colNames    = FALSE
  )
  
  tabSty <- createStyle(fontSize = 10.5,
                        fontName = "Century Gothic",
                        halign = "left",
                        textDecoration = "bold"
  )
  
  tabSty1 <- createStyle(fontSize = 10.5,
                         fontName = "Century Gothic",
                         halign = "left"
  )
  
  addStyle(wb,
           sheet      = y,
           style      = tabSty,
           rows       = 2:6,
           cols       = 2,  
           gridExpand = TRUE
  )
  
  addStyle(wb,
           sheet      = y,
           style      = tabSty1,
           rows       = 2:6,
           cols       = 3,  
           gridExpand = TRUE
  )
  
  setColWidths(wb,
               sheet  = y,
               cols   = "B",
               widths = 16.5
  )
  
  setColWidths(wb,
               sheet  = y,
               cols   = "C",
               widths = 24.25
  )
  return (fila_ind)
  
} 


#==============================================================================#
####                              Exportacion                               ####
#==============================================================================#

# Crear un libro de trabajo 
wb  <- createWorkbook()
modifyBaseFont(wb, fontSize = 12, fontName = "Calibri")

# Aplicacion de funciones -----------------------------------------------------#  

Style_titulos("ENCUESTA NACIONAL DE DESNUTRICIÓN INFANTIL - ENDI 2023 - 2024", "Indicadores")

fila_ind = 6

for (i in seq_along(t(df_title[,1]))) {
  # Directamente se exporta el archivo con la funcion Style, aqui se busca  
  # inicializar establecer los parametros por cada variable del data frame df_title
  # este data frame tiene los indicadores y mas informacion, adicional se 
  # busca recuperar el contador fila_ind
  
  yy <- fila_ind  #recupero el valor generado en la función Style_tabulados
  y  <- df_title[i,1]
  xz <- df_title[i,2]
  zz <- i
  
  fila_ind <-Style_tabulados(y,xz,yy,zz) #para recuperar el contador
}

# ---- manejar el error de permisos por documentos abiertos-----

result <- tryCatch({
  saveWorkbook(wb, ruta_archivo_formato, overwrite = TRUE)
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
  cat(sprintf("\nSe ha exportado el documento que recogera todos los tabulados en:\n"))
  message (ruta_archivo_formato)
} else {
  cat(sprintf("\t<<NO SE PUDO EXPORTAR>>\n Cerrar el documento si está abierto.\n En la ruta:\n"))
  message (ruta_archivo_formato)
}


