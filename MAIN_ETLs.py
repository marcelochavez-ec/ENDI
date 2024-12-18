# -*- coding: utf-8 -*-
"""
Created on Thu Nov 21 13:52:30 2024

@author: marcelo.chavez
"""

from CLASS_ETL_ENDI import *
import qgridnext as qt

# Instancia de la clase ETL_ENDI
etl_endi = ETL_ENDI()

# Path de los file de RDS
file_f1_hogar = 'BDD/BDD_ENDI_R2_f1_hogar.rds'
file_f1_personas = 'BDD/BDD_ENDI_R2_f1_personas.rds'
file_f2_lactancia = 'BDD/BDD_ENDI_R2_f2_lactancia.rds'
file_f2_mef = 'BDD/BDD_ENDI_R2_f2_mef.rds'
file_f2_salud_ninez = 'BDD/BDD_ENDI_R2_f2_salud_ninez.rds'
file_f3_desarrollo_infantil = 'BDD/BDD_ENDI_R2_f3_desarrollo_inf.rds'

# Cargar de los archivos .RDS

f1_hogar = etl_endi.leer_rds(file_f1_hogar)
f1_personas = etl_endi.leer_rds(file_f1_personas)
f2_lactancia = etl_endi.leer_rds(file_f2_lactancia)
f2_mef = etl_endi.leer_rds(file_f2_mef)
f2_salud_niniez = etl_endi.leer_rds(file_f2_salud_ninez)
f3_desarrollo_infantil = etl_endi.leer_rds(file_f3_desarrollo_infantil)

# Generar y mostrar resumen estadístico
exploratorio_hogar = etl_endi.exploratorio(f1_hogar)

# Almacenar en PostgreSQL
etl_endi.almacenamiento_postgresql(f1_hogar, 'f1_hogar')
etl_endi.almacenamiento_postgresql(f1_personas, 'f1_personas')
etl_endi.almacenamiento_postgresql(f2_lactancia, 'f2_lactancia')
etl_endi.almacenamiento_postgresql(f2_mef, 'f2_mef')
etl_endi.almacenamiento_postgresql(f2_salud_niniez, 'f2_salud_ninez')
etl_endi.almacenamiento_postgresql(f3_desarrollo_infantil, 'f3_desarrollo_infantil')

# Ruta del archivo Excel que contiene las pestañas
file_path = 'BDD/Diccionario_variables_ENDI_R2.xlsx'

# Lista de nombres de las pestañas que quieres leer
sheets = ['f1_personas', 
          'f1_hogar', 
          'f2_mef', 
          'f2_lactancia',
          'f2_salud_niñez',
          'f3_desarrollo_infantil']

# Llamar al método para leer las pestañas y concatenar los DataFrames
endi_diccionarios = etl_endi.lectura_diccionario(file_path, sheets)


# Búsqueda de variable en el diccionario
print(etl_endi.buscar_variable(endi_diccionarios, 'dcronica2_5'))

















