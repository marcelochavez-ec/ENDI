# -*- coding: utf-8 -*-
"""
Created on Thu Nov 21 13:52:30 2024

@author: marcelo.chavez
"""

from CLASS_ETL_ENDI import *
import qgridnext as qt
import pandas as pd

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

# Creación de las fechas de nacimiento (dob) y de la última (dov) directamente con `pd.to_datetime`
# Primero, aseguramos que las columnas no tengan valores faltantes, luego las convertimos a fechas
f1_personas['dob'] = pd.to_datetime(
    f1_personas[['f1_s5_2_3', 'f1_s5_2_2', 'f1_s5_2_1']].astype(str).agg('-'.join, axis=1), 
    format='%Y-%m-%d', errors='coerce'
)

f1_personas['dov'] = pd.to_datetime(
    f1_personas[['f1_s5_3_3', 'f1_s5_3_2', 'f1_s5_3_1']].astype(str).agg('-'.join, axis=1), 
    format='%Y-%m-%d', errors='coerce'
)

# Verificar que las fechas se han convertido correctamente
# Ahora se puede proceder a calcular las edades con las fechas válidas.

# Cálculo de la edad en días, meses y años de manera vectorizada para mejorar el rendimiento
f1_personas['edaddias'] = (f1_personas['dov'] - f1_personas['dob']).dt.days
f1_personas['edadmeses'] = (f1_personas['dov'].dt.year - f1_personas['dob'].dt.year) * 12 + (f1_personas['dov'].dt.month - f1_personas['dob'].dt.month)
f1_personas['edadanios'] = (f1_personas['dov'] - f1_personas['dob']).dt.days // 365


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

















