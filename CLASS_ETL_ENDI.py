# -*- coding: utf-8 -*-
"""
ETL para la ingesta de archivos de la ENDI
Versión: 1.0
Developer: Marcelo Chávez
ESPOL
Maestría en Estadística Aplicada
"""

# Borrar todas las variables en el entorno global
globals().clear()

import pyreadr
import pandas as pd
import numpy as np
from scipy.stats import kurtosis, skew
from sqlalchemy import create_engine
from tabulate import tabulate
import os

# Configurar el directorio de trabajo
# os.chdir('C:/Users/marcelochavez/Documents/TESIS/')

class ETL_ENDI:
    def __init__(self,
                 db_user='postgres',
                 db_password='marce',
                 db_name='db_stat',
                 db_host='localhost', 
                 db_port='5432'):
        self.engine = create_engine(f'postgresql://{db_user}:{db_password}@{db_host}:{db_port}/{db_name}')
    
    def leer_rds(self, ruta_archivo):
        """Leer archivo RDS y devolver DataFrame."""
        data = pyreadr.read_r(ruta_archivo)
        return list(data.values())[0]  # Devolver primer DataFrame encontrado en el archivo
    
    def exploratorio(self, df):
        """Generar resumen estadístico de un DataFrame."""
        resumen = pd.DataFrame({
            'Variable': df.columns,
            'Tipo': df.dtypes,
            'Minimo': df.apply(lambda x: x.min() if np.issubdtype(x.dtype, np.number) else np.nan),
            'Maximo': df.apply(lambda x: x.max() if np.issubdtype(x.dtype, np.number) else np.nan),
            'Rango': df.apply(lambda x: (x.max() - x.min()) if np.issubdtype(x.dtype, np.number) else np.nan),
            'Promedio': df.apply(lambda x: round(x.mean(), 2) if np.issubdtype(x.dtype, np.number) else "No aplica"),
            'Mediana': df.apply(lambda x: round(x.median(), 2) if np.issubdtype(x.dtype, np.number) else "No aplica"),
            'Moda': df.apply(lambda x: x.mode().iloc[0] if not x.mode().empty else np.nan),
            'Desviacion_Estandar': df.apply(lambda x: round(x.std(), 2) if np.issubdtype(x.dtype, np.number) else "No aplica"),
            'Coeficiente_Variacion': df.apply(lambda x: round(x.std() / x.mean(), 2) if np.issubdtype(x.dtype, np.number) and x.mean() != 0 else np.nan),
            'Varianza': df.apply(lambda x: round(x.var(), 2) if np.issubdtype(x.dtype, np.number) else "No aplica"),
            'Coeficiente_Asimetria': df.apply(lambda x: round(skew(x.dropna()), 2) if np.issubdtype(x.dtype, np.number) else "No aplica"),
            'Curtosis': df.apply(lambda x: round(kurtosis(x.dropna()), 2) if np.issubdtype(x.dtype, np.number) else "No aplica")
        })

        # Ajustar tipos de datos para visualización
        resumen['Tipo'] = resumen['Tipo'].replace({
            'object': 'Categórica',
            'datetime64[ns]': 'Fecha',
            'bool': 'Booleana',
            'float64': 'Numérica',
            'int64': 'Numérica'
        })
        return resumen

    def almacenamiento_postgresql(self, 
                                  df,
                                  table_name,
                                  schema='endi'):
        """Guardar DataFrame en PostgreSQL."""
        try:
            df.to_sql(table_name, 
                      self.engine, 
                      schema=schema, 
                      if_exists='replace', 
                      index=False)
            print(f"Tabla '{table_name}' almacenada en el esquema '{schema}' de la base de datos.")
        except Exception as e:
            print(f"Error al almacenar en PostgreSQL: {e}")
            
    def lectura_diccionario(self, 
                            file_path, 
                            sheets):
        """Leer múltiples pestañas de un archivo Excel y concatenar en un solo DataFrame."""
        dataframes_diccionarios = []
    
        # Leer cada pestaña, seleccionar columnas y procesar
        for sheet in sheets:
            # Leer la pestaña completa con las columnas A a F y a partir de la fila 11
            df = pd.read_excel(file_path, 
                               sheet_name=sheet, 
                               usecols='A:F', 
                               skiprows=10)  # Lee desde la fila 11, columnas A a F
    
            # Filtrar filas hasta la primera fila vacía o que contenga la palabra "Fuente" en la primera columna
            df = df[df.iloc[:, 0].notnull() &\
                    ~df.iloc[:, 0].astype(str).str.contains('Fuente:', case=False, na=False)]
    
            # Añadir el DataFrame procesado a la lista
            dataframes_diccionarios.append(df)
    
        # Concatenar todos los DataFrames en uno solo
        endi_diccionarios = pd.concat(dataframes_diccionarios, ignore_index=True)
        
        # Eliminar duplicados
        endi_diccionarios.drop_duplicates(inplace=True)
    
        # Renombrar las columnas
        endi_diccionarios.columns = ['codigo_variable', 
                                     'nombre_variable', 
                                     'pregunta', 
                                     'categorias', 
                                     'tipo_variable', 
                                     'formato']
        return endi_diccionarios
    
    def buscar_variable(self,
                        diccionario_df, 
                        variable):
        """Buscar una variable en el diccionario y devolver su contenido en formato tabular vertical."""
        # Filtrar los resultados que coinciden con el código de la variable
        resultados = diccionario_df.loc[diccionario_df['codigo_variable'] == variable]
        
        if not resultados.empty:
            # Preparar los datos para la visualización en formato vertical
            datos_verticales = []
            for idx, row in resultados.iterrows():
                for column in resultados.columns:
                    datos_verticales.append([column, row[column]])
    
            # Imprimir el resultado con tabulate, maximo dos columnas
            return tabulate(datos_verticales, headers=["Campo", "Valor"], tablefmt="fancy_grid", numalign="center")
        else:
            return f"No se encontraron resultados para la variable '{variable}'."


