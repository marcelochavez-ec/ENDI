import numpy as np
import pandas as pd

from sklearn.datasets import fetch_california_housing
from sklearn.preprocessing import normalize, MinMaxScaler


# normalize() for np array
array = np.array([1, 2, 3, 9, 6, 4, 5])
normalized_array = normalize([array])
print(normalized_array)


# normalize() for one column of df
housing_dataset = fetch_california_housing(as_frame=True)
print(housing_dataset.DESCR)

housing_df = housing_dataset.data
print(housing_df.head())

med_inc_array = np.array(housing_df["MedInc"])
normalized_med_inc = normalize([med_inc_array])
print(normalized_med_inc)

# housing_df["MedIncNorm"] = normalized_med_inc[0]


# normalize() for all the df
normalized_data = normalize(housing_df)
normalized_housing_df = pd.DataFrame(normalized_data, columns=housing_df.columns)
print(normalized_housing_df.head())


# MinMaxScaler applied to an array
scaler = MinMaxScaler(feature_range=(0, 1))
minmax_normalized_array = scaler.fit_transform(array)
print(minmax_normalized_array)

import pandas as pd
import prince

# Ejemplo de datos categóricos
data = pd.DataFrame({
    'Color': ['Rojo', 'Verde', 'Azul', 'Rojo', 'Azul'],
    'Tamaño': ['Grande', 'Pequeño', 'Mediano', 'Grande', 'Pequeño'],
    'Forma': ['Círculo', 'Cuadrado', 'Círculo', 'Cuadrado', 'Círculo']
})

# Crear y ajustar el modelo ACM
mca = prince.MCA(
    n_components=2,  # Número de componentes principales
    benzecri=True,   # Corrección de Benzécri para inercia
    copy=True,
    check_input=True
)

mca = mca.fit(data)

# Varianza explicada por cada componente (Inercia parcial)
inertia = mca.explained_inertia_
print(f"Inercia explicada por componente: {inertia}")

# Varianza total explicada (Suma de inercia parcial)
total_inertia = sum(inertia)
print(f"Inercia total explicada: {total_inertia}")

