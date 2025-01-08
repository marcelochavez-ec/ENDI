import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

from sklearn.datasets import load_wine
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler


wine_dataset = load_wine()
print(wine_dataset.feature_names)

scaler = StandardScaler()
wine_data_scaled = scaler.fit_transform(wine_dataset.data)

df_wine_scaled = pd.DataFrame(wine_data_scaled, columns=wine_dataset.feature_names)

pca = PCA()
pca_wine_data = pca.fit_transform(df_wine_scaled)
var = pca.explained_variance_ratio_
print(var)

sum_var = np.cumsum(var)
sum_var_percent = np.round(sum_var, decimals=4) * 100
print(sum_var_percent)

plt.grid()
plt.plot(range(1, len(sum_var_percent)+1), sum_var_percent, marker='o')
plt.show()


wine_reduced_data = pca_wine_data[:, :8]
wine_reduced_data_df = pd.DataFrame(wine_reduced_data)
print(wine_reduced_data_df)
