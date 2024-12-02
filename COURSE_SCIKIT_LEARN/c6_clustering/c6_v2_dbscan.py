import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

from sklearn.cluster import DBSCAN
from sklearn.datasets import make_moons
from sklearn.preprocessing import StandardScaler


X, y = make_moons(n_samples=500, noise=0.1, random_state=10)
moons_df = pd.DataFrame({"X1": X[:, 0], "X2": X[:, 1]})

# sns.scatterplot(data=moons_df, x="X1", y="X2")
# plt.show()

scaler = StandardScaler()
X_scaled = scaler.fit_transform(X)

dbscan = DBSCAN(eps=0.3, min_samples=4)
y = dbscan.fit_predict(X_scaled)

moons_df["cluster"] = y

sns.scatterplot(data=moons_df, x="X1", y="X2", hue="cluster")
plt.show()
