import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

from sklearn.cluster import KMeans
from sklearn.datasets import load_iris


iris_dataset = load_iris()
df_iris = pd.DataFrame(iris_dataset.data, columns=iris_dataset.feature_names)
df_iris["target"] = iris_dataset.target

# Elbow method
inertia = []
for k in range(1, 11):
    kmeans = KMeans(random_state=5, max_iter=300, n_clusters=k)
    kmeans.fit(iris_dataset.data)
    inertia.append(kmeans.inertia_)

plt.grid()
plt.plot(range(1, 11), inertia, marker="o")
plt.show()

# Select k = 3
kmeans = KMeans(random_state=10, max_iter=300, n_clusters=3)
kmeans.fit(iris_dataset.data)
print(kmeans.labels_)

df_iris["cluster"] = kmeans.labels_

grouped_iris = (
    df_iris
    .groupby(["target", "cluster"])
    .agg({"petal length (cm)": "count"})
)
print(grouped_iris)

_, ax = plt.subplots(ncols=2, figsize=(10, 5))

sns.scatterplot(
    ax=ax[0],
    data=df_iris,
    hue="cluster",
    x="sepal length (cm)",
    y="sepal width (cm)"
)
ax[0].set_title("Clusters")

sns.scatterplot(
    ax=ax[1],
    data=df_iris,
    hue="target",
    x="sepal length (cm)",
    y="sepal width (cm)"
)
ax[1].set_title("Target")

plt.show()
