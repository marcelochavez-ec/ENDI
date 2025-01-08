from sklearn.model_selection import cross_val_score, KFold, StratifiedKFold
from sklearn.neighbors import KNeighborsClassifier
from sklearn.datasets import load_iris


iris_dataset = load_iris()
X, y = iris_dataset.data, iris_dataset.target

knn = KNeighborsClassifier(n_neighbors=3)

kf = KFold(n_splits=5, shuffle=True, random_state=10)
skf = StratifiedKFold(n_splits=5, shuffle=True, random_state=10)

scores = cross_val_score(knn, X, y, cv=skf, scoring='accuracy')

print("Cross validation scores:", scores)
print("Average scores:", scores.mean())
