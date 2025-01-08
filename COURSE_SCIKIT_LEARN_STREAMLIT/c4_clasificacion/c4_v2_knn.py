import numpy as np

from sklearn.datasets import load_breast_cancer
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import train_test_split


cancer_dataset = load_breast_cancer()
print("Dataset data size:", cancer_dataset.data.shape)
print("Dataset target size:", cancer_dataset.target.shape)
print("Dataset target size:", np.unique(cancer_dataset.target))
print("Dataset target names:", cancer_dataset.target_names)

X_train, X_test, y_train, y_test = train_test_split(
    cancer_dataset.data,
    cancer_dataset.target,
    test_size=0.3,
    random_state=10,
    stratify=cancer_dataset.target,
)


knn_classifier = KNeighborsClassifier(n_neighbors=3)
knn_classifier.fit(X_train, y_train)

y_pred = knn_classifier.predict(X_test)

score = knn_classifier.score(X_test, y_test)
print(score)
