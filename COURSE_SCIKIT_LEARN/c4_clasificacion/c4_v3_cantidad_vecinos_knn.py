import matplotlib.pyplot as plt

from sklearn.datasets import load_breast_cancer
from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import train_test_split


cancer_dataset = load_breast_cancer()

X_train, X_test, y_train, y_test = train_test_split(
    cancer_dataset.data,
    cancer_dataset.target,
    test_size=0.3,
    random_state=10,
    stratify=cancer_dataset.target,
)

k_values = range(1, 15)
score_list = []

for k in k_values:
    knn_classifier = KNeighborsClassifier(n_neighbors=k)
    knn_classifier.fit(X_train, y_train)
    score = knn_classifier.score(X_test, y_test)
    score_list.append(score)


plt.plot(k_values, score_list, label="Test score")
plt.show()
