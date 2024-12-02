from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split


iris_dataset = load_iris()
X_train, X_test, y_train, y_test = train_test_split(
    iris_dataset.data,
    iris_dataset.target,
    test_size=0.3,
    random_state=42,
    stratify=iris_dataset.target,
)

print(len(iris_dataset.data))
print(len(X_train))
print(len(X_test))

print(y_test)
