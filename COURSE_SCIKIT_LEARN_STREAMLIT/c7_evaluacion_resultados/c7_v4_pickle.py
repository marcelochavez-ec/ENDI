import joblib
import pickle

from sklearn.datasets import load_diabetes
from sklearn.linear_model import Ridge
from sklearn.model_selection import train_test_split


diabetes_data, diabetes_target = load_diabetes(return_X_y=True)

X_train, X_test, y_train, y_test = train_test_split(
    diabetes_data,
    diabetes_target,
    test_size=0.3,
    random_state=10
)

ridge = Ridge(alpha=100)
ridge.fit(X_train, y_train)

# Using pickle
with open("ridge.pickle", "wb") as file:
    pickle.dump(ridge, file)

with open("ridge.pickle", "rb") as file:
    ridge = pickle.load(file)

# Using joblib
with open("ridge.joblib", "wb") as file:
    joblib.dump(ridge, file)

with open("ridge.joblib", "rb") as file:
    ridge = joblib.load(file)


print(ridge.score(X_test, y_test))
