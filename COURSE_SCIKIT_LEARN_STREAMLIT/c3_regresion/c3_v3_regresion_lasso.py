
from sklearn.datasets import fetch_california_housing
from sklearn.linear_model import Lasso
from sklearn.metrics import r2_score
from sklearn.model_selection import train_test_split


housing_data, housing_target = fetch_california_housing(return_X_y=True)

X_train, X_test, y_train, y_test = train_test_split(
    housing_data,
    housing_target,
    test_size=0.3,
    random_state=10
)


lasso = Lasso(alpha=0.01)
lasso.fit(X_train, y_train)

y_pred = lasso.predict(X_test)

r2 = r2_score(y_test, y_pred)

print(f"R2 Score:{r2}")
print(f"Coeficientes: {lasso.coef_}")
