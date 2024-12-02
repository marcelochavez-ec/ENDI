import numpy as np
import matplotlib.pyplot as plt

from sklearn.linear_model import LinearRegression
from sklearn.preprocessing import PolynomialFeatures
from sklearn.pipeline import make_pipeline


def sin_function(X):
    return 2 *  np.sin(X)


np.random.seed(10)

X = np.linspace(0, np.pi, 30)
y = sin_function(X) + np.random.randn(X.shape[0]) * 0.2

# plt.scatter(X, y, color='black')
# plt.show()

degrees = [1, 3, 20]

plt.figure(figsize=(14, 5))
for i in range(len(degrees)):

    polynomial_features = PolynomialFeatures(degree=degrees[i], include_bias=False)
    linear_regression = LinearRegression()
    pipeline = make_pipeline(polynomial_features, linear_regression)
    pipeline.fit(X[:, np.newaxis], y)

    X_test = np.linspace(0, np.pi, 50)
    y_predict = pipeline.predict(X_test[:, np.newaxis])

    y_real = sin_function(X_test)

    ax = plt.subplot(1, len(degrees), i + 1)
    plt.scatter(X, y, color='black')
    plt.plot(X_test, y_predict, label="Model")
    plt.plot(X_test, y_real, label="Ground Truth")

    plt.xlabel("X")
    plt.ylabel("y")
    plt.title(f"Linear Regression {degrees[i]} degrees")

plt.show()
