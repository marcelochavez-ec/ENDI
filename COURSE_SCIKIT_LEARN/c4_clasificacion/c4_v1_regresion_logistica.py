import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

from sklearn.metrics import confusion_matrix
from sklearn.datasets import load_iris
from sklearn.linear_model import LogisticRegression
from sklearn.model_selection import train_test_split


iris_data, iris_target = load_iris(return_X_y=True)


X_train, X_test, y_train, y_test = train_test_split(
    iris_data,
    iris_target,
    test_size=0.3,
    random_state=4,
    stratify=iris_target
)


logistic_regr = LogisticRegression(max_iter=1000)
logistic_regr.fit(X_train, y_train)

y_pred = logistic_regr.predict(X_test)

score = logistic_regr.score(X_test, y_test)
print(score)

confusion = confusion_matrix(y_test, y_pred)
print(confusion)

confusion_df = pd.DataFrame(
    confusion,
    index=["setosa", "versicolor", "virginica"],
    columns=["setosa", "versicolor", "virginica"]
)

sns.heatmap(confusion_df, annot=True, cmap = 'Blues_r')
plt.ylabel("Actual value")
plt.xlabel("Predicted value")
plt.show()
