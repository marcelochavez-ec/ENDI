import pandas as pd

from sklearn.compose import make_column_transformer
from sklearn.preprocessing import OneHotEncoder


# One hot usando OneHotEncoder
adult_df = pd.read_csv("adult.csv")
print(adult_df.columns)
print(adult_df.head())
print(adult_df["sex"].unique())

one_hot_enconder = OneHotEncoder()
encode_adult_sex = one_hot_enconder.fit_transform(adult_df[["sex"]])
print(encode_adult_sex.toarray())
print(one_hot_enconder.categories_)

adult_df[one_hot_enconder.categories_[0]] = encode_adult_sex.toarray()
print(adult_df.columns)

# One hot usando OneHotEncoder y make_column_transformer
adult_df = pd.read_csv("adult.csv")
transformer = make_column_transformer(
    (OneHotEncoder(), ["sex"]),
    remainder='passthrough'
)
transformed_data = transformer.fit_transform(adult_df)
transformed_adult_df = pd.DataFrame(
    transformed_data, 
    columns=transformer.get_feature_names_out()
)
transformed_adult_df = transformed_adult_df.rename(columns=lambda x: x.split("__")[-1])
print(transformed_adult_df.columns)
