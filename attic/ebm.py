import openml
import pandas as pd
from sklearn.model_selection import train_test_split
from interpret.glassbox import ExplainableBoostingClassifier
from interpret import show
from interpret import set_visualize_provider
from interpret.provider import InlineProvider
from interpret.blackbox import PartialDependence

dataset = openml.datasets.get_dataset(41144)
X, y, categorical_indicator, attribute_names = dataset.get_data(
    dataset_format="dataframe", target=dataset.default_target_attribute
)

seed = 1
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.20, random_state=seed)

ebm = ExplainableBoostingClassifier(random_state=seed, interactions=0)
ebm.fit(X_train, y_train)
ebm.feature_groups_
y_hat = ebm.predict(X_test)
(y_hat != y_test).mean()  # CE

EBM und monotonie? https://github.com/interpretml/interpret/issues/184

