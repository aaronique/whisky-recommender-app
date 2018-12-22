import os
import numpy as np
import pandas as pd

from sklearn.decomposition import PCA
from sklearn.cluster import KMeans, MeanShift, DBSCAN

PROFILE_DATA = os.path.abspath('data/profile.csv')

df = pd.read_csv(PROFILE_DATA)

profile_col_names = ['smoky', 'peaty', 'spicy', 'herbal', 'oily', 'full', 'rich', 'sweet', 'briny', 'salty', 'vanilla', 'tart', 'fruity', 'floral']

X = np.array(df[profile_col_names])

# PCA
pca = PCA(n_components=2)
_ = pca.fit(X)

# clustering

cluster = KMeans(n_clusters=5, random_state=0).fit(X)
# cluster = MeanShift(bandwidth=2).fit(X)
# cluster = DBSCAN(eps=5, min_samples=2).fit(X)

df['cluster'] = cluster.labels_
df = df[['id', 'name', 'cluster'] + profile_col_names]
