##########
# HDSCAN
##########

# Imports
import numpy as np
import pandas as pd
import hdbscan
from kneed import KneeLocator

# Functions
def runHDB(points,minClusterSize,epsDist,minSampleSize=None,distMatrix=None,verbose=False):

    # Organize parameters
    if minSampleSize is None:
        minSampleSize = minClusterSize # restore default

    # Define cluster object
    if distMatrix is not None:
        clusterFinder = hdbscan.HDBSCAN(min_cluster_size=int(minClusterSize),
                                        min_samples=int(minSampleSize),
                                        metric="precomputed",
                                        cluster_selection_epsilon=epsDist,
                                        cluster_selection_method="eom")
        X = distMatrix

    else:
        clusterFinder = hdbscan.HDBSCAN(min_cluster_size=int(minClusterSize),
                                        min_samples=int(minSampleSize),
                                        metric="haversine",
                                        cluster_selection_epsilon=(epsDist/1000)/6371,
                                        cluster_selection_method="eom")
        X = np.radians(points)
    if verbose:
        print("Running HDBSCAN on {} observations".format(len(X)))

    res = clusterFinder.fit(X)
    y = res.labels_
    if verbose:
        print("Found {} clusters".format(pd.Series(y).max() + 1))

    return y + 1

def findKnee(order,distances,smoothParam,interpMethod='interp1d'):
    kneedle = KneeLocator(order,
                          distances,
                          S=smoothParam,
                          interp_method=interpMethod,
                          curve='convex',
                          direction='increasing')
    return kneedle.knee_y


