
![](./assets/images/results_screen_shot.png)
[Full Report](./inferLoiter.html)

The rapid growth of spatial-temporal data is advancing intelligence capabilities. With the abundance of available GPS data, analysts can inform resource allocation decisions. Visibility on when a preyed animal stops, how long they stop, and when they are in-transit can aid in characterizing an area of interest, especially in areas when other information is sparse. Analysts currently use spatial density-based clustering techniques to glean insights from large GPS data sets, which has proved useful in characterizing areas of interest by identifying locations where this is a high number of observations occuring relatively close together. Loiter information, however, can only be analyzed by adding a time component to the clustering. This tutorial offers a method to employ Hierarchical Density-Based Spatial Clustering of Applications with Noise (HDBSCAN) on both spatial and temporal information. We found that clustering with HDBSCAN spatially and temporally can overcome the known disadvantages of common density-based clustering methods and yield reliable loiter information to inform a pattern of life.

This tutorial demonstrates an implementation on the density-based clustering algorithm HDBSCAN, to infer loiter information about a wolf of interest from a GPS data set. We begin by conducting collection analysis to understand the limitations of the available data. Then we compute data-informed HDBSCAN parameters and cluster spatially. Next, we examine each spatial cluster and cluster again temporally to ensure that the cluster is better understood as a loiter location than a location frequently visited in transit. Finally, we process each cluster for loiter information and visualize our results on an interactive map.

The map above conveys the results of clustering spatially and temporally using HDBSCAN. The loiter information for each cluster is shown when hovering over each cluster, and a heatmap of time of day by weekday is shown when clicking each cluster. We group the clusters by the time of day and include interactive toggles to gain insights on the wolf's pattern of life.



