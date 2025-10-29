**Spatial ICA (sICA) is more suitable for fMRI data in general.**

-   Spatial ICA assumes that spatial maps (brain networks) are statistically independent. Different functional networks are reasonably independent in their spatial patterns. This matches our neuroscientific understanding that different brain systems occupy distinct anatomical regions.
-   Temporal signals are often correlated. The assumption that time courses must be independent (as required in tICA) is less reasonable in fMRI data.
-   Number of observations can be a factor. fMRI datasets typically have many more spatial locations (voxels) than time points, making sICA more feasible.