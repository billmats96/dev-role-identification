# dev-role-identification
Pattern Recognition project utilizing github user metrics as well as statistical and machine learning techniques(clustering) using R language to identify developer roles.

## A Brief Description 
The core project extracts user metrics per repository by utilizing github's api (e.g. issues opened, issues closed, total_commits, total_comments..)
There is a dedicated preprocessing procedure (remove irregularities, handle missing values, dimensionality reduction, normalize data) 
The algorithm moves on to an initial K-means clustering to identify roles with evaluation beased on silhouette, cohesion, separation criteria.
Within the resulting clustering, a second K-means clustering is applied to each cluster to identify the behaviour of each role

 
