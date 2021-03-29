dataset = read.csv("dataset.csv", sep=';', header = TRUE, na.strings = c("NA"),
                   colClasses = c("character", "character", "numeric", "numeric", "integer", "integer",
                                  "integer", "numeric", "numeric", "integer", "integer", "integer", "integer",
                                  "integer", "integer", "numeric", "integer", "integer", "integer", "integer",
                                  "integer", "integer", "integer", "integer", "integer", "integer", "integer"))


inactive = dataset[dataset[,'inactive_period_within_active_period']>90,]
active = dataset[dataset[,'inactive_period_within_active_period']<=90,]


# Spot attributes with NA values
colSums(is.na(dataset))

# Handling Missing Values
active$average_issues_comments_length[is.na(active$average_issues_comments_length)] = 0
active$average_comments_per_issue[is.na(active$average_comments_per_issue)] = 0

# Determine whether active contributors with activity period = 1, have useful info or should be thrown out of dataset
active_more_1_day = active[which(active$activity_period_in_days>1), ]

# Reduce dimensions based on correlation
correlations = cor(active[,3:27])
active_reduced = active[,c(1:5,8:12,14:17,19:21,24:25)]

# Irregular attribute values
par(mfrow = c(1, 2))
a = cut(active_reduced$average_issues_comments_length, 10)
hist(active_reduced$average_issues_comments_length, main = "Before Outlier Removal")
#plot(a, main = "Before Outlier Removal")
active_reduced = active_reduced[active_reduced[,'average_issues_comments_length']<=1430,]
hist(active_reduced$average_issues_comments_length, main = "After Outlier Removal")
par(mfrow = c(1, 1))

a = cut(active_reduced$average_time_to_close_issues, 10)
plot(a)
# active_reduced = active_reduced[active_reduced[,'average_time_to_close_issues']<=1430,]
# hist(active_reduced$average_time_to_close_issues)

a = cut(active_reduced$issues_closed, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'issues_closed']<=984,]
hist(active_reduced$issues_closed)

a = cut(active_reduced$issues_closed_per_day, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'issues_closed_per_day']<=0.373,]
hist(active_reduced$issues_closed_per_day)

a = cut(active_reduced$average_comments_per_issue, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'average_comments_per_issue']<=6.5,]
hist(active_reduced$average_comments_per_issue)


a = cut(active_reduced$early_additions, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'early_additions']<=2220,]
hist(active_reduced$early_additions)


a = cut(active_reduced$early_deletions, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'early_deletions']<=1430,]
hist(active_reduced$early_deletions)

a = cut(active_reduced$late_additions, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'late_additions']<=2490,]
hist(active_reduced$late_additions)

a = cut(active_reduced$change_bursts, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'change_bursts']<=36.4,]
hist(active_reduced$change_bursts)

a = cut(active_reduced$inactive_period_within_active_period, 10)
plot(a)
#active_reduced = active_reduced[active_reduced[,'inactive_period_within_active_period']<=36.4,]
#hist(active_reduced$inactive_period_within_active_period)

a = cut(active_reduced$tot_additions, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'tot_additions']<=13000,]
hist(active_reduced$tot_additions)

a = cut(active_reduced$activity_period_in_days, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'activity_period_in_days']<=1885,]
hist(active_reduced$activity_period_in_days)

a = cut(active_reduced$total_file_additions, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'total_file_additions']<=338,]
hist(active_reduced$total_file_additions)

a = cut(active_reduced$total_file_deletions, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'total_file_deletions']<=90,]
hist(active_reduced$total_file_deletions)

a = cut(active_reduced$commits_authored, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'commits_authored']<=90,]
hist(active_reduced$commits_authored)

a = cut(active_reduced$total_lines_of_code_changed, 10)
plot(a)
active_reduced = active_reduced[active_reduced[,'total_lines_of_code_changed']<=17000,]
hist(active_reduced$total_lines_of_code_changed)


# Data normalization
active_reduced_scaled <- as.data.frame(scale(active_reduced[, c(3:19)], center = TRUE, scale = TRUE))
normalized = as.data.frame(lapply(active_reduced[, c(3:19)], function(x) (x - min(x))/(max(x) - min(x))))


############################################################################

# Kmeans clustering evaluation criteria

kdata = normalized[,c(1, 3, 4, 16)]

COH = c(); 
SEP = c();
SIL = c();

library(cluster)
for (i in 2:15)  
{
  model <- kmeans(kdata, centers = i)
  COH[i-1] <- model$tot.withinss 
  SEP[i-1] <- model$betweenss
  model_silhouette = silhouette(model$cluster, dist(kdata))
  SIL[i-1] <- mean(model_silhouette[, 3])
}

par(mfrow = c(3, 1))
plot(2:15, COH, type="b", xlab="Number of Clusters", ylab="COH", main = "Cohesion") 
plot(2:15, SEP, type="b", xlab="Number of Clusters", ylab="SEP", main = "Separation")
plot(2:15, SIL, type="b", xlab="Number of Clusters", ylab="SIL", main = "Mean Silhouette")
par(mfrow = c(1, 1))

# Clustering in 4 clusters
set.seed(0);
model = kmeans(kdata, centers = 4) 
#model$centers 
#model$cluster 

# Cohesion and Separation 
cohesion = model$tot.withinss 
separation = model$betweenss

# Plot Data
plot(kdata, col = model$cluster, pch = 15) 
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)


model$cluster[intersect(which(kdata$commits_authored < 0.1), which(model$cluster==3))] = 4

# Plot Data
plot(kdata, col = model$cluster, pch = 15) 
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)

# Clustered datasets
undecided = normalized[model$cluster == 4,]
ops = normalized[model$cluster == 2,]
dev = normalized[model$cluster == 1,]
devops = normalized[model$cluster == 3,]



# Profile Recognition 



# Devs

devdata = dev[, c(3, 6,15,17)]

COH = c(); 
SEP = c();
SIL = c();


for (i in 2:15)  
{
  model <- kmeans(devdata, centers = i)
  COH[i-1] <- model$tot.withinss 
  SEP[i-1] <- model$betweenss
  model_silhouette = silhouette(model$cluster, dist(devdata))
  SIL[i-1] <- mean(model_silhouette[, 3])
}

par(mfrow = c(3, 1))
plot(2:15, COH, type="b", xlab="Number of Clusters", ylab="COH", main = "Cohesion") 
plot(2:15, SEP, type="b", xlab="Number of Clusters", ylab="SEP", main = "Separation")
plot(2:15, SIL, type="b", xlab="Number of Clusters", ylab="SIL", main = "Mean Silhouette")
par(mfrow = c(1, 1))

# Clustering in 4 clusters
set.seed(0);
model = kmeans(devdata, centers = 4) 

# Cohesion and Separation 
cohesion = model$tot.withinss 
separation = model$betweenss

# Plot Data
plot(devdata, col = model$cluster, pch = 15) 
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)

# Box Plots
par(mfrow = c(2,2 ))
boxplot(issues_closed~model$cluster, data = devdata, main = "issues_closed")
boxplot(early_additions~model$cluster, data = devdata, main = "early_additions")
boxplot(total_file_deletions~model$cluster, data = devdata, main = "total_file_deletions")
boxplot(total_lines_of_code_changed~model$cluster, data = devdata, main = "total_lines_of_code_changed")

par(mfrow = c(1, 1))




# Ops

opsdata = ops[, c(1,5)]

COH = c(); 
SEP = c();
SIL = c();


for (i in 2:15)  
{
  model <- kmeans(opsdata, centers = i)
  COH[i-1] <- model$tot.withinss 
  SEP[i-1] <- model$betweenss
  model_silhouette = silhouette(model$cluster, dist(opsdata))
  SIL[i-1] <- mean(model_silhouette[, 3])
}

par(mfrow = c(3, 1))
plot(2:15, COH, type="b", xlab="Number of Clusters", ylab="COH", main = "Cohesion") 
plot(2:15, SEP, type="b", xlab="Number of Clusters", ylab="SEP", main = "Separation")
plot(2:15, SIL, type="b", xlab="Number of Clusters", ylab="SIL", main = "Mean Silhouette")
par(mfrow = c(1, 1))

# Clustering in 4 clusters
set.seed(0);
model = kmeans(opsdata, centers = 4) 

# Cohesion and Separation 
cohesion = model$tot.withinss 
separation = model$betweenss

# Plot Data
plot(opsdata, col = model$cluster, pch = 15) 
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)


# Box Plots
par(mfrow = c(1,2))
boxplot(average_issues_comments_length~model$cluster, data = opsdata, main = "average_issues_comments_length")
boxplot(average_comments_per_issue~model$cluster, data = opsdata, main = "average_comments_per_issue")
par(mfrow = c(1, 1))




# DevOps

devopsdata = devops[, c(1,5,12,16)] 

COH = c(); 
SEP = c();
SIL = c();

for (i in 2:15)  
{
  model <- kmeans(devopsdata, centers = i)
  COH[i-1] <- model$tot.withinss 
  SEP[i-1] <- model$betweenss
  model_silhouette = silhouette(model$cluster, dist(devopsdata))
  SIL[i-1] <- mean(model_silhouette[, 3])
}

par(mfrow = c(3, 1))
plot(2:15, COH, type="b", xlab="Number of Clusters", ylab="COH", main = "Cohesion") 
plot(2:15, SEP, type="b", xlab="Number of Clusters", ylab="SEP", main = "Separation")
plot(2:15, SIL, type="b", xlab="Number of Clusters", ylab="SIL", main = "Mean Silhouette")
par(mfrow = c(1, 1))

# Clustering in 5 clusters
set.seed(0);
model = kmeans(devopsdata, centers = 5) 

# Cohesion and Separation 
cohesion = model$tot.withinss 
separation = model$betweenss

# Plot Data
plot(devopsdata, col = model$cluster, pch = 15) 
points(model$centers, col = 1:length(model$centers), pch = "+", cex = 2)

# Box Plots
par(mfrow = c(2, 2))

boxplot(average_issues_comments_length~model$cluster, data = devopsdata, main = "average_issues_comments_length")
boxplot(average_comments_per_issue~model$cluster, data = devopsdata, main = "average_comments_per_issue" )
boxplot(commits_authored~model$cluster, data = devopsdata, main = "commits_authored")
boxplot(tot_additions~model$cluster, data = devopsdata, main = "tot_additions")

par(mfrow = c(1, 1))
