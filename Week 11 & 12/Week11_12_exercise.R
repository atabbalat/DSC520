#####---- Assignment 8 ----#####

# 1. Introduction to Machine Learning

# 1. These assignments are here to provide you with an introduction to the "Data Science" use for these tools. 
# This is your future. It may seem confusing and weird right now but it hopefully seems far less so than earlier in the semester. 
# Attempt these homework assignments. You will not be graded on your answer but on your approach. 
# This should be a, "Where am I on learning this stuff" check. If you can't get it done, please explain why.

# 2. Include all of your answers in a R Markdown report. 

# 3. Regression algorithms are used to predict numeric quantity while classification algorithms predict categorical outcomes.
# A spam filter is an example use case for a classification algorithm.
# The input dataset is emails labeled as either spam (i.e. junk emails) or ham (i.e. good emails). 
# The classification algorithm uses features extracted from the emails to learn which emails fall into which category.

# 4. In this problem, you will use the nearest neighbors algorithm to fit a model on two simplified datasets. 
# The first dataset (found in binary-classifier-data.csv) contains three variables; label, x, and y.
# The label variable is either 0 or 1 and is the output we want to predict using the x and y variables
# (You worked with this dataset last week!). The second dataset (found in trinary-classifier-data.csv) 
# is similar to the first dataset except that the label variable can be 0, 1, or 2.

library(plyr)
library(dplyr)
library(ggplot2)
library(class)

binaryClassData_Raw <- read.csv('binary-classifier-data.csv')
trinaryClassData_Raw <- read.csv('trinary-classifier-data.csv')

# 5. Note that in real-world datasets, your labels are usually not numbers, but text-based descriptions of the categories
# (e.g. spam or ham). In practice, you will encode categorical variables into numeric values.

# 5-1. Plot the data from each dataset using a scatter plot.
binaryClassData_Raw %>% 
  mutate(label = as.character(label)) %>% 
  ggplot(aes(x = x, 
             y = y)) + 
  geom_point(aes(col = label)) + 
  ggtitle('Binary Classifier Data Scatterplot')

trinaryClassData_Raw %>% 
  mutate(label = as.character(label)) %>% 
  ggplot(aes(x = x, 
             y = y)) + 
  geom_point(aes(col = label)) + 
  ggtitle('Trinary Classifier Data Scatterplot')

# 5-2. The k nearest neighbors algorithm categorizes an input value by looking at the labels for the k nearest points and 
# assigning a category based on the most common label. 
# In this problem, you will determine which points are nearest by calculating the Euclidean distance between two points.
# As a refresher, the Euclidean distance between two points:
# p1 = (x1, y1) and p2 = (x2, y2)
# d = sqrt[(x1 - x2)^2  + (y1 - y2)^2]

# 6. Fitting a model is when you use the input data to create a predictive model. 
# There are various metrics you can use to determine how well your model fits the data. 
# For this problem, you will focus on a single metric, accuracy. Accuracy is simply the percentage of
# how often the model predicts the correct result. If the model always predicts the correct result, it is 100% accurate.
# If the model always predicts the incorrect result, it is 0% accurate.

# 7. Fit a k nearest neighbors model for each dataset for k=3, k=5, k=10, k=15, k=20, and k=25.

#* Binary Model Data
{
  randomNumberBinary <- sample(1:nrow(binaryClassData_Raw), .9 * nrow(binaryClassData_Raw))
  
  binaryTrain <- binaryClassData_Raw[randomNumberBinary, 2:3]
  
  binaryTest <- binaryClassData_Raw[-randomNumberBinary, 2:3]
  
  binaryTrainTarget <- binaryClassData_Raw[randomNumberBinary, 1]
  
  binaryTestTarget <- binaryClassData_Raw[-randomNumberBinary, 1]
  }

#* Binary k = 3 
{
  binary_k3 <- knn(train = binaryTrain,
                   test = binaryTest,
                   cl = binaryTrainTarget,
                   k = 3)
  
  binary_k3_results <- table(binary_k3, binaryTestTarget)
  
  binary_k3_accuracy <- sum(diag(binary_k3_results) / sum(rowSums(binary_k3_results))) 
}

#* Binary k = 5 
{
  binary_k5 <- knn(train = binaryTrain,
                   test = binaryTest,
                   cl = binaryTrainTarget,
                   k = 5)
  
  binary_k5_results <- table(binary_k5, binaryTestTarget)
  
  binary_k5_accuracy <- sum(diag(binary_k5_results) / sum(rowSums(binary_k5_results)))
}

#* Binary k = 10
{
  binary_k10 <- knn(train = binaryTrain,
                    test = binaryTest,
                    cl = binaryTrainTarget,
                    k = 10)
  
  binary_k10_results <- table(binary_k10, binaryTestTarget)
  
  binary_k10_accuracy <- sum(diag(binary_k10_results) / sum(rowSums(binary_k10_results)))
}

#* Binary k = 15
{
  binary_k15 <- knn(train = binaryTrain,
                    test = binaryTest,
                    cl = binaryTrainTarget,
                    k = 15)
  
  binary_k15_results <- table(binary_k15, binaryTestTarget)
  
  binary_k15_accuracy <- sum(diag(binary_k15_results) / sum(rowSums(binary_k15_results)))
}

#* Binary k = 20
{
  binary_k20 <- knn(train = binaryTrain,
                    test = binaryTest,
                    cl = binaryTrainTarget,
                    k = 20)
  
  binary_k20_results <- table(binary_k20, binaryTestTarget)
  
  binary_k20_accuracy <- sum(diag(binary_k20_results) / sum(rowSums(binary_k20_results)))
}

#* Binary k = 25
{
  binary_k25 <- knn(train = binaryTrain,
                    test = binaryTest,
                    cl = binaryTrainTarget,
                    k = 25)
  
  binary_k25_results <- table(binary_k25, binaryTestTarget)
  
  binary_k25_accuracy <- sum(diag(binary_k25_results) / sum(rowSums(binary_k25_results)))
}

#* Trinary Model Data
{
  randomNumberTrinary <- sample(1:nrow(trinaryClassData_Raw), .9 * nrow(trinaryClassData_Raw))
  
  trinaryTrain <- trinaryClassData_Raw[randomNumberTrinary, 2:3]
  
  trinaryTest <- trinaryClassData_Raw[-randomNumberTrinary, 2:3]
  
  trinaryTrainTarget <- trinaryClassData_Raw[randomNumberTrinary, 1]
  
  trinaryTestTarget <- trinaryClassData_Raw[-randomNumberTrinary, 1]
}

#* trinary k = 3 
{
  trinary_k3 <- knn(train = trinaryTrain,
                    test = trinaryTest,
                    cl = trinaryTrainTarget,
                    k = 3)
  
  trinary_k3_results <- table(trinary_k3, trinaryTestTarget)
  
  trinary_k3_accuracy <- sum(diag(trinary_k3_results) / sum(rowSums(trinary_k3_results)))
}

#* trinary k = 5 
{
  trinary_k5 <- knn(train = trinaryTrain,
                    test = trinaryTest,
                    cl = trinaryTrainTarget,
                    k = 5)
  
  trinary_k5_results <- table(trinary_k5, trinaryTestTarget)
  
  trinary_k5_accuracy <- sum(diag(trinary_k5_results) / sum(rowSums(trinary_k5_results)))
}

#* trinary k = 10
{
  trinary_k10 <- knn(train = trinaryTrain,
                     test = trinaryTest,
                     cl = trinaryTrainTarget,
                     k = 10)
  
  trinary_k10_results <- table(trinary_k10, trinaryTestTarget)
  
  trinary_k10_accuracy <- sum(diag(trinary_k10_results) / sum(rowSums(trinary_k10_results)))
}

#* trinary k = 15
{
  trinary_k15 <- knn(train = trinaryTrain,
                     test = trinaryTest,
                     cl = trinaryTrainTarget,
                     k = 15)
  
  trinary_k15_results <- table(trinary_k15, trinaryTestTarget)
  
  trinary_k15_accuracy <- sum(diag(trinary_k15_results) / sum(rowSums(trinary_k15_results)))
}

#* trinary k = 20
{
  trinary_k20 <- knn(train = trinaryTrain,
                     test = trinaryTest,
                     cl = trinaryTrainTarget,
                     k = 20)
  
  trinary_k20_results <- table(trinary_k20, trinaryTestTarget)
  
  trinary_k20_accuracy <- sum(diag(trinary_k20_results) / sum(rowSums(trinary_k20_results)))
}

#* trinary k = 25
{
  trinary_k25 <- knn(train = trinaryTrain,
                     test = trinaryTest,
                     cl = trinaryTrainTarget,
                     k = 25)
  
  trinary_k25_results <- table(trinary_k25, trinaryTestTarget)
  
  trinary_k25_accuracy <- sum(diag(trinary_k25_results) / sum(rowSums(trinary_k25_results)))
}


# Compute the accuracy of the resulting models for each value of k.
# Plot the results in a graph where the x-axis is the different values of k and the y-axis is the accuracy of the model.

`K =` <- c(3, 5, 10, 15, 20, 25)
dataType <- c('Binary Classifier', 'Trinary Classifier')
modelAccuracy <- c(binary_k3_accuracy, binary_k5_accuracy, binary_k10_accuracy, 
                   binary_k15_accuracy, binary_k20_accuracy, binary_k25_accuracy,
                   trinary_k3_accuracy, trinary_k5_accuracy, trinary_k10_accuracy, 
                   trinary_k15_accuracy, trinary_k20_accuracy, trinary_k25_accuracy)

modelAccuracyData <- merge(`K =`, dataType) %>% 
  cbind(modelAccuracy) %>% 
  rename(K = x, 
         `Data Type` = y,
         `Model Accuracy` = modelAccuracy) %>% 
  mutate(K = as.character(K),
         K = factor(as.factor(K), 
                    levels = c(3, 5, 10, 15, 20, 25)),
         `Model Accuracy` = round(`Model Accuracy`, 3)) 

modelAccuracyData %>% 
  ggplot(aes(x = K, 
             y = `Model Accuracy`)) + 
  geom_bar(stat = 'identity', 
           color = 'black', 
           alpha = .8, 
           fill = 'navy') + 
  scale_y_continuous(label = scales::percent) + 
  xlab('K =') +
  facet_wrap(.~`Data Type`, 
             scales = 'free') + 
  ggtitle('Binary and Trinary Classifiers - K Nearest Neighbors Model Accuracy')
# 8. Looking back at the plots of the data, do you think a linear classifier would work well on these datasets?

# 9. How does the accuracy of your logistic regression classifier from last week compare?  
# Why is the accuracy different between these two methods?



# 2. Clustering

# 1. These assignments are here to provide you with an introduction to the "Data Science" use for these tools. 
# This is your future. It may seem confusing and weird right now but it hopefully seems far less so than earlier in the semester.
# Attempt these homework assignments. You will not be graded on your answer but on your approach. 
# his should be a, "Where am I on learning this stuff" check. If you can't get it done, please explain why.

# 2. Remember to submit this assignment in an R Markdown report.

# 3. Labeled data is not always available. For these types of datasets, you can use unsupervised algorithms to extract structure.
# The k-means clustering algorithm and the k nearest neighbor algorithm both use the Euclidean distance between points to group data points.
# The difference is the k-means clustering algorithm does not use labeled data.

clusteringData_raw <- read.csv('clustering-data.csv')

# 1. Plot the dataset using a scatter plot.
clusteringData_raw %>% 
  ggplot(aes(x = x, 
             y = y)) + 
  geom_point() + 
  ggtitle('Clustering Data Scatterplot')

# 2. Fit the dataset using the k-means algorithm from k=2 to k=12.

#* Clustering Model Data
{
  clusteringModelData <- clusteringData_raw %>% 
    scale()
  }

#* Clustering k = 2 
{
  cluster_k2 <- kmeans(clusteringModelData, 2, n = 25)
  
  cluster_k2_results <- cbind(clusteringData_raw, cluster = cluster_k2$cluster)
}

#* Clustering k = 12 
{
  cluster_k12 <- kmeans(clusteringModelData, 12, n = 25)
  
  cluster_k12_results <- cbind(clusteringData_raw, cluster = cluster_k12$cluster)
}

# Create a scatter plot of the resultant clusters for each value of k.
factoextra::fviz_cluster(cluster_k2, 
                         data = clusteringData_raw,
                         geom = 'point',
                         ellipse.type = 'convex',
                         main = 'K Means Cluster, K = 2 Cluster Plot')

factoextra::fviz_cluster(cluster_k12, 
                         data = clusteringData_raw,
                         geom = 'point',
                         ellipse.type = 'convex',
                         main = 'K Means Cluster, K = 12 Cluster Plot')

# 3. As k-means is an unsupervised algorithm, you cannot compute the accuracy as there are no correct values to compare 
# the output to. Instead, you will use the average distance from the center of each cluster as a measure of how well the model fits the data.
# To calculate this metric, simply compute the distance of each data point to the center of the cluster it is assigned to and
# take the average value of all of those distances.

clusterCenter_k2 <- cluster_k2_results %>% 
  group_by(cluster) %>% 
  summarise(`X Value Cluster Center` = mean(x),
            `Y Value Cluster Center` = mean(y))

clusterCenterDistance_k2 <- cluster_k2_results %>% 
  left_join(clusterCenter_k2) %>% 
  mutate(`X Value Point Distance from Cluster Center` = x - `X Value Cluster Center`,
         `Y Value Point Distance from Cluster Center` = y - `Y Value Cluster Center`) %>% 
  group_by(cluster) %>% 
  summarise(`Average X Value Point Distance from Cluster Center` = mean(`X Value Point Distance from Cluster Center`),
            `Average Y Value Point Distance from Cluster Center` = mean(`Y Value Point Distance from Cluster Center`))


# 5. Calculate this average distance from the center of each cluster for each value of k and plot it as a line chart where
# k is the x-axis and the average distance is the y-axis.

# 6.One way of determining the "right" number of clusters is to look at the graph of k versus average distance and
# finding the "elbow point" 
# Looking at the graph you generated in the previous example, what is the elbow point for this dataset?


