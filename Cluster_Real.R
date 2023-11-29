### Application to real data
#We use client data and cluster data based on the PCA result of 12 last predictors
packages <- c("factoextra", "cluster", "dplyr","ggplot2", "readxl", "caret", "ROCR")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)

client_train <- read_excel("C:/Users/ASUS/Downloads/client-data.xlsx", 
                           sheet = "client_train")

client_train
client_train1 = client_train[,-c(1,2:12)]

## PCA
pca_train1 = prcomp(client_train1[,-13], scale = TRUE) #use first 2 components
comp_client = as.data.frame(pca_train1$x[,1:2])
# Take 1200 from training for example and learn the cluster pattern
set.seed(12345)
examples = comp_client$PC1 %>% 
  createDataPartition(p = 0.05, list = FALSE)
exm_comp  = comp_client[examples,]

## Clustering
# Use Hierarchical to see how the good is 2 cluster in the data
hier_client = hclust(dist(exm_comp), method = "complete")
plot(hier_client)
# Result from the example doesnt show a good indication that the data can be clustered into 2 clusters
# Lets apply this k means and try to look for optimal k
# Using wss on k means to find optimal k number from example data
fviz_nbclust(exm_comp, kmeans, method = "wss", k.max = 6) #2 seems to be a reasonable number of clusters
# Lets try different quality comparison method
fviz_nbclust(exm_comp, kmeans, method = "silhouette", k.max = 6)
gap_stat = clusGap(exm_comp, FUN = kmeans, nstart = 25,
                   K.max = 6, B = 50)
fviz_gap_stat(gap_stat)

# We conclude that from the example 2 clusters is the optimal number of cluster
# Next we apply kmeans on the complete data. We choose to set 100 random starting points and 1000 max iterations.
km_client = kmeans(comp_client, iter.max = 1000, centers = 2, nstart = 100)
fviz_cluster(km_client, comp_client, stand = F, geom = "point")

## Comparison
# Extract the result and compare to actual label
# Since we dont know which cluster resembles which label, we'll do it interchangeably
result_kmcluster = as.data.frame(cbind(client_train$default.payment.next.month, km_client$cluster%%2))
result_kmcluster2 = as.data.frame(cbind(client_train$default.payment.next.month, km_client$cluster-1))
names(result_kmcluster) = c("actual label", "cluster")
names(result_kmcluster2) = c("actual label", "cluster")
# We'll use specificity and sensitivity to measure our accuracy of clustering prediction
specificity(table(result_kmcluster))
sensitivity(table(result_kmcluster))
specificity(table(result_kmcluster2))
sensitivity(table(result_kmcluster2))
table(result_kmcluster2)
# The 2nd scheme seems like a better approach. It means that cluster 1 is people with less tendency to skip payment
# As a side note, prediction using clustering based on PCA components doesnt seems to be better model than using logistics regression on the PCA components
