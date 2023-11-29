packages <- c("factoextra", "cluster", "dplyr","ggplot2")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)

## Lets use dummy data to illustrate how it works first
set.seed(1010)
my_dummy = as.data.frame(cbind(c(rnorm(10, 5, 2)), c(rbeta(10, 2,4))))
names(my_dummy) = c("x", "y")
my_dummy %>% ggplot(aes(x,y)) + geom_point() + labs(x='X', y="Y") 

## Dissimilarity matrix
dist(my_dummy)

## Hierarchical clustering
hier = hclust(dist(my_dummy), method = "complete")
plot(hier)
# Decide by dendogram we have 3 clusters
clusterCut = cutree(hier, 3)
my_dummy$cluster = clusterCut
my_dummy %>% ggplot(aes(x,y), color = cluster) + geom_point(alpha = 1.8, size = 2.5 ,col = clusterCut) + 
  scale_color_manual(values = c('black', 'red', 'green')) + labs(x='X', y="Y")

## K Means clustering
#Lets cluster the dummy into 3 clusters, and choose the initiation for the centers
cent = as.data.frame(cbind(c(5,8.75, 1), c(0.2, 0.1, 0.4)))
names(cent) = c("x", "y")
cent$cluster = c(1, 2, 3)
cent %>% ggplot(aes(x,y)) + 
  geom_point(alpha = 1, size = 2, colour = c(2,3,4), shape = 2, stroke = 2) + 
  scale_color_manual(values = c('red', 'green', 'blue')) + labs(x='X', y="Y") 
#Plot the data (circles) and the initial centers (triangles)
my_dummy = my_dummy[,1:2]
my_dummy$cluster = c(1,1,1,1,1,1,1,1,1,1)
km_dummy = as.data.frame(rbind(cent, my_dummy))
km_dummy %>% ggplot(aes(x,y)) + 
  geom_point(alpha = 1, size = 2, shape = c(2,2,2,1,1,1,1,1,1,1,1,1,1), colour = c(2,3,4,1,1,1,1,1,1,1,1,1,1), stroke = c(2,2,2,1,1,1,1,1,1,1,1,1,1)) + 
  scale_color_manual(values = c('black', 'red', 'green', 'blue')) + labs(x='X', y="Y")
dist(km_dummy)
#Place the each observation to their respective nearest cluster
km_cl1 = c(1,2,3,1,1,2,1,1,2,2,3,1,1)
km_dummy$cluster1 = km_cl1
#Compute the new centers for each cluster
km_dummy[4:13,] %>% group_by(cluster1) %>%
  summarise(x = mean(as.numeric(x)),
            y = mean(y))
km_dummy[1:3,1] = c(4.41,8.1,0.278)
km_dummy[1:3,2] = c(0.267, 0.150, .381)
km_dummy %>% ggplot(aes(x,y)) + 
  geom_point(alpha = 1, size = 2, shape = c(2,2,2,1,1,1,1,1,1,1,1,1,1), colour = km_dummy$cluster1+1, stroke = c(1,1,1,1,1,1,1,1,1,1,1,1,1)) + 
  scale_color_manual(values = c('black', 'red', 'green', 'blue')) + labs(x='X', y="Y")
#Use the function kmeans
kmean_1st = kmeans(my_dummy, centers = cent, iter.max = 100)
kmean_1st$cluster #it is found that after 1st step the algorithm already reached convergence
#Optimal number of clusters based on several methods
my_dummy = my_dummy[,1:2]
fviz_nbclust(my_dummy, kmeans, method = "wss", k.max = 5)

## NOTE : The previous results may differs since we are doing illustration based on randomized dummy data