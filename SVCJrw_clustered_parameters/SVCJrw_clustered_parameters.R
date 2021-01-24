library(factoextra)

#read data
cluster_data <- read.csv("/Users/konstantin/Desktop/dateien/IRTG 1792/econCRIX/svcj/svcj/clustering_data.csv")
cluster_scaled <- scale(cluster_data[ , c(2:5)])

#elbow method to decide on optimal number of clusters
# Function to compute total within-cluster sum of square
wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
}

# plotting values for each cluster starting from 1 to 9
par(mfrow = c(1,1))
wssplot(cluster_scaled[,c("mu", "beta")], nc = 9)

#compute clusters
mu_beta <- kmeans(cluster_data[,c("mu", "beta")], centers = 4,
                  iter.max = 10,
                  nstart = 25)

#visualize clusters
fviz_cluster(mu_beta, data = cluster_data[,c("mu", "beta")], main="", 
             ggtheme = theme_minimal()) + scale_color_manual(values=c("#9E9AC8", "#1B9E77", "#FD8D3C", "#26828E"))

#add date and clusters to cluster_scaled
cluster_data$clusters_mu_beta <- factor(mu_beta$cluster)
color_mu_beta = c("#9E9AC8", "#1B9E77", "#FD8D3C", "#26828E")[cluster_data$clusters_mu_beta]

#visualize CRIX coloured by cluster
plot(cluster_data$crix ~as.Date(cluster_data$X), type="p", 
     pch=16,
     ylab="CRIX",
     col=color_mu_beta)