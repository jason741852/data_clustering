library(ggplot2)
library(cluster)
library(clusteval)

normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }

mydata = read.csv("wine.csv")
mydata.label = mydata[,c(1)] # wine labels
removeLabel = within(mydata, rm("Wine"))
#mydata = as.data.frame(lapply(mydata, normalize))
#mydata$Proline <- with(mydata, (Proline - min(Proline)) / (max(Proline) - min(Proline)))
#mydata$Mg <- with(mydata, (Mg - min(Mg)) / (max(Mg) - min(Mg)))
mydata_H = mydata


# Use silhouette scores to determine the best cluster number
for (n_cluster in 2:5){
  # do k-mean with raw data without normalization first
  fit = kmeans(mydata,n_cluster)
  agg = aggregate(mydata,by=list(fit$cluster),FUN=mean)
  mydata = data.frame(mydata, fit$cluster)
  sil_title = paste("silhouette score for k = ", n_cluster)
  k_title = paste("k-means clustering for k = ", n_cluster)
  print(ggplot(mydata,aes(mydata[,2],mydata[,14], color = factor(mydata[,15+n_cluster-2]))) +
  geom_point() +
  scale_color_manual(values = c("red", "black", "green","blue","purple")) +
  ggtitle(k_title))
  #fit$cluster, use "$" to access dataframe columns
  sil = silhouette(fit$cluster,dist(mydata))
  print(plot(sil, main=sil_title))
}
mydata_H.use = mydata_H[,c(2,14)]
medians = apply(mydata_H.use,2,median)
mads = apply(mydata_H.use,2,mad)
mydata_H.use = scale(mydata_H.use,center=medians,scale=mads)

mydata_H.dist = dist(mydata_H.use)
mydata_H.hclust_complete = hclust(mydata_H.dist,method="complete")
plot(mydata_H.hclust_complete, main='Complete Linkage Dendrogram')

mydata_H.hclust_average = hclust(mydata_H.dist,method="average")
plot(mydata_H.hclust_average, main='Average Linkage Dendrogram')

mydata_H.hclust_single = hclust(mydata_H.dist,method="single")
plot(mydata_H.hclust_single, main='Single Linkage Dendrogram')



mydata.two_means = kmeans(mydata,3)
avg_linkage = cutree(mydata_H.hclust_average, k=3)
print("Rand index of average linkage: ")
cluster_similarity(mydata.label,avg_linkage)
print("Rand index of k-means clustering with k=3: ")
cluster_similarity(mydata.label,mydata.two_means$cluster)


# fit = kmeans(mydata,4)
# agg = aggregate(mydata,by=list(fit$cluster),FUN=mean)
# mydata = data.frame(mydata, fit$cluster)
#
# for (i in 1:14){
#   for (j in 1:14){
#     if(i != j){
#       print(ggplot(mydata,aes(mydata[,i],mydata[,j], color = factor(mydata[,15]))) + geom_point() + scale_color_manual(values = c("red", "black", "green","blue"))+ggtitle(j))
#     }
#   }
# }

# Hierarchical Cluster Analysis
# complete_linkage = hclust(dist(mydata_H[,c(2,14)]), method = "complete", members = NULL)
# plot(complete_linkage)
#
# average_linkage = hclust(dist(mydata_H[,c(2,14)]), method = "average", members = NULL)
# plot(average_linkage)
#
# single_linkage = hclust(dist(mydata_H[,c(2,14)]), method = "single", members = NULL)
# plot(single_linkage)
