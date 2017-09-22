library(ggplot2)


mydata = read.csv("wine.csv")

# do k-mean with raw data without normalization first
fit = kmeans(mydata,4)

agg = aggregate(mydata,by=list(fit$cluster),FUN=mean)

mydata = data.frame(mydata, fit$cluster)

#for (i in 1:14){
#  for (j in 1:14){
#    if(i != j){
#      print(ggplot(mydata,aes(mydata[,i],mydata[,j], color = factor(mydata[,15]))) + geom_point() + scale_color_manual(values = c("red", "black", "green","blue"))+ggtitle(j))
#    }
#  }
#}
ggplot(mydata,aes(mydata[,2],mydata[,14], color = factor(mydata[,15]))) + geom_point() + scale_color_manual(values = c("red", "black", "green","blue"))+ggtitle("k-means clustering without normalization")
