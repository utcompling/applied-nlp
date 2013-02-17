# Generate clusters for clustering 

# Set equal to 1 to generate clusters with spread out x values
unequal_x_variance = 1

# Generate clusters with equal variance in x and y dimensions.
outputName = "clusters_equal_variance"
variance_in_x = 1.0
variance_in_y = 1.0

# Generate clusters with greater variance in x dimension.
if (unequal_x_variance) {
  outputName = "clusters_bigger_x_variance"
  variance_in_x = 5.0
}

# Three centroids are supported. You can change their x/y values here.
centroid1 = c(-1,-1)
centroid2 = c(6,4)
centroid3 = c(-2,8)

outputFile = paste(outputName,"dat",sep=".")
outputPdf = paste(outputName,"pdf",sep=".")

# Three clusters in 2 dimensions
createCluster <- function(numPoints, centroid, xvariance=1.0, yvariance=1.0) {
  xmean = centroid[1]
  ymean = centroid[2]
  cbind(rnorm(numPoints,mean=xmean,xvariance),rnorm(numPoints,mean=ymean,yvariance))
}

cluster1 = cbind(1, createCluster(50,centroid1,variance_in_x,variance_in_y))
cluster2 = cbind(2, createCluster(30,centroid2,variance_in_x,variance_in_y))
cluster3 = cbind(3, createCluster(20,centroid3,variance_in_x,variance_in_y))

data = round(rbind(cluster1,cluster2,cluster3),2)

pdf(outputPdf)

plot(cluster1[,2:3],xlab="x value",ylab="y value",xlim=c(-12,12),ylim=c(-12,12))
points(cluster2[,2:3],pch=2)
points(cluster3[,2:3],pch=3)
points(centroid1[1],centroid1[2],pch=20,col="blue")
points(centroid2[1],centroid2[2],pch=20,col="blue")
points(centroid3[1],centroid3[2],pch=20,col="blue")
abline(v=0,lty=2)
abline(h=0,lty=2)

dev.off()

write.table(data, file=outputFile, quote=FALSE, col.names=FALSE)


