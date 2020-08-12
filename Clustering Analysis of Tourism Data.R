#Clustering using K-Means and Fuzzy C-Means

library(stats)
library(e1071)
library(cluster)
library(factoextra)
library(gridExtra)
setwd("D:/ITS/DATA AKADEMIK/SEMESTER 7/Statistical Machine Learning")
data=read.table("pariwisata.csv",sep=",",header=T)
X=data.frame(data[,2:3])

#ICD Rate Function
icdrate = function(Data, nc, c)
{
  n = dim(Data)[1]
  p = dim(Data)[2]
  X = Data[,1:(p-1)]
  Group = Data[,p]
  
  p = dim(X)[2]
  Mean.X = matrix(ncol = p, nrow = (nc+1))
  for (i in 1:nc)
  {
    for (j in 1:p)
    {
      Mean.X[i,j] = mean(X[which(Group==i),j])
      Mean.X[(nc+1),j] = mean(X[,j])
    }
  }
  
  SST = matrix(ncol=p, nrow=n)
  for(i in 1:n)
  {
    for(j in 1:p)
    {
      SST[i,j] = (X[i,j] - Mean.X[(nc+1),j])^2
    }
  }
  SST = sum(sum(SST))
  
  SSE = matrix(ncol=p, nrow=n)
  for (i in 1:n)
  {
    for (j in 1:p)
    {
      for (k in 1:nc)
      {
        if (Group[i]==k)
        {
          SSE[i,j] = (X[i,j] - Mean.X[k,j])^2
        }
      }
    }
  }
  SSE = sum(sum(SSE))
  
  Rsq = (SST - SSE)/SST
  pseudof = (Rsq/(c-1))/((1-Rsq)/(nc-c))
  icdrate = 1-Rsq
  list(Rsq=Rsq, pseudof=pseudof, icdrate=icdrate)
}

#Optimum Cluster using Elbow Method
fviz_nbclust(X,kmeans,method="wss")

#K-Means Clustering
tourism.k2=kmeans(X,2,iter.max=10,nstart=25)
tourism.k5=kmeans(X,5,iter.max=10,nstart=25)
k2=fviz_cluster(tourism.k2,data=X,palette="Set2",xlab="Avg Expenditure per Visit",ylab="Avg Length of Stay")
k5=fviz_cluster(tourism.k5,data=X,palette="Set2",xlab="Avg Expenditure per Visit",ylab="Avg Length of Stay")
grid.arrange(k2,k5,nrow=1)
tourism.k2$centers
tourism.k5$centers

#Fuzzy C-Means Clustering
tourism.c2=cmeans(X,2,m=2,iter.max=10)
tourism.c5=cmeans(X,5,m=3,iter.max=10)
c2=fviz_cluster(list(data=X,cluster=tourism.c2$cluster),palette="Set2",xlab="Avg Expenditure per Visit",ylab="Avg Length of Stay")
c5=fviz_cluster(list(data=X,cluster=tourism.c5$cluster),palette="Set2",xlab="Avg Expenditure per Visit",ylab="Avg Length of Stay")
grid.arrange(c2,c5,nrow=1)
tourism.c2$centers
tourism.c5$centers

#ICD Rate Results
icdk2=icdrate(cbind(X,tourism.k2$cluster),35,2)
icdk5=icdrate(cbind(X,tourism.k5$cluster),35,5)
icdc2=icdrate(cbind(X,tourism.c2$cluster),35,2)
icdc5=icdrate(cbind(X,tourism.c5$cluster),35,5)
k=c('2','5')
icd_k=c(icdk2$icdrate,icdk5$icdrate)
icd_c=c(icdc2$icdrate,icdc5$icdrate)
icd_hasil=data.frame(k,icd_k,icd_c)
icd_hasil

#Save Clustering Results
hasil_cluster=data.frame(data,
                         tourism.k2$cluster,
                         tourism.k5$cluster,
                         tourism.c2$cluster,
                         tourism.c5$cluster)
write.table(hasil_cluster,"D:/ITS/DATA AKADEMIK/SEMESTER 7/Statistical Machine Learning/hasilcluster.csv",sep=";")