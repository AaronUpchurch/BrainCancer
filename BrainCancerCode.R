library(data.table)
library(dendextend)
library(grid)
library(lattice)
library(gridExtra)
library(plyr)

data <- fread("C:/Users/aaron/OneDrive/Desktop/Projects/BrainCancer/Brain_GSE50161.csv",header=TRUE)
data <- as.data.frame(data)



expressionData <- data[,-2]
row.names(expressionData) <- expressionData[,1]
expressionData <- expressionData[,-1]



data$Color <- apply(data,1,FUN=function(x) if(x[2]=="ependymoma") 'red' else if(x[2]=="glioblastoma") 'blue' else if(x[2]=="medulloblastoma") 
'green' else if(x[2] == "pilocytic_astrocytoma") "purple" else 'black')
 hc <- hclust(dist(expressionData),method="complete")
 dend <- as.dendrogram(hc)
 labels_colors(dend) <- data$Color
 
 plot(hang.dendrogram(dend),main="Hierachical Dendogram Complete",ylab="Clusters")
# 
# hc <- hclust(dist(expressionData),method="single")
# dend <- as.dendrogram(hc)
# labels_colors(dend) <- data$Color
# 
# plot(hang.dendrogram(dend),main="Hierachical Dendogram Single",ylab="Clusters")
# 
# 
# hc <- hclust(dist(expressionData),method="average")
# dend <- as.dendrogram(hc)
# labels_colors(dend) <- data$Color
# 
# plot(hang.dendrogram(dend),main="Hierachical Dendogram Average",ylab="Clusters")
# 
# hc <- hclust(dist(expressionData),method="ward.D")
# dend <- as.dendrogram(hc)
# labels_colors(dend) <- data$Color
# 

# hc <- hclust(dist(expressionData),method="ward.D2")
# dend <- as.dendrogram(hc)
# labels_colors(dend) <- data$Color
# # 
# plot(hang.dendrogram(dend),main="Hierachical Dendogram Ward.D.2",ylab="Clusters")
# # 
# hc <- hclust(dist(expressionData),method="mcquitty")
# dend <- as.dendrogram(hc)
# labels_colors(dend) <- data$Color
# 
# plot(hang.dendrogram(dend),main="Hierachical Dendogram McQuitty",ylab="Clusters")
# 
# hc <- hclust(dist(expressionData),method="median")
# dend <- as.dendrogram(hc)
# labels_colors(dend) <- data$Color
# 
# plot(hang.dendrogram(dend),main="Hierachical Dendogram Median",ylab="Clusters")
# 
# hc <- hclust(dist(expressionData),method="centroid")
# dend <- as.dendrogram(hc)
# labels_colors(dend) <- data$Color
# 
# plot(hang.dendrogram(dend),main="Hierachical Dendogram Centroid",ylab="Clusters")

expressionData <- na.omit(expressionData)
k <- kmeans(expressionData,centers=10,iter.max=100,nstart=5)
data$Clusters <- as.data.frame(k[1])
resultsTable <- as.data.frame(data[,c("samples","type","Clusters")])
resultsTable <- resultsTable[order(resultsTable$Clusters),]



nums <- count(as.numeric(unlist(as.list(subset(resultsTable,type=="ependymoma")$Clusters))))
pie(nums$freq,main="Ependymoma",labels =nums$x)

nums <- count(as.numeric(unlist(as.list(subset(resultsTable,type=="normal")$Clusters))))
pie(nums$freq,main="Normal",labels =nums$x)

nums <- count(as.numeric(unlist(as.list(subset(resultsTable,type=="medulloblastoma")$Clusters))))
pie(nums$freq,main="medulloblastoma",labels =nums$x)

nums <- count(as.numeric(unlist(as.list(subset(resultsTable,type=="pilocytic_astrocytoma")$Clusters))))
pie(nums$freq,main="pilocytic_astrocytoma",labels =nums$x)

nums <- count(as.numeric(unlist(as.list(subset(resultsTable,type=="glioblastoma")$Clusters))))
pie(nums$freq,main="glioblastoma",labels =nums$x)





print(resultsTable)




nums <- count(unlist(as.list(subset(resultsTable,Clusters=="1")$type)))
pie(nums$freq,main="1",labels =nums$x)

nums <- count(unlist(as.list(subset(resultsTable,Clusters=="2")$type)))
pie(nums$freq,main="2",labels =nums$x)

nums <- count(unlist(as.list(subset(resultsTable,Clusters=="3")$type)))
pie(nums$freq,main="3",labels =nums$x)

nums <- count(unlist(as.list(subset(resultsTable,Clusters=="4")$type)))
pie(nums$freq,main="4",labels =nums$x)

nums <- count(unlist(as.list(subset(resultsTable,Clusters=="5")$type)))
pie(nums$freq,main="5",labels =nums$x)

nums <- count(unlist(as.list(subset(resultsTable,Clusters=="6")$type)))
pie(nums$freq,main="6",labels =nums$x)

nums <- count(unlist(as.list(subset(resultsTable,Clusters=="7")$type)))
pie(nums$freq,main="7",labels =nums$x)

nums <- count(unlist(as.list(subset(resultsTable,Clusters=="8")$type)))
pie(nums$freq,main="8",labels =nums$x)

nums <- count(unlist(as.list(subset(resultsTable,Clusters=="9")$type)))
pie(nums$freq,main="9",labels =nums$x)

nums <- count(unlist(as.list(subset(resultsTable,Clusters=="10")$type)))
pie(nums$freq,main="10",labels =nums$x)





print("Program Complete")