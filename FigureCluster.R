#Set-Up
setwd("/Users/abbiepopa/Documents/Lab/DPTB/RT and Overall Eye Gaze/Data")
AllData<-read.csv("PartUsedInPaper_Clus.csv")

#Import Participant List
#setwd("~/Documents/Lab/DPTB")
#all<-read.csv("everyone151019 .csv")
##colnames(all)[1]<-"Subj"
#library(psych)
#library(MASS)

#mergeData<-merge(Data,all)

#rownames(mergeData)<-mergeData$Subj

setwd("/Users/abbiepopa/Documents/Lab/DPTB")
KData <- read.csv("Clustering_Database_150722.csv",header=T,na.strings="-9999")
SubjNeeded<-AllData$Subj
SubjNeeded<-as.data.frame(SubjNeeded)
colnames(SubjNeeded)<-"CABIL_ID"
KDataCorr<-merge(SubjNeeded,KData)
KDataCorrClip<-KDataCorr[1:16]
#KDataCorrClipUni<-unique(KDataCorrClip)

BigKRowNamesStep<-KDataCorrClip["CABIL_ID"]
BigKRowNames<-t(BigKRowNamesStep)
BigKRowNames<-as.vector(BigKRowNames)

rownames(KDataCorrClip)<-BigKRowNames

BigKData<-KDataCorrClip[2:16]

#BigKData[BigKData == "N/A"] <- NA

BigKDataCleaned <-na.omit(BigKData)
BigKDataCleaned <-scale(BigKDataCleaned)

###ALL the data!!###

#determine number of clusters
wss <-(nrow(BigKDataCleaned)-1)*sum(apply(BigKDataCleaned,2,var))
 for(i in 2:15) wss[i] <- sum(kmeans(BigKDataCleaned, centers=1)$withinss)
quartz()
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#do kmeans clustering
 fit <- kmeans(BigKDataCleaned, 2)
 aggregate(BigKDataCleaned,by=list(fit$cluster),FUN=mean)
BigKDataCleaned<-data.frame(BigKDataCleaned,fit$cluster)

#plot clusters
 library(cluster)
quartz()
 clusplot(BigKDataCleaned,fit$cluster,col.clus="black",shade=F,labels=4,lines=0) #labels=4 only the ellipses are labelled in the plot; #labels=2 points and ellipses are labelled in the plot

#to find what the components are use princomp
pcfitBigK <- princomp(BigKDataCleaned)
loadings(pcfitBigK)

##Find those kids!
setwd("~/Documents/Lab/DPTB")
ever<-read.csv("everyone151019 .csv")

cluslist<-data.frame(rownames(BigKDataCleaned), BigKDataCleaned$fit.cluster)
colnames(cluslist)<-c("CABIL_ID","Cluster")

find<-merge(cluslist,ever)

subset(find, find$Cluster==1 & find$Dx_Code==2)