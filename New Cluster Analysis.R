setwd("~/Documents/Lab/DPTB/Spences I entered 150724")
s<-read.csv("newSCAS_forTable1.csv")

library(psych)

describeBy(s[,c("Spence_Total_T_Score_Child", "Spence_Total_T_Score_Parent1")],group=s$CABIL_SPENCE..Diagnosis)

setwd("~/Documents/Lab/DPTB")
a<-read.csv("ABAS_noDups.csv")

a2<-read.csv("ABAS-II.csv")

a3<-merge(a, a2, by = "Study_ID", all.x = T, all.y = F)

c<-read.csv("Clustering_Database_150722.csv")

###Clustering
s2<-s[,c("CABIL", "Spence_PanicAgorophobia_T_Score_Parent1", "Spence_SeparationAnxiety_T_Score_Parent1","Spence_PhysicalInjuryFears_T_Score_Parent1","Spence_SocialPhobia_T_Score_Parent1","Spence_ObsessiveCompulsive_T_Score_Parent1","Spence_GeneralizedAnxietyDisorder_T_Score_Parent1")]

a4<-a3[,c("CABIL_ID.x","ABAS_Communication_Scaled_Parent1","ABAS_CommunityUse_Scaled_Parent1","ABAS_FunctionalAcademics_Scaled_Parent1","ABAS_HomeLiving_Scaled_Parent1","ABAS_HealthAndSafety_Scaled_Parent1","ABAS_Leisure_Scaled_Parent1","ABAS_SelfCare_Scaled_Parent1","ABAS_SelfDirection_Scaled_Parent1","ABAS_SocialSkills_Scaled_Parent1")]

as<-merge(s2, a4, by.x = "CABIL", by.y="CABIL_ID.x")

###recode -999 to NA

for(i in colnames(as)){
	as[which(as[,i] == -999), i]<-NA
}

###change Cabilid to row names and get rid of CABIL column
rownames(as)<-as$CABIL
as<-as[,c(2:16)]

###omit missing data
as<-na.omit(as)

###scale data
as<-scale(as)

# K-Means Cluster Analysis
fit <- kmeans(as, 2)
# get cluster means 
aggregate(as,by=list(fit$cluster),FUN=mean)
# append cluster assignment
as <- data.frame(as, fit$cluster)

library(cluster) 
clusplot(as, fit$cluster, color=TRUE, shade=TRUE, 
   labels=2, lines=0)

pcfit <- princomp(as)
loadings(pcfit)


setwd("~/Documents/Lab/DPTB")
ever<-read.csv("everyone151019 .csv")

as$CABIL_ID<-rownames(as)
ase<-merge(as, ever, all.x=T, all.y=F)
p<-pcfit$scores
p1<-data.frame(CABIL_ID<-dimnames(p)[[1]],Comp.1<-p[,"Comp.1"], Comp.2<-p[,"Comp.2"])
colnames(p1)<-c("CABIL_ID","C1","C2")

asep<-merge(ase, p1, all.x=T, all.y=T)

m<-merge(ever, asep, all.x=T)

write.csv(asep, "princompcluster.csv",row.names=F)

###compare total spence in a t-test###

s[which(s$Spence_Total_T_Score_Parent1 == -999), "Spence_Total_T_Score_Parent1"]<-NA
t.test(s[which(s$CABIL_SPENCE..Diagnosis == "22q11.2DS"), "Spence_Total_T_Score_Parent1"],s[which(s$CABIL_SPENCE..Diagnosis == "Typical Developing"), "Spence_Total_T_Score_Parent1"])

###and ABAS GAC
t.test(a[which(a$CABIL_ABAS..Diagnosis=="22q11.2DS"), "ABAS_GAC_Composite_Parent1"], a[which(a$CABIL_ABAS..Diagnosis=="Typical Developing"), "ABAS_GAC_Composite_Parent1"])

###make a document based on s, a, ever, and the age gender etc. doc for figuring out systematic missing data and if age and gender matter###

ever2<-read.csv("everyone160620a.csv")

es<-merge(ever2, s[,c("CABIL","Spence_Total_T_Score_Parent1")], by.x="CABIL_ID",by.y="CABIL", all.x=T)

esa<-merge(es, a[,c("CABIL_ID","ABAS_GAC_Composite_Parent1")], by="CABIL_ID", all.x=T)

###IQ###

setwd("~/Documents/Lab/DPTB/IQ for paper")
iq<-read.csv("all_IQs.csv")

setwd("~/Documents/Lab/DPTB")

esai<-merge(esa, iq[,c("CABIL_ID","WISCIV_FullScale_C")], all.x=T)
###Cluster Test###

#Dx#
chisq.test(table(esai$Dx_Code, esai$Cluster.Analysis.))

#age#
t.test(esai[which(esai$Cluster.Analysis. == "Y"), "Age"], esai[which(esai$Cluster.Analysis. == "N"), "Age"])

#gender#
chisq.test(table(esai$Gender_Code, esai$Cluster.Analysis.))

#SCAS#
t.test(esai[which(esai$Cluster.Analysis. == "Y"), "Spence_Total_T_Score_Parent1"], esai[which(esai$Cluster.Analysis. == "N"), "Spence_Total_T_Score_Parent1"])

#ABAS#
t.test(esai[which(esai$Cluster.Analysis. == "Y"), "ABAS_GAC_Composite_Parent1"], esai[which(esai$Cluster.Analysis. == "N"), "ABAS_GAC_Composite_Parent1"])

#iq#
t.test(esai[which(esai$Cluster.Analysis. == "Y"), "WISCIV_FullScale_C"], esai[which(esai$Cluster.Analysis. == "N"), "WISCIV_FullScale_C"])

###EG Test###

#Dx#
chisq.test(table(esai$Dx_Code, esai$Overall.EG))

#age#
t.test(esai[which(esai$Overall.EG == "Y"), "Age"], esai[which(esai$Overall.EG == "N"), "Age"])

#gender#
chisq.test(table(esai$Gender_Code, esai$Overall.EG))

#SCAS#
t.test(esai[which(esai$Overall.EG == "Y"), "Spence_Total_T_Score_Parent1"], esai[which(esai$Overall.EG == "N"), "Spence_Total_T_Score_Parent1"])

#ABAS#
t.test(esai[which(esai$Overall.EG == "Y"), "ABAS_GAC_Composite_Parent1"], esai[which(esai$Overall.EG == "N"), "ABAS_GAC_Composite_Parent1"])

#iq#
t.test(esai[which(esai$Overall.EG == "Y"), "WISCIV_FullScale_C"], esai[which(esai$Overall.EG == "N"), "WISCIV_FullScale_C"])