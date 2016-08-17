#Import Participant List
setwd("~/Documents/Lab/DPTB")
all<-read.csv("everyone151019 .csv")
library(psych)
library(MASS)

#Sanity Check... should have 79 participants total -- YES!
#import Dx, 1 is TD, 2 is 22q
#sanity check... should have 47 children with 22q and 32 children who are TD -- YES!
#import age, really should have all 79
#import gender, really should have all 79, 1 is male, 2 is female

describeBy(all, group = all$Dx_Code)
t.test(all[which(all$Dx_Code==1),"Age"], all[which(all$Dx_Code==2),"Age"])
gender<-table(all$Dx_Code, all$Gender_Code)
chisq.test(gender)

#import FSIQ, this will definitely have some missing data
setwd("~/Documents/Lab/DPTB/IQ for paper")
IQ<-read.csv("all_IQs.csv")

#who has missing IQs
missedIQ<-rbind(all[,c("CABIL_ID","Dx_Code")],IQ[,c("CABIL_ID","Dx_Code")])
misstbl<-table(missedIQ)
misstbl<-data.frame(misstbl)
misstbl<-misstbl[which(misstbl$Freq==1),c("CABIL_ID","Dx_Code")]
#removing 170 because okay due to 1701
misstbl<-misstbl[c(1:4,6:8),]

#actually mean IQs
describeBy(IQ, group=IQ$Dx_Code)
t.test(IQ[which(IQ$Dx_Code==1),"WISCIV_FullScale_C"], IQ[which(IQ$Dx_Code==2),"WISCIV_FullScale_C"])

#WISCIV vs. WASI
table(IQ$Dx_Code, IQ$Which)

#import SCAS parent, this will definitely have some missing data, should have 24 + 2 + 4 + 1 TD; and 44 + 2 + 0 + 1 22q -- hmm, still need to figure out how many there should be; figure seems to indicate 27 and 44 but IDevenK anymore...
setwd("~/Documents/Lab/DPTB/Spences I entered 150724")
SCAS<-read.csv("newSCAS_forTable1.csv", na.strings=c("-999","NA"))

SCAS_abr<-SCAS[,c("CABIL","CABIL_SPENCE..Diagnosis","Spence_Total_T_Score_Child","Spence_Total_T_Score_Parent1")]

describeBy(SCAS_abr, group=SCAS_abr$CABIL_SPENCE..Diagnosis)
t.test(SCAS_abr[which(SCAS_abr$CABIL_SPENCE..Diagnosis=="22q11.2DS"),"Spence_Total_T_Score_Parent1"],
	SCAS_abr[which(SCAS_abr$CABIL_SPENCE..Diagnosis=="Typical Developing"),"Spence_Total_T_Score_Parent1"])
	
t.test(SCAS_abr[which(SCAS_abr$CABIL_SPENCE..Diagnosis=="22q11.2DS"),"Spence_Total_T_Score_Child"],
	SCAS_abr[which(SCAS_abr$CABIL_SPENCE..Diagnosis=="Typical Developing"),"Spence_Total_T_Score_Child"])
	
#Fig 3 Test
mean(SCAS$Spence_SeparationAnxiety_T_Score_Parent1)
sd(SCAS$Spence_SeparationAnxiety_T_Score_Parent1)

mean(SCAS$Spence_PhysicalInjuryFears_T_Score_Parent1)
sd(SCAS$Spence_PhysicalInjuryFears_T_Score_Parent1)

mean(SCAS$Spence_GeneralizedAnxietyDisorder_T_Score_Parent1)
sd(SCAS$Spence_GeneralizedAnxietyDisorder_T_Score_Parent1)

t.test(SCAS$Spence_SeparationAnxiety_T_Score_Parent1, SCAS$Spence_GeneralizedAnxietyDisorder_T_Score_Parent1)

t.test(SCAS$Spence_PhysicalInjuryFears_T_Score_Parent1, SCAS$Spence_GeneralizedAnxietyDisorder_T_Score_Parent1)


t.test(SCAS[which(SCAS$CABIL_SPENCE..Diagnosis == "22q11.2DS"), "Spence_SeparationAnxiety_T_Score_Parent1"], SCAS[which(SCAS$CABIL_SPENCE..Diagnosis == "22q11.2DS"), "Spence_GeneralizedAnxietyDisorder_T_Score_Parent1"])

t.test(SCAS[which(SCAS$CABIL_SPENCE..Diagnosis == "22q11.2DS"), "Spence_PhysicalInjuryFears_T_Score_Parent1"], SCAS[which(SCAS$CABIL_SPENCE..Diagnosis == "22q11.2DS"), "Spence_GeneralizedAnxietyDisorder_T_Score_Parent1"])

#import ABAS, this will definitely have some missing data, check with "who has data"
setwd("/Users/abbiepopa/Downloads")
abas<-read.csv("ABAS 7-10-2015.csv", na.strings="-999.00")

myabas<-merge(all, abas)

myabas_abr<-myabas[,c("CABIL_ID","Study_ID","ABAS_Validity","CABIL_ABAS..Diagnosis","CABIL_ABAS..AgeCurrent","STUDY..Age","STUDY..DateTest","STUDY..Study","ABAS_Date_Parent1","ABAS_GAC_Scaled_Parent1","ABAS_GAC_Composite_Parent1")]

write.csv(myabas_abr, "ABAS_Dups.csv")

abas2<-read.csv("ABAS_noDups.csv")

describeBy(abas2, group=abas2$CABIL_ABAS..Diagnosis)

t.test(abas2[which(abas2$CABIL_ABAS..Diagnosis=="Typical Developing"),"ABAS_GAC_Composite_Parent1"],abas2[which(abas2$CABIL_ABAS..Diagnosis=="22q11.2DS"),"ABAS_GAC_Composite_Parent1"])