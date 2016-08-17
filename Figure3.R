setwd("~/Documents/Lab/DPTB/Spences I entered 150724")
SCAS<-read.csv("newSCAS_forTable1.csv", na.strings=c("-999","NA"))

mycols<-c("CABIL","CABIL_SPENCE..Diagnosis","Spence_Total_T_Score_Child",
	"Spence_Total_T_Score_Parent1","Spence_PanicAgorophobia_T_Score_Parent1",
	"Spence_SeparationAnxiety_T_Score_Parent1",
	"Spence_PhysicalInjuryFears_T_Score_Parent1",
	"Spence_SocialPhobia_T_Score_Parent1",
	"Spence_ObsessiveCompulsive_T_Score_Parent1",
	"Spence_GeneralizedAnxietyDisorder_T_Score_Parent1")
	
SCAS_abr<-SCAS[,mycols]

library(psych)

write.csv(describeBy(SCAS_abr, group=SCAS_abr$CABIL_SPENCE..Diagnosis)$`22q11.2DS`, "tryit_22q.csv")


write.csv(describeBy(SCAS_abr, group=SCAS_abr$CABIL_SPENCE..Diagnosis)$`Typical Developing`, "tryit_TD.csv")