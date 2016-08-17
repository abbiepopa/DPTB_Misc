setwd("/Users/abbiepopa/Downloads")

scas<-read.csv("SPENCE 7-10-2015.csv",na.strings="-999.0000")
abas<-read.csv("ABAS 7-10-2015.csv", na.strings="-999.00")

library(psych)

#rowstograb<- 822 826 831

columnstograb<-c(1,3,seq(from=30, to=40, by=2))

myscas<-rbind(scas[which(scas$CABIL_ID==822),columnstograb],
	scas[which(scas$CABIL_ID==826),columnstograb],
	scas[which(scas$CABIL_ID==831),columnstograb])
	
write.csv(myscas,"mscas.csv")

coltograb<-c(1,3,11,seq(from=23, to=47, by=3),49)

myabas<-rbind(
	abas[which(abas$CABIL_ID==822),coltograb],
	abas[which(abas$CABIL_ID==826),coltograb],
	abas[which(abas$CABIL_ID==831),coltograb])
	
write.csv(myabas, "mabas.csv")

### Find additional for Table 1 ###

setwd("/Users/abbiepopa/Downloads")

scas<-read.csv("SPENCE 7-10-2015.csv",na.strings="-999.0000")
abas<-read.csv("ABAS 7-10-2015.csv", na.strings="-999.00")

library(psych)

myscas2<-rbind(
	scas[which(scas$CABIL_ID==687),],
	scas[which(scas$CABIL_ID==745),],
	scas[which(scas$CABIL_ID==759),],
	scas[which(scas$CABIL_ID==777),],
	scas[which(scas$CABIL_ID==783),],
	scas[which(scas$CABIL_ID==800),],
	scas[which(scas$CABIL_ID==822),],
	scas[which(scas$CABIL_ID==831),])
	
write.csv(myscas2, "myscas2.csv")