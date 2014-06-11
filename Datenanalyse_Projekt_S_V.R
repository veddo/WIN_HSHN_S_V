#Einlesen der Datei, System Linux Ubuntu 12.04
setwd("/home/vhx/Dokumente/R-Statistik/Projekt_S_V/WIN_HSHN_S_V/")
d<-read.csv2("unfalldaten.csv",encoding="utf-8")
d
colors = c("yellow", "orange", "red", "blue", "green")



#Bundesland Baden Württenberg 
# Entwicklung von 2008 bis 2012 

BW<-subset(d,d$Bundesland =="Baden-Württemberg")

BWJ1<-sum(BW$X2008)
BWJ2<-sum(BW$X2009)
BWJ3<-sum(BW$X2010)
BWJ4<-sum(BW$X2011)
BWJ5<-sum(BW$X2012)

BWJahre<-c(BWJ1,BWJ2,BWJ3,BWJ4,BWJ5)
summary(Jahre)
plot(BWJahre,col=colors,xlab="Jahr",pch=16)
lines(BWJahre,col="grey")

#Bayern
# Entwicklung von 2008 bis 2012 
BY<-subset(d,d$Bundesland =="Bayern")
BY
BYJ1<-sum(BY$X2008)
BYJ2<-sum(BY$X2009)
BYJ3<-sum(BY$X2010)
BYJ4<-sum(BY$X2011)
BYJ5<-sum(BY$X2012)

BYJahre<-c(BYJ1,BYJ2,BYJ3,BYJ4,BYJ5)
summary(BYJahre)
plot(BYJahre,col=colors,xlab="Jahr",pch=16)
lines(BYJahre,col="grey")

#Berlin
# Entwicklung von 2008 bis 2012 
BER<-subset(d,d$Bundesland =="Berlin")
BER
BERJ1<-sum(BER$X2008)
BERJ2<-sum(BER$X2009)
BERJ3<-sum(BER$X2010)
BERJ4<-sum(BER$X2011)
BERJ5<-sum(BER$X2012)

BERJahre<-c(BERJ1,BERJ2,BERJ3,BERJ4,BERJ5)
summary(BERJahre)
plot(BERJahre,col=colors,xlab="Jahr",pch=16)
lines(BERJahre,col="grey")

#Brandenburg
# Entwicklung von 2008 bis 2012

BRA<-subset(d,d$Bundesland =="Brandenburg")
BRA
BRAJ1<-sum(BRA$X2008)
BRAJ2<-sum(BRA$X2009)
BRAJ3<-sum(BRA$X2010)
BRAJ4<-sum(BRA$X2011)
BRAJ5<-sum(BRA$X2012)

BRAJahre<-c(BRAJ1,BRAJ2,BRAJ3,BRAJ4,BRAJ5)
summary(BRAJahre)
plot(BRAJahre,col=colors,xlab="Jahr",pch=16)
lines(BRAJahre,col="grey")

# Bremen

BREM<-subset(d,d$Bundesland =="Bremen")
BREM
BREMJ1<-sum(BREM$X2008)
BREMJ2<-sum(BREM$X2009)
BREMJ3<-sum(BREM$X2010)
BREMJ4<-sum(BREM$X2011)
BREMJ5<-sum(BREM$X2012)

BREMJahre<-c(BREMJ1,BREMJ2,BREMJ3,BREMJ4,BREMJ5)
summary(BREMJahre)
plot(BREMJahre,col=colors,xlab="Jahr",pch=16)
lines(BREMJahre,col="grey")

#Hamburg

HH<-subset(d,d$Bundesland =="Hamburg")
HH
HHJ1<-sum(HH$X2008)
HHJ2<-sum(HH$X2009)
HHJ3<-sum(HH$X2010)
HHJ4<-sum(HH$X2011)
HHJ5<-sum(HH$X2012)

HHJahre<-c(HHJ1,HHJ2,HHJ3,HHJ4,HHJ5)
summary(HHJahre)
plot(HHJahre,col=colors,xlab="Jahr",pch=16)
lines(HHJahre,col="grey")




