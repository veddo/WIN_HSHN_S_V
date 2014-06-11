#Einlesen der Datei, System Linux Ubuntu 12.04
setwd("/home/vhx/Dokumente/R-Statistik/Projekt_S_V/WIN_HSHN_S_V/")
d<-read.csv2("unfalldaten.csv",encoding="utf-8")
d
colors = c("yellow", "orange", "red", "blue", "green")



#Bundesland Baden Württenberg 
# Entwicklung von 2008 bis 2012 

BW<-subset(d,d$Bundesland =="Baden-Württemberg"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt" )
BW

BWJahre<-c(BW$X2008,BW$X2009,BW$X2010,BW$X2011,BW$X2012)
summary(BWJahre)
plot(BWJahre,col=colors,xlab="Jahr",pch=16)
lines(BWJahre,col="grey")

#Bayern
# Entwicklung von 2008 bis 2012 
BY<-subset(d,d$Bundesland =="Bayern"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BY


BYJahre<-c(BY$X2008,BY$X2009,BY$X2010,BY$X2011,BY$X2012)
summary(BYJahre)
plot(BYJahre,col=colors,xlab="Jahr",pch=16)
lines(BYJahre,col="grey")

#Berlin
# Entwicklung von 2008 bis 2012 
BER<-subset(d,d$Bundesland =="Berlin" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BER

BERJahre<-c(BER$X2008,BER$X2009,BER$X2010,BER$X2011,BER$X2012)
summary(BERJahre)
plot(BERJahre,col=colors,xlab="Jahr",pch=16)
lines(BERJahre,col="grey")

#Brandenburg
# Entwicklung von 2008 bis 2012

BRA<-subset(d,d$Bundesland =="Brandenburg" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BRA


BRAJahre<-c(BRA$X2008,BRA$X2009,BRA$X2010,BRA$X2011,BRA$X2012)
summary(BRAJahre)
plot(BRAJahre,col=colors,xlab="Jahr",pch=16)
lines(BRAJahre,col="grey")

# Bremen

BREM<-subset(d,d$Bundesland =="Bremen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BREM


BREMJahre<-c(BREM$X2008,BREM$X2009,BREM$X2010,BREM$X2011,BREM$X2012)
summary(BREMJahre)
plot(BREMJahre,col=colors,xlab="Jahr",pch=16)
lines(BREMJahre,col="grey")

#Hamburg

HH<-subset(d,d$Bundesland =="Hamburg" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
HH

HHJahre<-c(HH$X2008,HH$X2009,HH$X2010,HH$X2011,HH$X2012)
summary(HHJahre)
plot(HHJahre,col=colors,xlab="Jahr",pch=16)
lines(HHJahre,col="grey")

# Hessen
HE<-subset(d,d$Bundesland =="Hessen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
HE

HEJahre<-c(HE$X2008,HE$X2009,HE$X2010,HE$X2011,HE$X2012)
summary(HEJahre)
plot(HEJahre,col=colors,xlab="Jahr",pch=16)
lines(HEJahre,col="grey")

# Mecklenburg-Vorpommern


MV<-subset(d,d$Bundesland =="Mecklenburg-Vorpommern"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
MV

MVJahre<-c(MV$X2008,MV$X2009,MV$X2010,MV$X2011,MV$X2012)
summary(MVJahre)
plot(MVJahre,col=colors,xlab="Jahr",pch=16)
lines(MVJahre,col="grey")

# Niedersachsen

NS<-subset(d,d$Bundesland =="Niedersachsen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
NS

NSJahre<-c(NS$X2008,NS$X2009,NS$X2010,NS$X2011,NS$X2012)
summary(NSJahre)
plot(NSJahre,col=colors,xlab="Jahr",pch=16)
lines(NSJahre,col="grey")

# Nordrhein-Westfalen


NRW<-subset(d,d$Bundesland =="Nordrhein-Westfalen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
NRW

NRWJahre<-c(NRW$X2008,NRW$X2009,NRW$X2010,NRW$X2011,NRW$X2012)
summary(NRWJahre)
plot(NRWJahre,col=colors,xlab="Jahr",pch=16)
lines(NRWJahre,col="grey")

# Rheinland-Pfalz


RP<-subset(d,d$Bundesland =="Rheinland-Pfalz"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
RP

RPJahre<-c(RP$X2008,RP$X2009,RP$X2010,RP$X2011,RP$X2012)
summary(RPJahre)
plot(RPJahre,col=colors,xlab="Jahr",pch=16)
lines(RPJahre,col="grey")


# Saarland

SA<-subset(d,d$Bundesland =="Saarland"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SA

SAJahre<-c(SA$X2008,SA$X2009,SA$X2010,SA$X2011,SA$X2012)
summary(SAJahre)
plot(SAJahre,col=colors,xlab="Jahr",pch=16)
lines(SAJahre,col="grey")

# Sachsen

SAC<-subset(d,d$Bundesland =="Sachsen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SAC

SACJahre<-c(SAC$X2008,SAC$X2009,SAC$X2010,SAC$X2011,SAC$X2012)
summary(SACJahre)
plot(SACJahre,col=colors,xlab="Jahr",pch=16)
lines(SACJahre,col="grey")

# Sachsen-Anhalt

SAA<-subset(d,d$Bundesland =="Sachsen-Anhalt"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SAA

SAAJahre<-c(SAA$X2008,SAA$X2009,SAA$X2010,SAA$X2011,SAA$X2012)
summary(SAAJahre)
plot(SAAJahre,col=colors,xlab="Jahr",pch=16)
lines(SAAJahre,col="grey")



#Thüringen


TH<-subset(d,d$Bundesland =="Thüringen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
TH

THJahre<-c(TH$X2008,TH$X2009,TH$X2010,TH$X2011,TH$X2012)
summary(THJahre)
plot(THJahre,col=colors,xlab="Jahr",pch=16)
lines(THJahre,col="grey")





