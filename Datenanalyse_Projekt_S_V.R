#Einlesen der Datei, System Linux Ubuntu 12.04
setwd("/home/vhx/Dokumente/R-Statistik/Projekt_S_V/WIN_HSHN_S_V/")
d<-read.csv2("unfalldaten.csv",encoding="utf-8")
d
colors = c("yellow", "orange", "red", "blue", "green")



# 1.Bundesland Baden Württenberg 
# Entwicklung von 2008 bis 2012 

BW<-subset(d,d$Bundesland =="Baden-Württemberg"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt" )
BW

BWJahre<-c(BW$X2008,BW$X2009,BW$X2010,BW$X2011,BW$X2012)
summary(BWJahre)
plot(BWJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Baden-Württemberg")
lines(BWJahre,col="grey")

# 2. Bundesland Bayern
# Entwicklung von 2008 bis 2012 
BY<-subset(d,d$Bundesland =="Bayern"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BY


BYJahre<-c(BY$X2008,BY$X2009,BY$X2010,BY$X2011,BY$X2012)
summary(BYJahre)
plot(BYJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Bayern")
lines(BYJahre,col="grey")

# 3.Bundesland Berlin
# Entwicklung von 2008 bis 2012 
BER<-subset(d,d$Bundesland =="Berlin" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BER

BERJahre<-c(BER$X2008,BER$X2009,BER$X2010,BER$X2011,BER$X2012)
summary(BERJahre)
plot(BERJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Berlin")
lines(BERJahre,col="grey")

# 4. Bundesland Brandenburg
# Entwicklung von 2008 bis 2012

BRA<-subset(d,d$Bundesland =="Brandenburg" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BRA


BRAJahre<-c(BRA$X2008,BRA$X2009,BRA$X2010,BRA$X2011,BRA$X2012)
summary(BRAJahre)
plot(BRAJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Brandenburg")
lines(BRAJahre,col="grey")

# 5. Bundesland Bremen
# Entwicklung von 2008 bis 2012

BREM<-subset(d,d$Bundesland =="Bremen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BREM

BREMJahre<-c(BREM$X2008,BREM$X2009,BREM$X2010,BREM$X2011,BREM$X2012)
summary(BREMJahre)
plot(BREMJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Bremen")
lines(BREMJahre,col="grey")

# 6. Bundesland Hamburg
# Entwicklung von 2008 bis 2012

HH<-subset(d,d$Bundesland =="Hamburg" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
HH

HHJahre<-c(HH$X2008,HH$X2009,HH$X2010,HH$X2011,HH$X2012)
summary(HHJahre)
plot(HHJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Hamburg")
lines(HHJahre,col="grey")

# 7. Bundesland Hessen
# Entwicklung von 2008 bis 2012

HE<-subset(d,d$Bundesland =="Hessen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
HE

HEJahre<-c(HE$X2008,HE$X2009,HE$X2010,HE$X2011,HE$X2012)
summary(HEJahre)
plot(HEJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Hessen")
lines(HEJahre,col="grey")

# 8. Bundesland Mecklenburg-Vorpommern
# Entwicklung von 2008 bis 2012

MV<-subset(d,d$Bundesland =="Mecklenburg-Vorpommern"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
MV

MVJahre<-c(MV$X2008,MV$X2009,MV$X2010,MV$X2011,MV$X2012)
summary(MVJahre)
plot(MVJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Mecklenburg-Vorpommern")
lines(MVJahre,col="grey")

# 9. Bundesland Niedersachsen
# Entwicklung von 2008 bis 2012

NS<-subset(d,d$Bundesland =="Niedersachsen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
NS

NSJahre<-c(NS$X2008,NS$X2009,NS$X2010,NS$X2011,NS$X2012)
summary(NSJahre)
plot(NSJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Niedersachsen")
lines(NSJahre,col="grey")

# 10. Bundesland Nordrhein-Westfalen
# Entwicklung von 2008 bis 2012

NRW<-subset(d,d$Bundesland =="Nordrhein-Westfalen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
NRW

NRWJahre<-c(NRW$X2008,NRW$X2009,NRW$X2010,NRW$X2011,NRW$X2012)
summary(NRWJahre)
plot(NRWJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Nordrhein-Westfalen")
lines(NRWJahre,col="grey")

# 11. Rheinland-Pfalz
# Entwicklung von 2008 bis 2012

RP<-subset(d,d$Bundesland =="Rheinland-Pfalz"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
RP

RPJahre<-c(RP$X2008,RP$X2009,RP$X2010,RP$X2011,RP$X2012)
summary(RPJahre)
plot(RPJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Rheinland-Pfalz")
lines(RPJahre,col="grey")


# 12. Bundesland Saarland
# Entwicklung von 2008 bis 2012

SA<-subset(d,d$Bundesland =="Saarland"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SA

SAJahre<-c(SA$X2008,SA$X2009,SA$X2010,SA$X2011,SA$X2012)
summary(SAJahre)
plot(SAJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Saarland")
lines(SAJahre,col="grey")

# 13. Bundesland Sachsen
# Entwicklung von 2008 bis 2012

SAC<-subset(d,d$Bundesland =="Sachsen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SAC

SACJahre<-c(SAC$X2008,SAC$X2009,SAC$X2010,SAC$X2011,SAC$X2012)
summary(SACJahre)
plot(SACJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Sachsen")
lines(SACJahre,col="grey")

# 14. Bundesland Sachsen-Anhalt
# Entwicklung von 2008 bis 2012

SAA<-subset(d,d$Bundesland =="Sachsen-Anhalt"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SAA

SAAJahre<-c(SAA$X2008,SAA$X2009,SAA$X2010,SAA$X2011,SAA$X2012)
summary(SAAJahre)
plot(SAAJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Sachsen-Anhalt")
lines(SAAJahre,col="grey")

# 15. Bundesland Schleswig-Holstein
# Entwicklung von 2008 bis 2012

SH<-subset(d,d$Bundesland =="Schleswig-Holstein"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SH

SHJahre<-c(SH$X2008,SH$X2009,SH$X2010,SH$X2011,SH$X2012)
summary(SHJahre)
plot(SHJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Schleswig-Holstein")
lines(SHJahre,col="grey")



# 16 Bundesland Thüringen
# Entwicklung von 2008 bis 2012

TH<-subset(d,d$Bundesland =="Thüringen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
TH

THJahre<-c(TH$X2008,TH$X2009,TH$X2010,TH$X2011,TH$X2012)
summary(THJahre)
plot(THJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Thüringen")
lines(THJahre,col="grey")

#Ganz Deutschland
DE<-THJahre+SHJahre+SAAJahre+SACJahre+BWJahre+BYJahre+BERJahre+BRAJahre+BREMJahre+HHJahre+HEJahre+MVJahre+NSJahre+NRWJahre+RPJahre+SAJahre
DE



