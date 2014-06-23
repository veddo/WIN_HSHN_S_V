#Einlesen der Datei, System Linux Ubuntu 12.04
setwd("/home/vhx/Dokumente/R-Statistik/Projekt_S_V/WIN_HSHN_S_V/")
d<-read.csv2("unfalldaten.csv",encoding="utf-8")
d
colors = c("yellow", "orange", "red", "blue","green")

options(scipen=999)

# 1.Bundesland Baden Württenberg 
# Entwicklung von 2008 bis 2012 

BW<-subset(d,d$Bundesland =="Baden-Württemberg"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt" )
BW

#Unfälle unter dem Einfluss berausch. Mittel
RBW<-subset(d,d$Bundesland =="Baden-Württemberg" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RBW
#Vektoren  
RBWJ<-c(RBW$X2008,RBW$X2009,RBW$X2010,RBW$X2011,RBW$X2012)
BWJahre<-c(BW$X2008,BW$X2009,BW$X2010,BW$X2011,BW$X2012)
BWJahre
RBWJ

#Schwerwiegende Unfälle mit Sachschaden i.e.S
BWmd<-subset(d,d$Bundesland =="Baden-Württemberg" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
BWmd
BWmdD<-c(BWmd$X2008,BWmd$X2009,BWmd$X2010,BWmd$X2011,BWmd$X2012)
BWmdD

#Unfälle mit Personenschaden
BWpd<-subset(d,d$Bundesland =="Baden-Württemberg" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
BWpd
BWpdD<-c(BWpd$X2008,BWpd$X2009,BWpd$X2010,BWpd$X2011,BWpd$X2012)
BWpdD

#Übrige Sachschadensunfälle
BWrmd<-subset(d,d$Bundesland =="Baden-Württemberg" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
BWrmd
BWrmdD<-c(BWrmd$X2008,BWrmd$X2009,BWrmd$X2010,BWrmd$X2011,BWrmd$X2012)
BWrmdD


#Für gestapelte Saeulendiagramm 
t<-c(BWmdD,BWrmdD,BWpdD,RBWJ)
BadenW<-t
BadenW

BWtest<-matrix(BadenW, nrow=5,ncol=5, byrow = TRUE )
BWtest


#Entwicklung in 5 Jahren
plot(BWJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Baden-Württemberg",cex=1)
lines(BWJahre,col="grey")

#Aufteilung
bw<-barplot(BWtest,a,beside=F,col=colors,main="Baden-Württenberg")
legend(500000,160000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
BW

#----------------------------------------------------------------#

# 2. Bundesland Bayern
# Entwicklung von 2008 bis 2012 
BY<-subset(d,d$Bundesland =="Bayern"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BY

# Unfälle insgesammt Bayern
BYJahre<-c(BY$X2008,BY$X2009,BY$X2010,BY$X2011,BY$X2012)
BYJahre
#Drogeneinfluss
RBY<-subset(d,d$Bundesland =="Bayern" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RBY
#Vektoren  
RBYJ<-c(RBY$X2008,RBY$X2009,RBY$X2010,RBY$X2011,RBY$X2012)
RBYJ


#Schwerwiegende Unfälle mit Sachschaden i.e.S
BYmd<-subset(d,d$Bundesland =="Bayern" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
BYmd
BYmdD<-c(md$X2008,md$X2009,md$X2010,md$X2011,md$X2012)
BYmdD

#Unfälle mit Personenschaden
BYpd<-subset(d,d$Bundesland =="Bayern" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
BYpd
BYpdD<-c(pd$X2008,pd$X2009,pd$X2010,pd$X2011,pd$X2012)
BYpdD

#Übrige Sachschadensunfälle
BYrmd<-subset(d,d$Bundesland =="Bayern" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
BYrmd
BYrmdD<-c(BYrmd$X2008,BYrmd$X2009,BYrmd$X2010,BYrmd$X2011,BYrmd$X2012)
BYrmdD


#Für gestapelte Saeulendiagramm 
byt<-c(BYmdD,BYrmdD,BYpdD,RBYJ)
Bayern<-byt
testBY<-matrix(byt, nrow=5,ncol=5, byrow = TRUE )
testBY
RBERJ
#Entwicklung in 5 Jahren
plot(BYJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Bayern",cex=1)
lines(BYJahre,col="grey")

#Aufteilung
by<-barplot(testBY,beside=F,col=colors,main="Bayern")
legend(4.5,244000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))


#----------------------------------------------------------------#

# 3.Bundesland Berlin
# Entwicklung von 2008 bis 2012 
BER<-subset(d,d$Bundesland =="Berlin" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BER

BERJahre<-c(BER$X2008,BER$X2009,BER$X2010,BER$X2011,BER$X2012)


#Drogeneinfluss
RBER<-subset(d,d$Bundesland =="Berlin" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RBER
#Vektoren  
RBERJ<-c(RBER$X2008,RBER$X2009,RBER$X2010,RBER$X2011,RBER$X2012)
RBERJ


#Schwerwiegende Unfälle mit Sachschaden i.e.S
BERmd<-subset(d,d$Bundesland =="Berlin" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
BERmd
BERmdD<-c(BERmd$X2008,BERmd$X2009,BERmd$X2010,BERmd$X2011,BERmd$X2012)
BERmdD

#Unfälle mit Personenschaden
BERpd<-subset(d,d$Bundesland =="Berlin" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
BERpd
BERpdD<-c(BERpd$X2008,BERpd$X2009,BERpd$X2010,BERpd$X2011,BERpd$X2012)
BERpdD

#Übrige Sachschadensunfälle
BERrmd<-subset(d,d$Bundesland =="Berlin" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
BERrmd
BERrmdD<-c(BERrmd$X2008,BERrmd$X2009,BERrmd$X2010,BERrmd$X2011,BERrmd$X2012)
BERrmdD


#Für gestapelte Saeulendiagramm 
bert<-c(BERmdD,BERrmdD,BERpdD,RBERJ)
Berlin<-bert
testBER<-matrix(bert, nrow=5,ncol=5, byrow = TRUE )
testBER
#Entwicklung in 5 Jahren
plot(BERJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="BERLIN",cex=1)
lines(BERJahre,col="grey")

#Aufteilung
ber<-barplot(testBER,beside=F,col=colors,main="Berlin")
legend(4,11000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))

#-----------------------------------------------------------------------------------#

# 4. Bundesland Brandenburg
# Entwicklung von 2008 bis 2012

BRA<-subset(d,d$Bundesland =="Brandenburg" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BRA



#Unfälle unter dem Einfluss berausch. Mittel
RBRA<-subset(d,d$Bundesland =="Brandenburg" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )

#Vektoren  
#Rauschmittel
RBRAJ<-c(RBRA$X2008,RBRA$X2009,RBRA$X2010,RBRA$X2011,RBRA$X2012)
RBRAJ
#Umfälle in 5 Jahren

BRAJahre<-c(BRA$X2008,BRA$X2009,BRA$X2010,BRA$X2011,BRA$X2012)

#Schwerwiegende Unfälle mit Sachschaden i.e.S
BRAmd<-subset(d,d$Bundesland =="Brandenburg" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
BRAmd

BRAmdD<-c(BRAmd$X2008,BRAmd$X2009,BRAmd$X2010,BRAmd$X2011,BRAmd$X2012)
BRAmdD

#Unfälle mit Personenschaden
BRApd<-subset(d,d$Bundesland =="Brandenburg" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
BRApd
BRApdD<-c(BRApd$X2008,BRApd$X2009,BRApd$X2010,BRApd$X2011,BRApd$X2012)
BRApdD

#Übrige Sachschadensunfälle
BRArmd<-subset(d,d$Bundesland =="Brandenburg" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
BRArmdD<-c(BRArmd$X2008,BRArmd$X2009,BRArmd$X2010,BRArmd$X2011,BRArmd$X2012)



#Für gestapelte Saeulendiagramm 
brat<-c(BRAmdD,BRArmdD,BRApdD,RBRAJ)
Brandenburg<-brat
BRAtest<-matrix(brat, nrow=5,ncol=5, byrow = TRUE )
BRAtest
#Entwicklung in 5 Jahren
plot(BRAJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Brandenburg",cex=1)
lines(BRAJahre,col="grey",main="Brandenburg")

#Aufteilung
bra<-barplot(BRAtest,beside=F,col=colors)
legend(4.5,44000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))

#---------------------------------------------------------------------------#


# 5. Bundesland Bremen
# Entwicklung von 2008 bis 2012

BREM<-subset(d,d$Bundesland =="Bremen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BREM




#Unfälle unter dem Einfluss berausch. Mittel
RBREM<-subset(d,d$Bundesland =="Bremen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RBREM
#Vektoren  
#Rauschmittel
RBREMJ<-c(RBREM$X2008,RBREM$X2009,RBREM$X2010,RBREM$X2011,RBREM$X2012)
RBREMJ
#Umfälle in 5 Jahren
BREMJahre<-c(BREM$X2008,BREM$X2009,BREM$X2010,BREM$X2011,BREM$X2012)
BREMJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
BREMmd<-subset(d,d$Bundesland =="Bremen" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
BREMmd

BREMmdD<-c(BREMmd$X2008,BREMmd$X2009,BREMmd$X2010,BREMmd$X2011,BREMmd$X2012)
BREMmdD

#Unfälle mit Personenschaden
BREMpd<-subset(d,d$Bundesland =="Bremen" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
BREMpd
BREMpdD<-c(BREMpd$X2008,BREMpd$X2009,BREMpd$X2010,BREMpd$X2011,BREMpd$X2012)
BREMpdD

#Übrige Sachschadensunfälle
BREMrmd<-subset(d,d$Bundesland =="Bremen" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
BREMrmd
BREMrmdD<-c(BREMrmd$X2008,BREMrmd$X2009,BREMrmd$X2010,BREMrmd$X2011,BREMrmd$X2012)
BREMrmdD


#Für gestapelte Saeulendiagramm 
bremt<-c(BREMmdD,BREMrmdD,BREMpdD,RBREMJ)
Bremen<-bremt
BREMtest<-matrix(bremt, nrow=5,ncol=5, byrow = TRUE )
BREMtest
#Entwicklung in 5 Jahren
plot(BREMJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Bremen",cex=1)
lines(BREMJahre,col="grey")

#Aufteilung
brem<-barplot(BREMtest,beside=F,col=colors,main="Bremen")
legend(4.5,10000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
BREM
#---------------------------------------------------------------------#

# 6. Bundesland Hamburg
# Entwicklung von 2008 bis 2012

HH<-subset(d,d$Bundesland =="Hamburg" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
HH




#Unfälle unter dem Einfluss berausch. Mittel
RHH<-subset(d,d$Bundesland =="Hamburg" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RHH
#Vektoren  
#Rauschmittel
RHHJ<-c(RHH$X2008,RHH$X2009,RHH$X2010,RHH$X2011,RHH$X2012)
RHHJ
#Umfälle in 5 Jahren
HHJahre<-c(HH$X2008,HH$X2009,HH$X2010,HH$X2011,HH$X2012)
HHJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
HHmd<-subset(d,d$Bundesland =="Hamburg" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
HHmd

HHmdD<-c(HHmd$X2008,HHmd$X2009,HHmd$X2010,HHmd$X2011,HHmd$X2012)
HHmdD

#Unfälle mit Personenschaden
HHpd<-subset(d,d$Bundesland =="Hamburg" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
HHpd
HHpdD<-c(HHpd$X2008,HHpd$X2009,HHpd$X2010,HHpd$X2011,HHpd$X2012)
HHpdD

#Übrige Sachschadensunfälle
HHrmd<-subset(d,d$Bundesland =="Hamburg" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
HHrmd
HHrmdD<-c(HHrmd$X2008,HHrmd$X2009,HHrmd$X2010,HHrmd$X2011,HHrmd$X2012)
HHrmdD


#Für gestapelte Saeulendiagramm 
hht<-c(HHmdD,HHrmdD,HHpdD,RHHJ)
Hamburg<-hht
HHtest<-matrix(hht, nrow=5,ncol=5, byrow = TRUE )
HHtest
#Entwicklung in 5 Jahren
plot(HHJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Hamburg",cex=1)
lines(HHJahre,col="grey")

#Aufteilung
hh<-barplot(HHtest,beside=F,col=colors,main="Hamburg")
legend(4.5,25000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
HH
#-------------------------------------------------------------------------#

# 7. Bundesland Hessen
# Entwicklung von 2008 bis 2012

HE<-subset(d,d$Bundesland =="Hessen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
HE

#Unfälle unter dem Einfluss berausch. Mittel
RHE<-subset(d,d$Bundesland =="Hessen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RHE
#Vektoren  
#Rauschmittel
RHEJ<-c(RHE$X2008,RHE$X2009,RHE$X2010,RHE$X2011,RHE$X2012)
RHEJ
#Umfälle in 5 Jahren
HEJahre<-c(HE$X2008,HE$X2009,HE$X2010,HE$X2011,HE$X2012)
HEJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
HEmd<-subset(d,d$Bundesland =="Hessen" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
HEmd

HEmdD<-c(HEmd$X2008,HEmd$X2009,HEmd$X2010,HEmd$X2011,HEmd$X2012)
HEmdD

#Unfälle mit Personenschaden
HEpd<-subset(d,d$Bundesland =="Hessen" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
HEpd

HEpdD<-c(HEpd$X2008,HEpd$X2009,HEpd$X2010,HEpd$X2011,HEpd$X2012)
HEpdD

#Übrige Sachschadensunfälle
HErmd<-subset(d,d$Bundesland =="Hessen" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
HErmd

HErmdD<-c(HErmd$X2008,HErmd$X2009,HErmd$X2010,HErmd$X2011,HErmd$X2012)
HErmdD


#Für gestapelte Saeulendiagramm 
het<-c(HEmdD,HErmdD,HEpdD,RHEJ)
Hessen<-het
HEtest<-matrix(het, nrow=5,ncol=5, byrow = TRUE )
HEtest
#Entwicklung in 5 Jahren
plot(HEJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Hessen",cex=1)
lines(HEJahre,col="grey")

#Aufteilung
he<-barplot(HEtest,beside=F,col=colors, main="Hessen")
legend(4.5,55000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
HE

#-------------------------------------------------------------------------#

# 8. Bundesland Mecklenburg-Vorpommern
# Entwicklung von 2008 bis 2012

MV<-subset(d,d$Bundesland =="Mecklenburg-Vorpommern"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
MV


#Unfälle unter dem Einfluss berausch. Mittel
RMV<-subset(d,d$Bundesland =="Mecklenburg-Vorpommern" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RMV
#Vektoren  
#Rauschmittel
RMVJ<-c(RMV$X2008,RMV$X2009,RMV$X2010,RMV$X2011,RMV$X2012)
RMVJ
#Umfälle in 5 Jahren
MVJahre<-c(MV$X2008,MV$X2009,MV$X2010,MV$X2011,MV$X2012)
MVJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
MVmd<-subset(d,d$Bundesland =="Mecklenburg-Vorpommern" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
MVmd

MVmdD<-c(MVmd$X2008,MVmd$X2009,MVmd$X2010,MVmd$X2011,MVmd$X2012)
MVmdD

#Unfälle mit Personenschaden
MVpd<-subset(d,d$Bundesland =="Mecklenburg-Vorpommern" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
MVpd

MVpdD<-c(MVpd$X2008,MVpd$X2009,MVpd$X2010,MVpd$X2011,MVpd$X2012)
MVpdD

#Übrige Sachschadensunfälle
MVrmd<-subset(d,d$Bundesland =="Mecklenburg-Vorpommern" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
MVrmd

MVrmdD<-c(MVrmd$X2008,MVrmd$X2009,MVrmd$X2010,MVrmd$X2011,MVrmd$X2012)
MVrmdD


#Für gestapelte Saeulendiagramm 
mvt<-c(MVmdD,MVrmdD,MVpdD,RMVJ)
MecklenburgVorpommern<-mvt
MVtest<-matrix(mvt, nrow=5,ncol=5, byrow = TRUE )
MVtest
#Entwicklung in 5 Jahren
plot(MVJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Mecklenburg-Vorpommern",cex=1)
lines(MVJahre,col="grey")

#Aufteilung
mv<-barplot(MVtest,beside=F,col=colors,main="Mecklenburg-Vorpommern")
legend(4.5,45000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
MV
#------------------------------------------------------------------#

# 9. Bundesland Niedersachsen
# Entwicklung von 2008 bis 2012

NS<-subset(d,d$Bundesland =="Niedersachsen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
NS

#Unfälle unter dem Einfluss berausch. Mittel
RNS<-subset(d,d$Bundesland =="Niedersachsen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RNS
#Vektoren  
#Rauschmittel
RNSJ<-c(RNS$X2008,RNS$X2009,RNS$X2010,RNS$X2011,RNS$X2012)
RNSJ
#Umfälle in 5 Jahren
NSJahre<-c(NS$X2008,NS$X2009,NS$X2010,NS$X2011,NS$X2012)
NSJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
NSmd<-subset(d,d$Bundesland =="Niedersachsen" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
NSmd

NSmdD<-c(NSmd$X2008,NSmd$X2009,NSmd$X2010,NSmd$X2011,NSmd$X2012)
NSmdD

#Unfälle mit Personenschaden
NSpd<-subset(d,d$Bundesland =="Niedersachsen" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
NSpd

NSpdD<-c(NSpd$X2008,NSpd$X2009,NSpd$X2010,NSpd$X2011,NSpd$X2012)
NSpdD

#Übrige Sachschadensunfälle
NSrmd<-subset(d,d$Bundesland =="Niedersachsen" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
NSrmd

NSrmdD<-c(NSrmd$X2008,NSrmd$X2009,NSrmd$X2010,NSrmd$X2011,NSrmd$X2012)
NSrmdD


#Für gestapelte Saeulendiagramm 
nst<-c(NSmdD,NSrmdD,NSpdD,RNSJ)
Niedersachsen<-nst
NStest<-matrix(nst, nrow=5,ncol=5, byrow = TRUE )
NStest
#Entwicklung in 5 Jahren
plot(NSJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Niedersachsen",cex=1)
lines(NSJahre,col="grey")

#Aufteilung
barplot(NStest,beside=F,col=colors,main="Niedersachsen")
legend(4.5,99000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
NS
#-------------------------------------------------------------------------------#


# 10. Bundesland Nordrhein-Westfalen
# Entwicklung von 2008 bis 2012

NRW<-subset(d,d$Bundesland =="Nordrhein-Westfalen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
NRW

#Unfälle unter dem Einfluss berausch. Mittel
RNRW<-subset(d,d$Bundesland =="Nordrhein-Westfalen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RNRW
#Vektoren  
#Rauschmittel
RNRWJ<-c(RNRW$X2008,RNRW$X2009,RNRW$X2010,RNRW$X2011,RNRW$X2012)
RNRWJ
#Umfälle in 5 Jahren
NRWJahre<-c(NRW$X2008,NRW$X2009,NRW$X2010,NRW$X2011,NRW$X2012)
NRWJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
NRWmd<-subset(d,d$Bundesland =="Nordrhein-Westfalen" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
NRWmd

NRWmdD<-c(NRWmd$X2008,NRWmd$X2009,NRWmd$X2010,NRWmd$X2011,NRWmd$X2012)
NRWmdD

#Unfälle mit Personenschaden
NRWpd<-subset(d,d$Bundesland =="Nordrhein-Westfalen" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
NRWpd
Nordrhein-Westfalen;Unfälle mit Personenschaden;Insgesamt;64515;63209;58130;62055;59658
NRWpdD<-c(NRWpd$X2008,NRWpd$X2009,NRWpd$X2010,NRWpd$X2011,NRWpd$X2012)
NRWpdD

#Übrige Sachschadensunfälle
NRWrmd<-subset(d,d$Bundesland =="Nordrhein-Westfalen" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
NRWrmd

NRWrmdD<-c(NRWrmd$X2008,NRWrmd$X2009,NRWrmd$X2010,NRWrmd$X2011,NRWrmd$X2012)
NRWrmdD


#Für gestapelte Saeulendiagramm 
nrwt<-c(NRWmdD,NRWrmdD,NRWpdD,RNRWJ)
NordrheinWestfalen<-nrwt
nrwt
NRWtest<-matrix(nrwt, nrow=5,ncol=5, byrow = TRUE )
NRWtest
#Entwicklung in 5 Jahren
plot(NSJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Nordrhein-Westfalen",cex=1)
lines(NSJahre,col="grey")

#Aufteilung
nrw<-barplot(NRWtest,beside=F,col=colors,main="Nordrhein-Westfalen")
legend(4.5,400000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
NRW
#-------------------------------------------------------------------------------#

# 11. Rheinland-Pfalz
# Entwicklung von 2008 bis 2012

RP<-subset(d,d$Bundesland =="Rheinland-Pfalz"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
RP



#Unfälle unter dem Einfluss berausch. Mittel
RRP<-subset(d,d$Bundesland =="Rheinland-Pfalz" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RRP
#Vektoren  
#Rauschmittel
RRPJ<-c(RRP$X2008,RRP$X2009,RRP$X2010,RRP$X2011,RRP$X2012)
RRPJ
#Umfälle in 5 Jahren
RPJahre<-c(RP$X2008,RP$X2009,RP$X2010,RP$X2011,RP$X2012)
RPJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
RPmd<-subset(d,d$Bundesland =="Rheinland-Pfalz" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
RPmd

RPmdD<-c(RPmd$X2008,RPmd$X2009,RPmd$X2010,RPmd$X2011,RPmd$X2012)
RPmdD

#Unfälle mit Personenschaden
RPpd<-subset(d,d$Bundesland =="Rheinland-Pfalz" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
RPpd

RPpdD<-c(RPpd$X2008,RPpd$X2009,RPpd$X2010,RPpd$X2011,RPpd$X2012)
RPpdD

#Übrige Sachschadensunfälle
RPrmd<-subset(d,d$Bundesland =="Rheinland-Pfalz" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
RPrmd

RPrmdD<-c(RPrmd$X2008,RPrmd$X2009,RPrmd$X2010,RPrmd$X2011,RPrmd$X2012)
RPrmdD
#

#Für gestapelte Saeulendiagramm 
rpt<-c(RPmdD,RPrmdD,RPpdD,RRPJ)
RheinlandPfalz<-rpt
RPtest<-matrix(rpt, nrow=5,ncol=5, byrow = TRUE )
RPtest
#Entwicklung in 5 Jahren
plot(RPJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Rheinland-Pfalz",cex=1)
lines(RPJahre,col="grey")

#Aufteilung
barplot(RPtest,beside=F,col=colors,main="Rheinland-Pfalz")
legend(4.5,65000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
RP
#-------------------------------------------------------------------------------#


# 12. Bundesland Saarland
# Entwicklung von 2008 bis 2012

SA<-subset(d,d$Bundesland =="Saarland"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SA

#Unfälle unter dem Einfluss berausch. Mittel
RSA<-subset(d,d$Bundesland =="Saarland" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RSA
#Vektoren  
#Rauschmittel
RSAJ<-c(RSA$X2008,RSA$X2009,RSA$X2010,RSA$X2011,RSA$X2012)
RSAJ
#Umfälle in 5 Jahren
SAJahre<-c(SA$X2008,SA$X2009,SA$X2010,SA$X2011,SA$X2012)
SAJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
SAmd<-subset(d,d$Bundesland =="Saarland" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
SAmd

SAmdD<-c(SAmd$X2008,SAmd$X2009,SAmd$X2010,SAmd$X2011,SAmd$X2012)
SAmdD

#Unfälle mit Personenschaden
SApd<-subset(d,d$Bundesland =="Saarland" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
SApd

SApdD<-c(SApd$X2008,SApd$X2009,SApd$X2010,SApd$X2011,SApd$X2012)
SApdD

#Übrige Sachschadensunfälle
SArmd<-subset(d,d$Bundesland =="Saarland" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
SArmd

SArmdD<-c(SArmd$X2008,SArmd$X2009,SArmd$X2010,SArmd$X2011,SArmd$X2012)
SArmdD
#

#Für gestapelte Saeulendiagramm 
sat<-c(SAmdD,SArmdD,SApdD,RSAJ)
Saarland<-sat
SAtest<-matrix(sat, nrow=5,ncol=5, byrow = TRUE )
SAtest
#Entwicklung in 5 Jahren
plot(SAJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Saarland",cex=1)
lines(SAJahre,col="grey")

#Aufteilung
barplot(SAtest,beside=F,col=colors,main="Saarland")
legend(4.5,450000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
SA
#-------------------------------------------------------------------------------#


# 13. Bundesland Sachsen
# Entwicklung von 2008 bis 2012

SAC<-subset(d,d$Bundesland =="Sachsen"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SAC



#Unfälle unter dem Einfluss berausch. Mittel
RSAC<-subset(d,d$Bundesland =="Sachsen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RSAC
#Vektoren  
#Rauschmittel
RSACJ<-c(RSAC$X2008,RSAC$X2009,RSAC$X2010,RSAC$X2011,RSAC$X2012)
RSACJ
#Umfälle in 5 Jahren
SACJahre<-c(SAC$X2008,SAC$X2009,SAC$X2010,SAC$X2011,SAC$X2012)
SACJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
SACmd<-subset(d,d$Bundesland =="Sachsen" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
SACmd

SACmdD<-c(SACmd$X2008,SACmd$X2009,SACmd$X2010,SACmd$X2011,SACmd$X2012)
SACmdD

#Unfälle mit Personenschaden
SACpd<-subset(d,d$Bundesland =="Sachsen" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
SACpd

SACpdD<-c(SACpd$X2008,SACpd$X2009,SACpd$X2010,SACpd$X2011,SACpd$X2012)
SACpdD

#Übrige Sachschadensunfälle
SACrmd<-subset(d,d$Bundesland =="Sachsen" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
SACrmd

SACrmdD<-c(SACrmd$X2008,SACrmd$X2009,SACrmd$X2010,SACrmd$X2011,SACrmd$X2012)
SACrmdD
#

#Für gestapelte Saeulendiagramm 
sact<-c(SACmdD,SACrmdD,SACpdD,RSACJ)
Sachsen<-sact
SACtest<-matrix(sact, nrow=5,ncol=5, byrow = TRUE )
SAtest
#Entwicklung in 5 Jahren
plot(SACJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Sachsen",cex=1)
lines(SACJahre,col="grey")

#Aufteilung
barplot(SACtest,beside=F,col=colors,main="Sachsen")
legend(4.5,60000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
SAC
#-------------------------------------------------------------------------------#
# 14. Bundesland Sachsen-Anhalt
# Entwicklung von 2008 bis 2012

SAA<-subset(d,d$Bundesland =="Sachsen-Anhalt"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SAA

SAAJahre<-c(SAA$X2008,SAA$X2009,SAA$X2010,SAA$X2011,SAA$X2012)


#Unfälle unter dem Einfluss berausch. Mittel
RSAA<-subset(d,d$Bundesland =="Sachsen-Anhalt" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RSAA
#Vektoren  
#Rauschmittel
RSAAJ<-c(RSAA$X2008,RSAA$X2009,RSAA$X2010,RSAA$X2011,RSAA$X2012)
RSAAJ
#Umfälle in 5 Jahren
SAAJahre<-c(SAA$X2008,SAA$X2009,SAA$X2010,SAA$X2011,SAA$X2012)
SAAJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
SAAmd<-subset(d,d$Bundesland =="Sachsen-Anhalt" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
SAAmd

SAAmdD<-c(SAAmd$X2008,SAAmd$X2009,SAAmd$X2010,SAAmd$X2011,SAAmd$X2012)
SAAmdD

#Unfälle mit Personenschaden
SAApd<-subset(d,d$Bundesland =="Sachsen-Anhalt" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
SAApd

SAApdD<-c(SAApd$X2008,SAApd$X2009,SAApd$X2010,SAApd$X2011,SAApd$X2012)
SAApdD

#Übrige Sachschadensunfälle
SAArmd<-subset(d,d$Bundesland =="Sachsen-Anhalt" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
SAArmd

SAArmdD<-c(SAArmd$X2008,SAArmd$X2009,SAArmd$X2010,SAArmd$X2011,SAArmd$X2012)
SAArmdD
#

#Für gestapelte Saeulendiagramm 
saat<-c(SAAmdD,SAArmdD,SAApdD,RSAAJ)
SachsenAnhalt<-saat
SAAtest<-matrix(saat, nrow=5,ncol=5, byrow = TRUE )
SAAtest
#Entwicklung in 5 Jahren
plot(SAAJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Sachsen-Anhalt",cex=1)
lines(SAAJahre,col="grey")

#Aufteilung
barplot(SAAtest,beside=F,col=colors,main="Sachsen-Anhalt")
legend(4.5,45000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
SAA
#-------------------------------------------------------------------------------#
# 15. Bundesland Schleswig-Holstein
# Entwicklung von 2008 bis 2012

SH<-subset(d,d$Bundesland =="Schleswig-Holstein"  & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SH

#Unfälle unter dem Einfluss berausch. Mittel
RSH<-subset(d,d$Bundesland =="Schleswig-Holstein" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RSH
#Vektoren  
#Rauschmittel
RSHJ<-c(RSH$X2008,RSH$X2009,RSH$X2010,RSH$X2011,RSH$X2012)
RSHJ
#Umfälle in 5 Jahren
SHJahre<-c(SH$X2008,SH$X2009,SH$X2010,SH$X2011,SH$X2012)
SHJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
SHmd<-subset(d,d$Bundesland =="Schleswig-Holstein" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
SHmd

SHmdD<-c(SHmd$X2008,SHmd$X2009,SHmd$X2010,SHmd$X2011,SHmd$X2012)
SHmdD

#Unfälle mit Personenschaden
SHpd<-subset(d,d$Bundesland =="Schleswig-Holstein" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
SHpd

SHpdD<-c(SHpd$X2008,SHpd$X2009,SHpd$X2010,SHpd$X2011,SHpd$X2012)
SHpdD

#Übrige Sachschadensunfälle
SHrmd<-subset(d,d$Bundesland =="Schleswig-Holstein" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
SHrmd

SHrmdD<-c(SHrmd$X2008,SHrmd$X2009,SHrmd$X2010,SHrmd$X2011,SHrmd$X2012)
SHrmdD
#

#Für gestapelte Saeulendiagramm 
sht<-c(SHmdD,SHrmdD,SHpdD,RSHJ)
SchleswigHolstein<-sht
SHtest<-matrix(sht, nrow=5,ncol=5, byrow = TRUE )
SHtest
#Entwicklung in 5 Jahren
plot(SHJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Baden-Württemberg",cex=1)
lines(SHJahre,col="grey")

#Aufteilung
barplot(SHtest,beside=F,col=colors,main="Schleswig-Holstein")
legend(4.5,45000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))

SH

#-------------------------------------------------------------------------------#

# 16 Bundesland Thüringen
# Entwicklung von 2008 bis 2012

TH<-subset(d,d$Bundesland =="Thüringen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
TH

#Unfälle unter dem Einfluss berausch. Mittel
RTH<-subset(d,d$Bundesland =="Thüringen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel"  &  d$Lage =="Insgesamt" )
RTH
#Vektoren  
#Rauschmittel
RTHJ<-c(RTH$X2008,RTH$X2009,RTH$X2010,RTH$X2011,RTH$X2012)
RTHJ
#Umfälle in 5 Jahren
THJahre<-c(TH$X2008,TH$X2009,TH$X2010,TH$X2011,TH$X2012)
THJahre

#Schwerwiegende Unfälle mit Sachschaden i.e.S
THmd<-subset(d,d$Bundesland =="Thüringen" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
THmd

THmdD<-c(THmd$X2008,THmd$X2009,THmd$X2010,THmd$X2011,THmd$X2012)
THmdD

#Unfälle mit Personenschaden
THpd<-subset(d,d$Bundesland =="Thüringen" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
THpd

THpdD<-c(THpd$X2008,THpd$X2009,THpd$X2010,THpd$X2011,THpd$X2012)
THpdD

#Übrige Sachschadensunfälle
THrmd<-subset(d,d$Bundesland =="Thüringen" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
THrmd

THrmdD<-c(THrmd$X2008,THrmd$X2009,THrmd$X2010,THrmd$X2011,THrmd$X2012)
THrmdD
#

#Für gestapelte Saeulendiagramm 
tht<-c(THmdD,THrmdD,THpdD,RTHJ)
Thüringen<-tht
Thüringen
THtest<-matrix(tht, nrow=5,ncol=5, byrow = TRUE )
THtest
#Entwicklung in 5 Jahren
plot(THJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Thüringen",cex=1)
lines(THJahre,col="grey")

#Aufteilung
barplot(THtest,beside=F,col=colors,main="Thüringen")
legend(4.5,45000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
TH
#-------------------------------------------------------------------------------#

#Ganz Deutschland Statistiken
DE<-THJahre+SHJahre+SAAJahre+SACJahre+BWJahre+BYJahre+BERJahre+BRAJahre+BREMJahre+HHJahre+HEJahre+MVJahre+NSJahre+NRWJahre+RPJahre+SAJahre

#Entwicklung in der BRD gesamt
plot(DE,col=colors)
lines(DE,col="grey")

#Aufteilung der Umfallarten in der BRD gesamt
ansicht.A<-THtest+SHtest+SAtest+SACtest+SAAtest+RPtest+NRWtest+MVtest+HEtest+HHtest+BREMtest+BRAtest+testBER+testBY+BWtest
jpeg(filename="/vhx/Dokumente/R-Statistik/Projekt_S_V/Bilder/gesammt01.jpeg")
barplot(ansicht.A,beside=F,col=colors,main="Deutschland")
legend(4.5,4800000,c("MD","RMD","PD","Drug","test"), col=colors,lty=c(1,1))
jpeg(filename="/vhx/Dokumente/R-Statistik/Projekt_S_V/Bilder/gesammt01.jpeg")

