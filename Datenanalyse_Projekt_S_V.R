#Einlesen der Datei, System Linux Ubuntu 12.04
setwd("/home/vhx/Dokumente/R-Statistik/Projekt_S_V/WIN_HSHN_S_V/")
d<-read.csv2("unfalldaten.csv",encoding="utf-8")
d
colors = c("yellow", "orange", "red", "blue","green")

options(scipen=999)

#Einlesen der Datei, System Windows 7
# Working-Directory setzen
setwd("C:/R_Dateien")
# Einlesen des Datensatzes
d <- read.csv2("unfalldaten.csv", header = T, sep = ";")
colors = c("yellow", "orange", "red", "blue","green")

# Zusammenfassung des Datensatzes anzeigen
summary(d)

# 1.Bundesland Baden Württenberg----------------------------------------------------------------------------------
# Entwicklung von 2008 bis 2012

# Unfälle insgesamt
BW<-subset(d,d$Bundesland =="Baden-Württemberg" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt" )
BW
#Vektor
BWJahre<-c(BW$X2008,BW$X2009,BW$X2010,BW$X2011,BW$X2012)
BWJahre

#Unfälle unter dem Einfluss berausch. Mittel
RBW<-subset(d,d$Bundesland =="Baden-Württemberg" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
RBW
#Vektor
RBWJ<-c(RBW$X2008,RBW$X2009,RBW$X2010,RBW$X2011,RBW$X2012)
RBWJ


#Schwerwiegende Unfälle mit Sachschaden i.e.S
BWmd<-subset(d,d$Bundesland =="Baden-Württemberg" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
BWmd
#Vektor
BWmdD<-c(BWmd$X2008,BWmd$X2009,BWmd$X2010,BWmd$X2011,BWmd$X2012)
BWmdD

#Unfälle mit Personenschaden
BWpd<-subset(d,d$Bundesland =="Baden-Württemberg" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
BWpd
#Vektor
BWpdD<-c(BWpd$X2008,BWpd$X2009,BWpd$X2010,BWpd$X2011,BWpd$X2012)
BWpdD

#Übrige Sachschadensunfälle
BWrmd<-subset(d,d$Bundesland =="Baden-Württemberg" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
BWrmd
#Vektor
BWrmdD<-c(BWrmd$X2008,BWrmd$X2009,BWrmd$X2010,BWrmd$X2011,BWrmd$X2012)
BWrmdD

#Unfälle innerorts
BWinnerorts <- subset(d, d$Bundesland == "Baden-Württemberg" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
BWinnerorts
#Vektor
BWinnerortsV <- c(BWinnerorts$X2008, BWinnerorts$X2009, BWinnerorts$X2010, BWinnerorts$X2011, BWinnerorts$X2012)
BWinnerortsV

#Unfälle außerorts
BWaußerorts <- subset(d, d$Bundesland == "Baden-Württemberg" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
BWaußerorts
#Vektor
BWaußerortsV <- c(BWaußerorts$X2008, BWaußerorts$X2009, BWaußerorts$X2010, BWaußerorts$X2011, BWaußerorts$X2012)
BWaußerortsV

#Unfälle auf Autobahnen
BWautobahn <- subset(d, d$Bundesland == "Baden-Württemberg" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
BWautobahn
#Vektor
BWautobahnV <- c(BWautobahn$X2008, BWautobahn$X2009, BWautobahn$X2010, BWautobahn$X2011, BWautobahn$X2012)
BWautobahnV

#Für gestapelte Saeulendiagramm Unfallkategorie
t<-c(BWmdD,BWrmdD,BWpdD,RBWJ)
BadenW<-t
BadenW

BWtest<-matrix(BadenW, nrow=4,ncol=5, byrow = TRUE )
BWtest

#Für gestapelte Saeulendiagramm Unfalllage
BWUnfalllage <- c(BWinnerortsV,BWaußerortsV,BWautobahnV)
BWUnfalllage

BWUnfalllageMatrix <- matrix(BWUnfalllage, nrow=3, ncol=5, byrow = TRUE )
BWUnfalllageMatrix


#Entwicklung in 5 Jahren
plot(BWJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Baden-Württemberg Gesamt",cex=1)
lines(BWJahre,col="black")
#jpeg(filename="01BWGesamt.jpeg")



# Entwicklung in 5 Jahren nach Unfallkategorie
#Entwicklung in 5 Jahren
par(mfrow=c(1,1))
plot(BWrmdD, col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Übrige",cex=1)
lines(BWrmdD,col="black")

# Entwicklung in 5 Jahren nach Unfalllage
plot(BWautobahnV, col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Autobahn",cex=1)
lines(BWautobahnV, col="black")

#Aufteilung Unfallkategorie
bw<-barplot(BWtest,beside=F,col=colors,main="Baden-Württenberg")
legend(2,100000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
BW

#Aufteilung Unfalllage
BWUnfalllageDarstellung <- barplot(BWUnfalllageMatrix,beside=F,col=colors,main="Baden-Württenberg")
#legend(2,100000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
BWUnfalllageDarstellung


#----------------------------------------------------------------#

# 2. Bundesland Bayern------------------------------------------------------------------------------------------
# Entwicklung von 2008 bis 2012
BY<-subset(d,d$Bundesland =="Bayern" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BY

# Unfälle insgesammt Bayern
BYJahre<-c(BY$X2008,BY$X2009,BY$X2010,BY$X2011,BY$X2012)
BYJahre

#Drogeneinfluss
RBY<-subset(d,d$Bundesland =="Bayern" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
RBY
#Vektoren
RBYJ<-c(RBY$X2008,RBY$X2009,RBY$X2010,RBY$X2011,RBY$X2012)
RBYJ


#Schwerwiegende Unfälle mit Sachschaden i.e.S
BYmd<-subset(d,d$Bundesland =="Bayern" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
BYmd
BYmdD<-c(BYmd$X2008,BYmd$X2009,BYmd$X2010,BYmd$X2011,BYmd$X2012)
BYmdD

#Unfälle mit Personenschaden
BYpd<-subset(d,d$Bundesland =="Bayern" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
BYpd
BYpdD<-c(BYpd$X2008,BYpd$X2009,BYpd$X2010,BYpd$X2011,BYpd$X2012)
BYpdD

#Übrige Sachschadensunfälle
BYrmd<-subset(d,d$Bundesland =="Bayern" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
BYrmd
BYrmdD<-c(BYrmd$X2008,BYrmd$X2009,BYrmd$X2010,BYrmd$X2011,BYrmd$X2012)
BYrmdD

#Unfälle innerorts
BYinnerorts <- subset(d, d$Bundesland == "Bayern" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
BYinnerorts
#Vektor
BYinnerortsV <- c(BYinnerorts$X2008, BYinnerorts$X2009, BYinnerorts$X2010, BYinnerorts$X2011, BYinnerorts$X2012)
BYinnerortsV

#Unfälle außerorts
BYaußerorts <- subset(d, d$Bundesland == "Bayern" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
BYaußerorts
#Vektor
BYaußerortsV <- c(BYaußerorts$X2008, BYaußerorts$X2009, BYaußerorts$X2010, BYaußerorts$X2011, BYaußerorts$X2012)
BYaußerortsV

#Unfälle auf Autobahnen
BYautobahn <- subset(d, d$Bundesland == "Bayern" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
BYautobahn
#Vektor
BYautobahnV <- c(BYautobahn$X2008, BYautobahn$X2009, BYautobahn$X2010, BYautobahn$X2011, BYautobahn$X2012)
BYautobahnV


#Für gestapelte Saeulendiagramm
byt<-c(BYmdD,BYrmdD,BYpdD,RBYJ)
Bayern<-byt

testBY<-matrix(byt, nrow=4,ncol=5, byrow = TRUE )
testBY

#Entwicklung in 5 Jahren
plot(BYJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Bayern",cex=1)
lines(BYJahre,col="grey")

#Aufteilung
by<-barplot(testBY,beside=F,col=colors,main="Bayern")
legend(4.5,244000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))


#----------------------------------------------------------------#

# 3.Bundesland Berlin-------------------------------------------------------------------------------------------
# Entwicklung von 2008 bis 2012
BER<-subset(d,d$Bundesland =="Berlin" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BER

BERJahre<-c(BER$X2008,BER$X2009,BER$X2010,BER$X2011,BER$X2012)
BERJahre

#Drogeneinfluss
RBER<-subset(d,d$Bundesland =="Berlin" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
RBER
#Vektoren
RBERJ<-c(RBER$X2008,RBER$X2009,RBER$X2010,RBER$X2011,RBER$X2012)
RBERJ


#Schwerwiegende Unfälle mit Sachschaden i.e.S
BERmd<-subset(d,d$Bundesland =="Berlin" & d$Unfälle =="Schwerwiegende Unfälle mit Sachschaden i.e.S" & d$Lage =="Insgesamt")
BERmd
#Vektor
BERmdD<-c(BERmd$X2008,BERmd$X2009,BERmd$X2010,BERmd$X2011,BERmd$X2012)
BERmdD

#Unfälle mit Personenschaden
BERpd<-subset(d,d$Bundesland =="Berlin" & d$Unfälle =="Unfälle mit Personenschaden" & d$Lage =="Insgesamt")
BERpd
#Vektor
BERpdD<-c(BERpd$X2008,BERpd$X2009,BERpd$X2010,BERpd$X2011,BERpd$X2012)
BERpdD

#Übrige Sachschadensunfälle
BERrmd<-subset(d,d$Bundesland =="Berlin" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
BERrmd
#Vektor
BERrmdD<-c(BERrmd$X2008,BERrmd$X2009,BERrmd$X2010,BERrmd$X2011,BERrmd$X2012)
BERrmdD

#Unfälle innerorts
BERinnerorts <- subset(d, d$Bundesland == "Berlin" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
BERinnerorts
#Vektor
BERinnerortsV <- c(BERinnerorts$X2008, BERinnerorts$X2009, BERinnerorts$X2010, BERinnerorts$X2011, BERinnerorts$X2012)
BERinnerortsV

#Unfälle außerorts
BERaußerorts <- subset(d, d$Bundesland == "Berlin" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
BERaußerorts
#Vektor
BERaußerortsV <- c(BERaußerorts$X2008, BERaußerorts$X2009, BERaußerorts$X2010, BERaußerorts$X2011, BERaußerorts$X2012)
BERaußerortsV

#Unfälle auf Autobahnen
BERautobahn <- subset(d, d$Bundesland == "Berlin" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
BERautobahn
#Vektor
BERautobahnV <- c(BERautobahn$X2008, BERautobahn$X2009, BERautobahn$X2010, BERautobahn$X2011, BERautobahn$X2012)
BERautobahnV

#Für gestapelte Saeulendiagramm
bert<-c(BERmdD,BERrmdD,BERpdD,RBERJ)
Berlin<-bert
testBER<-matrix(bert, nrow=4,ncol=5, byrow = TRUE )
testBER

#Entwicklung in 5 Jahren
plot(BERJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="BERLIN",cex=1)
lines(BERJahre,col="grey")

#Aufteilung
ber<-barplot(testBER,beside=F,col=colors,main="Berlin")
legend(4,11000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))

#-----------------------------------------------------------------------------------#

# 4. Bundesland Brandenburg------------------------------------------------------------------------------------
# Entwicklung von 2008 bis 2012

BRA<-subset(d,d$Bundesland =="Brandenburg" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BRA

BRAJahre<-c(BRA$X2008,BRA$X2009,BRA$X2010,BRA$X2011,BRA$X2012)
BRAJahre

#Unfälle unter dem Einfluss berausch. Mittel
RBRA<-subset(d,d$Bundesland =="Brandenburg" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
RBRA
#Vektoren
RBRAJ<-c(RBRA$X2008,RBRA$X2009,RBRA$X2010,RBRA$X2011,RBRA$X2012)
RBRAJ

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
BRArmd
BRArmdD<-c(BRArmd$X2008,BRArmd$X2009,BRArmd$X2010,BRArmd$X2011,BRArmd$X2012)
BRArmdD

#Unfälle innerorts
BRAinnerorts <- subset(d, d$Bundesland == "Brandenburg" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
BRAinnerorts
#Vektor
BRAinnerortsV <- c(BRAinnerorts$X2008, BRAinnerorts$X2009, BRAinnerorts$X2010, BRAinnerorts$X2011, BRAinnerorts$X2012)
BRAinnerortsV

#Unfälle außerorts
BRAaußerorts <- subset(d, d$Bundesland == "Brandenburg" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
BRAaußerorts
#Vektor
BRAaußerortsV <- c(BRAaußerorts$X2008, BRAaußerorts$X2009, BRAaußerorts$X2010, BRAaußerorts$X2011, BRAaußerorts$X2012)
BRAaußerortsV

#Unfälle auf Autobahnen
BRAautobahn <- subset(d, d$Bundesland == "Brandenburg" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
BRAautobahn
#Vektor
BRAautobahnV <- c(BRAautobahn$X2008, BRAautobahn$X2009, BRAautobahn$X2010, BRAautobahn$X2011, BRAautobahn$X2012)
BRAautobahnV

#Für gestapelte Saeulendiagramm
brat<-c(BRAmdD,BRArmdD,BRApdD,RBRAJ)
Brandenburg<-brat
BRAtest<-matrix(brat, nrow=4,ncol=5, byrow = TRUE )
BRAtest
#Entwicklung in 5 Jahren
plot(BRAJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Brandenburg",cex=1)
lines(BRAJahre,col="grey",main="Brandenburg")

#Aufteilung
bra<-barplot(BRAtest,beside=F,col=colors)
legend(4.5,44000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))

#---------------------------------------------------------------------------#


# 5. Bundesland Bremen----------------------------------------------------------------------------------------
# Entwicklung von 2008 bis 2012

BREM<-subset(d,d$Bundesland =="Bremen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
BREM




#Unfälle unter dem Einfluss berausch. Mittel
RBREM<-subset(d,d$Bundesland =="Bremen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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

#Unfälle innerorts
BREMinnerorts <- subset(d, d$Bundesland == "Bremen" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
BREMinnerorts
#Vektor
BREMinnerortsV <- c(BREMinnerorts$X2008, BREMinnerorts$X2009, BREMinnerorts$X2010, BREMinnerorts$X2011, BREMinnerorts$X2012)
BREMinnerortsV

#Unfälle außerorts
BREMaußerorts <- subset(d, d$Bundesland == "Bremen" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
BREMaußerorts
#Vektor
BREMaußerortsV <- c(BREMaußerorts$X2008, BREMaußerorts$X2009, BREMaußerorts$X2010, BREMaußerorts$X2011, BREMaußerorts$X2012)
BREMaußerortsV

#Unfälle auf Autobahnen
BREMautobahn <- subset(d, d$Bundesland == "Bremen" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
BREMautobahn
#Vektor
BREMautobahnV <- c(BREMautobahn$X2008, BREMautobahn$X2009, BREMautobahn$X2010, BREMautobahn$X2011, BREMautobahn$X2012)
BREMautobahnV

#Für gestapelte Saeulendiagramm
bremt<-c(BREMmdD,BREMrmdD,BREMpdD,RBREMJ)
Bremen<-bremt
BREMtest<-matrix(bremt, nrow=4,ncol=5, byrow = TRUE )
BREMtest
#Entwicklung in 5 Jahren
plot(BREMJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Bremen",cex=1)
lines(BREMJahre,col="grey")

#Aufteilung
brem<-barplot(BREMtest,beside=F,col=colors,main="Bremen")
legend(4.5,10000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
BREM
#---------------------------------------------------------------------#

# 6. Bundesland Hamburg--------------------------------------------------------------------------------------
# Entwicklung von 2008 bis 2012

HH<-subset(d,d$Bundesland =="Hamburg" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
HH




#Unfälle unter dem Einfluss berausch. Mittel
RHH<-subset(d,d$Bundesland =="Hamburg" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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

#Unfälle innerorts
HHinnerorts <- subset(d, d$Bundesland == "Hamburg" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
HHinnerorts
#Vektor
HHinnerortsV <- c(HHinnerorts$X2008, HHinnerorts$X2009, HHinnerorts$X2010, HHinnerorts$X2011, HHinnerorts$X2012)
HHinnerortsV

#Unfälle außerorts
HHaußerorts <- subset(d, d$Bundesland == "Hamburg" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
HHaußerorts
#Vektor
HHaußerortsV <- c(HHaußerorts$X2008, HHaußerorts$X2009, HHaußerorts$X2010, HHaußerorts$X2011, HHaußerorts$X2012)
HHaußerortsV

#Unfälle auf Autobahnen
HHautobahn <- subset(d, d$Bundesland == "Hamburg" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
HHautobahn
#Vektor
HHautobahnV <- c(HHautobahn$X2008, HHautobahn$X2009, HHautobahn$X2010, HHautobahn$X2011, HHautobahn$X2012)
HHautobahnV

#Für gestapelte Saeulendiagramm
hht<-c(HHmdD,HHrmdD,HHpdD,RHHJ)
Hamburg<-hht
HHtest<-matrix(hht, nrow=4,ncol=5, byrow = TRUE )
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
# Entwicklung von 2008 bis 2012-------------------------------------------------------------------------------

HE<-subset(d,d$Bundesland =="Hessen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
HE

#Unfälle unter dem Einfluss berausch. Mittel
RHE<-subset(d,d$Bundesland =="Hessen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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

#Unfälle innerorts
HEinnerorts <- subset(d, d$Bundesland == "Hessen" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
HEinnerorts
#Vektor
HEinnerortsV <- c(HEinnerorts$X2008, HEinnerorts$X2009, HEinnerorts$X2010, HEinnerorts$X2011, HEinnerorts$X2012)
HEinnerortsV

#Unfälle außerorts
HEaußerorts <- subset(d, d$Bundesland == "Hessen" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
HEaußerorts
#Vektor
HEaußerortsV <- c(HEaußerorts$X2008, HEaußerorts$X2009, HEaußerorts$X2010, HEaußerorts$X2011, HEaußerorts$X2012)
HEaußerortsV

#Unfälle auf Autobahnen
HEautobahn <- subset(d, d$Bundesland == "Hessen" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
HEautobahn
#Vektor
HEautobahnV <- c(HEautobahn$X2008, HEautobahn$X2009, HEautobahn$X2010, HEautobahn$X2011, HEautobahn$X2012)
HEautobahnV

#Für gestapelte Saeulendiagramm
het<-c(HEmdD,HErmdD,HEpdD,RHEJ)
Hessen<-het
HEtest<-matrix(het, nrow=4,ncol=5, byrow = TRUE )
HEtest
#Entwicklung in 5 Jahren
plot(HEJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Hessen",cex=1)
lines(HEJahre,col="grey")

#Aufteilung
he<-barplot(HEtest,beside=F,col=colors, main="Hessen")
legend(4.5,55000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
HE

#-------------------------------------------------------------------------#

# 8. Bundesland Mecklenburg-Vorpommern-----------------------------------------------------------------------
# Entwicklung von 2008 bis 2012

MV<-subset(d,d$Bundesland =="Mecklenburg-Vorpommern" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
MV


#Unfälle unter dem Einfluss berausch. Mittel
RMV<-subset(d,d$Bundesland =="Mecklenburg-Vorpommern" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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

#Unfälle innerorts
MVinnerorts <- subset(d, d$Bundesland == "Mecklenburg-Vorpommern" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
MVinnerorts
#Vektor
MVinnerortsV <- c(MVinnerorts$X2008, MVinnerorts$X2009, MVinnerorts$X2010, MVinnerorts$X2011, MVinnerorts$X2012)
MVinnerortsV

#Unfälle außerorts
MVaußerorts <- subset(d, d$Bundesland == "Mecklenburg-Vorpommern" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
MVaußerorts
#Vektor
MVaußerortsV <- c(MVaußerorts$X2008, MVaußerorts$X2009, MVaußerorts$X2010, MVaußerorts$X2011, MVaußerorts$X2012)
MVaußerortsV

#Unfälle auf Autobahnen
MVautobahn <- subset(d, d$Bundesland == "Mecklenburg-Vorpommern" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
MVautobahn
#Vektor
MVautobahnV <- c(MVautobahn$X2008, MVautobahn$X2009, MVautobahn$X2010, MVautobahn$X2011, MVautobahn$X2012)
MVautobahnV

#Für gestapelte Saeulendiagramm
mvt<-c(MVmdD,MVrmdD,MVpdD,RMVJ)
MecklenburgVorpommern<-mvt
MVtest<-matrix(mvt, nrow=4,ncol=5, byrow = TRUE )
MVtest
#Entwicklung in 5 Jahren
plot(MVJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Mecklenburg-Vorpommern",cex=1)
lines(MVJahre,col="grey")

#Aufteilung
mv<-barplot(MVtest,beside=F,col=colors,main="Mecklenburg-Vorpommern")
legend(4.5,45000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
MV
#------------------------------------------------------------------#

# 9. Bundesland Niedersachsen---------------------------------------------------------------------------------
# Entwicklung von 2008 bis 2012

NS<-subset(d,d$Bundesland =="Niedersachsen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
NS

#Unfälle unter dem Einfluss berausch. Mittel
RNS<-subset(d,d$Bundesland =="Niedersachsen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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

#Unfälle innerorts
NSinnerorts <- subset(d, d$Bundesland == "Niedersachsen" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
NSinnerorts
#Vektor
NSinnerortsV <- c(NSinnerorts$X2008, NSinnerorts$X2009, NSinnerorts$X2010, NSinnerorts$X2011, NSinnerorts$X2012)
NSinnerortsV

#Unfälle außerorts
NSaußerorts <- subset(d, d$Bundesland == "Niedersachsen" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
NSaußerorts
#Vektor
NSaußerortsV <- c(NSaußerorts$X2008, NSaußerorts$X2009, NSaußerorts$X2010, NSaußerorts$X2011, NSaußerorts$X2012)
NSaußerortsV

#Unfälle auf Autobahnen
NSautobahn <- subset(d, d$Bundesland == "Niedersachsen" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
NSautobahn
#Vektor
NSautobahnV <- c(NSautobahn$X2008, NSautobahn$X2009, NSautobahn$X2010, NSautobahn$X2011, NSautobahn$X2012)
NSautobahnV

#Für gestapelte Saeulendiagramm
nst<-c(NSmdD,NSrmdD,NSpdD,RNSJ)
Niedersachsen<-nst
NStest<-matrix(nst, nrow=4,ncol=5, byrow = TRUE )
NStest
#Entwicklung in 5 Jahren
plot(NSJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Niedersachsen",cex=1)
lines(NSJahre,col="grey")

#Aufteilung
barplot(NStest,beside=F,col=colors,main="Niedersachsen")
legend(4.5,99000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
NS
#-------------------------------------------------------------------------------#


# 10. Bundesland Nordrhein-Westfalen--------------------------------------------------------------------------
# Entwicklung von 2008 bis 2012

NRW<-subset(d,d$Bundesland =="Nordrhein-Westfalen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
NRW

#Unfälle unter dem Einfluss berausch. Mittel
RNRW<-subset(d,d$Bundesland =="Nordrhein-Westfalen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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
NRWpdD<-c(NRWpd$X2008,NRWpd$X2009,NRWpd$X2010,NRWpd$X2011,NRWpd$X2012)
NRWpdD

#Übrige Sachschadensunfälle
NRWrmd<-subset(d,d$Bundesland =="Nordrhein-Westfalen" & d$Unfälle =="Übrige Sachschadensunfälle" & d$Lage =="Insgesamt")
NRWrmd

NRWrmdD<-c(NRWrmd$X2008,NRWrmd$X2009,NRWrmd$X2010,NRWrmd$X2011,NRWrmd$X2012)
NRWrmdD

#Unfälle innerorts
NRWinnerorts <- subset(d, d$Bundesland == "Nordrhein-Westfalen" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
NRWinnerorts
#Vektor
NRWinnerortsV <- c(NRWinnerorts$X2008, NRWinnerorts$X2009, NRWinnerorts$X2010, NRWinnerorts$X2011, NRWinnerorts$X2012)
NRWinnerortsV

#Unfälle außerorts
NRWaußerorts <- subset(d, d$Bundesland == "Nordrhein-Westfalen" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
NRWaußerorts
#Vektor
NRWaußerortsV <- c(NRWaußerorts$X2008, NRWaußerorts$X2009, NRWaußerorts$X2010, NRWaußerorts$X2011, NRWaußerorts$X2012)
NRWaußerortsV

#Unfälle auf Autobahnen
NRWautobahn <- subset(d, d$Bundesland == "Nordrhein-Westfalen" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
NRWautobahn
#Vektor
NRWautobahnV <- c(NRWautobahn$X2008, NRWautobahn$X2009, NRWautobahn$X2010, NRWautobahn$X2011, NRWautobahn$X2012)
NRWautobahnV

#Für gestapelte Saeulendiagramm
nrwt<-c(NRWmdD,NRWrmdD,NRWpdD,RNRWJ)
NordrheinWestfalen<-nrwt
nrwt
NRWtest<-matrix(nrwt, nrow=4,ncol=5, byrow = TRUE )
NRWtest
#Entwicklung in 5 Jahren
plot(NSJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Nordrhein-Westfalen",cex=1)
lines(NSJahre,col="grey")

#Aufteilung
nrw<-barplot(NRWtest,beside=F,col=colors,main="Nordrhein-Westfalen")
legend(4.5,400000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
NRW
#-------------------------------------------------------------------------------#

# 11. Rheinland-Pfalz-----------------------------------------------------------------------------------------
# Entwicklung von 2008 bis 2012

RP<-subset(d,d$Bundesland =="Rheinland-Pfalz" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
RP



#Unfälle unter dem Einfluss berausch. Mittel
RRP<-subset(d,d$Bundesland =="Rheinland-Pfalz" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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

#Unfälle innerorts
RPinnerorts <- subset(d, d$Bundesland == "Rheinland-Pfalz" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
RPinnerorts
#Vektor
RPinnerortsV <- c(RPinnerorts$X2008, RPinnerorts$X2009, RPinnerorts$X2010, RPinnerorts$X2011, RPinnerorts$X2012)
RPinnerortsV

#Unfälle außerorts
RPaußerorts <- subset(d, d$Bundesland == "Rheinland-Pfalz" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
RPaußerorts
#Vektor
RPaußerortsV <- c(RPaußerorts$X2008, RPaußerorts$X2009, RPaußerorts$X2010, RPaußerorts$X2011, RPaußerorts$X2012)
RPaußerortsV

#Unfälle auf Autobahnen
RPautobahn <- subset(d, d$Bundesland == "Rheinland-Pfalz" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
RPautobahn
#Vektor
RPautobahnV <- c(RPautobahn$X2008, RPautobahn$X2009, RPautobahn$X2010, RPautobahn$X2011, RPautobahn$X2012)
RPautobahnV

#Für gestapelte Saeulendiagramm
rpt<-c(RPmdD,RPrmdD,RPpdD,RRPJ)
RheinlandPfalz<-rpt
RPtest<-matrix(rpt, nrow=4,ncol=5, byrow = TRUE )
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
# Entwicklung von 2008 bis 2012-------------------------------------------------------------------------------

SA<-subset(d,d$Bundesland =="Saarland" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SA

#Unfälle unter dem Einfluss berausch. Mittel
RSA<-subset(d,d$Bundesland =="Saarland" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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

#Unfälle innerorts
SAinnerorts <- subset(d, d$Bundesland == "Saarland" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
SAinnerorts
#Vektor
SAinnerortsV <- c(SAinnerorts$X2008, SAinnerorts$X2009, SAinnerorts$X2010, SAinnerorts$X2011, SAinnerorts$X2012)
SAinnerortsV

#Unfälle außerorts
SAaußerorts <- subset(d, d$Bundesland == "Saarland" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
SAaußerorts
#Vektor
SAaußerortsV <- c(SAaußerorts$X2008, SAaußerorts$X2009, SAaußerorts$X2010, SAaußerorts$X2011, SAaußerorts$X2012)
SAaußerortsV

#Unfälle auf Autobahnen
SAautobahn <- subset(d, d$Bundesland == "Saarland" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
SAautobahn
#Vektor
SAautobahnV <- c(SAautobahn$X2008, SAautobahn$X2009, SAautobahn$X2010, SAautobahn$X2011, SAautobahn$X2012)
SAautobahnV

#Für gestapelte Saeulendiagramm
sat<-c(SAmdD,SArmdD,SApdD,RSAJ)
Saarland<-sat
SAtest<-matrix(sat, nrow=4,ncol=5, byrow = TRUE )
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
# Entwicklung von 2008 bis 2012-------------------------------------------------------------------------------

SAC<-subset(d,d$Bundesland =="Sachsen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SAC



#Unfälle unter dem Einfluss berausch. Mittel
RSAC<-subset(d,d$Bundesland =="Sachsen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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

#Unfälle innerorts
SACinnerorts <- subset(d, d$Bundesland == "Sachsen" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
SACinnerorts
#Vektor
SACinnerortsV <- c(SACinnerorts$X2008, SACinnerorts$X2009, SACinnerorts$X2010, SACinnerorts$X2011, SACinnerorts$X2012)
SACinnerortsV

#Unfälle außerorts
SACaußerorts <- subset(d, d$Bundesland == "Sachsen" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
SACaußerorts
#Vektor
SACaußerortsV <- c(SACaußerorts$X2008, SACaußerorts$X2009, SACaußerorts$X2010, SACaußerorts$X2011, SACaußerorts$X2012)
SACaußerortsV

#Unfälle auf Autobahnen
SACautobahn <- subset(d, d$Bundesland == "Sachsen" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
SACautobahn
#Vektor
SACautobahnV <- c(SACautobahn$X2008, SACautobahn$X2009, SACautobahn$X2010, SACautobahn$X2011, SACautobahn$X2012)
SACautobahnV

#Für gestapelte Saeulendiagramm
sact<-c(SACmdD,SACrmdD,SACpdD,RSACJ)
Sachsen<-sact
SACtest<-matrix(sact, nrow=4,ncol=5, byrow = TRUE )
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
# Entwicklung von 2008 bis 2012-------------------------------------------------------------------------------

SAA<-subset(d,d$Bundesland =="Sachsen-Anhalt" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SAA

SAAJahre<-c(SAA$X2008,SAA$X2009,SAA$X2010,SAA$X2011,SAA$X2012)


#Unfälle unter dem Einfluss berausch. Mittel
RSAA<-subset(d,d$Bundesland =="Sachsen-Anhalt" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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

#Unfälle innerorts
SAAinnerorts <- subset(d, d$Bundesland == "Sachsen-Anhalt" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
SAAinnerorts
#Vektor
SAAinnerortsV <- c(SAAinnerorts$X2008, SAAinnerorts$X2009, SAAinnerorts$X2010, SAAinnerorts$X2011, SAAinnerorts$X2012)
SAAinnerortsV

#Unfälle außerorts
SAAaußerorts <- subset(d, d$Bundesland == "Sachsen-Anhalt" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
SAAaußerorts
#Vektor
SAAaußerortsV <- c(SAAaußerorts$X2008, SAAaußerorts$X2009, SAAaußerorts$X2010, SAAaußerorts$X2011, SAAaußerorts$X2012)
SAAaußerortsV

#Unfälle auf Autobahnen
SAAautobahn <- subset(d, d$Bundesland == "Sachsen-Anhalt" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
SAAautobahn
#Vektor
SAAautobahnV <- c(SAAautobahn$X2008, SAAautobahn$X2009, SAAautobahn$X2010, SAAautobahn$X2011, SAAautobahn$X2012)
SAAautobahnV

#Für gestapelte Saeulendiagramm
saat<-c(SAAmdD,SAArmdD,SAApdD,RSAAJ)
SachsenAnhalt<-saat
SAAtest<-matrix(saat, nrow=4,ncol=5, byrow = TRUE )
SAAtest
#Entwicklung in 5 Jahren
plot(SAAJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Sachsen-Anhalt",cex=1)
lines(SAAJahre,col="grey")

#Aufteilung
barplot(SAAtest,beside=F,col=colors,main="Sachsen-Anhalt")
legend(4.5,45000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))
SAA
#-------------------------------------------------------------------------------#
# 15. Bundesland Schleswig-Holstein---------------------------------------------------------------------------
# Entwicklung von 2008 bis 2012

SH<-subset(d,d$Bundesland =="Schleswig-Holstein" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
SH

#Unfälle unter dem Einfluss berausch. Mittel
RSH<-subset(d,d$Bundesland =="Schleswig-Holstein" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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

#Unfälle innerorts
SHinnerorts <- subset(d, d$Bundesland == "Schleswig-Holstein" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
SHinnerorts
#Vektor
SHinnerortsV <- c(SHinnerorts$X2008, SHinnerorts$X2009, SHinnerorts$X2010, SHinnerorts$X2011, SHinnerorts$X2012)
SHinnerortsV

#Unfälle außerorts
SHaußerorts <- subset(d, d$Bundesland == "Schleswig-Holstein" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
SHaußerorts
#Vektor
SHaußerortsV <- c(SHaußerorts$X2008, SHaußerorts$X2009, SHaußerorts$X2010, SHaußerorts$X2011, SHaußerorts$X2012)
SHaußerortsV

#Unfälle auf Autobahnen
SHautobahn <- subset(d, d$Bundesland == "Schleswig-Holstein" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
SHautobahn
#Vektor
SHautobahnV <- c(SHautobahn$X2008, SHautobahn$X2009, SHautobahn$X2010, SHautobahn$X2011, SHautobahn$X2012)
SHautobahnV

#Für gestapelte Saeulendiagramm
sht<-c(SHmdD,SHrmdD,SHpdD,RSHJ)
SchleswigHolstein<-sht
SHtest<-matrix(sht, nrow=4,ncol=5, byrow = TRUE )
SHtest
#Entwicklung in 5 Jahren
plot(SHJahre,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Baden-Württemberg",cex=1)
lines(SHJahre,col="grey")

#Aufteilung
barplot(SHtest,beside=F,col=colors,main="Schleswig-Holstein")
legend(4.5,45000,c("MD","RMD","PD","Drug"), col=colors,lty=c(1,1))

SH

#-------------------------------------------------------------------------------#

# 16 Bundesland Thüringen------------------------------------------------------------------------------------
# Entwicklung von 2008 bis 2012

TH<-subset(d,d$Bundesland =="Thüringen" & d$Lage=="Insgesamt" & d$Unfälle =="Insgesamt")
TH

#Unfälle unter dem Einfluss berausch. Mittel
RTH<-subset(d,d$Bundesland =="Thüringen" & d$Unfälle =="Sonst. Unfälle unter dem Einfluss berausch. Mittel" & d$Lage =="Insgesamt" )
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

#Unfälle innerorts
THinnerorts <- subset(d, d$Bundesland == "Thüringen" & d$Unfälle == "Insgesamt" & d$Lage == "innerorts")
THinnerorts
#Vektor
THinnerortsV <- c(THinnerorts$X2008, THinnerorts$X2009, THinnerorts$X2010, THinnerorts$X2011, THinnerorts$X2012)
THinnerortsV

#Unfälle außerorts
THaußerorts <- subset(d, d$Bundesland == "Thüringen" & d$Unfälle == "Insgesamt" & d$Lage == "außerorts (ohne Autobahnen)")
THaußerorts
#Vektor
THaußerortsV <- c(THaußerorts$X2008, THaußerorts$X2009, THaußerorts$X2010, THaußerorts$X2011, THaußerorts$X2012)
THaußerortsV

#Unfälle auf Autobahnen
THautobahn <- subset(d, d$Bundesland == "Thüringen" & d$Unfälle == "Insgesamt" & d$Lage == "auf Autobahnen")
THautobahn
#Vektor
THautobahnV <- c(THautobahn$X2008, THautobahn$X2009, THautobahn$X2010, THautobahn$X2011, THautobahn$X2012)
THautobahnV

#Für gestapelte Saeulendiagramm
tht<-c(THmdD,THrmdD,THpdD,RTHJ)
Thüringen<-tht
Thüringen
THtest<-matrix(tht, nrow=4,ncol=5, byrow = TRUE )
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
plot(DE,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Deutschland Gesamt",cex=1)
lines(DE,col="black")

#Aufteilung der Umfallarten in der BRD gesamt
ansicht.A<-THtest+SHtest+SAtest+SACtest+SAAtest+RPtest+NRWtest+MVtest+HEtest+HHtest+BREMtest+BRAtest+testBER+testBY+BWtest
barplot(ansicht.A,beside=F,col=colors,main="Deutschland")
legend(4.5,4800000,c("MD","RMD","PD","Drug","test"), col=colors,lty=c(1,1))

#Ganz Deutschland Rauschmittel
DERauschmittel <- RBWJ+RBYJ+RBERJ+RBRAJ+RBREMJ+RHHJ+RHEJ+RMVJ+RNSJ+RNRWJ+RRPJ+RSAJ+RSACJ+RSAAJ+RSHJ+RTHJ
DERauschmittel
plot(DERauschmittel,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Deutschland Rauschmittel",cex=1)
lines(DERauschmittel,col="black")

#Ganz Deutschland Personenschaden
DEPersonenschaden <- BWpdD+BYpdD+BERpdD+BRApdD+BREMpdD+HHpdD+HEpdD+MVpdD+NSpdD+NRWpdD+RPpdD+SApdD+SACpdD+SAApdD+SHpdD+THpdD
DEPersonenschaden
plot(DEPersonenschaden,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Deutschland Personenschaden",cex=1)
lines(DEPersonenschaden,col="black")

#Ganz Deutschland Sachschaden
DESachschaden <- BWmdD+BYmdD+BERmdD+BRAmdD+BREMmdD+HHmdD+HEmdD+MVmdD+NSmdD+NRWmdD+RPmdD+SAmdD+SACmdD+SAAmdD+SHmdD+THmdD
DESachschaden
plot(DESachschaden,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Deutschland Sachschaden",cex=1)
lines(DESachschaden,col="black")

#Ganz Deutschland Übrige
DEUebrige <- BWrmdD+BYrmdD+BERrmdD+BRArmdD+BREMrmdD+HHrmdD+HErmdD+MVrmdD+NSrmdD+NRWrmdD+RPrmdD+SArmdD+SACrmdD+SAArmdD+SHrmdD+THrmdD
DEUebrige
plot(DEUebrige,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Deutschland Uebrige",cex=1)
lines(DEUebrige,col="black")

#Ganz Deutschland Innerorts
DEInnerorts <- BWinnerortsV+BYinnerortsV+BERinnerortsV+BRAinnerortsV+BREMinnerortsV+HHinnerortsV+HEinnerortsV+MVinnerortsV+NSinnerortsV+NRWinnerortsV+RPinnerortsV+SAinnerortsV+SACinnerortsV+SAAinnerortsV+SHinnerortsV+THinnerortsV
DEInnerorts
plot(DEInnerorts,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Deutschland Innerorts",cex=1)
lines(DEInnerorts,col="black")

#Ganz Deutschland Außerorts
DEAußerorts <- BWaußerortsV+BYaußerortsV+BERaußerortsV+BRAaußerortsV+BREMaußerortsV+HHaußerortsV+HEaußerortsV+MVaußerortsV+NSaußerortsV+NRWaußerortsV+RPaußerortsV+SAaußerortsV+SACaußerortsV+SAAaußerortsV+SHaußerortsV+THaußerortsV
DEAußerorts
plot(DEAußerorts,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Deutschland Außerorts",cex=1)
lines(DEAußerorts,col="black")

#Ganz Deutschland Autobahn
DEAutobahn <- BWautobahnV+BYautobahnV+BERautobahnV+BRAautobahnV+BREMautobahnV+HHautobahnV+HEautobahnV+MVautobahnV+NSautobahnV+NRWautobahnV+RPautobahnV+SAautobahnV+SACautobahnV+SAAautobahnV+SHautobahnV+THautobahnV
DEAutobahn
plot(DEAutobahn,col=colors,ylab="Unfälle",xlab="Jahr",pch=16,main="Deutschland Autobahn",cex=1)
lines(DEAutobahn,col="black")

#----------------------------------------------------------------------------------------------------------#

# Überprüfung der Signifikanz der Änderungen

# Zusammensetzen der hierfür relevanten Daten
DatenSig <- rbind(BW, BY, BER, BRA, BREM, HH, HE, MV, NS, NRW, RP, SA, SAC, SAA, SH, TH)
DatenSig

# Überprüfung auf Normalverteilung (einzelne Jahre)
boxplot(DatenSig$X2008)
boxplot(DatenSig$X2009)
boxplot(DatenSig$X2010)
boxplot(DatenSig$X2011)
boxplot(DatenSig$X2012)

hist(DatenSig$X2008)
hist(DatenSig$X2009)
hist(DatenSig$X2010)
hist(DatenSig$X2011)
hist(DatenSig$X2012)

shapiro.test(DatenSig$X2008)
shapiro.test(DatenSig$X2009)
shapiro.test(DatenSig$X2010)
shapiro.test(DatenSig$X2011)
shapiro.test(DatenSig$X2012)

# ==> die einzelnen Jahresdaten sind also NICHT normalverteilt

# Überprüfung auf Normalverteilung der paarweisen Differenzen
# Dies ist Voraussetzung für den t.test
boxplot(DatenSig$X2009 - DatenSig$X2008)
boxplot(DatenSig$X2010 - DatenSig$X2009)
boxplot(DatenSig$X2011 - DatenSig$X2010)
boxplot(DatenSig$X2012 - DatenSig$X2011)

hist(DatenSig$X2009 - DatenSig$X2008)
hist(DatenSig$X2010 - DatenSig$X2009)
hist(DatenSig$X2011 - DatenSig$X2010)
hist(DatenSig$X2012 - DatenSig$X2011)

shapiro.test(DatenSig$X2009 - DatenSig$X2008)
shapiro.test(DatenSig$X2010 - DatenSig$X2009)
shapiro.test(DatenSig$X2011 - DatenSig$X2010)
shapiro.test(DatenSig$X2012 - DatenSig$X2011)

# Vor allem bei §X2011 - $X2010 liegt eine Normalverteilung vor, bei den anderen nicht.
# Daher ist der t.test eher ungeeignet


