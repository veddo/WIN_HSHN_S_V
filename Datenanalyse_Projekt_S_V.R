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
