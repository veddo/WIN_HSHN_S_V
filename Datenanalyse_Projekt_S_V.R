#Einlesen der Datei, System Linux Ubuntu 12.04
setwd("/home/vhx/Dokumente/R-Statistik/Projekt_S_V/WIN_HSHN_S_V/")
d<-read.csv2("unfalldaten.csv",encoding="utf-8")
d
subset(d,d$Bundesland =="Baden-Württemberg")

#Bundesland Baden Württenberg 
#Jahr 2008 