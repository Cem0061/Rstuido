#####################################################################################
#                                                                                   #
#                       W2453 - Angewandtes Projekt mit R                           #
#                           Aufgabe 3: GARCH-Modelle                                #
#                                                                                   #
#####################################################################################

# Beispiel zur Nutzung des lubridate-Paketes (k?nnen Sie l?schen, auskommentieren oder so lassen)
  library(lubridate)                # load lubridate package
  time = "15:45:23"                 # example for time (works also if "time is a vector)
  seconds = as.numeric(hms(time))   # transform the time-format to seconds
                                    # where 00:00:00 (midnight) is 0 seconds
  hours = seconds/60^2              # transforming seconds in decimal hours

# Staten Sie Ihren Code hier
  "a"
  library(lubridate)
  daimler=read.csv("Daimler-27.12.19.csv",header=TRUE,sep=";",dec=",")
  Kurs12=daimler$Kurs
  Zeit=daimler$Uhrzeit
  n_1=length(Kurs12)
  seconds2=as.numeric(hms(Zeit))
  time2=seconds2/60^2
  plot.ts(time2,Kurs12,type="l",xlab="Uhrzeit",ylab="Aktienwert")
  title("Aktienwert von Daimler")
  
  Index=read.csv("DEU.IDXEUR_Ticks_27.12.2019-27.12.2019.csv",header=TRUE,sep=",")
  
  Kurs123=Index$Bid
  Zeit=read.table("txt2.txt",sep=".")
  Zeit1=Zeit$V1
  n_2=length(Kurs123)
  seconds2=as.numeric(hms(Zeit1))
  time3=seconds2/60^2
  plot(time3,Kurs123,type="l",xlab = "Uhrzeit",ylab="Indexwert")
  title("Index von")
"b"
Rendite = diff(log(Kurs12))   # Berechne die log-Rendite
time4 = time2[2:n_1]        # Jahr f?r die Rendite


plot(time4, Rendite, type = "l", xlab = "Uhrzeit")
title("Log-Rendite des Daimler")
Rendite2=diff(log(Kurs123))
time5=time3[2:n_2]  
plot(time5,Rendite2,typ="l",xlab="Uhrzeit")
title("Rendite vom Index")
library(fGarch) 

RenditeG=Rendite*100
g_model = garchFit(~garch(1, 1), data = RenditeG, trace = FALSE) # Sch?tze das GARCH(1,1) Modell.
g_model
RenditeG2=Rendite2*100
g_model2 = garchFit(~garch(1, 1), data = RenditeG2, trace = FALSE) # Sch?tze das 2.GARCH(1,1) Modell.
g_model2
"c"
Wartezeit=diff(time2)*(-1)*1000
plot.ts(Wartezeit,type="l")
hist(Wartezeit)
rnorm(Wartezeit)
qqnorm(Wartezeit) # QQ Normal Plot der Renditen
qqline(Wartezeit) # QQ line der Renditen

g_SD = g_model@sigma.t      # Speichere die berechneten bed. Standardabweichungen

g_SD2 = g_model2@sigma.t      # Speichere die berechneten bed. Standardabweichungen

plot(Jahr2, Rendite, type = "l", xlab = "Jahr")
title("Log-Rendite des S&P 500 mit bed. Volatilit?t")
lines(Jahr2, g_SD2, col = 2) # plotte die bed. Standardabweichungen

plot(Jahr2, g_SD, type = "l", xlab = "Jahr", ylab = "Volatilit?t", col = 2)
title("Bed. Volatilit?t des S&P 500 nach GARCH(1,1)")
plot(Jahr2, Rendite, type = "l", xlab = "Jahr")
title("Log-Rendite des S&P 500 mit bed. Volatilit?t")
lines(Jahr2, g_SD, col = 2) # plotte die bed. Standardabweichungen

plot(Jahr2, g_SD, type = "l", xlab = "Jahr", ylab = "Volatilit?t", col = 2)
title("Bed. Volatilit?t des S&P 500 nach GARCH(1,1)")
WartezeitIndex=diff(time3)*1000
hist(WartezeitIndex)
rnorm(WartezeitIndex)
qqnorm(WartezeitIndex) # QQ Normal Plot der Renditen
qqline(WartezeitIndex) # QQ line der Renditen

