#####################################################################################
#                                                                                   #
#                       W2453 - Angewandtes Projekt mit R                           #
#                         Aufgabe 2: Finanzmarkanalyse                              #
#                                                                                   #
#####################################################################################


# Geben Sie fuer das Objekt 'MatrNr' in Zeile 8 die Matrikelnummer
# eines Ihrer Gruppenmitglieder ein. Markieren Sie die Zeilen 8 bis 15 
# und fuehren Sie diese zusammen aus.

MatrNr = 7000548              # Geben Sie hier Ihre Matrikelnummer ein!
# Die Nummer eines einzelnen Gruppenmitglieds 
# genuegt.

source("W2453 - choose_data - nicht bearbeiten.txt")
daten = choose_data(MatrNr) 
U1 = daten[[1]]             # Daten fuer das erste Unternehmen
U2 = daten[[2]]             # Daten fuer das zweite Unternehmen
rm("daten", "choose_data")  # L?scht nicht mehr ben?tigte Objekte

# Nach der Ausfuehrung des Codes finden Sie in der Konsole einen Hinweis, welche Daten 
# fuer Ihre Projektgruppe zu bearbeiten sind.

# In dem Objekt 'U1' befinden sich nun die Finanzdaten des erst-
# genannten Unternehmens, in 'U2' hingegen die des zweiten 
# Unternehmens.

#-------------------------------------------------------------------
# Beginnen Sie hier mit Ihrem eigenen Code:
#-------------------------------------------------------------------
"a)"

Kurs = U1$Close # speichere den Kurs
n_1 = length(Kurs)    # speichere die Zahl der Beobachtungen
Rendite = diff(log(Kurs))   # Berechne die log-Rendite
n_2 = length(Rendite)       # Zahl der Renditen
Year1=(1:n_1)/n_1*(1/12+5)+2008+0.5 # speichere das Jahr
Year2=(1:n_2)/n_2*(1/12+5)+2008

plot(Year1, Kurs, type = "l", xlab = "Jahr")  # plotte die log-Renditen
title("Kurs vom Loreal")   # Titel hinzuf?gen

plot(Year2, Rendite, type = "l", xlab = "Jahr")
title("Log-Rendite vom Loreal")
Kurs2 = U2$Close # speichere den Kurs

n_1 = length(Kurs2)    # speichere die Zahl der Beobachtungen
Rendite2 = diff(log(Kurs2))   # Berechne die log-Rendite
n_2 = length(Rendite2)       # Zahl der Renditen
Year01=(1:n_1)/n_1*(1/12+5)+2008 # speichere das Jahr
Year02=(1:n_2)/n_2*(1/12+5)+2008
# ?ffne Plot mit zwei Zeilen
plot(Year01, Kurs2, type = "l", xlab = "Jahr")  # plotte die log-Renditen
title("Kurs des UNITEDHEALTH")   # Titel hinzuf?gen

plot(Year02, Rendite2, type = "l", xlab = "Jahr")
title("Log-Rendite des UNITEDHEALTH")
"Aufgabe 2b"
plot(Year2, Rendite^2, type = "l", xlab = "Jahr", ylim=c(0,0.015))  # plotte die quadrite log-Renditen
title("Quadritte Rendite des Loreal")   # Titel hinzuf?gen
plot(Year02, Rendite2^2, type = "l", xlab = "Jahr", ylim=c(0,0.015))
title("Quadritte Rendite vom UNITEDHEALTH")   # Titel hinzuf?gen
acf(Rendite,lag.max = length(Kurs),main= "Autokorrelation der Rendite vom Loreal")
acf(Rendite^2,lag.max= length(Kurs),main= "Autokorrelation der Quadritten Rendite vom Loreal")
acf(Rendite2,lag.max = length(Kurs2),main= "Autokorrelation der Rendite des UNITEDHEALTH")
acf(Rendite2^2,lag.max= length(Kurs2),main= "Autokorrelation der Quadriten Rendite des UNITEDHEALTH")

"c"
mean(Rendite)
var(Rendite)
skewness(Rendite) # Schiefe der Renditen
kurtosis(Rendite) # Kurtosis der Renditen
hist(Rendite,main="Histrogram von Loreal") # Histogramm der Renditen
qqnorm(Rendite) # QQ Normal Plot der Renditen
qqline(Rendite) # QQ line der Renditen

mean(Rendite2)
var(Rendite2)
skewness(Rendite2) # Schiefe der Renditen
kurtosis(Rendite2) # Kurtosis der Renditen
hist(Rendite2,main="Histrogram von Underneath" ) # Histogramm der Renditen
qqnorm(Rendite2) # QQ Normal Plot der Renditen
qqline(Rendite2) # QQ line der Renditen

