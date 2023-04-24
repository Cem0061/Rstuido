#####################################################################################
#                                                                                   #
#                           W2453 - Uebung4 - Code                                  #
#                                                                                   #
#####################################################################################

### B4.1

# a)
  N = 400								        	### Anzahl der zu simulierenden Beobachtungen
  set.seed(1000)					  			### Start des Zufallszahlengenerators bei 42 (Zahl kann beliebig festgelegt werden)
  X = arima.sim(N, model = list(ar = 0.6, ma = 0.7)) + 30				### Simulation eines ARMA Modells mit alpha = 0.6, beta = 0.7 und intercept = 30

  par(mfrow=c(2,1))							  ### Aufteilung des Grafikfensters auf 2 Grafiken (2 Zeilen, 1 Spalte, Zeilen werden zuerst aufgefüllt)
  plot.ts(X)							      	### Plotten des simulierten ARMA Prozesses
  title("Die simulierte Zeitreihe nach dem ARMA[1, 1]-Modell")		### Überschrift für Grafik
  acf(X)									        ### Autokorrelationsfunktion

# b)
  AIC = matrix(0, 4, 4)							### Festlegen der AIC Matrix, 4 Zeilen, 4 Spalten
  for(i in 0:3) {	  							### Schleife für die AR-Ordnungen p
    for(j in 0:3)	{   						### Schleife für die MA-Ordnungen q
      Modell = arima(X, order=c(i, 0, j))	### Schätzung des Modelles für p = 1, q = j 
      AIC[i+1, j+1]=Modell$aic		### Schreibe die AIC Werte in die Matrix
    }
  }

  AIC	            								### Ausgabe der Tabelle
  min(AIC)					         			### Anzeige des kleinstes Wertes der Tabelle

  BIC = matrix(0, 4, 4)						### Festlegen der AIC Matrix, 4 Zeilen, 4 Spalten
  for(i in 0:3) {		      				### Schleife für die AR-Ordnungen p
    for(j in 0:3) {				      	### Schleife für die MA-Ordnungen q 
      Modell2=arima(X, order=c(i, 0, j))				### Schätzung des Modelles für p = i, q = j
      BIC[i+1, j+1]=-2*Modell2$loglik+log(length(X))*(i+j+2)-log(length(X))*2		### Berechne und schreibe BIC Werte in Matrix
    }
  }

  BIC	            								### Ausgabe der Tabelle
  min(BIC)				        				### Anzeige des kleinstes Wertes der Tabelle

# c)
  Modell.opt=arima(X, order=c(1, 0, 1))	### Gewähltes Modell nach AIC und BIC wird erneut geschätzt
  Modell.opt                      ### Zeige optimales Modell an

### B4.2 

# a)
  library(smoots)           # Aufruf des smoots-Pakets
  Yt = scan("W2453-Uebung4-Sim.txt") # Einlesen der simulierten Daten aus gegebener Textdatei im Arbeitsverzeichnis
  est = msmooth(Yt)        # Nichtparametrische Trendschaetzung
  est$b0                    # Optimale Bandbreite
  trend.est = est$ye       # Die geschaetzten Werte des Trends
  resid = est$res          # Die Residuen
  resid = Yt - trend.est   # Alternative berechnung der Residuen
  
# b)
  ## Plot der Daten mit dem geschaetztem Trend
  par(mfrow = c(1, 2))
  plot.ts(Yt, xlab = "Beobachtungszeitpunkt", ylab = expression(X[t]))
  lines(trend.est, col = "red")
  plot.ts(resid, xlab = "Beobachtungszeitpunkt", ylab = expression(Y[t]))
  
  ## Analyse der ACF der Residuen
  acf(resid)                # -> Mehr als 5% der vertikalen Linien ueberschreiten die
                            # blauen Linien. -> Autokorrelation in den Residuen.
  
# c)
  ## Auswahl des optimalen ARMA(p, q)-Modells fuer die Residuen
  p.max = 5                # Maximale zu pruefende Ordnung fuer den AR-Teil 
  q.max = 5                # Maximale zu pruefende Ordnung fuer den MA-Teil
  
  # Erstellen einer leeren Matrix, die mit BIC-Werten gefuellt werden soll
  # Spaltennummer = Ordnung q + 1
  # Zeilennummer = Ordnung p + 1 
  BIC = matrix(0, ncol = q.max + 1, nrow = p.max + 1) 
  rownames(BIC) = paste0("p = ", 0:p.max)  # Umbennennung der Zeilen (optional)
  colnames(BIC) = paste0("q = ", 0:q.max)  # Umbenennung der Spalten (optional)
  
  n = length(resid)        # Anzahl der Beobachtungen
  
  # Doppelte Schleife: Der BIC-Wert jedes moeglichen ARMA-Modells der 
  # Kombinationen aus p = 0, 1, ..., 5 und q = 0, 1, ..., 5 wird
  # berechnet.
  # Setze ausserdem 'include.mean = FALSE', da wir davon ausgehen, dass der 
  # Erwartungswert des Fehlerterms des additiven Komponentenmodells gleich 
  # Null ist. --> Der Erwartungswert braucht in diesem Fall nicht geschaetzt 
  # zu werden.
  for (i in 0:p.max) {      # Beginn Schleife 1: Ordnung p
    for (j in 0:q.max) {    # Beginn Schleife 2: Ordnung q
      arma.est = arima(resid, order = c(i, 0, j), include.mean = FALSE)
      BIC[i + 1, j + 1] = -2 * arma.est$loglik + log(n) * (i + j)
    }                       # Ende Schleife 2
  }                         # Ende Schleife 1
  
  # Finde die optimalen Ordnungen p,q (Alternative: SieheB4.1 b) )
  ij.opt = which(BIC == min(BIC), arr.ind = TRUE) # Abruf der Matrixindizes des minimalen BIC-Werts
  i.opt = ij.opt[1] - 1    # Optimale Ordnung p = Zeilennummer des minimalen Werts - 1
  j.opt = ij.opt[2] - 1    # Optimale Ordnung q = Spaltennumer des minimalen Werts - 1
  
  ## Finale Schaetzung mit den optimalen Ordnungen fuer p und q
  arma.opt = arima(resid, order = c(i.opt, 0, j.opt), include.mean = FALSE)
  arma.opt
  
