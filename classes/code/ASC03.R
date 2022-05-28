###########################################################################
# Analiza szeregow czasowych                                              #
# semestr letni 2022                                                      #
# 3                                                                       #
# Uniwersytet Warszawski, Wydzial Nauk Ekonomicznych                      #
###########################################################################


###################################################
# I. Metoda sredniej ruchomej - prosta            #
###################################################


#  1. 
# Importujemy dane z pliku "rate.csv" 
# Japanese Yen to one U.S. Dollar
#	Kwartalne dane dla okresu 1q.1985 - 2q.2007, (zrodlo: http://www.economagic.com/)

# import pliku CSV
rate <- read.csv("dane/rate.csv",   # nazwa pliku
                header = TRUE,    	# czy obserwacje w pierwszym wierszu?
                sep = ";",    # separator kolumn
                dec = ".")	  # separator dziesiatny


# weryfikujemy strukture obiektu
str(rate)

# utworzymy obiekt typu ts (time series) dla danych 
rate.ts <- 
  ts(rate$rate, start = c(1985, 1), freq = 4)

rate.ts
str(rate.ts)

#  2. 
#  Wizualizacja szeregu 

plot(rate.ts,
     main = "Japanese Yen to one U.S. Dollar",
     xlab = "data",
     ylab = "Exchage rate")


# 3. 
# Metoda sredniej ruchomej

# funkcja filter() (pakiet stats)

# filter(x,filter,sides=1,...)
# x - szereg czasowy
# filter  - wektor wag (wspolczynniki filtra)
# sides   - mozliwe wartosci: 1 lub 2
# sides=1  - tylko t oraz t-s obserwacje
# sides=2 - t+s oraz t oraz t-s obserwacje 

rate.ma3 <- filter(rate.ts, sides=1, filter=(c(0,1,1,1)/3)) # moving average - œrednie ruchoma trzyokresowa
rate.ma5 <- filter(rate.ts, sides=1, filter=(c(0,1,1,1,1,1)/5)) # moving average - srednia ruchoma pieciokresowa

# Zobaczmy uzyskane oszacowania
head(rate.ma3,n=4)
tail(rate.ma3,n=4)

head(rate.ma5,n=8)
tail(rate.ma5,n=8)


# Porownanie uzyskanych wynikow na wykresie
plot(rate.ma3,
     main=paste("Metoda sredniej ruchomej  \n",
                "(szereg 'Japanese Yen to one U.S. Dollar')"),
     col="blue", lty=2)

lines(rate.ma5, col="red", lty=2)
lines(rate.ts, col="black", lty=1)
abline(v =2007.00, lty = 2)          # dodajemy pionowa linie referencyjna


# legenda
legend("topright",
       legend=c("wyjsciowy szereg",
                "ruchoma srednia (ma=3)",
                "ruchoma srednia (ma=5)"),
       col=c("black", "blue", "red"),
       lty=c(1,2,2))


# 4.
# Bledy predykcji ex post

# laczymy wszystkie kolumny razem 
rate.summary <- ts.union(rate.ts,
                         rate.ma3,
                         rate.ma5)



# utworzymy takze zmienna binarna dzielaca caly zbior obserwacji
# na okresy in-sample i out-of-sample

library(xts)
rate.summary = as.xts(rate.summary)

ifelse(index(rate.summary) < "2007-02", 1, 0)

sample_period <-
  ts(ifelse(index(rate.summary) < "2007-02", 1, 0), 
     start  =c(1985, 1), freq = 4)

names(rate.summary)

# nazwy zmiennych uproscimy
names(rate.summary)<-c("rate","ma3","ma5")

# utworzymy zmienna indeksujaca czas (date)
rate.summary$date <- index(rate.ts)

rate.summary$sample_period <- sample_period



# 5.
# porownanie przecietnych bledow prognoz w okresie out-of-sample 
# z tymi z okresu in-sample

# liczymy odpowiednie roznice ...
rate.summary$mae_ma3     <- abs(rate.summary$ma3-rate.summary$rate)
rate.summary$mae_ma5     <- abs(rate.summary$ma5-rate.summary$rate)

rate.summary$mse_ma3     <- (rate.summary$ma3-rate.summary$rate)^2
rate.summary$mse_ma5     <- (rate.summary$ma5-rate.summary$rate)^2

rate.summary$mape_ma3     <- abs((rate.summary$ma3-rate.summary$rate)/rate.summary$rate)
rate.summary$mape_ma5     <- abs((rate.summary$ma5-rate.summary$rate)/rate.summary$rate)

rate.summary$amape_ma3     <- abs((rate.summary$ma3-rate.summary$rate)/(rate.summary$ma3+rate.summary$rate))
rate.summary$amape_ma5     <- abs((rate.summary$ma5-rate.summary$rate)/(rate.summary$ma5+rate.summary$rate))



# nastepnie je usredniamy 
# oddzielnie dla okresu in-sample oraz dla okresu prognozy (out-of-sample)

names(rate.summary)

# wartosci do usrednienie mamy w kolumnach 6:13
# musimy zatem uwzgledniac braki danych (zignorowac je w obliczeniach)
aggregate(rate.summary[, 6:11],
          by = list(rate.summary$sample_period),
          FUN = function(x) mean(x, na.rm = T))


###################################################
# II. Metoda sredniej ruchomej - wazona           #
###################################################

# t=0, bo prognozujemy ten okres
rate.ma_w <- filter(rate.ts, sides=1, filter=(c(0,6,5,4,3,2,1)/21)) # moving average



# Zobaczmy uzyskane oszacowania
head(rate.ma_w,n=12)
tail(rate.ma_w,n=12)


# Porownanie uzyskanych wynikow na wykresie
plot(rate.ma_w,
     main=paste("Metoda wazonej sredniej ruchomej  \n",
                "(szereg 'Japanese Yen to one U.S. Dollar')"),
     col="blue", lty=1)
lines(rate.ts, col="black", lty=1)
abline(v =2007.00, lty = 2)          # dodajemy pionowa linie referencyjna


# legenda
legend("topright",
       legend=c("wyjsciowy szereg",
                "wazona ruchoma srednia"),
       col=c("black", "blue"),
       lty=c(1,1))


#################### przerobic na srednia ruchoma wazona
# 4.
# Bledy predykcji ex post

# laczymy wszystkie kolumny razem 
rate.summary <- ts.union(rate.ts,
                         rate.ma3,
                         rate.ma5)



# utworzymy takze zmienna binarna dzielaca caly zbior obserwacji
# na okresy in-sample i out-of-sample

library(xts)
rate.summary = as.xts(rate.summary)

ifelse(index(rate.summary) < "2007-02", 1, 0)

sample_period <-
  ts(ifelse(index(rate.summary) < "2007-02", 1, 0), 
     start  =c(1985, 1), freq = 4)

names(rate.summary)

# nazwy zmiennych uproscimy
names(rate.summary)<-c("rate","ma3","ma5")

# utworzymy zmienna indeksujaca czas (date)
rate.summary$date <- index(rate.ts)

rate.summary$sample_period <- sample_period



# 5.
# porownanie przecietnych bledow prognoz w okresie out-of-sample 
# z tymi z okresu in-sample

# liczymy odpowiednie roznice ...
rate.summary$mae_ma3     <- abs(rate.summary$ma3-rate.summary$rate)
rate.summary$mae_ma5     <- abs(rate.summary$ma5-rate.summary$rate)

rate.summary$mse_ma3     <- (rate.summary$ma3-rate.summary$rate)^2
rate.summary$mse_ma5     <- (rate.summary$ma5-rate.summary$rate)^2

rate.summary$mape_ma3     <- abs((rate.summary$ma3-rate.summary$rate)/rate.summary$rate)
rate.summary$mape_ma5     <- abs((rate.summary$ma5-rate.summary$rate)/rate.summary$rate)

rate.summary$amape_ma3     <- abs((rate.summary$ma3-rate.summary$rate)/(rate.summary$ma3+rate.summary$rate))
rate.summary$amape_ma5     <- abs((rate.summary$ma5-rate.summary$rate)/(rate.summary$ma5+rate.summary$rate))



# nastepnie je usredniamy 
# oddzielnie dla okresu in-sample oraz dla okresu prognozy (out-of-sample)

names(rate.summary)

# wartosci do usrednienie mamy w kolumnach 6:13
# musimy zatem uwzgledniac braki danych (zignorowac je w obliczeniach)
aggregate(rate.summary[, 6:11],
          by = list(rate.summary$sample_period),
          FUN = function(x) mean(x, na.rm = T))



