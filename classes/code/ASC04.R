###########################################################################
# Analiza szeregow czasowych                                              #
# semestr letni 2022                                                      #
# 4                                                                       #
# Uniwersytet Warszawski, Wydzial Nauk Ekonomicznych                      #
###########################################################################


#############################################################################
# III. Algorytmy wygladzania wykladniczego
#############################################################################

####################################
# 1. Proste wygladzanie wykladnicze
####################################


#  1. 
# Importujemy dane z pliku "bsales.csv" 


# import pliku CSV
bsales <- read.csv("dane/bsales.csv",   # nazwa pliku
                   header = TRUE,    	  # czy obserwacje w pierwszym wierszu?
                   sep = ";",           # separator kolumn
                   dec = ".")	          # separator dziesiatny


# weryfikujemy strukture obiektu
str(bsales)

# utworzymy obiekt typu ts (time series) dla danych 
bsales.ts <- 
  ts(bsales$sales)

bsales.ts
str(bsales.ts)



#  2. 
#  Wizualizacja szeregu 

plot(bsales.ts,
     main = "Sprzedaz",
     xlab = "data",
     ylab = "Sprzedaz")

abline(h =mean(bsales.ts), col="red", lty = 2)          # dodajemy pozioma linie referencyjna


# Rozwazmy model postaci: Yt=b1t+et



# 3. 
#Podzial na in-sample i out-of-sample

# do procesu wygładzenia danych wykorzystamy okres bez ostatnich 4 obserwacji
# t < 56
# a następnie oszacujemy prognozy dla tego okresu (ostatnie 4 obserwacje)

# do zawężenia zbioru obserwacji możemy wykorzystać funkcję window()

#pr�ba in-sample
bsales.ts.train <- 
  window(bsales.ts,
         end = 56 )

#okres out of sample
bsales.ts.test <- 
  window(bsales.ts,
         start = 57 )

#  4. 
# Proste wygladzanie wykladniczea (bez trendu, bez efektów sezonowych)

# a. Oszacowanie modelu na probie in sample

# Proste wygladzanie wykladnicze (bez trendu, bez efektów sezonowych)
bsales.SES_ <- HoltWinters(bsales.ts.train,
                            alpha = 0.8,
                            beta  = FALSE, # beta jest czynnikiem trendu
                            gamma = FALSE) # gamma jest czynnikiem sezonowym


#automatycznie dobiera alphe zeby b��dy ex-post by�y najmniejsze
bsales.SES <- HoltWinters(bsales.ts.train,
                           beta  = FALSE, # beta jest czynnikiem trendu
                           gamma = FALSE) # gamma jest czynnikiem sezonowym

bsales.SES # info o modelu


# wykres na probie in-sample
plot(bsales.SES)


# wartości wygładzone przechowywane są w $fitted 
# (razem z poszczególnymi komponentami po zdekomponowaniu)

plot(bsales.SES$fitted)
#level - wyjsciowy szereg


# pierwsza kolumna w $fitted zawiera wartości wygładzone 

#xhat to pierwsza kolumna; wartosci dopasowane
plot(bsales.SES$fitted[, 1]) #prognoza w in-sample

# b. Policzenie prognozy

# policzmy prognozę na 4 obserwacje do przodu
bsales.SES.forecast <- predict(bsales.SES,
                                n.ahead = 4,
                                prediction.interval = TRUE)



# c. Porównanie 

# porównamy prognozę z oryginalnym szeregiem
plot(bsales.ts)
lines(bsales.SES.forecast[, 1], col = "blue") # prognozy 
lines(bsales.SES.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności dla prognozy
lines(bsales.SES.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności dla prognozy
abline(v = 57, lty = 2) # dodajemy pionową linię referencyjną (zeby zobaczyc okres out-of-sample)


# 5. 
# utwórzmy teraz szereg z wartościami wygładzonymi dla okresu in-sample
# oraz prognozami dla okresu out-of-sample

bsales.SES$fitted[, 1] #obserwacje historyczne

# extend	
# If true, the start and end values are allowed to extend the series. 
# If false, attempts to extend the series give a warning and are ignored

bsales.SES.summary <- 
  window(bsales.SES$fitted[, 1], end = 60, extend = TRUE) #extend=true, przygotowujemy miejsce do wpisania prognozy

# w miejsce braków danych na końcu wstawiamy prognozy 
#chcemy nasze prognozy (4 obserwacje) chcemy uzupelnic poprzednim szeregiem
window(bsales.SES.summary, start = 57) <-
  bsales.SES.forecast[, 1]




##############################
# 2. Metoda Holta
##############################

#  1. 
#  Wizualizacja szeregu 

plot(bsales.ts,
     main = "Sprzedaz",
     xlab = "data",
     ylab = "Sprzedaz")

abline(lm(bsales.ts~time(bsales.ts)), col="red", lty = 2)          # dodajemy pozioma linie referencyjna

# Yt=b1+b2*trend+et

# Rozwazmy model postaci: Yt=b1t+b2t*trend+et

#  2. 
# modeli liniowy Holta (bez efektów sezonowych)

# a. Oszacowanie modelu na probie in sample

# modeli liniowy Holta (bez efektów sezonowych)
bsales.Holt_ <- HoltWinters(bsales.ts.train,
                            alpha = 0.8,
                            beta  = 0.1,
                            gamma = FALSE) # gamma jest czynnikiem sezonowym
# jeżeli gamma= FALSE -> szacujemy wtedy model liniowy Holta

#automatyczna aplha i beta
bsales.Holt <- HoltWinters(bsales.ts.train,
                           gamma = FALSE) # gamma jest czynnikiem sezonowym

bsales.Holt # info o modelu

# wykres na probie in-sample
plot(bsales.Holt)


# wartości wygładzone przechowywane są w $fitted 
# (razem z poszczególnymi komponentami po zdekomponowaniu)
plot(bsales.Holt$fitted)
#xhat - wartosci prognozowane
#level - F_t
#trend - C_t

# pierwsza kolumna w $fitted zawiera wartości wygładzone 

plot(bsales.Holt$fitted[, 1])


# b. Policzenie prognozy

# policzmy prognozę na 4 obserwacje do przodu
bsales.Holt.forecast <- predict(bsales.Holt,
                                n.ahead = 4,
                                prediction.interval = TRUE)


# c. Porównanie 

# porównamy prognozę z oryginalnym szeregiem
plot(bsales.ts)
lines(bsales.Holt.forecast[, 1], col = "blue") # prognozy 
lines(bsales.Holt.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności dla prognozy
lines(bsales.Holt.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności dla prognozy
abline(v = 57, lty = 2) # dodajemy pionową linię referencyjną


# Jak możemy ocenić uzyskaną prognozę?
#

# 3. 
# utwórzmy teraz szereg z wartościami wygładzonymi dla okresu in-sample
# oraz prognozami dla okresu out-of-sample

bsales.Holt$fitted[, 1]

bsales.Holt.summary <- 
  window(bsales.Holt$fitted[, 1], end = 60, extend = TRUE)

# w miejsce braków danych na końcu wstawiamy prognozy 
window(bsales.Holt.summary, start = 57) <-
  bsales.Holt.forecast[, 1]


# 4.

# łączymy wszystkie kolumny razem 
bsales.summary <- ts.union(bsales.ts,
                           bsales.SES.summary, #model EWMA
                           bsales.Holt.summary)

#porownanie prognozy
plot(bsales.ts)
lines(bsales.Holt$fitted[, 1], col = "blue")
lines(bsales.Holt.forecast[, 1], col = "blue") # prognozy 
lines(bsales.SES$fitted[, 1], col = "red")
lines(bsales.SES.forecast[, 1], col = "red") # prognozy 
abline(v = 57, lty = 2) # dodajemy pionową linię referencyjną


# utworzymy także zmienną binarną dzielącą cały zbiór obserwacji
# na okresy in-sample i out-of-sample

library(xts)
bsales.summary = as.xts(bsales.summary)

ifelse(index(bsales.summary) < "0057-01-01", 0, 1)

sample_period <-
  ts(ifelse(index(bsales.summary) < "0057-01-01", 0, 1), 
     start  =1, freq = 1)

names(bsales.summary)

# nazwy trochę uprościmy
names(bsales.summary)<-c("Sales","SES","Holt")

# utworzymy też zmienną indeksującą czas (date)
bsales.summary$date <- index(bsales.ts)

bsales.summary$sample_period <- sample_period




# 5.
# porownanie przecietnych bledow prognoz w okresie out-of-sample 
# z tymi z okresu in-sample

bsales.summary$mae_SES     <- abs(bsales.summary$SES-bsales.summary$Sales)
bsales.summary$mse_SES     <- (bsales.summary$SES-bsales.summary$Sales)^2
bsales.summary$mape_SES    <- abs((bsales.summary$SES-bsales.summary$Sales)/bsales.summary$Sales)
bsales.summary$amape_SES   <- abs((bsales.summary$SES-bsales.summary$Sales)/(bsales.summary$SES+bsales.summary$Sales))



bsales.summary$mae_Holt     <- abs(bsales.summary$Holt-bsales.summary$Sales)
bsales.summary$mse_Holt     <- (bsales.summary$Holt-bsales.summary$Sales)^2
bsales.summary$mape_Holt    <- abs((bsales.summary$Holt-bsales.summary$Sales)/bsales.summary$Sales)
bsales.summary$amape_Holt   <- abs((bsales.summary$Holt-bsales.summary$Sales)/(bsales.summary$Holt+bsales.summary$Sales))


# nastepnie je usredniamy 
# oddzielnie dla okresu in-sample oraz dla okresu prognozy (out-of-sample)

names(bsales.summary)

# wartosci do usrednienie mamy w kolumnach 6:13
# musimy zatem uwzgladniac braki danych (zignorowac je w obliczeniach)

#b��dy prognozy ex-post
aggregate(bsales.summary[, 6:13],
          by = list(bsales.summary$sample_period),
          FUN = function(x) mean(x, na.rm = T))

# 1 - okres out of sample
# mae i mse bo szereg przyjmuje wartosci powyzej 1; model holta ma nizesze wartosci w okresie out of sample 
# w modelu ses nizsze wartosci in sample


# UWAGA! model z najlepszym dopasowaniem w okresie in-sample
# wcale nie musi generować najlepszej prognozy!


###########################
# 3. Metoda Holta-Wintersa (model tam gdzie szeregi s� sezownowe)
###########################

#  1. 
# Importujemy dane z pliku "visits.csv" 


# import pliku CSV
visits <- read.csv("DANE.csv",   # nazwa pliku
                   header = TRUE,    	# czy obserwacje w pierwszym wierszu?
                   sep = ",",    # separator kolumn
                   dec = ".")	  # separator dziesiatny


drops <- c('MSN' ,'Column_Order','Description', 'Unit')
df[ , !(names(df) %in% drops)]

# weryfikujemy strukture obiektu
str(visits)

# utworzymy obiekt typu ts (time series) dla danych 
visits.ts <- 
  ts(visits$visits, start = c(1985, 1), freq = 12)

visits.ts
str(visits.ts)

#  2. 
#  Wizualizacja szeregu 
#sezonowo�� multiplikatywna
plot(visits.ts,
     main = "Visits",
     xlab = "data",
     ylab = "Visits")


# 3.

# do procesu wygładzenia danych wykorzystamy okres bez ostatnich 12 obserwacji
# a następnie oszacujemy prognozy dla tego okresu (ostatnie 12 obserwacje)

# do zawężenia zbioru obserwacji możemy wykorzystać funkcję window()
visits.ts.train <- 
  window(visits.ts,
         end = c(2003, 12)) #in-sample

visits.ts.test <- 
  window(visits.ts,
         start = c(2004, 1)) #out-of sample minimum jeden cykl sezonowy, u nas: 12 obserwacji bo dane miesieczne

# 4.
# addytywny model Holta-Wintersa 
visits.HWadd <- HoltWinters(visits.ts.train,
                             seasonal = "additive") #szereg in-sample

# multiplikatywny model Holta-Wintersa - u nas jest sezonowosc multiplikatywna, wiec wykorzystujemy ten kod
visits.HWmult <- HoltWinters(visits.ts.train,
                              seasonal="multiplicative")
visits.HWmult # info o modelu; beta bliska zero wiec nie ma duzego lokalnego trendu zmienijacego sie w czasie

plot(visits.HWmult)

# oszacujmy prognozę na 12 okresu naprzód
visits.HWmult.forecast <- predict(visits.HWmult,
                                   n.ahead = 12,
                                   prediction.interval = TRUE) #fit = prognoza, dalej dolny i gorny przedzia� ufnosci

# następnie porównajmy prognozę z oryginalnym szeregiem 
plot(visits.ts)
lines(visits.HWmult.forecast[, 1], col = "blue") # prognoza
lines(visits.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(visits.HWmult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2004, lty = 2) # dodajemy pionową linię referencyjną, gdzie sie zaczyna out-of-sample

# obejrzyjmy prognozę w zbliżeniu
length(visits.ts)

# zmienimy w tym celu jedynie pierwszą linię (pierwsze polecenie)
plot(window(visits.ts, start = c(2001, 12)))
lines(visits.HWmult.forecast[, 1], col = "blue")  
lines(visits.HWmult.forecast[, 2], col = "red", lty = 2) 
lines(visits.HWmult.forecast[, 3], col = "red", lty = 2) 
abline(v = 2004, lty = 2) 

#bledy prognozy ex-post


###########################
# Praca domowa # 3
###########################

##################################################################################
# Analiza cen energii elektrycznej w USA 
# Żródło: Energy Information Administration, https://www.eia.gov/totalenergy/data/browser/?tbl=T02.01#/?f=M
##################################################################################

# a. 
# Prosze zaimportowac dane z pliku Table_2.1_Energy_Consumption_by_Sector dzielac na dwa zbiory:
# 1. Zbior krotki do 60 obserwacji ostatnich czyli aktualnych
# 2. zbior dlugi czyli caly szereg


# b. 
# Prosze stwórzyc wykresy szeregów dla czterech szeregów: 
#Total Energy Consumed by the Residential Sector
#Total Energy Consumed by the Commercial Sector
#Total Energy Consumed by the Industrial Sector
#Total Energy Consumed by the Transportation Sector

# Która wersja modelu ekstrapolacyjnego wydaje się odpowiednia
# w poszczególnych przypadkach?

# c. 
# Prosze podzielic na okres in-sample and out-of-sample 
# Prosze dopasac odpowiedni model a następnie wygenerowac prognozę
# dla każdego z szeregów. 

# d. 
# Prosze przedstawic prognozy szeregów czterech szeregow 
# na wykresie i porównac je z faktycznie zrealizowanymi wartościami
# zmiennej zależnej.

# e. 
# Dla którego szeregu uzyskano najbardziej precyzyjne prognozy?
# Które z miar jakości prognozy ex-post można uznać za odpowiednie?
