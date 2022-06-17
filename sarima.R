###########################################################################
# Model SARIMA(p,d,q)(P,D,Q)                                              #
###########################################################################

# wczytanie bibliotek
library(foreign)
library(xts)
library(urca)
library(lmtest)
library(fBasics)
library(forecast)
library(tseries)
library(fUnitRoots)
library(readxl)
library(zoo)
library(ggplot2)
options(scipen=999)

library(rcompanion)
par(mfrow=c(1,1))

# wczytanie funkcji do wykorzystania w analizie
source("funs/testdf.R")

###########################################################################
# I. Przygogowanie danych                                                 #
###########################################################################

# 1. Import danych

dfs <- read_excel("sez.xls")

class(dfs) #obiekt typu "data frame"
str(dfs)

# przekonwertujmy dane do obiektu typu xts
dfs.xts <- xts(dfs$Value,   # kolumny z danymi
               dfs$Date)  # kolumny z datą/czasem
names(dfs.xts)[1] <- "dfs"
tail(dfs.xts)

# 2. Ustalamy okres IN-SAMPLE (out of sample to jeden cykl = 12 miesiecy albo 4 kwartały)
dfs.in.xts <- window(dfs.xts, end = as.Date("2020-12-01"))
tail(dfs.in.xts)

###########################################################################
# PRACUJEMY NA PROBIE IN-SAMPLE                                           #
###########################################################################

# 3. wykresy szeregu wejsciowego i pierwszych roznic

# wykresy szeregu wejsciowego
par(mfrow=c(2,1))
plot(dfs.in.xts$dfs)
# widoczna sezonowść i trend liniowy

tsdisplay(dfs.in.xts$dfs)

par(mfrow = c(2, 1))
acf(dfs.in.xts$dfs,  lag.max = 36,
    xlim=c(1,36),
    lwd = 4,
    col = "red")
pacf(dfs.in.xts$dfs, lag.max=36,
     xlim=c(1,36),
     lwd = 4,
     col = "red")
par(mfrow = c(1, 1))

# wprowadzenie roznic regularnych (pierwsze roznice), sezonowosc addytywna
dfs.in.xts$diff.dfs.in <- diff.xts(dfs.in.xts$dfs, lag = 1)
plot(dfs.in.xts$diff.dfs.in)
tsdisplay(dfs.in.xts$diff.dfs.in)

par(mfrow = c(2, 1))
acf(dfs.in.xts$diff.dfs.in,  lag.max = 36,
    xlim=c(1,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(dfs.in.xts$diff.dfs.in, lag.max=36,
     xlim=c(1,36),
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

#wahania wyrownane

# Pierwsze różnice wykazują silną sezonowość
# (wolno wygasająca ACF wielokrotności 12-tego opóźnienia)
# A zatem dodatkowo różnicujemy szereg sezonowo, tj. obliczamy dwunaste różnice
# (ponieważ mamy dane miesięczne).


# 6. wprowadzenie roznic sezonowych
dfs.in.xts$d12.diff.dfs.in <- diff.xts(dfs.in.xts$diff.dfs.in, lag = 12) #dla roznic kwartalnych lag=4
plot(dfs.in.xts$d12.diff.dfs.in)


# Obejrzyjmy korelogramy dla roznic sezonowych
tsdisplay(dfs.in.xts$d12.diff.dfs.in )

#nie ma niestacjonarnosci regularnej i sezonowej


# 7. Formalne testowanie
# Krok 1. Test Dickeya, Haszy, Fullera (DHF) 

dfs.in.xts$d12.dfs.in <- diff.xts(dfs.in.xts$dfs, lag = 12) #zmienna na róznicach sezonowych
dfs.in.xts$lag12.dfs.in <- lag.xts(dfs.in.xts$dfs, k = 12) # zmienna opozniona o 12 okresów

plot(dfs.in.xts$d12.dfs.in)
plot(dfs.in.xts$lag12.dfs.in)

model1=lm(d12.dfs.in ~ lag12.dfs.in, data=dfs.in.xts)  #jaknie ma stałej to piszemy zero
summary(model1)   # ro12 - 0,021961 => (estimate)  | ale poczatkowo test na autokorelacje
bg1 <- bgtest(model1, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy test D-H-F


# Test ADHF
dfs.in.xts$lag1.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 1) #zmienna zalezna opozniona o 1 okres

#dokladamy zmienną na pierwszych roznicach 
model2=lm(d12.dfs.in ~ lag12.dfs.in + lag1.d12.dfs.in, data=dfs.in.xts)
summary(model2)
bg1 <- bgtest(model2, order = 1)
bg1
# p-value < 0.05 -> rozszerzamy znow test A-D-H-F

dfs.in.xts$lag2.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 2) # generuje opoznienie lag=2

model3=lm(d12.dfs.in ~ lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in, data=dfs.in.xts)
summary(model3)
bg1 <- bgtest(model3, order = 1)
bg1 #brak podstaw do odrzucenia h0 bo p-value >0.05
bg2 <- bgtest(model3, order = 2)
bg2 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F
bg3 <- bgtest(model3, order = 3)
bg3


dfs.in.xts$lag3.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 3) # generuje opoznienie lag=3

model4=lm(d12.dfs.in ~ lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in, data=dfs.in.xts)
summary(model4)
bg1 <- bgtest(model4, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F


dfs.in.xts$lag4.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 4) # generuje opoznienie lag=4

model5=lm(d12.dfs.in ~ lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in + lag4.d12.dfs.in, data=dfs.in.xts)
summary(model5)
bg1 <- bgtest(model5, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F



dfs.in.xts$lag5.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 5) # generuje opoznienie lag=5

model6=lm(d12.dfs.in ~ lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in + lag4.d12.dfs.in +
            lag5.d12.dfs.in, data=dfs.in.xts)
summary(model6)
bg1 <- bgtest(model6, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F
bg2 <- bgtest(model6, order = 2)
bg2 
bg3 <- bgtest(model6, order = 3)
bg3 
bg4 <- bgtest(model6, order = 4)
bg4 

dfs.in.xts$lag6.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 6) # generuje opoznienie lag=6

model7=lm(d12.dfs.in ~ 0+lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in + lag4.d12.dfs.in +
            lag5.d12.dfs.in + lag6.d12.dfs.in, data=dfs.in.xts)
summary(model7)
bg1 <- bgtest(model7, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F
bg2 <- bgtest(model7, order = 2)
bg2 
bg3 <- bgtest(model7, order = 3)
bg3 
bg4 <- bgtest(model7, order = 4)
bg4 

dfs.in.xts$lag7.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 7) # generuje opoznienie lag=6

model8=lm(d12.dfs.in ~ lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in + lag4.d12.dfs.in +
            lag5.d12.dfs.in + lag6.d12.dfs.in +lag7.d12.dfs.in, data=dfs.in.xts)
summary(model8)
bg1 <- bgtest(model8, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F
bg2 <- bgtest(model8, order = 2)
bg2 
bg3 <- bgtest(model7, order = 3)
bg3 
bg4 <- bgtest(model7, order = 4)
bg4 


dfs.in.xts$lag8.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 8) # generuje opoznienie lag=8

model9=lm(d12.dfs.in ~ 0+lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in + lag4.d12.dfs.in +
            lag5.d12.dfs.in + lag6.d12.dfs.in +lag7.d12.dfs.in + lag8.d12.dfs.in, data=dfs.in.xts)
summary(model9)
bg1 <- bgtest(model9, order = 1)
bg1 
bg2 <- bgtest(model9, order = 2)
bg2 
bg3 <- bgtest(model9, order = 3)
bg3 
bg4 <- bgtest(model9, order = 4)
bg4 
bg5 <- bgtest(model9, order = 5)
bg5


dfs.in.xts$lag9.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 9) # generuje opoznienie lag=9

model10=lm(d12.dfs.in ~ 0 + lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in + lag4.d12.dfs.in +
            lag5.d12.dfs.in + lag6.d12.dfs.in +lag7.d12.dfs.in + lag8.d12.dfs.in + lag9.d12.dfs.in, data=dfs.in.xts)
summary(model10)
bg1 <- bgtest(model10, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F
bg2 <- bgtest(model10, order = 2)
bg2 
bg3 <- bgtest(model10, order = 3)
bg3 
bg4 <- bgtest(model10, order = 4)
bg4 


dfs.in.xts$lag10.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 10) # generuje opoznienie lag=10

model11=lm(d12.dfs.in ~ lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in + lag4.d12.dfs.in +
             lag5.d12.dfs.in + lag6.d12.dfs.in +lag7.d12.dfs.in + lag8.d12.dfs.in + lag9.d12.dfs.in + lag10.d12.dfs.in, data=dfs.in.xts)
summary(model11)
bg1 <- bgtest(model11, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F
bg2 <- bgtest(model10, order = 2)
bg2 
bg3 <- bgtest(model10, order = 3)
bg3 
bg4 <- bgtest(model10, order = 4)
bg4 


dfs.in.xts$lag11.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 11) # generuje opoznienie lag=10

model12=lm(d12.dfs.in ~ lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in + lag4.d12.dfs.in +
             lag5.d12.dfs.in + lag6.d12.dfs.in +lag7.d12.dfs.in + lag8.d12.dfs.in + lag9.d12.dfs.in + lag10.d12.dfs.in +
             lag11.d12.dfs.in, data=dfs.in.xts)
summary(model12)
bg1 <- bgtest(model12, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F


dfs.in.xts$lag12.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 12) # generuje opoznienie lag=10

model13=lm(d12.dfs.in ~ lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in + lag4.d12.dfs.in +
             lag5.d12.dfs.in + lag6.d12.dfs.in +lag7.d12.dfs.in + lag8.d12.dfs.in + lag9.d12.dfs.in + lag10.d12.dfs.in +
             lag11.d12.dfs.in + lag12.d12.dfs.in, data=dfs.in.xts)
summary(model13)
bg1 <- bgtest(model13, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F


dfs.in.xts$lag13.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 13) # generuje opoznienie lag=13

model14=lm(d12.dfs.in ~ lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in + lag4.d12.dfs.in +
             lag5.d12.dfs.in + lag6.d12.dfs.in +lag7.d12.dfs.in + lag8.d12.dfs.in + lag9.d12.dfs.in + lag10.d12.dfs.in +
             lag11.d12.dfs.in + lag12.d12.dfs.in + lag13.d12.dfs.in, data=dfs.in.xts)
summary(model14)
bg1 <- bgtest(model14, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F

dfs.in.xts$lag14.d12.dfs.in <- lag.xts(dfs.in.xts$d12.dfs.in, k = 14) # generuje opoznienie lag=14

model15=lm(d12.dfs.in ~ lag12.dfs.in + lag1.d12.dfs.in + lag2.d12.dfs.in + lag3.d12.dfs.in + lag4.d12.dfs.in +
             lag5.d12.dfs.in + lag6.d12.dfs.in +lag7.d12.dfs.in + lag8.d12.dfs.in + lag9.d12.dfs.in + lag10.d12.dfs.in +
             lag11.d12.dfs.in + lag12.d12.dfs.in + lag13.d12.dfs.in + lag14.d12.dfs.in, data=dfs.in.xts)
summary(model15)
bg1 <- bgtest(model15, order = 1)
bg1 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F
bg2 <- bgtest(model15, order = 2)
bg2 # p-value < 0.05 -> rozszerzamy znow test A-D-H-F

#test DF

# H0: zmienna diff.d12  jest zmienna niestacjonarna

plot(dfs.in.xts$d12.diff.dfs.in)
testdf(variable = dfs.in.xts$d12.diff.dfs.in, ADF_type="nc", ADF_max_order = 3, BG_max_order = 6)
tsdisplay(dfs.in.xts$d12.diff.dfs.in)

# dla drugiego rozszerzenia -> p-value dla bg > 0.05, p-value dla adf < 0.05 
#wykres waha się wokół zera, wiec 'nc'

# d=1 - roznice regularne, D=1 - roznce sezonowe

# KPSS
library(urca)

dfs.in.kpss.test <- ur.kpss(dfs.in.xts$d12.diff.dfs.in, type = c("mu"))  # stała w równaniu testowym
summary(dfs.in.kpss.test)

# statystyka testowa (0,0827) > 0,463 => brak podstaw do odrzucenia H0 o stacjonarnosci zmiennej

# 8. Czy zmienna jest bialym szumem?

# H0: zmienna jest bialym szumem

# Test Ljung-Boxa
Box.test(dfs.in.xts$d12.diff.dfs.in, type = "Ljung-Box", lag = 36) # sezonowy badamy do 3*s = 3*12 (dla nsez lag = 24)
# p - value < 0,05  - odrzycamy h0 ze bialy szum

# Test Boxa-Pierce
Box.test(dfs.in.xts$d12.diff.dfs.in, type = "Box-Pierce", lag = 36)

###########################################################################
# II. Identyfikacja                                                       #
###########################################################################

# IDENTYFIKACJA rzędów P i Q

# Analiza korelogramow ACF i PACF dla szeregu d12dlair

tsdisplay(dfs.in.xts$d12.diff.dfs.in)

# sarima(1,1,1)(1,1,1)

###########################################################################
# III. Estymacja                                                          #
###########################################################################

# SARIMA(1,1,1)(1,1,1)
arima111111 <- arima(dfs.in.xts$dfs,
                     # rzędy (p,d,q)
                     order = c(1, 1, 1),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)
)

arima111111
coeftest(arima111111)

# wszystkie nieznaczące, nabardziej nieznaczący sar(1)
tsdisplay(resid(arima111111))


#acf wypustki sezonowe - znajduja sie w przedziale ufnosci - sa ok
# pacf wypustki sezonowe - znajduja sie w przedziale ufnosci - wymodelowane

#usuwamy sar(1)

# Estymacja modelu SARIMA(1,1,1)(0,1,1)

arima111011 <- arima(dfs.in.xts$dfs,
                     # rzędy (p,d,q)
                     order = c(1, 1, 1),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)
)

arima111011
coeftest(arima111011)

# wszystkie nieznaczące, nabardziej nieznaczący ma(1)
tsdisplay(resid(arima111011))

# test LR
teststat<- 2*(as.numeric(logLik(arima111111))-as.numeric(logLik(arima111011)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )
#nie ma postaw do odrzucenia h0, bo p-value (0,9017993)>0,05

#skracamy model dalej

# Estymacji modelu SARIMA(1,1,0)(0,1,1)

arima110011 <- arima(dfs.in.xts$dfs,
                     # rzędy (p,d,q)
                     order = c(1, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)
)

arima110011
coeftest(arima110011)

# wszystkie istotne
tsdisplay(resid(arima110011))

# test LR
teststat<- 2*(as.numeric(logLik(arima111011))-as.numeric(logLik(arima110011)))
teststat

pchisq(teststat, df=2, lower.tail = FALSE )
#nie ma postaw do odrzucenia h0, bo p-value (0.8350851)>0,05

#skracamy model dalej

# Estymacji modelu SARIMA(0,1,0)(0,1,1)

arima010011 <- arima(dfs.in.xts$dfs,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12)
)

arima010011
coeftest(arima010011)

# wszystkie istotne
tsdisplay(resid(arima010011))
# autokorelacja 

# test LR
teststat<- 2*(as.numeric(logLik(arima110011))-as.numeric(logLik(arima010011)))
teststat

pchisq(teststat, df=3, lower.tail = FALSE )
#odrzucamy h0, bo p-value (0.0003422686)<0,05


# PODSUMOWUJAC: czesc sezonowa SARIMA(0,1,0)(0,1,1) - procedura od ogolu do szczegolu

# wartości AIC
AIC(arima111111, arima111011, arima110011, arima010011)
#czesc sezonowa SARIMA(1,1,0)(0,1,1) - AIC - najmnijesza wartosc
# wartosci BIC
BIC(arima111111, arima111011, arima110011, arima010011)
#czesc sezonowa SARIMA(1,1,0)(0,1,1) - BIC - najmniejsza wartosc


# white noise, mozemy skracac

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima011011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima011011), type = "Box-Pierce", lag = 36)




###########################################################################
# IV. Diagnostyka                                                         #
###########################################################################

# reszty są białym szumem?

# test Ljung-Boxa
Box.test(resid(arima010011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima010011), type = "Box-Pierce", lag = 36)
# reszty nie są białym szumem

Box.test(resid(arima110011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima110011), type = "Box-Pierce", lag = 36)
# reszty wydają się być białym szumem

###########################################################################
# V. Prognoza                                                             #
###########################################################################

# SARIMA(1,1,0)(0,1,1)
arima110011 <- arima(dfs.in.xts$dfs,
                     order = c(1, 1, 0),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)


forecast <- predict(arima110011, n.ahead = 12)

# obejrzyjmy wyniki
forecast
str(forecast)

forecast_pred = ts(data=forecast$pred, frequency = 12,             
                   start=c(2021,1), end=c(2021,12)) 

forecast_pred

forecast_se = ts(data=forecast$se, frequency = 12,             
                 start=c(2021,1), end=c(2021,12)) 


# wykres prognozy
plot(dfs, main = "12-miesięczna prognoza dla liczby pasażerów na pokładach samolotów w Polsce")
abline(v = 2021, lty = 2, col = "gray")
lines(forecast_pred, col = "red", lwd = 2)
lines(forecast_pred + 2 * forecast_se, col = "red", lty = 3)
lines(forecast_pred - 2 * forecast_se, col = "red", lty = 3)

# i raz jeszcze,w zbliżeniu
plot(dfs, main = "12-miesięczna prognoza dla liczby pasażerów na pokładach samolotów w Polsce",
     xlim = c(2020, 2022))
abline(v = 2021, lty = 2, col = "gray")
lines(forecast_pred, col = "red", lwd = 2)
lines(forecast_pred + 2 * forecast_se, col = "red", lty = 3)
lines(forecast_pred - 2 * forecast_se, col = "red", lty = 3)

# łączymy prognozy z oryginalnym szeregiem
dfs.forecast <- data.frame(forecast = forecast$pred,
                             real = window(dfs,
                                           start = c(2021,1))
)
dfs.forecast

str(dfs.forecast)

###########################################################################
# VI. JAKOSC PROGNOZY                                                     #
###########################################################################

dfs.forecast$mae <- abs(as.numeric(dfs.forecast$real) - dfs.forecast$forecast)
dfs.forecast$mse <- (as.numeric(dfs.forecast$real) - dfs.forecast$forecast) ^ 2
dfs.forecast$mape <- abs((as.numeric(dfs.forecast$real) - dfs.forecast$forecast) /
                             as.numeric(dfs.forecast$real))
dfs.forecast$amape <- abs((as.numeric(dfs.forecast$real) - dfs.forecast$forecast) /
                              (as.numeric(dfs.forecast$real) + dfs.forecast$forecast))

str(dfs.forecast[, 3:6])

colMeans(dfs.forecast[, 3:6])

# aby zmienić 'naukowy' format liczb
# możemy skorzystać z opcji scipen
options(scipen = 5)
round(colMeans(dfs.forecast[, 3:6]), 3)


# jesli w arima robimy logarytm, to trzeba w ekstrapolacyjnych zrobic logarytm zeby moc porownać
# sarima lepiej prognozuje niz modelel ekstrapolacyjne, a w nsez raczej wygrywaja modele ekstrapolacyjne


