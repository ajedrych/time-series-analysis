# LIBRARIES


# wczytanie pakietow do pamieci

library(foreign)
library(xts)
library(urca)
library(lmtest)
library(fBasics)
library(forecast)
library(tseries)
library(fUnitRoots)


# wczytanie funkcji do wykorzystania w analizie
source("funs/testdf.R")

library(readxl)
library(zoo)
library(ggplot2)
options(scipen=999)

library(rcompanion)
par(mfrow=c(1,1))


################################################################################
##################### DANE SEZONOWE - LICZBA PASAZEROW #########################

dfs <- read_excel("sez.xls")
par(mfrow=c(1,1))

class(dfs)
is.ts(dfs)
dfs

#obiekt time-series
dfs <- xts(dfs$Value,   
               dfs$Date) 

names(dfs)[1] <- "Value"
dfs

# okres in-sample
dfs.in <- window(dfs, end = as.Date("2020-12-01"))
tail(dfs.in)

#okres out-of-sample
dfs.out <- window(dfs, start = as.Date("2021-01-01"))
dfs.out


plot(dfs.in)

# pierwsze różnice (różnicowanie regularne)
diff.dfs.in <- diff(dfs.in)

plot(diff.dfs.in)

# logarytm
ln.dfs.in <- log(dfs.in)

plot(ln.dfs.in)


# 2. Ustalamy okres IN-SAMPLE
air_sample.xts <- window(air.xts, end = as.Date("1959-12-01"))
tail(air_sample.xts)

# 1 cykl na out of sample, tutaj 12 obserwacji  

# tu koniec
###########################################################################
# PRACUJEMY NA PROBIE IN-SAMPLE                                           #
###########################################################################

# 3. wykresy szeregu wejsciowego i pierwszych roznic

# wykresy szeregu wejsciowego
plot(air_sample.xts)
# widoczna sezonowść i trend nieliniowy

# wprowadzenie roznic regularnych
air_sample.xts$dair <- diff.xts(air_sample.xts$air, lag = 1)
plot(air_sample.xts$dair)

# 4. logarytmujemy szereg ma to na celu zminiejszenie wariancji
air_sample.xts$lair <- log(air_sample.xts$air)
plot(air_sample.xts$lair)

# Obejrzyjmy korelogramy dla lair
par(mar = rep(2, 4))

par(mfrow = c(2, 1))
acf(air_sample.xts$lair,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(air_sample.xts$lair, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

# 5. wprowadzenie roznic regularnych lair
air_sample.xts$dlair <- diff.xts(air_sample.xts$lair, lag = 1)
plot(air_sample.xts$dlair)

# Obejrzyjmy korelogramy dla pierwszych różnic
par(mfrow = c(2, 1))
acf(air_sample.xts$dlair,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(air_sample.xts$dlair, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

# Pierwsze różnice wykazują silną sezonowość
# (wolno wygasająca ACF wielokrotności 12-tego opóźnienia)
# A zatem dodatkowo różnicujemy szereg sezonowo, tj. obliczamy dwunaste różnice
# (ponieważ mamy dane miesięczne).


# 6. wprowadzenie roznic sezonowych dla dlair
air_sample.xts$d12dlair <- diff.xts(air_sample.xts$dlair, lag = 12)
plot(air_sample.xts$d12dlair)

# Obejrzyjmy korelogramy dla roznic sezonowych dlair
par(mfrow = c(2, 1))
acf(air_sample.xts$d12dlair,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(air_sample.xts$d12dlair, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))

# 7. Formalne testowanie
# Krok 1. Test Dickeya, Haszy, Fullera (DHF) dla lair

air_sample.xts$d12lair <- diff.xts(air_sample.xts$lair, lag = 12)
air_sample.xts$lag12lair <- lag.xts(air_sample.xts$lair, k = 12)

plot(air_sample.xts$d12dlair)

model1=lm(d12lair~0+lag12lair, data=air_sample.xts)
summary(model1)
bg1 <- bgtest(model1, order = 1)
bg1


# Test ADHF dla lair
air_sample.xts$lagd12lair <- lag.xts(air_sample.xts$d12lair, k = 1)

model2=lm(d12lair~0+lag12lair+lagd12lair, data=air_sample.xts)
summary(model2)
bg1 <- bgtest(model2, order = 1)
bg1

air_sample.xts$lag2d12lair <- lag.xts(air_sample.xts$d12lair, k = 2)

model3=lm(d12lair~0+lag12lair+lagd12lair+lag2d12lair, data=air_sample.xts)
summary(model3)
bg1 <- bgtest(model3, order = 1)
bg1
bg2 <- bgtest(model3, order = 2)
bg2
bg3 <- bgtest(model3, order = 3)
bg3
bg4 <- bgtest(model3, order = 4)
bg4
bg5 <- bgtest(model3, order = 5)
bg5
bg6 <- bgtest(model3, order = 6)
bg6

# Podsumowując test ADHF:
# Statystyka testowa: 2.650
# Statystyka krytyczna: -5.86
# Decyzja: nie ma podstaw do odrzucenia H0, czyli róznice sezonowe sa potrzebne

#test DF
# H0: zmienna d12lair jest zmienna niestacjonarna

testdf(variable = air_sample.xts$d12lair ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 6)

# brak podstaw do odrzucenia H0.

#test DF
# H0: zmienna d12dlair jest zmienna niestacjonarna

testdf(variable = air_sample.xts$d12dlair ,ADF_type="nc", ADF_max_order = 3, BG_max_order = 6)

# odrzucamy H0.

# podsumowanie: otrzymaliśmy zatem szereg stacjonarny.
# d=1, D=1

# nalezy dodatkowo przeprowadzic test KPSS

# 8. Czy zmienna d12dlair jest bialym szumem?

# H0: zmienna d12dlair jest bialym szumem

# Test Ljung-Boxa
Box.test(air_sample.xts$d12dlair, type = "Ljung-Box", lag = 36)

# Test Boxa-Pierce
Box.test(air_sample.xts$d12dlair, type = "Box-Pierce", lag = 36)

# ACF i PACF
par(mfrow = c(2, 1))
acf(air_sample.xts$d12dlair,  lag.max = 36, 
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(air_sample.xts$d12dlair, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))



###########################################################################
# II. Identyfikacja                                                       #
###########################################################################

# IDENTYFIKACJA rzędów P i Q

# Analiza korelogramow ACF i PACF dla szeregu d12dlair

par(mfrow = c(2, 1))
acf(air_sample.xts$d12dlair,  lag.max = 36,
    xlim=c(2,36),
    lwd = 4,
    col = "red", na.action = na.pass)
pacf(air_sample.xts$d12dlair, lag.max=36,
     lwd = 4,
     col = "red", na.action = na.pass)
par(mfrow = c(1, 1))


# Korelogramy sugerują, że możemy mieć do czynienia z sezonowym procesem MA
# (malejąca PACF dla wielokrotnośći 12 opóźnienia)

###########################################################################
# III. Estymacja                                                          #
###########################################################################
nobs <- length(air_sample.xts$lair)

# SARIMA(0,1,0)(1,1,1)
arima010111 <- arima(air_sample.xts$lair,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(1, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)





arima010111
coeftest(arima010111)
# parametr przy sezonowym efekcie SMA jest istotny

par(mfrow = c(2, 1))
acf(resid(arima010111), lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36), lwd = 4, col = "red")
pacf(resid(arima010111), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# Estymacji modelu SARIMA(0,1,0)(0,1,1)

arima010011 <- arima(air_sample.xts$lair,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010011
coeftest(arima010011)
# parametr przy sezonowym efekcie SMA jest istotny

par(mfrow = c(2, 1))
acf(resid(arima010011), lag.max = 36,
    ylim = c(-0.4, 0.4), lwd = 4, col = "red")
pacf(resid(arima010011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# test LR
teststat<- 2*(as.numeric(logLik(arima010111))-as.numeric(logLik(arima010011)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )

# Estymacji modelu SARIMA(0,1,0)(0,1,0)

arima010010 <- arima(air_sample.xts$lair,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 0),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010010
coeftest(arima010010)
# parametr przy sezonowym efekcie SMA jest istotny

# test LR
teststat<- 2*(as.numeric(logLik(arima010111))-as.numeric(logLik(arima010010)))
teststat

pchisq(teststat, df=2, lower.tail = FALSE )


# PODSUMOWUJAC: czesc sezonowa SARIMA(0,1,0)(0,1,1) - procedura od ogolu do szczegolu

# wartości AIC
AIC(arima010111, arima010011, arima010010)
#czesc sezonowa SARIMA(0,1,0)(0,1,1) - AIC
# wartosci BIC
BIC(arima010111, arima010011, arima010010) 
#czesc sezonowa SARIMA(0,1,0)(0,1,1) - BIC


# Estymacji modelu SARIMA(0,1,0)(0,1,1)

arima010011 <- arima(air_sample.xts$lair,
                     # rzędy (p,d,q)
                     order = c(0, 1, 0),
                     # rzędy sezonowe (P,D,Q)
                     seasonal = list(order = c(0, 1, 1),
                                     # częstotliwość danych (12 dla danych miesięcznych)
                                     period = 12),
                     xreg = 1:nobs       # dodatkowe regresory - stala
)

arima010011
coeftest(arima010011)
# parametr przy sezonowym efekcie SMA jest istotny

par(mfrow = c(2, 1))
acf(resid(arima010011), lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36), lwd = 4, col = "red")
pacf(resid(arima010011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))






# efekty sezonowe zostały wyjaśnione
# przystępujemy do identyfikacji efektóW regularnych

# SARIMA(1,1,1)(0,1,1)
arima111011 <- arima(air_sample.xts$lair,
                     order = c(1, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)
arima111011
coeftest(arima111011)

par(mfrow = c(2, 1))
acf(resid(arima111011),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
pacf(resid(arima111011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima111011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima111011), type = "Box-Pierce", lag = 36)


# reszty wydają się być białym szumem


# SARIMA(0,1,1)(0,1,1)
arima011011 <- arima(air_sample.xts$lair,
                     order = c(0, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)
arima011011
coeftest(arima011011)

par(mfrow = c(2, 1))
acf(resid(arima011011),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
pacf(resid(arima011011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima011011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima011011), type = "Box-Pierce", lag = 36)



# reszty wydają się być białym szumem

# test LR
teststat<- 2*(as.numeric(logLik(arima111011))-as.numeric(logLik(arima011011)))
teststat

pchisq(teststat, df=1, lower.tail = FALSE )

# SARIMA(0,1,0)(0,1,1)
arima010011 <- arima(air_sample.xts$lair,
                     order = c(0, 1, 0),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)
arima010011
coeftest(arima010011)

# test LR
teststat<- 2*(as.numeric(logLik(arima111011))-as.numeric(logLik(arima010011)))
teststat

pchisq(teststat, df=2, lower.tail = FALSE )


# PODSUMOWUJAC: ostateczny model SARIMA(0,1,1)(0,1,1) - procedura od ogolu do szczegolu

# wartości AIC
AIC(arima111011, arima011011, arima010011)
#czesc sezonowa SARIMA(0,1,1)(0,1,1) - AIC
# wartosci BIC
BIC(arima111011, arima011011, arima010011) 
#czesc sezonowa SARIMA(0,1,1)(0,1,1) - BIC


###########################################################################
# IV. Diagnostyka                                                         #
###########################################################################

# SARIMA(0,1,1)(0,1,1) - AIR MODEL
arima011011 <- arima(air_sample.xts$lair,
                     order = c(0, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)
arima011011
coeftest(arima011011)

par(mfrow = c(2, 1))
acf(resid(arima011011),  lag.max = 36,
    ylim = c(-0.4, 0.4), xlim=c(2,36),
    lwd = 4, col = "red")
pacf(resid(arima011011), lag.max = 36,
     lwd = 4, col = "red")
par(mfrow=c(1, 1))

# czy reszty są białym szumem?
# test Ljung-Boxa
Box.test(resid(arima011011), type = "Ljung-Box", lag = 36)
Box.test(resid(arima011011), type = "Box-Pierce", lag = 36)



# reszty wydają się być białym szumem

###########################################################################
# V. Prognoza                                                             #
###########################################################################

# SARIMA(0,1,1)(0,1,1)
arima011011 <- arima(air_sample.xts$lair,
                     order = c(0, 1, 1),
                     seasonal = list(order = c(0, 1, 1),
                                     period = 12)
)


forecast <- predict(arima011011, n.ahead = 12)

# obejrzyjmy wyniki
forecast
str(forecast)

# zbior zawiera 2 elementy:
# pred - prognozy
# se - błąd standardowy prognozy

air.xts$lair<-log(air.xts$air)

# wykres prognoz
ts.plot(air.xts[, 2],
        main = "12 months forecast of air")
# pocztek okresu prognozy
abline(v = 132, lty = 2, col = "gray")
lines(forecast$pred, col = "red", lwd = 2)
lines(forecast$pred + 2 * forecast$se, col = "red", lty = 3)
lines(forecast$pred - 2 * forecast$se, col = "red", lty = 3)

# obejrzyjmy z bliżeniu
ts.plot(air.xts[, 2],
        main = "12 months forecast of air", xlim = c(100, 144), ylim=c(5.6,6.6))
abline(v = 132, lty = 2, col = "gray")
lines(forecast$pred, col = "red", lwd = 2)
lines(forecast$pred + 2 * forecast$se, col ="red", lty = 3)
lines(forecast$pred - 2 * forecast$se, col ="red", lty = 3)

# łączymy prognozy z oryginalnym szeregiem
air_forecast <- data.frame(forecast = forecast$pred,
                           window(air.xts$lair,
                                  start = as.Date("1960-01-01")))
air_forecast

# 7.
# sprawdzamy jakość prognozy
air_forecast$mae <- abs(air_forecast$lair -
                          air_forecast$forecast)
air_forecast$mse <- (air_forecast$lair -
                       air_forecast$forecast)^2
air_forecast$mape <- abs((air_forecast$lair -
                            air_forecast$forecast) /
                           air_forecast$lair)
air_forecast$amape <- abs((air_forecast$lair -
                             air_forecast$forecast) /
                            (air_forecast$lair +
                               air_forecast$forecast))

colMeans(air_forecast[, 3:6])

# aby zmienić 'naukowy' format liczb
# możemy skorzystać z opcji scipen
options(scipen = 5)
round(colMeans(air_forecast[, 3:6]), 3)

