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
##################### DANE NIESEZONOWE - REALNY KURS WALUTOWY POLSKI ###########
################################################################################
df <- read_excel("niesez.xls")

class(df)
is.ts(df)

#obiekt time-series
df = ts(data=df$Value, frequency = 12,             
        start=c(1994,1), end=c(2021,12)) 

df.in <- 
  window(df,
         end = c(2021, 09))

df.out <-       # trzy obserwacje out-of-sample, bo szereg niesezonowy
  window(df,
         start = c(2021, 10))

# test Ljung-Box
Box.test(df.in, lag=24, type="Ljung-Box") #odrzucamy H0: df to biały szum, bo p-value <0.05

# test Box-Pierce
Box.test(df.in, lag=24, type="Box-Pierce") #odrzucamy H0: df to biały szum, bo p-value <0.05

# wykres
plot(df.in,
     type = "l",
     main = "Realny kurs walutowy dla Polski")

# pierwsze różnice
library(xts)
diff.df.in<-diff.xts(df.in)

par(mfrow=c(2,1))

plot(diff.df.in,
     type = "l",
     main = "Pierwsze różnice realnego kursu walutowego dla Polski")

#logarytmowanie w celu wyeliminowania zmieniającej się wariancji
ln.df.in<-log(df.in)
diff.ln.df.in<-diff.xts(ln.df.in)

plot(diff.ln.df.in,
     type = "l",
     main = "Pierwsze różnice dla logarytmu realnego kursu walutowego dla Polski")

par(mfrow=c(1,1))

# test dickeya-fullera, pierwsza forma funkcyjna (non constant)
install.packages("fUnitRoots")
library(fUnitRoots)
source("funs/TESTDF.R") 

testdf(variable = ln.df.in, ADF_type="nc", ADF_max_order = 5,BG_max_order = 6)
# augmentations = 0 =>  dla testow Breuscha-Godfreya p-value <0.05 => odrzucamy H0 o braku autokorelacji
# przechodzimy do testu ADF z jednym rozszerzeniem (augmentations = 1): dla testów BG p-value >0,05 => brak podstaw do odrzucenia
# H0 o braku autokorelacji
# patrzymy na p-value dla ADF = 0.8392272 > 0,05 => brak podstaw do odrzucenia H0 o tym, że ln.df.in jest zmienna niestacjonarna

# pierwsze różnice regularne dla zmiennej ln.df.in 

testdf(variable = diff.ln.df.in, ADF_type="nc", ADF_max_order = 5,BG_max_order = 6)
# augmentations = 0 => dla testów B-G p-value > 0.05 => brak podstaw do odrzucenia h0 o braku autokorelacji
# p-value dla testu D-F 0.01 < 0.05 => odrzucamy H0 o tym, że diff.ln.df jest zmienną niestacjonarna


# test KPSS
library(urca)

df.in.kpss.test <- ur.kpss(ln.df.in, type = c("mu"))  # stała w równaniu testowym
summary(df.in.kpss.test)
# statystyka testowa (2,4579) > 0,463 => odrzucamy H0 o stacjonarnosci ln.df
# przeprowadzamy test dla pierwszych różnic

diff.df.in.kpss.test <- ur.kpss(diff.ln.df.in, type = c("mu"))  # stała w równaniu testowym
summary(diff.df.in.kpss.test)
# statystyka testowa = 0.1624 < 0,463 => brak podstaw do odrzucenia h0 o stacjonarnosci diff.ln.df

# stopień integracji zmiennej ln.df.in wynosi 1
# stopień integracji zmiennej diff.ln.df.in wynosi 0 (zmienna stacjonarna)

# SPRAWDZAMY CZY ZMIENNA NIE JEST BIAŁYM SZUMEM
# test Ljung-Box
Box.test(diff.ln.df.in, lag=24, type="Ljung-Box") #brak podstaw do odrzucenia H0 o tym, że diff.ln.df.in to bialy szum, poniewaz p-value>0.05 

# test Box-Pierce
Box.test(diff.ln.df.in, lag=24, type="Box-Pierce") #brak podstaw do odrzucenia H0 o tym, że diff.ln.df.in to bialy szum, poniewaz p-value>0.05 

# według testów zmienna jest białym szumem

# sprawdzam korelogramy ACF i PACF
library(forecast)
tsdisplay(diff.ln.df.in, lag.max=24)

par(mfrow = c(1, 2))
Acf(diff.ln.df.in, lag.max = 24,
    lwd = 4, col = "red",
    na.action = na.pass)  
Pacf(diff.ln.df.in, lag.max =24 ,
     lwd = 4, col = "red",
     na.action = na.pass)

# według korelogramów możemy zaproponować model arima (1,0,1) dla zmiennej diff.ln.df.in
# (1,1,1) dla ln.df.in

# lub

# (5,0,5)/(5,1,5)

###########################################################################
# III. Estymacja                                                          #
###########################################################################

# Od ogółu do szczegółu 

# 1.
# Zaczynamy od estymacji modelu ARIMA(5,1,5) dla ln.df.in

arima515 <- arima(ln.df.in,  # zmienna zależna
                  order = c(5, 1, 5))  # rzędy (p,d,q))
arima515

coeftest(arima515)

# wartości AIC
AIC(arima515)
# -1664.886

# wartosci BIC
BIC(arima515)
# -1623.029

# Czy reszty są białym szumem?
par(mfrow = c(1, 2))
Acf(resid(arima515), lag.max = 24,
    ylim = c(-1, 1),
    xlim=c(1,24),
    lwd = 4, col = "red",
    na.action = na.pass)    # nie przerywaj, jeśli w danych brakujace wartości 
Pacf(resid(arima515), lag.max =24 ,
     lwd = 4, col = "red",
     na.action = na.pass) # nie przerywaj, jeśli w danych brakujace wartości 
# reszty są białym szumem

# czyste reszty, znajduja sie w przedziale ufnosci, brak znaczacych wypustek
par(mfrow = c(1, 1))

# Test Ljung-Boxa (do sensowego opóźnienia).
Box.test(resid(arima515), type = "Ljung-Box", lag = 24)
Box.test(resid(arima515), type = "Box-Pierce", lag = 24)
# p-values > 0.05, czyli reszty są białym szumem

# Porównajmy wyniki z innymi modelami na podstawie:
#   - test LR
#	- kryterium AIC, BIC
#	- istotności parametrów

# 2. 

arima514 <- arima(ln.df.in,  # zmienna zależna
                  order = c(5, 1, 4)  # rzędy (p,d,q)
)
arima514

coeftest(arima514)

# wartości AIC
AIC(arima514)
# -1663.086, większe niż arima515 -> gorsze

# wartosci BIC
BIC(arima514)
# -1625.035, mniejsze niż arima 515 -> lepsze

# test LR
teststat<- 2*(as.numeric(logLik(arima515))-as.numeric(logLik(arima514)))
teststat
# 3.799509

pchisq(teststat, df=1, lower.tail = FALSE )
# 0.05126761 > 0.05 -> usuwam dalej


# 3. 

arima414 <- arima(ln.df.in,  # zmienna zależna
                  order = c(4, 1, 4)  # rzędy (p,d,q)
)
arima414

coeftest(arima414)

# wartości AIC
AIC(arima414)
# -1665.01

# wartosci BIC
BIC(arima414)
# -1630.764

# test LR
teststat<- 2*(as.numeric(logLik(arima514))-as.numeric(logLik(arima414)))
teststat
# 0.07581623

pchisq(teststat, df=2, lower.tail = FALSE )
# 0.9628014 > 0.05 -> usuwam dalej

# 4. 

arima314 <- arima(ln.df.in,  # zmienna zależna
                  order = c(3, 1, 4)  # rzędy (p,d,q)
)
arima314

coeftest(arima314)

# wartości AIC
AIC(arima314)
# -1665.01

# wartosci BIC
BIC(arima314)
# -1633.135

# test LR
teststat<- 2*(as.numeric(logLik(arima414))-as.numeric(logLik(arima314)))
teststat
# 3.434076

pchisq(teststat, df=3, lower.tail = FALSE )
# 0.3294135 > 0.05 -> usuwam dalej

# 5.

arima313 <- arima(ln.df.in,  # zmienna zależna
                  order = c(3, 1, 3)  # rzędy (p,d,q)
)
arima313

coeftest(arima313)

# wartości AIC
AIC(arima313)
# -1664.202

# wartosci BIC
BIC(arima313)
# -1637.566

# test LR
teststat<- 2*(as.numeric(logLik(arima314))-as.numeric(logLik(arima313)))
teststat
# 1.374409

pchisq(teststat, df=4, lower.tail = FALSE )
# 0.8486304 > 0.05 -> usuwam dalej

# 6.

arima213 <- arima(ln.df.in,  # zmienna zależna
                  order = c(2, 1, 3)  # rzędy (p,d,q)
)
arima213

coeftest(arima213)

# wartości AIC
AIC(arima213)
# -1667.576

# wartosci BIC
BIC(arima213)
# -1644.745

# test LR
teststat<- 2*(as.numeric(logLik(arima313))-as.numeric(logLik(arima213)))
teststat
# -1.374329

pchisq(teststat, df=5, lower.tail = FALSE )
# 1 > 0.05 -> usuwam dalej

# 7. 

arima212 <- arima(ln.df.in,  # zmienna zależna
                  order = c(2, 1, 2)  # rzędy (p,d,q)
)
arima212

coeftest(arima212)

# wartości AIC
AIC(arima212)
# -1668.159

# wartosci BIC
BIC(arima212)
# -1649.133

# test LR
teststat<- 2*(as.numeric(logLik(arima213))-as.numeric(logLik(arima212)))
teststat
# 1.416995

pchisq(teststat, df=6, lower.tail = FALSE )
# 0.9648164 > 0.05 -> usuwam dalej


# 8.
arima112 <- arima(ln.df.in,  # zmienna zależna
                  order = c(1, 1, 2)  # rzędy (p,d,q)
)
arima112

coeftest(arima112)

# wartości AIC
AIC(arima112)
# -1669.271

# wartosci BIC
BIC(arima112)
# -1654.05

# test LR
teststat<- 2*(as.numeric(logLik(arima212))-as.numeric(logLik(arima112)))
teststat
# -1654.05

pchisq(teststat, df=7, lower.tail = FALSE )
# 0.9964306 > 0.05 -> usuwam dalej

# 9.
arima111 <- arima(ln.df.in,  # zmienna zależna
                  order = c(1, 1, 1)  # rzędy (p,d,q)
)
arima111

coeftest(arima111)

# wartości AIC
AIC(arima111)
#  -1667.647

# wartosci BIC
BIC(arima111)
# -1656.231

# test LR
teststat<- 2*(as.numeric(logLik(arima112))-as.numeric(logLik(arima111)))
teststat
# 3.623649

pchisq(teststat, df=8, lower.tail = FALSE )
# 0.9964306 > 0.05 -> usuwam dalej

# 10.

arima011 <- arima(ln.df.in,  # zmienna zależna
                  order = c(0, 1, 1)  # rzędy (p,d,q)
)
arima011

coeftest(arima011)

# wartości AIC
AIC(arima011)
#  -1669.529

# wartosci BIC
BIC(arima011)
# -1661.919

# test LR
teststat<- 2*(as.numeric(logLik(arima111))-as.numeric(logLik(arima011)))
teststat
# 0.1179527

pchisq(teststat, df=9, lower.tail = FALSE )
# 0.9999999> 0.05 -> usuwam dalej

# 11.

arima010 <- arima(ln.df.in,  # zmienna zależna
                  order = c(0, 1, 0)  # rzędy (p,d,q)
)
arima010

coeftest(arima010)

# wartości AIC
AIC(arima011)
#  -1669.529

# wartosci BIC
BIC(arima011)
# -1661.919

# test LR
teststat<- 2*(as.numeric(logLik(arima111))-as.numeric(logLik(arima011)))
teststat
# 0.1179527

pchisq(teststat, df=10, lower.tail = FALSE )
# 1 > 0.05 -> usuwam dalej


# PODSUMOWUJAC: ostateczny model ARIMA(0,1,1) - procedura od ogolu do szcZegolu

# wartości AIC
AIC(arima515, arima514, arima414, arima314, arima313, arima213, arima212, arima112, arima111, arima011, arima010)
#ostateczny model ARIMA(2,1,4) - AIC
# wartosci BIC
BIC(arima515, arima514, arima414, arima314, arima313, arima213, arima212, arima112, arima111, arima011, arima010)
#ostateczny model ARIMA(1,1,1) - BIC
# AIC - arima 011
# BIC - arima 011

###########################################################################
# IV. Diagnostyka                                                         #
###########################################################################

# 1.
# Estymacji modelu ARIMA(0,1,1)

arima011 <- arima(ln.df.in,  # zmienna zależna
                  order = c(0, 1, 1)  # rzędy (p,d,q)
)
arima011

coeftest(arima011)
# parametr ma1 jest istotny statystycznie

# wartości AIC
AIC(arima011)
# wartosci BIC
BIC(arima011)

# Czy reszty są białym szumem?
par(mfrow = c(1, 2))
Acf(resid(arima011), lag.max = 24,
    ylim = c(-1, 1),
    xlim=c(1,24),
    lwd = 4, col = "red")
Pacf(resid(arima011), lag.max =24 ,
     lwd = 4, col = "red")

# piąte wypustki wystają, ale możemy to pominąć i uznać ze biały szum

par(mfrow = c(1, 1))
# Test Ljung-Boxa (do sensowego opóźnienia).
Box.test(resid(arima011), type = "Ljung-Box", lag = 24)

# Test Box-Pierce (do sensowego opóźnienia).
Box.test(resid(arima011), type = "Box-Pierce", lag = 24)

#testy: wszystko ok 


#Rozkład reszt
hist(residuals(arima011))

library(tseries)
jarque.bera.test(residuals(arima011))
# brak rozkladu normalnego reszt
# nadal jest problem, zastosowany ln jest za mały, sprwdzenie warunkowej heteroskedastycznosci

# TU KONIEC 

################################################################################
# Dodatkowo warto sprawdzic automatyczna wersje ARIMY dla AIC i BIC
################################################################################

arima.best.AIC <- auto.arima(ln.df.in,
                             d = 1,             # parameter d w modelu ARIMA
                             max.p = 5,         # p = maksymalna wartosc
                             max.q = 5,         # q = maksymalna wartosc
                             max.order = 10,     # suma p+q
                             start.p = 0,       # Wartosc startowa dla p
                             start.q = 0,       # Wartosc startowa dla q
                             ic = "aic",        # Wybor modelu na podstawie kryterium informcyjne
                             stepwise = FALSE,  # jezeli FALSE rozwaza wszystkie modeli
                             allowdrift = TRUE, # model zawiera stalą
                             trace = TRUE)      # wyswietlenie rozwazonych modeli


coeftest(arima.best.AIC)

arima.best.BIC <- auto.arima(ln.df.in,
                             d = 1,             # parameter d w modelu ARIMA
                             max.p = 5,         # p = maksymalna wartosc
                             max.q = 5,         # q = maksymalna wartosc
                             max.order = 10,     # suma p+q
                             start.p = 0,       # Wartosc startowa dla p
                             start.q = 0,       # Wartosc startowa dla q
                             ic = "bic",        # Wybor modelu na podstawie kryterium informcyjne
                             stepwise = FALSE,  # jezeli FALSE rozwaza wszystkie modeli
                             allowdrift = TRUE, # model zawiera stalą
                             trace = TRUE)      # wyswietlenie rozwazonych modeli


coeftest(arima.best.BIC)

# aic arima 011
# bic arima 011   

###########################################################################
# V. Prognoza                                                             #
###########################################################################

nobs <- length(ln.df.in)

# szacujemy model na próbie IN SAMPLE
arima011<- arima(as.numeric(ln.df.in), # zmienna zależna
                    order = c(0, 1, 1),  # rzędy (p,d,q)
                    xreg = 1:nobs,       # dodatkowe regresory - stala
                    fixed = c(NA, NA)
)
summary(arima111_4)

forecast_arima011 <- predict(arima011, n.ahead = 3,
                          newxreg = (nobs + 1) : (nobs + 3)) # prognozy dla dodatkowych regresorów
########### 

forecast_pred = ts(data=forecast_arima011$pred, frequency = 12,             
        start=c(2021,10), end=c(2021,12)) 

forecast_pred

forecast_se = ts(data=forecast_arima011$se, frequency = 12,             
                 start=c(2021,10), end=c(2021,12)) 

ln.df <- log(df)

# wykres prognozy
plot(ln.df, main = "3-miesięczna prognoza dla logarytmu realnego kursu walutowego")
abline(v = 2021+9/12, lty = 2, col = "gray")
lines(forecast_pred, col = "red", lwd = 2)
lines(forecast_pred + 2 * forecast_se, col = "red", lty = 3)
lines(forecast_pred - 2 * forecast_se, col = "red", lty = 3)

# i raz jeszcze,w zbliżeniu
plot(ln.df, main = "3-miesięczna prognoza dla logarytmu realnego kursu walutowego",
  xlim = c(2020, 2022), ylim = c(4.5, 4.6))
abline(v = 2021+9/12, lty = 2, col = "gray")
lines(forecast_pred, col = "red", lwd = 2)
lines(forecast_pred + 2 * forecast_se, col = "red", lty = 3)
lines(forecast_pred - 2 * forecast_se, col = "red", lty = 3)


# łączymy prognozy z oryginalnym szeregiem
ln.df.forecast <- data.frame(forecast = forecast_arima011$pred,
                           real = window(ln.df,
                                  start = c(2021,10))
)
ln.df.forecast

str(ln.df.forecast)

###########################################################################
# VI. JAKOSC PROGNOZY                                                     #
###########################################################################

ln.df.forecast$mae <- abs(as.numeric(ln.df.forecast$real) - ln.df.forecast$forecast)
ln.df.forecast$mse <- (as.numeric(ln.df.forecast$real) - ln.df.forecast$forecast) ^ 2
ln.df.forecast$mape <- abs((as.numeric(ln.df.forecast$real) - ln.df.forecast$forecast) /
                           as.numeric(ln.df.forecast$real))
ln.df.forecast$amape <- abs((as.numeric(ln.df.forecast$real) - ln.df.forecast$forecast) /
                            (as.numeric(ln.df.forecast$real) + ln.df.forecast$forecast))

str(ln.df.forecast[, 3:6])

colMeans(ln.df.forecast[, 3:6])

# aby zmienić 'naukowy' format liczb
# możemy skorzystać z opcji scipen
options(scipen = 5)
round(colMeans(ln.df.forecast[, 3:6]), 3)

# Wniosek: model, który jest nalepiej dopasowany w okresie in-sample
# wcale nie musi wypaść najlepiej w prognozie dla okresu out-of-sample.
#	Należy zatem porównać co najmniej kilka modeli w okresie out-of-sample..



