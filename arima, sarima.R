# LIBRARIES
library(readxl)
library(zoo)
library(ggplot2)

library(rcompanion)
par(mfrow=c(1,1))

################################################################################
##################### DANE NIESEZOWNOWE - REALNY KURS WALUTOWY POLSKI ##########
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

par(mfrow=c(1,2))

plot(diff.df.in,
     type = "l",
     main = "Pierwsze różnice")

#logarytmowanie w celu wyeliminowania zmieniającej się wariancji
ln.df.in<-log(df.in)
diff.ln.df.in<-diff.xts(ln.df.in)

plot(diff.ln.df.in,
     type = "l",
     main = "Pierwsze różnice dla logarytmu")

# test dickeya-fullera, druga forma funkcyjna (drift)
install.packages("fUnitRoots")
library(fUnitRoots)
source("funs/TESTDF.R") 

testdf(variable = ln.df.in, ADF_type="c", ADF_max_order = 5,BG_max_order = 6)
# dla testow Breuscha-Godfreya p-value <0.05 => odrzucamy H0 o braku autokorelacji (augmentations = 0)
# przechodzimy do testu ADF z jednym rozszerzeniem (augmentations = 1): dla testów BG p-value >0,05 => brak podstaw do odrzucenia
# H0 o braku autokorelacji
# patrzymy na p-value dla ADF = 0.4387209 < 0,05 => odrzucamy H0 o tym, że ln.df jest zmienna niestacjonarna

testdf(variable = diff.ln.df.in, ADF_type="c", ADF_max_order = 5,BG_max_order = 6)
# augmentations = 0 => dla testów B-G p-value > 0.05 => brak podstaw do odrzucenia h0 o braku autokorelacji
# p-value dla testu D-F 0.01 < 0.05 => odrzucamy H0 o tym, że ln.df jest zmienną nieatacjonarna


# test KPSS
library(urca)

df.in.kpss.test <- ur.kpss(ln.df.in, type = c("mu"))  # stała w równaniu testowym
summary(df.in.kpss.test)
# statystyka testowa (2,4579) > 0,463 => odrzucamy H0 o stacjonarnosci ln.df
# przeprowadzamy test dla pierwszych różnic

diff.df.in.kpss.test <- ur.kpss(diff.ln.df.in, type = c("mu"))  # stała w równaniu testowym
summary(diff.df.in.kpss.test)
# statystyka testowa = 0.1635 < 0,463 => brak podstaw do odrzucenia h0 o stacjonarnosci diff.ln.df

# test Ljung-Box
Box.test(diff.ln.df.in, lag=24, type="Ljung-Box") #brak podstaw do odrzucenia H0 o tym, że diff.ln.df.in to bialy szum, poniewaz p-value>0.05 

# test Box-Pierce
Box.test(diff.ln.df.in, lag=24, type="Box-Pierce") #brak podstaw do odrzucenia H0 o tym, że diff.ln.df.in to bialy szum, poniewaz p-value>0.05 

par(mfrow = c(1, 2))
acf(na.omit(diff.ln.df.in), lag.max = 24,
    ylim = c(-0.2, 0.2),
    xlim = c(1,4),
    lwd = 4, col = "red")
pacf(na.omit(diff.ln.df.in), lag.max = 24,
    ylim = c(-0.2, 0.2),
    xlim = c(1,4),
    lwd = 4, col = "red")

# analizowany szereg jest białym szumem (white noise) i nie można go prognozwać

################################################################################
##################### dane sezonowe ############################################

dfs <- read_excel("sez.xls")

class(dfs)
is.ts(dfs)

#obiekt time-series
dfs = ts(data=dfs$Value, frequency = 12,             
         start=c(2004,1), end=c(2021,12)) 

# test Ljung-Box
Box.test(dfs, lag=3, type="Ljung-Box") #odrzucamy H0: dfs to biały szum, bo p-value <0.05

# test Box-Pierce
Box.test(dfs, lag=3, type="Box-Pierce") #odrzucamy H0: dfs to biały szum, bo p-value <0.05

