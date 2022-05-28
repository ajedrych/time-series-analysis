# LIBRARIES
library(readxl)
library(zoo)
library(ggplot2)

library(rcompanion)
par(mfrow=c(1,1))

################################################################################
##################### DANE NIESEZOWNOWE - REALNY KURS WALUTOWY POLSKI ##########
df <- read_excel("niesez.xls")

class(df)
is.ts(df)

#obiekt time-series
df = ts(data=df$Value, frequency = 12,             
        start=c(1994,1), end=c(2021,12)) 

# test Ljung-Box
Box.test(df, lag=24, type="Ljung-Box") #odrzucamy H0: df to biały szum, bo p-value <0.05

# test Box-Pierce
Box.test(df, lag=24, type="Box-Pierce") #odrzucamy H0: df to biały szum, bo p-value <0.05

# wykres
plot(df,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Realny kurs walutowy dla Polski")

# pierwsze różnice
library(xts)
diff.df<-diff.xts(df)

par(mfrow=c(1,2))

plot(diff.df,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Pierwsze różnice")

#logarytmowanie w celu wyeliminowania zmieniającej się wariancji
ln.df<-log(df)
diff.ln.df<-diff.xts(ln.df)

plot(diff.ln.df,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Pierwsze różnice dla logarytmu")

# test dickeya-fullera, pierwsza forma funkcyjna (non drift)
install.packages("fUnitRoots")
library(fUnitRoots)
source("funs/TESTDF.R") 

testdf(variable = ln.df, ADF_type="nc", ADF_max_order = 5,BG_max_order = 6)

# dla testow Breuscha-Godfreya p-value <0.05 => odrzucamy H0 o braku autokorelacji (augmentations = 0)
# przechodzimy do testu ADF z jednym rozszerzeniem (augmentations = 1): sla testów BG p-value >0,05 => brak podstaw do odrzucenia
# H0 o braku autokorelacji
# patrzymy na p-value dla ADF = 0.84 => brak podstaw do odrzucenia H0 o tym, że ln.df jest zmienną niestacjonarną
# przeprowadzamy test dla pierwszych różnic

testdf(variable = diff.ln.df, ADF_type="nc", ADF_max_order = 5,BG_max_order = 6)
# dla testów B-G p-value >0,05 => brak podstaw do odrzucenia H0 o braku autokorelacji (augmentations=0)
# patrzymy na p-valu D-F = 0,01 <0.05 => odrzucamy h0 o tym, że diff.ln.df jest zmienna niestacjonarna

# test KPSS
df.kpss.test <- ur.kpss(ln.df, type = c("mu"))  # stała w równaniu testowym
summary(df.kpss.test)
# statystyka testowa (2,449) > 0,463 => odrzucamy H0 o stacjonarnosci ln.df
# przeprowadzamy test dla pierwszych różnic

diff.df.kpss.test <- ur.kpss(diff.ln.df, type = c("mu"))  # stała w równaniu testowym
summary(diff.df.kpss.test)
# statystyka testowa = 0.1635 < 0,463 => brak podstaw do odrzucenia h0 o stacjonarnosci diff.ln.df

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

