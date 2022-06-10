# LIBRARIES
library(readxl)
library(zoo)
library(ggplot2)

library(rcompanion)
par(mfrow=c(1,1))

################################################################################
############ DANE NIESEZONOWE - REALNY KURS WALUTOWY POLSKI ####################
################################################################################

# 1. Wczytanie danych
df <- read_excel("niesez.xls")

class(df)
is.ts(df)

#obiekt time-series
df = ts(data=df$Value, frequency = 12,             
             start=c(1994,1), end=c(2021,12)) 

class(df) 

# Wykres danych w czasie
plot(df, main = "Realny kurs walutowy Polski")

# próby in-sample i out-of-sample
df.in <- 
  window(df,
         end = c(2021, 09))

df.out <- 
  window(df,
         start = c(2021, 10))

##### modele ekstrapolacyjne ###################################################
# prosty model wygładzania wykładniczego (EWMA) - bez trendu i sezonowości

df.EWMA <- HoltWinters(df.in,
                          beta  = FALSE, # beta jest czynnikiem trendu
                          gamma = FALSE) # gamma jest czynnikiem sezonowym

plot(df.EWMA)
plot(df.EWMA$fitted)

df.EWMA.forecast <- predict(df.EWMA, # prognoza na 3 obserwacje do przodu
                               n.ahead = 3,
                               prediction.interval = TRUE)

library(rcompanion)
par(mfrow=c(1,2))

plot(df)
lines(df.EWMA.forecast[, 1], col = "blue") # prognozy 
lines(df.EWMA.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności dla prognozy
lines(df.EWMA.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności dla prognozy
abline(v = 2021+9/12, lty = 2)  # dodajemy pionową linię referencyjną (zeby zobaczyc okres out-of-sample)
title("EWMA")

plot(window(df, start = c(2019, 12)))
lines(df.EWMA.forecast[, 1], col = "blue") # prognozy 
lines(df.EWMA.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności dla prognozy
lines(df.EWMA.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności dla prognozy
abline(v = 2021+9/12, lty = 2)  # dodajemy pionową linię referencyjną (zeby zobaczyc okres out-of-sample)
title("EWMA")

# model Holta
df.Holt <- HoltWinters(df.in,
                       gamma = FALSE) # gamma jest czynnikiem sezonowym

plot(df.Holt)
plot(df.Holt$fitted)

df.Holt.forecast <- predict(df.Holt, # prognoza na 3 obserwacje do przodu
                            n.ahead = 3,
                            prediction.interval = TRUE)

plot(df)
lines(df.Holt.forecast[, 1], col = "blue") # prognozy 
lines(df.Holt.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności dla prognozy
lines(df.Holt.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności dla prognozy
abline(v = 2021+9/12, lty = 2)  # dodajemy pionową linię referencyjną (zeby zobaczyc okres out-of-sample)
title("Holt")

plot(window(df, start = c(2019, 12)))
lines(df.Holt.forecast[, 1], col = "blue") # prognozy 
lines(df.Holt.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności dla prognozy
lines(df.Holt.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności dla prognozy
abline(v = 2021+9/12, lty = 2)  # dodajemy pionową linię referencyjną (zeby zobaczyc okres out-of-sample)
title("Holt")

# porównanie - bledy prognozy ex-post

df.EWMA$fitted[, 1]
df.Holt$fitted[, 1]

df.EWMA.summary <- window(df.EWMA$fitted[, 1], end =c(2021, 12) , extend = TRUE)
df.Holt.summary <- window(df.Holt$fitted[, 1], end =c(2021, 12) , extend = TRUE)

window(df.EWMA.summary, start = c(2021, 1)) <- df.EWMA.forecast[, 1]
window(df.Holt.summary, start = c(2021, 1)) <- df.Holt.forecast[, 1]

df.EWMA.summary
df.Holt.summary

Holt.summary <- ts.union(df,df.EWMA.summary,df.Holt.summary)

library(xts)
Holt.summary = as.xts(Holt.summary)

sample_period.Holt <-
  ts(ifelse(index(Holt.summary) < "2021-10", 0, 1), 
     start  =c(1994, 1), freq = 12)

sample_period.Holt

names(Holt.summary)
Holt.summary$sample_period.Holt <- sample_period.Holt

Holt.summary$mae_EWMA <- abs(Holt.summary$df.EWMA.summary-Holt.summary$df)
Holt.summary$mse_EWMA   <- (Holt.summary$df.EWMA.summary-Holt.summary$df)^2
Holt.summary$mape_EWMA   <- abs((Holt.summary$df.EWMA.summary-Holt.summary$df)/Holt.summary$df)
Holt.summary$amape_EWMA   <- abs((Holt.summary$df.EWMA.summary-Holt.summary$df)/(Holt.summary$df.EWMA.summary+Holt.summary$df))

Holt.summary$mae_Holt   <- abs(Holt.summary$df.Holt.summary-Holt.summary$df)
Holt.summary$mse_Holt      <- (Holt.summary$df.Holt.summary-Holt.summary$df)^2
Holt.summary$mape_Holt     <- abs((Holt.summary$df.Holt.summary-Holt.summary$df)/Holt.summary$df)
Holt.summary$amape_Holt    <- abs((Holt.summary$df.Holt.summary-Holt.summary$df)/(Holt.summary$df.Holt.summary+Holt.summary$df))

aggregate(Holt.summary[, 4:12],
          by = list(Holt.summary$sample_period.Holt),
          FUN = function(x) mean(x, na.rm = T))

par(mfrow=c(1,1))

################################################################################
##################### dane sezonowe ############################################

dfs <- read_excel("sez.xls")

class(dfs)
is.ts(dfs)

#obiekt time-series
dfs = ts(data=dfs$Value, frequency = 12,             
        start=c(2004,1), end=c(2021,12)) 

class(dfs) 

# Wykres danych w czasie
plot(dfs, main = "Liczba pasażerów samolotów w Polsce")

############# model ekstrapolacyjny #########################################

# okresy in-sample i out-of-sample
dfs.in <- 
  window(dfs,
         end = c(2020, 12))

dfs.out <-    # 12 obserwacji out-of sample (jeden cykl)
  window(dfs,
         start = c(2021, 01))

# addytywny model Holta-Wintersa
dfs.in.HWadd <- HoltWinters(dfs.in,
                            seasonal = "additive") #szereg in-sample
dfs.in.HWadd
plot(dfs.in.HWadd)

dfs.in.HWadd.forecast <- predict(dfs.in.HWadd,
                                  n.ahead = 12,
                                  prediction.interval = TRUE) #fit = prognoza, dalej dolny i gorny przedzial ufnosci

library(rcompanion)
par(mfrow=c(1,2))

plot(dfs) # porownanie prognozy z oryginalnym szeregiem 
lines(dfs.in.HWadd.forecast[, 1], col = "blue") # prognoza
lines(dfs.in.HWadd.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(dfs.in.HWadd.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2021, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model addytywny")

plot(window(dfs, start = c(2019, 12)))  # porownanie prognozy z oryginalnym szeregiem 
lines(dfs.in.HWadd.forecast[, 1], col = "blue") # prognoza
lines(dfs.in.HWadd.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(dfs.in.HWadd.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2021, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model addytywny")


# multiplikatywny model Holta-Wintersa
dfs.in.HWmult <- HoltWinters(dfs.in,
                             seasonal="multiplicative")
dfs.in.HWmult # info o modelu; beta bliska zero wiec nie ma duzego lokalnego trendu zmienijacego sie w czasie

plot(dfs.in.HWmult)

dfs.in.HWmult.forecast <- predict(dfs.in.HWmult,
                              n.ahead = 12,
                              prediction.interval = TRUE) #fit = prognoza, dalej dolny i gorny przedzial ufnosci

plot(dfs) # porownanie prognozy z oryginalnym szeregiem 
lines(dfs.in.HWmult.forecast[, 1], col = "blue") # prognoza
lines(dfs.in.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(dfs.in.HWmult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2021, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model multiplikatywny")

plot(window(dfs, start = c(2019, 12)))  # porownanie prognozy z oryginalnym szeregiem 
lines(dfs.in.HWmult.forecast[, 1], col = "blue") # prognoza
lines(dfs.in.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(dfs.in.HWmult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2021, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model multiplikatywny")


# porównanie - bledy prognozy ex-post

dfs.in.HWadd$fitted[, 1]
dfs.in.HWmult$fitted[, 1]

dfs.in.HWadd.summary <- window(dfs.in.HWadd$fitted[, 1], end =c(2021, 12) , extend = TRUE)
dfs.in.HWmult.summary <- window(dfs.in.HWmult$fitted[, 1], end =c(2021, 12) , extend = TRUE)

# wstawiamy prognozy 
window(dfs.in.HWadd.summary, start = c(2021, 1)) <- dfs.in.HWadd.forecast[, 1]
window(dfs.in.HWmult.summary, start = c(2021, 1)) <- dfs.in.HWmult.forecast[, 1]

dfs.in.HWadd.summary
dfs.in.HWmult.summary

HW.summary <- ts.union(dfs,dfs.in.HWadd.summary,dfs.in.HWmult.summary)

library(xts)
HW.summary = as.xts(HW.summary)

sample_period <-
  ts(ifelse(index(HW.summary) < "2021-01", 0, 1), 
     start  =c(2004, 1), freq = 12)

sample_period
names(HW.summary)
HW.summary$sample_period <- sample_period

HW.summary$mae_HWadd <- abs(HW.summary$dfs.in.HWadd.summary-HW.summary$dfs)
HW.summary$mse_HWadd   <- (HW.summary$dfs.in.HWadd.summary-HW.summary$dfs)^2
HW.summary$mape_HWadd   <- abs((HW.summary$dfs.in.HWadd.summary-HW.summary$dfs)/HW.summary$dfs)
HW.summary$amape_HWadd   <- abs((HW.summary$dfs.in.HWadd.summary-HW.summary$dfs)/(HW.summary$dfs.in.HWadd.summary+HW.summary$dfs))

HW.summary$mae_HWmult    <- abs(HW.summary$dfs.in.HWmult.summary-HW.summary$dfs)
HW.summary$mse_HWmult      <- (HW.summary$dfs.in.HWmult.summary-HW.summary$dfs)^2
HW.summary$mape_HWmult     <- abs((HW.summary$dfs.in.HWmult.summary-HW.summary$dfs)/HW.summary$dfs)
HW.summary$amape_HWmult    <- abs((HW.summary$dfs.in.HWmult.summary-HW.summary$dfs)/(HW.summary$dfs.in.HWmult.summary+HW.summary$dfs))

aggregate(HW.summary[, 4:12],
          by = list(HW.summary$sample_period),
          FUN = function(x) mean(x, na.rm = T))

