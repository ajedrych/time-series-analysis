# LIBRARIES
library(readxl)
library(zoo)
library(ggplot2)

##################### dane niesezonowe #########################################
df <- read_excel("niesez.xls")

class(df)
is.ts(df)

#obiekt time-series
df = ts(data=df$Value, frequency = 12,             
             start=c(1994,1), end=c(2021,12)) 

class(df) 

# Wykres danych w czasie
plot(df, main = "Realny kurs walutowy Polski")

#############################################################################
##################### dane sezonowe #########################################

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
         end = c(2018, 12))

dfs.out <- 
  window(dfs,
         start = c(2019, 01))

# addytywny model Holta-Wintersa
dfs.HWadd <- HoltWinters(dfs.in,
                            seasonal = "additive") #szereg in-sample
dfs.HWadd
plot(dfs.HWadd)

dfs.HWadd.forecast <- predict(dfs.HWadd,
                                  n.ahead = 36,
                                  prediction.interval = TRUE) #fit = prognoza, dalej dolny i gorny przedzial ufnosci

library(rcompanion)
par(mfrow=c(1,2))

plot(dfs) # porownanie prognozy z oryginalnym szeregiem 
lines(dfs.HWadd.forecast[, 1], col = "blue") # prognoza
lines(dfs.HWadd.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(dfs.HWadd.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2019, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model addytywny")

plot(window(dfs, start = c(2018, 12)))  # porownanie prognozy z oryginalnym szeregiem 
lines(dfs.HWadd.forecast[, 1], col = "blue") # prognoza
lines(dfs.HWadd.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(dfs.HWadd.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2019, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model addytywny")


# multiplikatywny model Holta-Wintersa
dfs.HWmult <- HoltWinters(dfs.in,
                             seasonal="multiplicative")
dfs.HWmult # info o modelu; beta bliska zero wiec nie ma duzego lokalnego trendu zmienijacego sie w czasie

plot(dfs.HWmult)

dfs.HWmult.forecast <- predict(dfs.HWmult,
                              n.ahead = 36,
                              prediction.interval = TRUE) #fit = prognoza, dalej dolny i gorny przedzial ufnosci

plot(dfs) # porownanie prognozy z oryginalnym szeregiem 
lines(dfs.HWmult.forecast[, 1], col = "blue") # prognoza
lines(dfs.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(dfs.HWmult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2019, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model multiplikatywny")

plot(window(dfs, start = c(2018, 12)))  # porownanie prognozy z oryginalnym szeregiem 
lines(dfs.HWmult.forecast[, 1], col = "blue") # prognoza
lines(dfs.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(dfs.HWmult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2019, lty = 2) # dodajemy pionową linię referencyjną, gdzie zaczuna sie okres out-of-sample
title("Model multiplikatywny")



# oszacujmy prognozę na 36 obserwacji naprzód
dfs.HWmult.forecast <- predict(dfs.HWmult,
                                  n.ahead = 36,
                                  prediction.interval = TRUE) #fit = prognoza, dalej dolny i gorny przedzial ufnosci

# następnie porównajmy prognozę z oryginalnym szeregiem 
plot(dfs.ts)
lines(dfs.HWmult.forecast[, 1], col = "blue") # prognoza
lines(dfs.HWmult.forecast[, 2], col = "red", lty = 2) # dolna granica przedziału ufności
lines(dfs.HWmult.forecast[, 3], col = "red", lty = 2) # górna granica przedziału ufności
abline(v = 2004, lty = 2) # dodajemy pionową linię referencyjną, gdzie sie zaczyna out-of-sample

# obejrzyjmy prognozę w zbliżeniu
length(dfs.ts)

# zmienimy w tym celu jedynie pierwszą linię (pierwsze polecenie)
plot(window(dfs.ts, start = c(2001, 12)))
lines(dfs.HWmult.forecast[, 1], col = "blue")  
lines(dfs.HWmult.forecast[, 2], col = "red", lty = 2) 
lines(dfs.HWmult.forecast[, 3], col = "red", lty = 2) 
abline(v = 2004, lty = 2) 

# porównanie - bledy prognozy ex-post

dfs.HWadd$fitted[, 1]
dfs.HWmult$fitted[, 1]

dfs.HWadd.summary <- window(dfs.HWadd$fitted[, 1], end =c(2021, 12) , extend = TRUE)
dfs.HWmult.summary <- window(dfs.HWmult$fitted[, 1], end =c(2021, 12) , extend = TRUE)

# wstawiamy prognozy 
window(dfs.HWadd.summary, start = c(2019, 1)) <- dfs.HWadd.forecast[, 1]
window(dfs.HWmult.summary, start = c(2019, 1)) <- dfs.HWmult.forecast[, 1]

dfs.HWadd.summary
dfs.HWmult.summary

HW.summary <- ts.union(dfs,dfs.HWadd.summary,dfs.HWmult.summary)

library(xts)
HW.summary = as.xts(HW.summary)

sample_period <-
  ts(ifelse(index(HW.summary) < "2019-01", 0, 1), 
     start  =c(2004, 1), freq = 12)

names(HW.summary)
HW.summary$sample_period <- sample_period

HW.summary$mae_HWadd <- abs(HW.summary$dfs.HWadd.summary-HW.summary$dfs)
HW.summary$mse_HWadd   <- (HW.summary$dfs.HWadd.summary-HW.summary$dfs)^2
HW.summary$mape_HWadd   <- abs((HW.summary$dfs.HWadd.summary-HW.summary$dfs)/HW.summary$dfs)
HW.summary$amape_HWadd   <- abs((HW.summary$dfs.HWadd.summary-HW.summary$dfs)/(HW.summary$dfs.HWadd.summary+HW.summary$dfs))

HW.summary$mae_HWmult    <- abs(HW.summary$dfs.HWmult.summary-HW.summary$dfs)
HW.summary$mse_HWmult      <- (HW.summary$dfs.HWmult.summary-HW.summary$dfs)^2
HW.summary$mape_HWmult     <- abs((HW.summary$dfs.HWmult.summary-HW.summary$dfs)/HW.summary$dfs)
HW.summary$amape_HWmult    <- abs((HW.summary$dfs.HWmult.summary-HW.summary$dfs)/(HW.summary$dfs.HWmult.summary+HW.summary$dfs))

aggregate(HW.summary[, 4:12],
          by = list(HW.summary$sample_period),
          FUN = function(x) mean(x, na.rm = T))

