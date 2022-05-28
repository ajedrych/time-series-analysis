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

################################################################################
##################### dane sezonowe ############################################

dfs <- read_excel("sez.xls")

class(dfs)
is.ts(dfs)

#obiekt time-series
dfs = ts(data=dfs$Value, frequency = 12,             
         start=c(2004,1), end=c(2021,12)) 

# test Ljung-Box
Box.test(dfs, lag=24, type="Ljung-Box") #odrzucamy H0: dfs to biały szum, bo p-value <0.05

# test Box-Pierce
Box.test(dfs, lag=24, type="Box-Pierce") #odrzucamy H0: dfs to biały szum, bo p-value <0.05

