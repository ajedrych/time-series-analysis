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

# test dickeya-fullera
library(urca)
df.test <- ur.df(df, type = c("trend"), lags = 0) 
summary(df.test)

library(lmtest)
df.test.resids <- df.test@testreg$residuals #reszty
bg.df <- bgtest(df.test.resids ~ 1, order = 1) #autokorelacja 1 rzędu
bg.df

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

