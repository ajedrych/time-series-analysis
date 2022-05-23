# LIBRARIES
library(readxl)

##################### dane niesezonowe #########################################33
df <- read_excel("niesez.xls")

class(df)
is.ts(df)

#obiekt time-series
df = ts(data=df$Value, frequency = 12,             
             start=c(1994,1), end=c(2021,12)) 

class(df) 

# Wykres danych w czasie
plot(df, main = "Realny kurs walutowy Polski")

