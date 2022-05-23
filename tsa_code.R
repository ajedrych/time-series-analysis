# LIBRARIES
library(readxl)

#dane niesezonowe
df <- read_excel("niesez.xls")

class(df)
is.ts(df)


df = ts(data=df$Value, frequency = 12,             
             start=c(1994,1), end=c(2021,12)) 

class(df) 


##################################
# funkcja plot() 
##################################

#1
# Ustawiamy okno wykresu, pierwszy parametr oznacza liczbę wierszy, drugi liczbę kolumn
# Czyli będą dwa wykresy, jeden pod drugim
par(mfrow = c(2,1))

# szereg typu "ts"
plot (AirPass)

plot (AirPass, main="Szereg AirPassengers")


#2
par(mfrow = c(1,1))

# Zmiana parametrów w funkcji plot()


plot(AirPass,
                    type="b", # punkty odpowiadające kolejnym obserwacjom
                    pch=16,   # rodzaj symbolu graficznego
                    lty=3,    # linia kropkowana
                    lwd=2     # podwójna grubosć
                    )


###################################################
# Wczytanie danych ze strony internetowej stooq.com
###################################################

url <- "http://stooq.com/q/d/l/?s=wig20&i=d"
# import pliku CSV
WIG20 <- read.csv(url,               # nazwa pliku
                  header = TRUE,  	 # czy obserwacje w pierwszym wierszu?
                  sep = ",", 	       # separator kolumn
                  dec = ".")	       # separator dziesiętny

# obejrzymy dane zaimportowane
str(WIG20)
WIG20
head(WIG20)
tail(WIG20)

class(WIG20) 

# data we własciwym formacie
WIG20$Date <- as.Date(WIG20$Date)

# wykres w czasie
plot(WIG20$Date, WIG20$Close, type = 'l')


# zapisywanie obiektu do pliku CSV
write.csv(WIG20, "WIG20_copy.csv")

# Aby przedstawić notowania za pomocą pakietu `dygraphs` 
# musimy stworzyć obiekt typu xts:

str(WIG20)  
library(xts)
WIG20.xts <- xts(WIG20[, c("Open", "Close")], order.by = WIG20$Date)


# Następnie wrzucamy obiekt `xts` bezposrednio do funkcji `dygraph`
library(dygraphs)
dygraph(WIG20.xts$Close) %>% dyRangeSelector(height = 40)


# Agregowanie danych do niższej częstotliwości - np. do rocznych

# TYLKO dla xts object 

WIG20.xts_1months=to.period(WIG20.xts, 
                           period = 'months', # 'seconds', "minutes", "hours", "days", "weeks", "months", "quarters", "years"
                           k = 1,              
                           OHLC = FALSE) # czy Open, High, Low, Close?  
head(WIG20.xts_1months)
tail(WIG20.xts_1months)


WIG20.xts_1quarters=to.period(WIG20.xts, 
                            period = 'quarters', 
                            k = 1,              
                            OHLC = FALSE)  
head(WIG20.xts_1quarters)
tail(WIG20.xts_1quarters)


WIG20.xts_1years=to.period(WIG20.xts, 
                              period = 'years', 
                              k = 1,              
                              OHLC = FALSE)  
head(WIG20.xts_1years)
tail(WIG20.xts_1years)


########################
# PRACA DOMOWA #1
########################
# 1. Wykorzystac poznane wykresy do zapoznania się z danymi w pliku pkb.csv

# 2. Wykorzystac poznane wykresy do zapoznania się z dowolnym szeregiem 
#    czasowym w pakiecie datasets (pakiet ładuje się automatycznie)

library(help="datasets")

# 3. Wykorzystac poznane wykresy do zapoznania się z dowolnym szeregim czasowym 
#    udostępnionym przez GUS (stat.gov.pl)

# 4. Wykorzystac poznane wykresy do zapoznania się z dowolnym szeregim czasowym 
#    ze strony stooq.com


