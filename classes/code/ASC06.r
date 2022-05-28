###########################################################################
# Analiza szeregów czasowych                                              #
# semestr letni 2022                                                      #
# 6                                                                       #
# Uniwersytet Warszawski, Wydział Nauk Ekonomicznych                      #
###########################################################################

###################################################
# I. Proces stacjonarny                           #
###################################################

#############################
# a. WHITE NOISE            #
#############################


#n - dlugosc szeregu
n<-1000

# 1.

# biały szum (e)
# e jest białym szumem (e~WN(0,sigma^2))
e<- ts(rnorm(n,mean=0,sd=1),frequency=1)


# wykres
plot(e,
     type = "l",      # rodzaj wykresu (l=line)
     col  = "black",  # kolor linii
     lwd  = 2,        # podwójna grubosć
     main = "Normalny Bialy szum")

# waha się wokół zera, pierwsza forma funkcyjna
# Test D-F przeprowadzimy za pomocą funkcji ur.df()

# c("none", "drift", "trend")
# test DF# uwzględniamy "none" w róWnaniu testującym (wariant 1 testu)
# lags = 0 to liczba rozszerzeń w teście ADF


library(urca)
# urca: Unit Root and Cointegration Tests for Time Series Data 


df.test.e <- ur.df(e, type = c("none"), lags = 0) # e - zmienna, type - typ testu (pierwsza forma funkcyjna - "none"), lags = 0
# lags maja znaczenie w rozszerzonym teście Dickeya-Fullera
summary(df.test.e)

# sprawdźmy autokorelację reszt
library(lmtest)
resids.e <- df.test.e@testreg$residuals #reszty
bg1 <- bgtest(resids.e ~ 1, order = 1) #autokorelacja 1 rzędu
bg1



# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau1 -2.58 -1.95 -1.62
# statystyka tau jest mniejsza od 1%, 5%, 10%  wartości krytycznej 
# zatem odrzucamy H0 o niestacjonarności szeregu e~WN




###################################################
# II. Proces niestacjonarny                       #
###################################################


#############################
# a. Bladzenie przypadkowe  #
#############################

#n - dlugosc szeregu
n<-1000

# e jest białym szumem (e~WN(0,sigma^2))
e<- ts(rnorm(n,mean=0,sd=1),frequency=1)


# tworzymy wektor y wypełniony zerami
y  <- rep(0,n)

for(i in 2:n)  y[i] <- y[i - 1] + e[i]



# wykres
plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Bladzenie przypadkowe")

# Pierwsze różnice policzymy za pomocą funkcji diff.xts()
library(xts)

Dy<-diff.xts(y)

# wykres
plot(Dy,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Pierwsze roznice dla szeregu: Bladzenie przypadkowe")
#zmienna stacjonarna po pierwszym róznicowaniu -> pierwsza forma funkcyjna

# Test D-F przeprowadzimy za pomocą funkcji ur.df()

# c("none", "drift", "trend")
# test DF# uwzględniamy "none" w róWnaniu testującym (wariant 1 testu)
# lags = 0 to liczba rozszerzeń w teście ADF
library(urca)
df.test.y <- ur.df(y, type = c("none"), lags = 0)
summary(df.test.y)

# sprawdźmy autokorelację reszt
library(lmtest)
resids.y <- df.test.y@testreg$residuals
bg1 <- bgtest(resids.y ~ 1, order = 1)
bg1


# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau1 -2.58 -1.95 -1.62
# statystyka tau jest wieksza od 1%, 5%, 10%  wartości krytycznej 
# zatem nie ma podstaw do odrzucenia H0 o niestacjonarności szeregu y~RW



# W drugim kroku powtarzamy całą procedurę dla pierwszych różnic y.
# Musimy utworzyć nową zmienną.

# Pierwsze różnice policzymy za pomocą funkcji diff.xts()
library(xts)

Dy<-diff.xts(y)

# wykres
plot(Dy,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Pierwsze roznice dla szeregu: Bladzenie przypadkowe")


# Test D-F przeprowadzimy za pomocą funkcji ur.df()

# c("none", "drift", "trend")
# test DF# uwzględniamy "none" w róWnaniu testującym (wariant 1 testu)
# lags = 0 to liczba rozszerzeń w teście ADF
library(urca)
df.test.Dy <- ur.df(na.omit(Dy), type = c("none"), lags = 0)  #na.omit bo R nie lubi missing values
summary(df.test.Dy)
#szereg zintegrowany pierwszego rzedu

# sprawdźmy autokorelację reszt
library(lmtest)
resids.Dy <- df.test.Dy@testreg$residuals
bg1 <- bgtest(resids.Dy ~ 1, order = 1)
bg1
#brak autokorelacji reszt, bo p-value >0.05 brak podstaw do odrzucenia h0 o autokorelacji reszt

# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau1 -2.58 -1.95 -1.62
# statystyka tau jest mniejsza od 1%, 5%, 10%  wartości krytycznej
# zatem  odrzucamy H0 o niestacjonarności szeregu y~RW


######################################
# b. Bladzenie przypadkowe z dryfem  #
######################################

# tworzymy wektor y wypełniony zerami
y  <- rep(0,n)

mu= #dryf = 1
#mu=0.1
#mu=0.3
for(i in 2:n)  y[i] <- mu+y[i - 1] + e[i]



# wykres
plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Bladzenie przypadkowe z dryfem")

# Pierwsze różnice policzymy za pomocą funkcji diff.xts()
library(xts)

Dy<-diff.xts(y)

# wykres
plot(Dy,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Pierwsze roznice dla szeregu: Bladzenie przypadkowe z dryfem")


# Test D-F przeprowadzimy za pomocą funkcji ur.df()

# c("none", "drift", "trend")
# test DF# uwzględniamy "drift" w róWnaniu testującym (wariant 2 testu)
# lags = 0 to liczba rozszerzeń w teście ADF
library(urca)
df.test.y <- ur.df(y, type = c("drift"), lags = 0)
summary(df.test.y)

# #brak podstaw do odrzucenia ho -

# sprawdźmy autokorelację reszt
library(lmtest)
resids.y <- df.test.y@testreg$residuals
bg1 <- bgtest(resids.y ~ 1, order = 1) #order rząd 1 - autokorelacja pierwszego rzędu; jak dane miesięczne to rzad 6, kwartalne 4
bg1

#p-value > 0.05 -> mozna d-f


# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau2 -3.43 -2.86 -2.57
# statystyka tau jest wieksza od 1%, 5%, 10%  wartości krytycznej 
# zatem nie ma podstaw do odrzucenia H0 o niestacjonarności szeregu y~RW with drift



# W drugim kroku powtarzamy całą procedurę dla pierwszych różnic y.
# Musimy utworzyć nową zmienną.

# Pierwsze różnice policzymy za pomocą funkcji diff.xts()
library(xts)

Dy<-diff.xts(y)

# wykres
plot(Dy,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Pierwsze roznice dla szeregu: Bladzenie przypadkowe z dryfem")


# Test D-F przeprowadzimy za pomocą funkcji ur.df()

# c("none", "drift", "trend")
# test DF# uwzględniamy "drift" w róWnaniu testującym (wariant 1 testu)
# lags = 0 to liczba rozszerzeń w teście ADF
library(urca)
df.test.Dy <- ur.df(na.omit(Dy), type = c("drift"), lags = 0)
summary(df.test.Dy)

# sprawdźmy autokorelację reszt
library(lmtest)
resids.Dy <- df.test.Dy@testreg$residuals
bg1 <- bgtest(resids.Dy ~ 1, order = 1)
bg1

# Critical values for test statistics: 
#       1pct  5pct 10pct
# tau2 -3.43 -2.86 -2.57
# statystyka tau jest mniejsza od 1%, 5%, 10%  wartości krytycznej 
# zatem odrzucamy H0 o niestacjonarności szeregu na pierwszych roznicach RW with drift


###################################################
# III. GDP                                        #
###################################################


#  1. 
# Importujemy dane z pliku "GDP_US.csv" 


# import pliku CSV
GDP <- read.csv("dane/GDP_US.csv",   # nazwa pliku
                   header = TRUE,    # czy obserwacje w pierwszym wierszu?
                   sep = ";",    # separator kolumn
                   dec = ".")	   # separator dziesiatny


# weryfikujemy strukture obiektu
str(GDP)

# utworzymy obiekt typu ts (time series) dla danych 
GDP <- 
  ts(GDP$gdp2005, start = c(1947, 1), freq = 4) #freq = 4 dla danych kawrtalnych, 12 dla miesiecznych

GDP
str(GDP)

#  2. 
#  Wizualizacja szeregu 

plot(GDP,
     main = "GDP",
     xlab = "data",
     ylab = "GDP")


# Wzrokowa ocena pozwala domniemywać, że szereg jest niestacjonarny.

# 3. pierwsze różnice

# Pierwsze różnice policzymy za pomocą funkcji diff.xts()
library(xts)

D_GDP<-diff.xts(GDP)


plot(D_GDP, type = "l", col="blue", main = "Pierwsze różnice GDP") # wahanie wokol stałej, amplituda wahan wzrasta; wariancja jest niestabilna -> tranformacja logarytmiczna

# Aby wyeliminowac zmieniajaca sie w czasie wariancje 
#logarytmujemy szereg przed róznicowaniem

# Generujemy zmienna bedaca logarytmem zmiennej gdp2005
lnGDP<-log(GDP)

library(xts)


D_lnGDP<-diff.xts(lnGDP)

plot(D_lnGDP, type = "l", col="blue", main = "Pierwsze różnice lnGDP") #efekt skla wariancji jest juz znikomy, ronice nie sa az tak gigantyczne
# szereg waha sie wokol mi, druga forma funkcyjna

# 4.  Pierwszy test ADF przeprowadzimy za pomocą funkcji ur.df()


# c("none", "drift", "trend")
# test DF# uwzględniamy "drift" w róWnaniu testującym (wariant 2 testu)
# lags = 0 to liczba rozszerzeń w teście ADF
library(urca)
df.test.lnGDP <- ur.df(lnGDP, type = c("drift"), lags = 0)
summary(df.test.lnGDP)

# sprawdźmy autokorelację reszt
library(lmtest)
resids.lnGDP <- df.test.lnGDP@testreg$residuals
bg1 <- bgtest(resids.lnGDP ~ 1, order = 1) # dane kwartalne, trzeba przprowadzic cztery korelacje - piewszego, drugiego, trzeciego i czwartego rzędu, ale zatzrtymuejmy sie tutaj na pierwszym bo juz w pierwszym rzedu jest odrzucenie h0 o autokorelacji
bg1

#dopiero jak cztery rzędy są ok to wracamy do testu d-f

# Na podstawie testu BG odrzucamy H0 o braku autokorelacji I-go rzedu
# Przechodzimy do testu ADF

# c("none", "drift", "trend")
# test DF# uwzględniamy "drift" w róWnaniu testującym (wariant 2 testu)
# lags = 1 to liczba rozszerzeń w teście ADF
library(urca)
df.test.lnGDP <- ur.df(lnGDP, type = c("drift"), lags = 1)
summary(df.test.lnGDP)

# logarytm gdp jst zmienna niestacjonarna przynajmniej rzedu pierwszego
#pierwsze roznice logarytmu gdp

# sprawdźmy autokorelację reszt
library(lmtest)
resids.lnGDP <- df.test.lnGDP@testreg$residuals
bg1 <- bgtest(resids.lnGDP ~ 1, order = 1)
bg1 #p-value > 0.05 - nie ma podstaw do odrzucenia h0 - jest ok
bg2 <- bgtest(resids.lnGDP ~ 1, order = 2)
bg2 #nie ma podstaw do odrzucenia h0 - jest ok
bg3 <- bgtest(resids.lnGDP ~ 1, order = 3)
bg3 #nie ma podstaw do odrzucenia h0 - jest ok
bg4 <- bgtest(resids.lnGDP ~ 1, order = 4)
bg4 #nie ma podstaw do odrzucenia h0 - jest ok


#jak procedura od ogolu do szcegolu w danych wartalnych - lags=5 pozniej 4 etc.
# dla dantych miesiecznych lags = 13

# Alternatywnie, test ADF możemy przeprowadzić za pomocą funkcji testADF()
# zdefiniowanej w zewnętrznym pliku. 
# Funkcja ta wyśtwietla dodatkowo
# wartości statystyki Breuscha-Godfreya, która weryfikuje hipotezę
# o braku autokorelacji reszt w równaniu testowym DF.  To pomoże nam
# wybrać właściwą liczbę rozszerzeń.

source("funs/TESTDF.R")

# ADF_type = {"nc", "c", "ct"} #non constant, constant (dryf), constant and trend
# ADF_max_order = k, # opcja ADF=k
# BG_max_order=d  # test B-G max liczba lagow=d

testdf(variable = lnGDP, ADF_type="c", ADF_max_order = 5,BG_max_order = 4)

# Ponieważ dla zerowej liczby rozszerzeń reszty podlegają autokorelacji
# (test BG odrzuca H0 o braku autokorelacji wśród reszt), to dodajemy jedno
# rozszerzenie. Okazuje się, że jest o zabieg wystarczający, aby usunąć
# autokorelację.  W efekcie, widzimy, że właściwa statystyka
# ADF nie odrzuca H0, a zatem GDP nie jest stacjonarny.

# W drugim kroku powtarzamy całą procedurę dla pierwszych różnic GDP.
# Musimy utworzyć nową zmienną.

# Pierwsze różnice policzymy za pomocą funkcji diff.xts()
library(xts)
testdf(variable = diff.xts(lnGDP), ADF_type="c", ADF_max_order = 5,BG_max_order = 4)
#test d-f jest wystarczajacy

# Która z wartości statystyki ADF jest poprawna?  Jaki jest wynik testu ADF?

# 5

# test KPSS W teście tym mamy odwrócone hipotezy: jeśli statystyka
# testowa jest większa od wartości krytycznej to odrzucamy H0
# o stacjonarności szeregu.

# ur.kpss(y, type = c("mu", "tau")) - "mu" - stała w równaniu testowym; "tau" - trend w równaniu testowym

kpss.test <- ur.kpss(lnGDP, type = c("mu"))  # stała w równaniu testowym
summary(kpss.test)

# statystyka KPSS (4.4216) jest większa od 5% wartości krytycznej
# (0.463) za zatem odrzucamy H0 o stacjonarności GDP

# powtórzmy procedurę dla pierwszych różnic GDP
kpss.test.d <- ur.kpss(diff.xts(lnGDP), type = c("mu"))
summary(kpss.test.d)

# statystyka KPSS jest mniejsza od 5% wartości krytycznej (0.463)
# a zatem nie możemy odrzucić H0 o stacjonarności pierwszych różnic lnGDP.

# A zatem ostatecznie lnGDP~I(1)

# wniosek: wszystkie testy dają ten sam wynik: lnGDP~I(1)


################ Praca domowa #4

# 1. Prosze zaimportować z portalu stooq.pl notowanie dowolnej spółki/waluty/indeksu (JEDNEGO SZEREGU)
# 2. Prosze przeprowadzić  test ADF i  KPSS oraz zinterpretować wyniki
# 3. Prosze określić stopień integracji tego szeregu

df <- read.csv("cps_m.csv",   # nazwa pliku
                header = TRUE,    # czy obserwacje w pierwszym wierszu?
                sep = ",",    # separator kolumn
                dec = ".")	   # separator dziesiatny

str(df)

df <- 
  ts(df$Close, start = c(2015, 4), freq = 12) #freq = 4 dla danych kawrtalnych, 12 dla miesiecznych

plot(df,
     main = "Notowania akcji spółki Polsat",
     xlab = "data",
     ylab = "PLN")

library(xts)
d_df<-diff.xts(df)
plot(d_df, type = "l", col="blue", main = "Pierwsze różnice") 
lndf<-log(df)

D_lndf<-diff.xts(lndf)

plot(D_lndf, type = "l", col="blue", main = "Pierwsze różnice lndf")

library(urca)
df.test.lndf <- ur.df(lndf, type = c("none"), lags = 0)
summary(df.test.lndf)

# sprawdźmy autokorelację reszt
library(lmtest)
resids.lndf <- df.test.lndf@testreg$residuals
bg1 <- bgtest(resids.lndf ~ 1, order = 1)
bg1

library(urca)
df.test.lndf <- ur.df(lndf, type = c("none"), lags = 1)
summary(df.test.lndf)

source("funs/TESTDF.R")

testdf(variable = lndf, ADF_type="nc", ADF_max_order = 5, BG_max_order = 6)

# dla augmentations = 0 (test d-f), na podstawie testu B-G od drugiego rzędu odrzuycamy h0 o braku autokorelacji
# pzechodzimy do adf (lag=1), autkorelacja na podstawie testu b-g: od drugiego rzędu odrzucamy h0 o braku autokorelacji (<0.05)
# adf (lag=2), autokorelacja dla wszystkich rzędów - nie ma podstaw do odrzucenia h0 (brak autokorelacji dla wszystkich rzędów)
# statystykowa testowa w tescie adf: p-value >0.05 -> nie ma podstaw do odrzucenia h0 o zmiennej niestacjonarnej
#przeprowadzamy adf na pierwszych różnicach:

testdf(variable = diff.xts(lndf), ADF_type="nc", ADF_max_order = 5, BG_max_order = 6)

# test d-f (augmentations = 0): test b-g: na poziomie rzędu drugiego odrzucamy h0 o braku autkorelacji
# test adf (lag=1): test b-g: brak podstaw do odrzucenia h0 na poziomie wszystkich rzędów
# mozemy interpretowac test adf (lag=1)
# test adf: odrzucamy h0 o tym ze pierwsze roznice logarytmu polsatu to zmienna niestacjonarna

#TEST KPSS ###################################################################################
kpss.test <- ur.kpss(lndf, type = c("mu"))  # stała w równaniu testowym
summary(kpss.test)

#odrzucamy h0 o zmiennej stacjonarnej, bo 1,2289 > 0,163
#robimy test na pierwszych różnicaxh

kpss.test.d <- ur.kpss(diff.xts(lndf), type = c("mu"))
summary(kpss.test.d)

# brak podstaw do odrzucenia h0 o stacjonarności pierwszych różnic na poziomie istotności 5%, bo 0,1264 < 0,463



# zminana ln df to zmienna zintegrowana rzędu I
