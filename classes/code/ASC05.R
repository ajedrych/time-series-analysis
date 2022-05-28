###########################################################################
# Analiza szeregów czasowych                                              #
# semestr letni 2022                                                      #
# 5                                                                       #
# Uniwersytet Warszawski, Wydział Nauk Ekonomicznych                      #
###########################################################################

###################################################
# I. Proces stacjonarny                           #
###################################################

#############################
# a. WHITE NOISE            #
#############################

#################
# Przyklad # 1  #
#################

#n - dlugosc szeregu
n<-1000

# 1.

# biały szum (e)
# e jest białym szumem (e~WN(0,sigma^2))
e<- ts(rnorm(n,mean=0,sd=1),frequency=1)
      # rozklad normalny z wartoscia oczekiwana =0, wariancja = 1  

# wykres
plot(e,
     type = "l",      # rodzaj wykresu (l=line)
     col  = "black",  # kolor linii
     lwd  = 2,        # podwójna grubosć
     main = "Normalny Bialy szum")  #oscyluje wokol wartosci zerowej

# test Ljung-Box
# Sprawdzenie za pomoca statystyki Q, H0: szereg jest bialym szumem
Box.test(e, lag=24, type="Ljung-Box") #
             # rzad p (max 24 rzad), typ testu (ljung box spk dla malej proby)
# p-value > 0.05 -> brak podstaw do odrzucenia hipotezy ze szereg jest bialym szumem
#p-value < 0.05 -> odrzucamy H0

# test Box-Pierce
# Sprawdzenie za pomoca statystyki Q, H0: szereg jest bialym szumem
Box.test(e, lag=24, type="Box-Pierce")


#################
# Przyklad # 2  #
#################

# biały szum (e)
# e1 jest białym szumem (e~U[0,1])
e1<- ts(runif(n,min =0, max = 1), frequency=1)
        # rozklad jednostajny na przedziale od 0 do 1
# bialy szum musi oscylowa wokol 0.5

# wykres
plot(e1,
     type = "l",      # rodzaj wykresu (l=line)
     col  = "black",  # kolor linii
     lwd  = 2,        # podwójna grubosć
     main = "Jednostajny Bialy szum")


# test Ljung-Box
# Sprawdzenie za pomoca statystyki Q, H0: szereg jest bialym szumem
Box.test(e1, lag=24, type="Ljung-Box")

# test Box-Pierce
# Sprawdzenie za pomoca statystyki Q, H0: szereg jest bialym szumem
Box.test(e1, lag=24, type="Box-Pierce")


###################################################
# b. Proces AR(1) - proces stacjonarny            #
#                                                 #
# y_t=alpha*y_t-1+e_t, |alpha|<1                  #
###################################################

# tworzymy wektor y wypełniony zerami
y  <- rep(0,n) 

alpha=0.2
#alpha=0.7
#alpha=-0.7
#alpha=0.9
for(i in 2:n)  y[i] <- alpha*y[i - 1] + e[i]



# wykres
plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "AR(1) - proces stacjonarny, alpha=0.2")


plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     xlim=c(0,100),
     main = "AR(1) - proces stacjonarny, alpha=0.2")


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


plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     xlim=c(0,100),
     ylim = c(-15,5),
     main = "Bladzenie przypadkowe")




######################################
# b. Bladzenie przypadkowe z dryfem  #
######################################

# tworzymy wektor y wypełniony zerami
y  <- rep(0,n)

mu=1
mu=0.1
mu=0.3
for(i in 2:n)  y[i] <- mu+y[i - 1] + e[i]



# wykres
plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Bladzenie przypadkowe z dryfem")

plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     xlim=c(0,100),
     main = "Bladzenie przypadkowe z dryfem")


###########################################
# c. Proces niestacjonarny alfa>1         #
###########################################

# tworzymy wektor y wypełniony zerami
y  <- rep(0,n)

alpha=1.01
for(i in 2:n)  y[i] <- alpha*y[i - 1] + e[i]



# wykres
plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Zmienna niestacjonarna")

plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     xlim=c(0,100),
     main = "Zmienna niestacjonarna")



# Jesli alfa > 1, to mamy  niestacjonarny szereg czasowy. 
# W tej konfiguracji, mozna uzyskac wygladzony szereg czasowy, który w końcu eksploduje. 

  

###################################################
# III. Regresje pozorne                           #
# Eksperyment Newbolda-Daviesa                    #
###################################################


# Utwórzmy zbior z 50 obserwacjami dwóch par zmiennych:
# 1) e1 i e2, niezależne względem siebie, obie IID, a zatem obie stacjonarne
# 2) y i x, niezależne względem siebie, obie będące błądzeniem losowym, a zatem obie niestacjonarne

# tworzymy tablicę x. y wypełnioną zerami
x  <- array(0, 50)
y  <- array(0, 50)
e1 <- rnorm(50, mean=0, sd=1)
e2 <- rnorm(50, mean=0, sd=1)

for(i in 2:50){
  x[i] <- x[i - 1] + e1[i]
  y[i] <- y[i - 1] + e2[i]
}

# 2.
# Regresja zmiennych stacjonarnych
model1 <- lm(e1 ~ e2)

# obejrzymy podsumowanie
summary(model1)

# sprawdźmy autokorelację reszt
library(lmtest)
dwtest(model1, alternative = "two.sided")


bg1 <- bgtest(model1, order = 1)
bg1
#brak autokorelacji reszt dla zmiennych stacjonarnych

# Regresja zmiennych niestacjonarnych
model2 <- lm(y ~ x)

# obejrzyjmy podsumowanie
summary(model2)

# sprawdźmy autokorelację reszt
dwtest(model2, alternative = "two.sided")


bg2 <- bgtest(model2, order = 1)
bg2


# Jaka byłaby interpretacja otrzymanych wyników?
#nie mozemy zmiennych niestacjonarnych uzywac w modelach, bo dostaemy regresje pozorna