###########################################################################
# Analiza szeregoww czasowych                                             #
# semestr letni 2021                                                      #
# 7                                                                       #
# Uniwersytet Warszawski, Wydzial Nauk Ekonomicznych                     #
###########################################################################

###########################################################################
# I. Symulacja szeregOw AR(p)                                             #
###########################################################################

# 1. AR(1)
# Model autoregresyjny rz?du p=1, spelniajacy rownanie:
# y_t=alpha*y_t-1+e_t

#n - dlugosc szeregu
n<-1000

# e jest bialym szumem (e~WN(0,sigma^2))
e<- ts(rnorm(n,mean=0,sd=1),frequency=1)


# tworzymy wektor y wypelniony zerami
y  <- rep(0,n)

alpha=0.2
#alpha=0.9
#alpha=1.2

for(i in 2:n)  y[i] <- alpha*y[i - 1] + e[i]



# wykres
plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "AR(1)")


# ALBO

# Funkcja arima.sim()  z pakietu stats

library(stats)
AR1.sim<-arima.sim(n=1000,
                   model=list(order=c(1,0,0),
                              ar=c(0.2)))

plot(AR1.sim)


# 2. AR(4)
# Model autoregresyjny rzedu p=4, spelniajacy rownanie:
# y_t=0.1y_t-1-0.4yt-2+0.3y_t-3+0.2y_t-4+e_t

# Funkcja arima.sim()  z pakietu stats


AR4.sim<-arima.sim(n=1000,
                   model=list(order=c(4,0,0),
                   ar=c(0.1,-0.4,0.3,0.2)))

plot(AR4.sim) #proces stacjonarny, wha si? wok?? sta?ej nie ma trendu



###########################################################################
# II. Symulacja szeregów MA(q)                                            #
###########################################################################

# 1. MA(1) spelniajacy rownanie:
# y_t=e_t+1.2*e_t-1

#n - dlugosc szeregu
n<-1000

# e jest bialym szumem (e~WN(0,sigma^2))
e<- ts(rnorm(n,mean=0,sd=1),frequency=1)


# tworzymy wektor y wypelniony zerami
y  <- rep(0,n)

#theta=0.2
#theta=0.9
theta=1.2

for(i in 2:n)  y[i] <-  e[i]+theta*e[i - 1] 



# wykres
plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "MA(1)")


# ALBO

MA1.sim<-arima.sim(n=1000,
                      model=list(order=c(0,0,1),
                                  ma=c(1.2)))

plot(MA1.sim)




###########################################################################
# III. Symulacja szeregow ARMA(p,q)                                       #
###########################################################################

# 1. ARMA(2,1) spelniajacy rownanie:
# y_t=0.8y_t-1-0.4yt-2+e_t+0.7e_t-1

ARMA21.sim<-arima.sim(n=1000,
                   model=list(order=c(2,0,1),
                              ar=c(0.8,-0.4), ma=0.7))

plot(ARMA21.sim)
