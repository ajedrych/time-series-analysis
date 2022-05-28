###########################################################################
# Analiza szeregów czasowych                                              #
# semestr letni 2022                                                      #
# 2                                                                       #
# Uniwersytet Warszawski, Wydział Nauk Ekonomicznych                      #
###########################################################################


###################################################
# I. TREND                                        #
###################################################



#n - dlugosc szeregu
n<-1000

# 1.

# bialy szum (e)
# e jest bialym szumem (e~WN(0,sigma^2))
e<- ts(rnorm(n,mean=0,sd=1),frequency=1)


# wykres
plot(e,
     type = "l",      # rodzaj wykresu (l=line)
     col  = "black",  # kolor linii
     lwd  = 2,        # podwojna grubosc
     main = "White noise")


# 2.

#Trend deterministyczny

# X_t=alpha + beta*t + e

alpha=1
beta=1.5
t<-1:n      #time variable
x<-alpha+beta*t+e


# wykres
plot(x,
     type = "l",
     col  = "red",
     lwd  = 2,
     main = "Trend deterministyczny")


# 3.
# Trend stochastyczny 

# tworzymy wektor y wypelniony zerami
y  <- rep(0,n)

for(i in 2:n)  y[i] <- y[i - 1] + e[i]



# wykres
plot(y,
     type = "l",
     col  = "blue",
     lwd  = 2,
     main = "Trend stochastyczny")



###################################################
# II. SEZONOWOSC                                  #
###################################################

#1
# Sezonowosc deterministyczna


library(zoo)

df <- data.frame("Date" = as.yearqtr(1960 + seq(0, 199)/4), "e"=rnorm(200,mean=0,sd=))

df$Quarter <- format(df$Date, "%q")
df$Year <- format(df$Date, "%Y")


df$Quarter1 <- ifelse(df$Quarter==1,1,0)
df$Quarter2 <- ifelse(df$Quarter==2,1,0)
df$Quarter3 <- ifelse(df$Quarter==3,1,0)
df$Quarter4 <- ifelse(df$Quarter==4,1,0)



df$xxx = -10*df$Quarter1 - 2*df$Quarter2 + 2*df$Quarter3 + 10*df$Quarter4 + df$e
print(head(df))

library(ggplot2)
plt1 <- ggplot(data=df, aes(x=Date, y=xxx)) +
  geom_line(color="navyblue") +    #typ wykresu - liniowy
  geom_point()                     #typ wykresu - punktowy
print(plt1)


plt2 <- ggplot(df, aes(x = Year, y = xxx, color = Quarter, group = Quarter)) + 
  geom_point() + 
  geom_line() + 
#  facet_grid(rows = vars(Quarter)) + #facet_grid() pozwala na podzielenie danych na podproby 
                                      #i wygenerowanie wykresu dla kazdej z nich osobno.
  theme(axis.text.x = element_text(angle = 45, hjust=1)) # theme - motyw; hjust - wyrownanie w poziomie
print(plt2)




# 2.
# Sezonowosc stochastyczna


df1 <- data.frame("Date" = as.yearqtr(1960 + seq(0, 499)/4), "e"=rnorm(500,mean=0,sd=))

df1$Quarter <- format(df1$Date, "%q")
df1$Year <- format(df1$Date, "%Y")


df1$Quarter1 <- ifelse(df1$Quarter==1,1,0)
df1$Quarter2 <- ifelse(df1$Quarter==2,1,0)
df1$Quarter3 <- ifelse(df1$Quarter==3,1,0)
df1$Quarter4 <- ifelse(df1$Quarter==4,1,0)

#n - dlugosc szeregu
n<-500

df1$y_s  <- rep(0,n)
for(i in 5:n)  df1$y_s[i] <- df1$y_s[i - 4] + df1$e[i]
print(head(df1))

plt1 <- ggplot(data=df1, aes(x=Date, y=y_s, group=1)) +
  geom_line(color="navyblue") +
  geom_point()
print(plt1)


plt2 <- ggplot(df1, aes(x = Year, y = y_s, color = Quarter, group = Quarter)) + 
  geom_point() + 
  geom_line() + 
  theme(axis.text.x = element_text(angle = 45, hjust=1))
print(plt2)
