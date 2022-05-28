###########################################################################
# Analiza szeregów czasowych                                              #
# semestr letni 2022                                                      #
# 2 dodatkowe                                                             #
# Uniwersytet Warszawski, Wydział Nauk Ekonomicznych                      #
###########################################################################



##########################################################################
# 2. Decomposition

# Recommended for seasonal adjustment for official statistics
# https://ec.europa.eu/eurostat/cros/content/software-jdemetra_en

# Open source:
# https://github.com/jdemetra


# Requirements: rJava and Java SE 8+
#  • RJDemetra interface for JDemetra+
#  • jdemetra-R for more JD+ routines in R
# Available on Github
# https://github.com/nbbrd/rjdemetra
# https://github.com/nbbrd/jdemetra-R


install.packages("rJava")
install.packages("RJDemetra")

# loading packets into memory
library(rJava)
library(RJDemetra)
library(foreign)
install.packages("readxl")
library(readxl)
library(dygraphs)

AirPass <- read.csv("dane/AirPass.csv", # file name
                    header = TRUE,    	  # are the observations in the first row? 
                    sep = ";") 	          # column separator

#                   dec = ","             # decimal separator

class(AirPass) # "data frame" type object 

# we will see the imported data
str(AirPass)
AirPass
head(AirPass)
tail(AirPass)

# After importing the data, we got an object of the data frame type, resembling
# spreadsheet, containing individual variables in the following columns.

# The data can be converted to the standard "ts" class
# to represent time series. 

is.ts(AirPass)


AirPass = ts(data=AirPass$TOTAL, frequency = 12,             
             start=c(2003,1), end=c(2019,12)) 

class(AirPass) 

# Graph
dygraph(AirPass, main="AirPass") %>% dyRangeSelector(height = 40)

# We decompose using the TRAMO/SEATS
model <- tramoseats(AirPass)
# for X-13ARIMA
# model <- x13(AirPass)

dec <- model$final$series

# plot of the seasonally adjusted series against the original series

dygraph(dec[,c("y","sa")], main="AirPass [seasonally adjusted series against the original]") %>% dyRangeSelector(height = 40) %>%
  dySeries("y", color = "gray", strokeWidth = 2, label="oryginalny") %>%
  dySeries("sa", color = "red", strokeWidth = 2, label="seasonally adjusted series")

# trend graph against the original series 

dygraph(dec[,c("y","t")], main="AirPass [trend against the original series]") %>% dyRangeSelector(height = 40) %>%
  dySeries("y", color = "gray",strokeWidth = 2, label="original series") %>%
  dySeries("t", color = "blue",strokeWidth = 2, label="trend")

# plot of seasonal effects against the original series 

dygraph(dec[,c("y","s")], main="AirPass [seasonal effects against the original series]") %>% 
  dyAxis("y", label = "original series") %>%
  dyAxis("y2", label = "seasonal effects") %>%
  dyRangeSelector(height = 40) %>%
  dySeries("y", color = "gray",strokeWidth = 2, label="original series") %>%
  dySeries("s", color = "green",strokeWidth = 1, label="seasonal effects",axis="y2")

# https://github.com/jennie-davies/SA-training-app


##########################################################################
#     Thank you !!!	                                                     #
##########################################################################
