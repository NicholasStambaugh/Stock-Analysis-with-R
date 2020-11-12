rm(list=ls())
#library
library(quantmod)
library(ggplot2)

#import stock data

NEE <- read_csv("C:/Users/Nick/Desktop/CODING/Stock analysis in R/NEE.csv")

#for best format

nee <- getSymbols("NEE", src = "yahoo", 
                  from = "2017-01-01", to = "2020-10-01", 
                  auto.assign = FALSE)

#more formating, lines 20 and 22 not working

nee <- read_csv("C:/Users/Nick/Desktop/CODING/Stock analysis in R/NEE.csv")

nee[,1] <- as.Date(nee[,1])

nee <- xts(nee)

nee <- nee[,-1]

### Basic visualization ###

head(nee)

tail(nee)

summary(nee)

str(nee)

# time to plot daily prices!

# redo line 8 or 12 if it doesn't plot

ggplot(nee, aes(x = index(nee), y = nee[,6])) + 
  geom_line(color = "red") + 
  ggtitle("Nextera Energy Prices Series") + 
  xlab("Date") + ylab("Price") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(date_labels = "%b %y", 
              date_breaks = "6 months")

# moving average

nee_mm <- subset(nee, index(nee) >="2019-01-01")

nee_mm10 <- rollmean(nee_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")
nee_mm30 <- rollmean(nee_mm[,6], 30, fill = list(NA, NULL, NA), align = "right")

nee_mm$mm10 <- coredata(nee_mm10)
nee_mm$mm30 <- coredata(nee_mm30)

# Plotting moving averages

## When 'Short Run' crosses 'Long Run' 
## while going upwards, BUY
#if opposite happens, SELL


ggplot(nee_mm, aes(x = index(nee_mm))) +
  geom_line(aes(y = nee_mm[,6], color = "NEE")) + ggtitle("Nextera Energy Price Series") +
  geom_line(aes(y = nee_mm$mm10, color = "Short Run")) +
  geom_line(aes(y = nee_mm$mm30, color = "Long Run" )) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_colour_manual("Series", values = c("NEE"="gray40", "Short Run"="firebrick4", "Long Run"="darkcyan"))

# Returns!
nee_ret <- diff(log(nee[,6]))
nee_ret <- nee_ret[-1,]

dailyReturn(nee)
weeklyReturn(nee)
monthlyReturn(nee)
quarterlyReturn(nee)
yearlyReturn(nee)

summary(nee_ret) #We can see an increase in price here!
sd(nee_ret) #We see here returns are okay, apple can have returns up to 35%!!


#time to plot our returns!

#we see here returns fell and rose quickly around april
#with returns being larger now

ggplot(nee_ret, aes(x = index(nee_ret), y = nee_ret)) +
  geom_line(color = "deepskyblue4") +
  ggtitle("Nextera Energy return series") +
  xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months")

#let's look at returns in 2019, after the crash

#Shows increase in returns after crash

nee_ret19 <- subset(nee_ret, index(nee_ret) > "2020-01-01")

ggplot(nee_ret19, aes(x = index(nee_ret19), y = nee_ret19)) +
  geom_line(color = "deepskyblue4") +
  ggtitle("Nextera Energy return series in 2020") + xlab("Date") + ylab("Return") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 months")

summary(nee_ret19)
sd(nee_ret19)
