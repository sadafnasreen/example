# This script solves the following contents
# first prepare the daily dataset into monthly and yearly using sum function
# Draw line and quantile plot
# Draw trend series of annual data set
# Draw mean monthly precipitation using bar plot
# Draw sum of monthly precipitation using box plot
# Trend and significance value of monthly rainfall
# Add libararies function which can be used here

library("readxl")
library(ggplot2)
library(dplyr)
library(lubridate)
library(hydroTSM)
library(data.table)
library(tidyverse)
library(astsa)
library(Kendall)
my_data <- read_excel("C:/Users/snasreen/Downloads/skill_eval.xlsx")

Mon_date = strftime(my_data$DATE, "%Y-%m")
#convert daily data into monthly 
Mon_Rainfall <- my_data$Rainfall
aggr_dailytomon = aggregate( Mon_Rainfall ~ Mon_date, FUN = sum)
#convert daily data into annual
Year_date = strftime(my_data$DATE, "%Y")
Year_Rainfall <- my_data$Rainfall
aggr_dailytoannual = aggregate( Year_Rainfall ~ Year_date, FUN = sum)

x <- aggr_dailytomon$Mon_date
y <- aggr_dailytomon$Mon_Rainfall
x1 <- aggr_dailytoannual$Year_date
y1 <- aggr_dailytoannual$Year_Rainfall

# line plot and quantile plot of yearly data set x1, y1
plot(x1,y1, type="b", col="blue", lwd=2, pch=5, xlab="time(years)", ylab="Rainfall")+
  title("Anuual Precipitation (1950-2019)")

#qqnorm(y1, pch = 1, frame = FALSE)
#qqline(y1, col = "steelblue", lwd = 2)

p <- ggplot(aggr_dailytoannual, aes(sample = Year_Rainfall))
p + stat_qq() + stat_qq_line()+
  labs(title = "Normal Q-Q quantile plot",y = "Rainfall_annual",x = "theoretical") + 
  theme_bw(base_size = 15)



# Find the significance of trend using man kandel method 
library(boot)
library(tseries)
library(trend)
data_yearly <- data.frame(years=x1,Rainfall_years=y1)
annual_trend<- ts (data_yearly$Rainfall_years, frequency = 1, start = 1950) # freq 12 => Monthly data. 
str(data_yearly)
adf.test(annual_trend)
kpss.test(annual_trend)
plot(annual_trend, col="blue")
lines(lowess(time(annual_trend), annual_trend), col="red", lwd=2)
par(mfrow=c(2,1))
acf(annual_trend)
pacf(annual_trend)
res <- MannKendall(annual_trend)
print(res)
summary(res)
MKtau <- function(z) MannKendall(z)$tau
tsboot(annual_trend_Series, MKtau, R=500, l=5, sim="fixed")
boot.out <- tsboot(annual_trend, MKtau, R=500, l=5, sim="fixed")
boot.ci(boot.out, type="perc")


# Convert daily data into monthly data set using sum function and box plot
my_data_new <- mutate(my_data, mo = month(DATE), yr = year(DATE)) %>%
  filter(DATE >= "1950-01-01") %>%
  group_by(yr, mo) %>% 
  summarise(prs1 = sum(Rainfall, na.rm = TRUE))
#mon <- mutate(my_data_new$mo, levels=c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec",))
ggplot(my_data_new) + 
  geom_boxplot(aes(x = mo, y = prs1, group = mo))+
  labs(title = "(Monthly Precipitation of Czech Repbulic (1950-2019))",y = "cummulative rainfall",x = "Date(Months)") + 
  theme_bw(base_size = 15)


# Monthly mean precipitation using bar plot
my_data <- mutate(my_data, mo = month(DATE), yr = year(DATE)) %>%
  filter(DATE >= "1950-01-01") %>%
  group_by(yr, mo) %>% 
  summarise(prs = mean(Rainfall, na.rm = TRUE))
ggplot(data = my_data, aes(x = mo, y = prs, group=mo)) +
  geom_bar(stat="identity",color="purple") +
  labs(title = "12 Months Rainfall of Czech Republic 01/1950-05/2019",
       x = "Date(Months)", y = " mean rainfall")

# Trend and significance of monthly rainfall data set
data_monthly <- data.frame(months=x,Rainfall_months=y)
Months_trend<- ts (data_monthly$Rainfall_months, frequency = 12, start = 1950) # freq 12 => Monthly data. 
print(data_monthly)
adf.test(Months_trend)
kpss.test(Months_trend)
plot(Months_trend, col="green")
lines(lowess(time(Months_trend), Months_trend), col="blue", lwd=2)
par(mfrow=c(2,1))
acf(Months_trend)
pacf(Months_trend)
stlRes <- stl(Months_trend, s.window = "periodic")
res <- MannKendall(Months_trend)
print(res)
summary(res)
MKtau <- function(z) MannKendall(z)$tau
tsboot(Months_trend, MKtau, R=500, l=5, sim="fixed")
boot.out <- tsboot(Months_trend, MKtau, R=500, l=5, sim="fixed")
boot.ci(boot.out, type="perc")

