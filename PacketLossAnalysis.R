#Load libraries

library(magrittr)
library(forecast)
library(ggplot2)
library(tseries)

#Read-in Data

# Read one hourly data:
DFHour <- read.csv("videoHourly02.csv",header=TRUE,sep=",")
# Calculate Percent Lost
DFHour$packtsPcnt <- (DFHour$pcktsLost/DFHour$pcktsCount)*100
DFAnalysis <- DFHour[,c(4,2,3,5)]
head(DFAnalysis)

#Convert to Time Series
pckt_data_full <- DFAnalysis$packtsPcnt%>%ts(start=c(0,1),frequency = 24)
# Keep only first 21 days
pckt_data <- pckt_data_full%>%subset.ts(end=21*24)
# Create Test Set for one day in the future
pckt_data_test <- pckt_data_full%>%subset.ts(start=21*24+1, end =  22*24)
pckt_data%>%autoplot() + ylab("Percent Packet Lost (%)") + xlab("Time (Days/Hours)")

# Diagnostics
adf.test(pckt_data)
kpss.test(pckt_data, null="Trend")


# Explore sesonality of the data

pckt_data%>%ggsubseriesplot() + xlab("Hour of the day") + ylab("Percent Packet Lost (%)")
pckt_data%>%ggPacf() +ggtitle("Autocorrelation for Percent Package Loss Series")
pckt_data%>%Box.test(type = "Ljung")
ggsubseriesplot(pckt_data)
ggseasonplot(pckt_data%>%subset(end=10*24))

# Fourier Transformation to find periodicity

library(TSA)
pdg <- periodogram(pckt_data)
pdg$spec%>%order

1/pdg$freq[21]
1/pdg$freq[85]
1/pdg$freq[7]
1/pdg$freq[64]


# Model Fitting

## Seasonal Naive

fitnaive <- snaive(pckt_data)
fcnaive <- forecast(fitnaive,h=24)
fcnaive%>%autoplot() + ylab("Percent Packet Lost (%)") + autolayer(pckt_data_test, size=0.2)
accuracy(fcnaive,pckt_data_test)
checkresiduals(fitnaive)

## ETS Model 

fitexp <- ets(pckt_data)
summary(fitexp)
fcets <- fitexp%>%forecast(h=24)
fcets%>%autoplot() +ylab("Percent Packet Lost (%)") + autolayer(pckt_data_test, size=0.2)
accuracy(fcets,pckt_data_test)
checkresiduals(fitexp)

## ARIMA Models

BoxCox.lambda(pckt_data)
autoplot(pckt_data)
pckt_data%>%BoxCox(-0.303)%>%autoplot

# ARIMA Models 
arimafit <- auto.arima(pckt_data)
summary(arimafit)
checkresiduals(arimafit)
fcarima<- arimafit%>% forecast(h=24)
fcarima%>%autoplot() + ylab("Percent Packet Lost (%)") + autolayer(pckt_data_test, size=0.2)
accuracy(fcarima, pckt_data_test)

## TBATS

fitbats <- tbats(pckt_data)
fctbats <- forecast(fitbats, h=24) 
fctbats%>%autoplot() + ylab("Percent Packet Lost (%)") + autolayer(pckt_data_test, size=0.2)
accuracy(f = fctbats, pckt_data_test)
checkresiduals(fitbats)