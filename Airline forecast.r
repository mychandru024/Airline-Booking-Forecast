library(xlsx)
library(prophet)
library(dplyr)

data <- read.xlsx("C:/Users/ABC/Desktop/infogain/data.xlsx", sheetName = "RawDataSet")

data_697_eco <- subset(data, FlightID==697 & Cabin=="Economy Cabin")
dim(data_697_eco)

ts_data <- data_697_eco[, c("DepartureDate", "CabinBookings")]
dim(ts_data)


data_train <- subset(ts_data, DepartureDate<'2015-04-01')
data_future <- subset(ts_data, DepartureDate>='2015-04-01')
dim(data_train)
dim(data_future)
min(data_train$ds)
max(data_train$ds)
min(data_future$ds)
max(data_future$ds)

data_train <- data_train %>% 
  rename(
    ds = DepartureDate,
    y = CabinBookings
  )

data_future <- data_future %>% 
  rename(
    ds = DepartureDate,
    y = CabinBookings
  )
head(data_train)

sapply(data_train, class)

m <- prophet(data_train)
future <- make_future_dataframe(m, periods = 92)
head(future)
forecast <- predict(m, future)
plot(m, forecast)
tail(future)
prophet_plot_components(m, forecast)

forecasted <- subset(forecast, ds>='2015-03-31')
head(forecasted)

mae = mean(abs(forecasted$yhat - data_future$y))
cat("MAE: ", mae)

