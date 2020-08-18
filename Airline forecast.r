library(xlsx)
library(prophet)
library(dplyr)

data <- read.xlsx("C:/Users/ABC/Desktop/infogain/data.xlsx", sheetName = "RawDataSet")

#summary(data)
#head(data)
#dim(data)

data_697_eco <- subset(data, FlightID==697 & Cabin=="Economy Cabin")
#dim(data_697_eco)

ts_data <- data_697_eco[, c("DepartureDate", "CabinBookings")]
#dim(ts_data)

#summary(ts_data)

data_train <- subset(ts_data, DepartureDate<'2015-04-01')
data_future <- subset(ts_data, DepartureDate>='2015-04-01')
#dim(data_train)
##dim(data_future)
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
#m <- add_country_holidays(m, country_name = 'IN')
#future <- data.frame("ds" = data_future$ds)
future <- make_future_dataframe(m, periods = 92)
head(future)
forecast <- predict(m, future)
plot(m, forecast)

prophet_plot_components(m, forecast)

cat("RMSE: ", sqrt(mean((forecast$yhat - data_future$y)^2)))
mae = mean(abs(forecast$yhat - data_future$y))
cat("MAE: ", mae)
dim(forecast)
head(forecast)

o <- data.frame(forecast$yhat, data_future$y)
print(o)

future_ <- make_future_dataframe(m, periods = 92, include_history = FALSE)
min(future_$ds)
max(future_$ds)
tail(m$history)
