library("Quandl", lib.loc="~/R/win-library/3.4")
library("quantmod", lib.loc="~/R/win-library/3.4")
library("PerformanceAnalytics", lib.loc="~/R/win-library/3.4")


Quandl.api_key("keyhere")


BTC <- Quandl("BCHARTS/BITSTAMPUSD", api_key="otkcuWHb5hYFTXTsiMds", transform="rdiff", start_date="2015-01-01")

BTC <- BTC[, c("Date", "Close")]

sig <- Lag(ifelse(BTC$Close < 0.15, 1, -1))

ret <- BTC$Close*sig
BTC <- cbind(BTC, ret)
BTC <- xts(BTC[,3], order.by = BTC[,1])
BTC <- BTC['2015-02-01/2017-10-28']

table.Drawdowns(BTC[, -2, drop=FALSE], top=10)
table.DownsideRisk(BTC)
charts.PerformanceSummary(BTC)
