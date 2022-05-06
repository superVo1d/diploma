{
  library('fGarch')
  library('rugarch')
  library('corrplot')
  library('PortfolioAnalytics')
  library('PerformanceAnalytics')
  library('quantmod')
  library('ROI')
  library('ROI.plugin.glpk')
  library('ROI.plugin.quadprog')
  library('dplyr')
  library('lmtest')
  library('tseries')
  library('forecast')
  library('vars')
  library('stargazer')
  library('fPortfolio')
  library('vioplot')
  library('rmgarch')
  library('ggplot2')
  library('dynlm')
  library('MTS')
}

#
# Подготовка данных
#

setwd("~/Desktop/diploma")
getwd()

# Криптовалюты
btc <- data.frame(read.csv('./data/coin_Bitcoin.csv'))
eth <- data.frame(read.csv('./data/coin_Ethereum.csv'))
bnb <- data.frame(read.csv('./data/coin_BinanceCoin.csv'))
ltc <- data.frame(read.csv('./data/coin_Litecoin.csv'))
xmr <- data.frame(read.csv('./data/coin_Monero.csv'))
doge <- data.frame(read.csv('./data/coin_Dogecoin.csv'))
usdt <- data.frame(read.csv('./data/coin_Tether.csv'))
xrp <- data.frame(read.csv('./data/coin_XRP.csv'))
ada <- data.frame(read.csv('./data/coin_Cardano.csv'))
link <- data.frame(read.csv('./data/coin_ChainLink.csv'))

btc <- data.frame(btc$Date, btc$Close)
eth <- data.frame(eth$Date, eth$Close)
bnb <- data.frame(bnb$Date, bnb$Close)
ltc <- data.frame(ltc$Date, ltc$Close)
xmr <- data.frame(xmr$Date, xmr$Close)
doge <- data.frame(doge$Date, doge$Close)
usdt <- data.frame(usdt$Date, usdt$Close)
xrp <- data.frame(xrp$Date, xrp$Close)
ada <- data.frame(ada$Date, ada$Close)
link <- data.frame(link$Date, link$Close)

colnames(btc) <- c('Date', 'btc')
colnames(eth) <- c('Date', 'eth')
colnames(bnb) <- c('Date', 'bnb')
colnames(ltc) <- c('Date', 'ltc')
colnames(xmr) <- c('Date', 'xmr')
colnames(doge) <- c('Date', 'doge')
colnames(usdt) <- c('Date', 'usdt')
colnames(xrp) <- c('Date', 'xrp')
colnames(ada) <- c('Date', 'ada')
colnames(link) <- c('Date', 'link')

btc$Date <- lapply(btc$Date, function(x) sub(" .*", "", x))
btc <-as.data.frame(lapply(btc, unlist))

eth$Date <- lapply(eth$Date, function(x) sub(" .*", "", x))
eth <-as.data.frame(lapply(eth, unlist))

bnb$Date <- lapply(bnb$Date, function(x) sub(" .*", "", x))
bnb <-as.data.frame(lapply(bnb, unlist))

ltc$Date <- lapply(ltc$Date, function(x) sub(" .*", "", x))
ltc <-as.data.frame(lapply(ltc, unlist))

xmr$Date <- lapply(xmr$Date, function(x) sub(" .*", "", x))
xmr <-as.data.frame(lapply(xmr, unlist))

doge$Date <- lapply(doge$Date, function(x) sub(" .*", "", x))
doge <-as.data.frame(lapply(doge, unlist))

usdt$Date <- lapply(usdt$Date, function(x) sub(" .*", "", x))
usdt <-as.data.frame(lapply(usdt, unlist))

xrp$Date <- lapply(xrp$Date, function(x) sub(" .*", "", x))
xrp <-as.data.frame(lapply(xrp, unlist))

ada$Date <- lapply(ada$Date, function(x) sub(" .*", "", x))
ada <-as.data.frame(lapply(ada, unlist))

link$Date <- lapply(link$Date, function(x) sub(" .*", "", x))
link <-as.data.frame(lapply(link, unlist))

# Акции из s&p 500 и индексы
AAPL <- getSymbols("AAPL", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$AAPL.Close
MSFT <- getSymbols("MSFT", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$MSFT.Close
AMZN <- getSymbols("AMZN", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$AMZN.Close
GOOGL <- getSymbols("GOOGL", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$GOOGL.Close
GOOG <- getSymbols("GOOG", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$GOOG.Close
TSLA <- getSymbols("TSLA", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$TSLA.Close
JNJ <- getSymbols("JNJ", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$JNJ.Close
NVDA <- getSymbols("NVDA", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$NVDA.Close
FB <- getSymbols("FB", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$FB.Close
UNH <- getSymbols("UNH", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$UNH.Close

GSPC <- getSymbols("^GSPC", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$GSPC.Close
DJI <- getSymbols("^DJI", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$DJI.Close
SX5E <- getSymbols("^STOXX50E", from = "2017-10-02", to = "2021-07-06", auto.assign = FALSE)$STOXX50E.Close

AAPL <- data.frame(index(AAPL), as.data.frame(AAPL))
colnames(AAPL) <- c('Date', 'AAPL')
AAPL$Date <- as.character(AAPL$Date)
row.names(AAPL) <- NULL
AAPL <- as.data.frame(lapply(AAPL, unlist))

MSFT <- data.frame(index(MSFT), as.data.frame(MSFT))
colnames(MSFT) <- c('Date', 'MSFT')
MSFT$Date <- as.character(MSFT$Date)
row.names(MSFT) <- NULL
MSFT <- as.data.frame(lapply(MSFT, unlist))

AMZN <- data.frame(index(AMZN), as.data.frame(AMZN))
colnames(AMZN) <- c('Date', 'AMZN')
AMZN$Date <- as.character(AMZN$Date)
row.names(AMZN) <- NULL
AMZN <- as.data.frame(lapply(AMZN, unlist))

GOOGL <- data.frame(index(GOOGL), as.data.frame(GOOGL))
colnames(GOOGL) <- c('Date', 'GOOGL')
GOOGL$Date <- as.character(GOOGL$Date)
row.names(GOOGL) <- NULL
GOOGL <- as.data.frame(lapply(GOOGL, unlist))

GOOG <- data.frame(index(GOOG), as.data.frame(GOOG))
colnames(GOOG) <- c('Date', 'GOOG')
GOOG$Date <- as.character(GOOG$Date)
row.names(GOOG) <- NULL
GOOG <- as.data.frame(lapply(GOOG, unlist))

TSLA <- data.frame(index(TSLA), as.data.frame(TSLA))
colnames(TSLA) <- c('Date', 'TSLA')
TSLA$Date <- as.character(TSLA$Date)
row.names(TSLA) <- NULL
TSLA <- as.data.frame(lapply(TSLA, unlist))

JNJ <- data.frame(index(JNJ), as.data.frame(JNJ))
colnames(JNJ) <- c('Date', 'JNJ')
JNJ$Date <- as.character(JNJ$Date)
row.names(JNJ) <- NULL
JNJ <- as.data.frame(lapply(JNJ, unlist))

NVDA <- data.frame(index(NVDA), as.data.frame(NVDA))
colnames(NVDA) <- c('Date', 'NVDA')
NVDA$Date <- as.character(NVDA$Date)
row.names(NVDA) <- NULL
NVDA <- as.data.frame(lapply(NVDA, unlist))

FB <- data.frame(index(FB), as.data.frame(FB))
colnames(FB) <- c('Date', 'FB')
FB$Date <- as.character(FB$Date)
row.names(FB) <- NULL
FB <- as.data.frame(lapply(FB, unlist))

UNH <- data.frame(index(UNH), as.data.frame(UNH))
colnames(UNH) <- c('Date', 'UNH')
UNH$Date <- as.character(UNH$Date)
row.names(UNH) <- NULL
UNH <- as.data.frame(lapply(UNH, unlist))

GSPC <- data.frame(index(GSPC), as.data.frame(GSPC))
colnames(GSPC) <- c('Date', 'GSPC')
GSPC$Date <- as.character(GSPC$Date)
row.names(GSPC) <- NULL
GSPC <- as.data.frame(lapply(GSPC, unlist))

DJI <- data.frame(index(DJI), as.data.frame(DJI))
colnames(DJI) <- c('Date', 'DJI')
DJI$Date <- as.character(DJI$Date)
row.names(DJI) <- NULL
DJI <- as.data.frame(lapply(DJI, unlist))

SX5E <- data.frame(index(SX5E), as.data.frame(SX5E))
colnames(SX5E) <- c('Date', 'SX5E')
SX5E$Date <- as.character(SX5E$Date)
row.names(SX5E) <- NULL
SX5E <- as.data.frame(lapply(SX5E, unlist))

# Объединим в один датафрейм
df <- na.omit(merge(btc, eth, by = 'Date', all.x = TRUE, all.y = TRUE))
df <- na.omit(merge(df, bnb, by = 'Date', all.x = TRUE, all.y = TRUE))
df <- na.omit(merge(df, ltc, by = 'Date', all.x = TRUE, all.y = TRUE))
df <- na.omit(merge(df, xmr, by = 'Date', all.x = TRUE, all.y = TRUE))
df <- na.omit(merge(df, doge, by = 'Date', all.x = TRUE, all.y = TRUE))
df <- na.omit(merge(df, usdt, by = 'Date', all.x = TRUE, all.y = TRUE))
df <- na.omit(merge(df, xrp, by = 'Date', all.x = TRUE, all.y = TRUE))
df <- na.omit(merge(df, ada, by = 'Date', all.x = TRUE, all.y = TRUE))
df <- na.omit(merge(df, link, by = 'Date', all.x = TRUE, all.y = TRUE))

df_all_crypto <- df

df <- merge(df, AAPL, by = 'Date', all.x = TRUE, all.y = TRUE)
df <- merge(df, MSFT, by = 'Date', all.x = TRUE, all.y = TRUE)
df <- merge(df, AMZN, by = 'Date', all.x = TRUE, all.y = TRUE)
df <- merge(df, GOOGL, by = 'Date', all.x = TRUE, all.y = TRUE)
df <- merge(df, GOOG, by = 'Date', all.x = TRUE, all.y = TRUE)
df <- merge(df, TSLA, by = 'Date', all.x = TRUE, all.y = TRUE)
df <- merge(df, JNJ, by = 'Date', all.x = TRUE, all.y = TRUE)
df <- merge(df, NVDA, by = 'Date', all.x = TRUE, all.y = TRUE)
df <- merge(df, FB, by = 'Date', all.x = TRUE, all.y = TRUE)
df <- merge(df, UNH, by = 'Date', all.x = TRUE, all.y = TRUE)

df_all_assets <- df

df <- merge(df, GSPC, by = 'Date', all.x = TRUE, all.y = TRUE)
df <- merge(df, DJI, by = 'Date', all.x = TRUE, all.y = TRUE)
df <- merge(df, SX5E, by = 'Date', all.x = TRUE, all.y = TRUE)

# Преобразуем во временной формат
df_all_crypto$Date <- as.Date(df_all_crypto$Date)  
df_all_crypto <- xts(df_all_crypto[,-c(1)], df_all_crypto$Date)

df_all_shares <- df[,c('Date', 'AAPL', 'MSFT', 'AMZN', 'GOOGL', 'GOOG', 'TSLA', 'JNJ', 'NVDA', 'FB', 'UNH')]
df_all_shares <- xts(df_all_shares[,-c(1)], as.Date(df_all_shares$Date))

df_all_assets$Date <- as.Date(df_all_assets$Date)  
df_all_assets <- xts(df_all_assets[,-c(1)], df_all_assets$Date)

df_ts <- df
df_ts$Date <- as.Date(df_ts$Date)  
df_ts <- xts(df_ts[,-c(1)], df_ts$Date)

# Описание данных

# Временной интервал: 2017-10-02 — 2021-07-06
# Число наблюдений:
dim(df_ts)[1]

# Перейдем к доходности
Returns <- Return.calculate(df_ts, method = "log")

# Описание статистик
table.Stats(Returns)

# Корреляционная матрица
cor(na.omit(df_ts))
corrplot(cor(na.omit(df_ts)))

Returns <- Return.calculate(df_ts, method = "log")

btc <- Returns$btc
eth <- Returns$eth
bnb <- Returns$bnb
ltc <- Returns$ltc
xmr <- Returns$xmr
doge <- Returns$doge
usdt <- Returns$usdt
xrp <- Returns$xrp
ada <- Returns$ada
link <- Returns$link

AAPL <- Returns$AAPL
MSFT <- Returns$MSFT
AMZN <- Returns$AMZN
GOOGL <- Returns$GOOGL
GOOG <- Returns$GOOG
TSLA <- Returns$TSLA
JNJ <- Returns$JNJ
NVDA <- Returns$NVDA
FB <- Returns$FB
UNH <- Returns$UNH

GSPC <- Returns$GSPC
DJI <- Returns$DJI
SX5E <- Returns$SX5E

dev.off()
# Графики доходности
par(mfrow=c(4,4))
for (asset_name in colnames(Returns)) {
  asset_R <- Returns[,asset_name]
  print(plot(asset_R, main=asset_name))
}

# Скользящее среднее отклонение
par(mfrow=c(4,4))
for (asset_name in colnames(Returns)) {
  asset_R <- Returns[,asset_name]
  print(chart.RollingPerformance(asset_R, width = 30, FUN = "sd", 'main' = asset_name))
}

# Функция распределния доходности
#par(mfrow=c(4,4))
#for (asset_name in colnames(Returns)) {
#  asset_R <- Returns[,asset_name]
#  print(chart.ECDF(asset_R))
#}

# Квантиль-квантиль
par(mfrow=c(4,4))
for (asset_name in colnames(Returns)) {
  asset_R <- Returns[,asset_name]

  qqnorm(asset_R, pch = 1, frame = FALSE, main=sprintf("Normal Q-Q Plot: %s", asset_name))
  qqline(asset_R, col = "#0000ff", lwd = 1)
}

# Гистограмма доходности
par(mfrow=c(4,4))
for (asset_name in colnames(Returns)) {
  asset_R <- Returns[,asset_name]

  print(chart.Histogram(asset_R, methods = c("add.density", "add.normal", "add.rug")))
}

# Графики виолончели

par(mfrow=c(1,1))
print(vioplot(na.omit(as.data.frame(Returns)), colnames(Returns), col= c(rep("lightblue", 10), rep('blueviolet', 10), rep('coral', 3))))

#
# Портфели
#

# Средние доходности
Returns <- na.omit(Returns)
colMeans(Returns)

# Запишем в отдельную переменную доходность торгуемых активов
Returns_with_index <- Returns
Returns <- Returns[,colnames(df_all_assets)]

# Разбиваем на обучающую и тестовую выборку
sample_size <- floor((1 - 1/exp(1)) * nrow(Returns))
train_indexes <- seq_len(sample_size)

R_train <- Returns[train_indexes, ]
R_test <- Returns[-train_indexes, ]

#
# 1. Портфель минимизирующий дисперсию
#
portfolio <- portfolio.spec(assets = colnames(Returns))

return_target <- mean(colMeans(R_train))

# Линейные ограничения
portfolio <- add.constraint(portfolio, type = "full_invsetment") # сумма весов 1
portfolio <- add.constraint(portfolio, type = "return", name = "mean", return_target = return_target)

# Минимизация дисперсии
portfolio <- add.objective(portfolio, type = "risk", name = "var")

# Строим на обучающей выборке
result <- optimize.portfolio(portfolio = portfolio, R = R_train, optimize_method = "ROI", portfolio_method="component", trace=TRUE)

# Обучающая выборка
day_return <- Return.portfolio(R = R_train, weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

plot(day_return)
charts.PerformanceSummary(day_return)

barplot <- barplot(result$weights, main="", xlab="")

#chart.RiskReward(result, risk.col = "StdDev", chart.assets=TRUE)
#chart.RiskReward(result, risk.col = "ES", chart.assets=TRUE)
plot(result, risk.col="StdDev", return.col="mean", main="", chart.assets=TRUE, xlim=c(0, 0.05), ylim=c(0,0.0085)) 

# Тестовая выборка
day_return <- Return.portfolio(R = R_test, weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)


# Портфель без криптовалют
portfolio <- portfolio.spec(assets = colnames(df_all_shares))

return_target <- mean(colMeans(R_train[,colnames(df_all_shares)]))

# Линейные ограничения
portfolio <- add.constraint(portfolio, type = "full_invsetment") # сумма весов 1
portfolio <- add.constraint(portfolio, type = "return", name = "mean", return_target = return_target)

# Минимизация дисперсии
portfolio <- add.objective(portfolio, type = "risk", name = "var")

# Строим на обучающей выборке
result <- optimize.portfolio(portfolio = portfolio, R = R_train[,colnames(df_all_shares)], optimize_method = "ROI", portfolio_method="component", trace=TRUE)

# Обучающая выборка
day_return <- Return.portfolio(R = R_train[,colnames(df_all_shares)], weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

charts.PerformanceSummary(day_return)

barplot(result$weights, main="", xlab="")


# Тестовая выборка
day_return <- Return.portfolio(R = R_test[,colnames(df_all_shares)], weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

charts.PerformanceSummary(day_return)

#
# 2. Портфель c запретом коротких продаж
#
portfolio_2 <- portfolio.spec(assets = colnames(Returns))

portfolio_2 <- add.constraint(portfolio_2, type = "full_invsetment")
portfolio_2 <- add.constraint(portfolio_2, type="long_only")
portfolio_2 <- add.constraint(portfolio_2, type = "return", name = "mean", return_target = return_target)
portfolio_2 <- add.constraint(portfolio_2, type="leverage", min_sum=0.99, max_sum=1.01) 

# Минимизация дисперсии
portfolio_2 <- add.objective(portfolio_2, type = "risk", name = "var")

# Строим на обучающей выборке
result <- optimize.portfolio(portfolio_2, R = R_train, optimize_method = "ROI", trace = TRUE)

# Обучающая выборка
day_return <- Return.portfolio(R = R_train, weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

charts.PerformanceSummary(day_return)

barplot(result$weights, main="", xlab="")

#chart.RiskReward(result, risk.col = "StdDev", chart.assets=TRUE)
#chart.RiskReward(result, risk.col = "ES", chart.assets=TRUE)
#plot(result, risk.col="StdDev", return.col="mean", main="", chart.assets=TRUE, xlim=c(0, 0.05), ylim=c(0,0.0085)) 

# Тестовая выборка
day_return <- Return.portfolio(R = R_test, weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

# Портфель без криптовалют
portfolio_2 <- portfolio.spec(assets = colnames(df_all_shares))

return_target <- mean(colMeans(R_train[,colnames(df_all_shares)]))

# Линейные ограничения
portfolio_2 <- add.constraint(portfolio_2, type = "full_invsetment")
portfolio_2 <- add.constraint(portfolio_2, type="long_only")
portfolio_2 <- add.constraint(portfolio_2, type = "return", name = "mean", return_target = return_target)
portfolio_2 <- add.constraint(portfolio_2, type="leverage", min_sum=0.99, max_sum=1.01) 

# Минимизация дисперсии
portfolio_2 <- add.objective(portfolio_2, type = "risk", name = "var")

# Строим на обучающей выборке
result <- optimize.portfolio(portfolio = portfolio_2, R = R_train[,colnames(df_all_shares)], optimize_method = "ROI", portfolio_method="component", trace=TRUE)

# Обучающая выборка
day_return <- Return.portfolio(R = R_train[,colnames(df_all_shares)], weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

charts.PerformanceSummary(day_return)

barplot(result$weights, main="", xlab="")

# Тестовая выборка
day_return <- Return.portfolio(R = R_test[,colnames(df_all_shares)], weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

charts.PerformanceSummary(day_return)

#
# 3. Максимизация функции полезности
#
portfolio_3 <- portfolio.spec(colnames(Returns))

portfolio_3 <- add.constraint(portfolio_3, type = "full_investment")

portfolio_3 <- add.objective(portfolio_3, type = "return", name = "mean")
portfolio_3 <- add.objective(portfolio_3, type = "risk", name = "var", risk_aversion = 20, conc_aversion=0.01)

result <- optimize.portfolio(R_train, portfolio_3, optimize_method = "ROI")

# Обучающая выборка
day_return <- Return.portfolio(R = R_train, weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

barplot(result$weights, main="", xlab="")

charts.PerformanceSummary(day_return)

# Тестовая выборка
day_return <- Return.portfolio(R = R_test, weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

# Без криптовалют
portfolio_3 <- portfolio.spec(colnames(df_all_shares))

portfolio_3 <- add.constraint(portfolio_3, type = "full_investment")

portfolio_3 <- add.objective(portfolio_3, type = "return", name = "mean")
portfolio_3 <- add.objective(portfolio_3, type = "risk", name = "var", risk_aversion = 20, conc_aversion=0.01)

result <- optimize.portfolio(R_train[,colnames(df_all_shares)df_all_shares], portfolio_3, optimize_method = "ROI")

# Обучающая выборка
day_return <- Return.portfolio(R = R_train[,colnames(df_all_shares)], weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

barplot(result$weights, main="", xlab="")

charts.PerformanceSummary(day_return)

# Тестовая выборка
day_return <- Return.portfolio(R = R_test[,colnames(df_all_shares)], weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

#
# 4. Максимизация коэффициента Шарпа
#
portfolio_4 <- portfolio.spec(assets = colnames(Returns))

portfolio_4 <- add.constraint(portfolio_4, type = "full_invsetment") # сумма весов 1
portfolio_4 <- add.constraint(portfolio_4, type="long_only")

portfolio_4 <- add.objective(portfolio_4, type="return", name="mean")
portfolio_4 <- add.objective(portfolio_4, type="risk", name="StdDev")

result <- optimize.portfolio(portfolio_4, R = R_train, optimize_method = "ROI", maxSR=TRUE)

# Обучающая выборка
day_return <- Return.portfolio(R = R_train, weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

barplot(result$weights, main="", xlab="")

charts.PerformanceSummary(day_return)

# Тестовая выборка
day_return <- Return.portfolio(R = R_test, weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

# Без криптовалют 
portfolio_4 <- portfolio.spec(assets = colnames(df_all_shares))

portfolio_4 <- add.constraint(portfolio_4, type = "full_invsetment") # сумма весов 1
portfolio_4 <- add.constraint(portfolio_4, type="long_only")

portfolio_4 <- add.objective(portfolio_4, type="return", name="mean")
portfolio_4 <- add.objective(portfolio_4, type="risk", name="StdDev")

result <- optimize.portfolio(portfolio_4, R = R_train[,colnames(df_all_shares)], optimize_method = "ROI", maxSR=TRUE)

# Обучающая выборка
day_return <- Return.portfolio(R = R_train[,colnames(df_all_shares)], weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

barplot(result$weights, main="", xlab="")

charts.PerformanceSummary(day_return)

# Тестовая выборка
day_return <- Return.portfolio(R = R_test[,colnames(df_all_shares)], weights = result$weights)

mean(day_return)
sum(day_return)
StdDev(day_return)
ES(day_return, p=.95)
PerformanceAnalytics::VaR(day_return, p = 0.95)

#
# Модели GARCH
#

for (asset_name in colnames(df_all_crypto)) {
  cat(asset_name, '\n')
  asset_retruns <- Returns[,asset_name]
  asset_retruns_train <- R_train[,asset_name]
  asset_retruns_test <- R_test[,asset_name]

  # Авто подбор коэффициентов ARIMA на обучающей выборке
  arima_model <- auto.arima(asset_retruns_train)
  print(summary(arima_model))
  
  readline(prompt="Press [enter] to continue")
  
  # Прогноз на тестовой выборке
  arima_predict <- forecast(arima_model, h = length(asset_retruns_test))
  print(plot(arima_predict))
  
  readline(prompt="Press [enter] to continue")
  
  # Оценим MSE
  arima_model_MSE <- mean(drop(coredata(asset_retruns_test) - arima_predict$mean)^2)
  cat('ARIMA MSE:', arima_model_MSE)
  zero_model_MSE <- mean(drop(coredata(asset_retruns_test))^2)
  cat('\nНулевая модель:', zero_model_MSE, '\n')
  
  readline(prompt="Press [enter] to continue")
  
  # GARCH-модель
  arma_order <- c(arima_model$arma[1], arima_model$arma[2])
  
  garch_model <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = arma_order, include.mean = TRUE),  distribution.model = "std")
  garch_model_fitted <- ugarchfit(spec = garch_model, data = asset_retruns)
  
  # Результаты оценивания GARCH
  print(round(garch_model_fitted@fit$matcoef, 4))
  print(garch_model_fitted@fit$LLH)
  
  readline(prompt="Press [enter] to continue")
  
  # APARCH-модель
  aparch_model <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = arma_order))
  aparch_model_fitted <- ugarchfit(spec = aparch_model, data = asset_retruns)
  
  # Результаты оценивания APARCH
  print(round(aparch_model_fitted@fit$matcoef, 4))
  #print(aparch_model_fitted@fit$LLH)
  
  readline(prompt="Press [enter] to continue")
  
  # Графики
  plot(aparch_model_fitted, which="all")
  
  readline(prompt="Press [enter] to continue")
}

asset_names <- c('btc', 'eth', 'GSPC')
asset_retruns <- Returns_with_index[,asset_names]

#
# Модель VAR
#

# Спецификация
VARselect(asset_retruns, lag.max = 10, type="const")
#lags_number <- as.numeric(VARselect(asset_retruns, lag.max = 10, type="const")$selection[1])

# Модель
#var_model_fitted <- VAR(asset_retruns, p = lags_number, type="const")
var_model_fitted <- VAR(asset_retruns, p = 3, type="const")

summary(var_model_fitted$varresult[['btc']])

for (asset_name in colnames(asset_retruns)) {
  print(asset_name)
  print(summary(var_model_fitted$varresult[[asset_name]]))
  #stargazer(var_model_fitted$varresult[[asset_name]], title=asset_name, align=TRUE, type="html", out = asset_name)
  
  readline(prompt="Press [enter] to continue")
}

asset_irf <- irf(var_model_fitted, n.ahead = 8, ortho = FALSE, runs = 1000)

plot(asset_irf)

#
# GARCH
#

for (asset_name in colnames(df_all_crypto)) {
  cat(asset_name, '\n')
  asset_retruns <- Returns[,asset_name]
  asset_retruns_train <- R_train[,asset_name]
  asset_retruns_test <- R_test[,asset_name]
  
  # Авто подбор коэффициентов ARIMA на обучающей выборке
  arima_model <- auto.arima(asset_retruns_train)
  #print(summary(arima_model))
  ?ugarchfit
  # Прогноз на тестовой выборке
  #arima_predict <- forecast(arima_model, h = length(asset_retruns_test))
  
  #plot(arima_predict)
  
  # Оценим MSE
  #arima_model_MSE <- mean(drop(coredata(asset_retruns_test) - arima_predict$mean)^2)
  #cat('ARIMA MSE:', arima_model_MSE)
  #zero_model_MSE <- mean(drop(coredata(asset_retruns_test))^2)
  #cat('\nНулевая модель:', zero_model_MSE, '\n')
  
  # GARCH-модель
  arma_order <- c(arima_model$arma[1], arima_model$arma[2])
  
  #garch_model <- ugarchspec(variance.model = list(garchOrder = c(1, 1)), mean.model = list(armaOrder = arma_order, include.mean = TRUE),  distribution.model = "std")
  #garch_model_fitted <- ugarchfit(spec = garch_model, data = asset_retruns)
  
  # Результаты оценивания GARCH
  #print(round(garch_model_fitted@fit$matcoef, 4))
  #print(garch_model_fitted@fit$LLH)
  
  # APARCH-модель
  aparch_model <- ugarchspec(variance.model = list(model = "apARCH", garchOrder = c(1,1)), mean.model = list(armaOrder = arma_order))
  aparch_model_fitted <- ugarchfit(spec = aparch_model, data = asset_retruns)
  
  # Результаты оценивания APARCH
  print(round(aparch_model_fitted@fit$matcoef, 4))
  #print(aparch_model_fitted@fit$LLH)
  
  # Графики
  #plot(aparch_model_fitted, which="all")
  
  readline(prompt="Press [enter] to continue")
}

#
# Портфель с использованием GARCH
#

# Готовим данные
asset_names <- c('btc', 'GSPC', 'SX5E')
asset_retruns <- Returns_with_index[,asset_names]

# Разбиваем на обучающую и тестовую выборку
sample_size <- floor((1 - 1/exp(1)) * nrow(asset_retruns))
train_indexes <- seq_len(sample_size)

asset_retruns_train <- asset_retruns[train_indexes, ]
asset_retruns_test <- asset_retruns[-train_indexes, ]

# Эффективный портфель
data <- as.timeSeries(asset_retruns_train)

spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
return_target <- mean(colMeans(asset_retruns_train))
setTargetReturn(spec) = return_target

portf_no_rebalance <- efficientPortfolio(data, spec);

#
# Портфели с использованием многомерного GARCH
#

customEstimator <- function (x, spec = NULL, ...) {
  stopifnot(inherits(x, "timeSeries"))
  
  # Строим многомерный GARCH
  dt <- x
  dt = getDataPart(dt)
  asset_names <- colnames(dt)
  
  dcc_model = ugarchspec(mean.model = list(armaOrder = c(1, 1)), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'), distribution.model = 'norm')
  dcc_model_spec = dccspec(uspec = multispec(replicate(ncol(dt), dcc_model)), dccOrder = c(1,1), distribution = 'mvnorm')
  
  dcc_model_fit = dccfit(dcc_model_spec, data = dt)
  dcc_model_fit
  
  # Прогноз
  dcc_model_cov_fcst <- dccforecast(dcc_model_fit)
  
  covmat_forecast <- rcov(dcc_model_cov_fcst)
  covmat_forecast <- as.data.frame(covmat_forecast)
  colnames(covmat_forecast) <- asset_names
  rownames(covmat_forecast) <- asset_names
  covmat_forecast <- data.matrix(covmat_forecast, rownames.force = NA)
  
  mean_forecast <- fitted(dcc_model_cov_fcst)
  mean_forecast <- as.data.frame(mean_forecast)
  colnames(mean_forecast) <- asset_names
  mean_forecast <- colMeans(mean_forecast)
  
  Sigma = covmat_forecast
  mu = mean_forecast
  
  list(mu = mu, Sigma = Sigma)
}

# Создаем эффективные портфели
data <- as.timeSeries(asset_retruns_train)
n <- nrow(asset_retruns_test)
column_names <- colnames(data)

portfolios <- NULL

for (i in 1:n) {
  data <- rbind(data, as.timeSeries(asset_retruns_test[i,]))
  colnames(data) <- column_names
  
  spec <- portfolioSpec()
  setSolver(spec) <- "solveRquadprog"
  setEstimator(spec) <- 'customEstimator'
  setTargetReturn(spec) = return_target
  
  portf <- efficientPortfolio(data, spec);
  portfolios <- append(portfolios, portf)
  
  print(paste('Day:', i))
}

daily_portf_return <- NULL
daily_portf_weights <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(daily_portf_weights) <- asset_names

for (i in 1:n) {
  portf <- portfolios[[i]]
  weights <- getWeights(portf)
  daily_portf_weights[nrow(daily_portf_weights) + 1,] <- weights
  day_returns <- asset_retruns_test[i,]
  daily_portf_return <- append(daily_portf_return, sum(weights * day_returns))
}
sum(daily_portf_return)

# Без криптовалют
asset_names <- c('GSPC', 'SX5E')
asset_retruns <- Returns_with_index[,asset_names]

sample_size <- floor((1 - 1/exp(1)) * nrow(asset_retruns))
train_indexes <- seq_len(sample_size)

asset_retruns_train <- asset_retruns[train_indexes, ]
asset_retruns_test <- asset_retruns[-train_indexes, ]

data <- as.timeSeries(asset_retruns_train)
n <- nrow(asset_retruns_test)
column_names <- colnames(data)

portfolios_no_crypto <- NULL

for (i in 1:n) {
  data <- rbind(data, as.timeSeries(asset_retruns_test[i,]))
  colnames(data) <- column_names
  
  spec <- portfolioSpec()
  setSolver(spec) <- "solveRquadprog"
  setEstimator(spec) <- 'customEstimator'
  setTargetReturn(spec) = return_target
  
  portf <- efficientPortfolio(data, spec);
  portfolios_no_crypto <- append(portfolios_no_crypto, portf)
  
  print(paste('Day:', i))
}

portf_no_crypto_return <- NULL

for (i in 1:n) {
  portf <- portfolios_no_crypto[[i]]
  weights <- getWeights(portf)
  day_returns <- asset_retruns_test[i,]
  portf_no_crypto_return <- append(portf_no_crypto_return, sum(weights * day_returns))
}

sum(portf_no_crypto_return)

plot(cumsum(daily_portf_return), type='l', , col = "red", xlab = 'day', ylab = 'return')
grid()
lines(cumsum(portf_no_crypto_return), type = "l", col = "blue")

plot(daily_portf_weights[['btc']], type="h", lwd=2, col = "red", xlab = 'day', ylab = 'btc weight', bg="lightblue")
grid()

#
# Создаем портфели с минимизацией риска
#
asset_names <- c('GSPC', 'SX5E', 'btc')
asset_retruns <- Returns_with_index[,asset_names]

sample_size <- floor((1 - 1/exp(1)) * nrow(asset_retruns))
train_indexes <- seq_len(sample_size)

asset_retruns_train <- asset_retruns[train_indexes, ]
asset_retruns_test <- asset_retruns[-train_indexes, ]

data <- as.timeSeries(asset_retruns_train)

spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
return_target <- mean(colMeans(asset_retruns_train))
setTargetReturn(spec) = return_target

portf_no_rebalance <- minriskPortfolio(data, spec);


data <- as.timeSeries(asset_retruns_train)
n <- nrow(asset_retruns_test)
column_names <- colnames(data)

portfolios <- NULL

for (i in 1:n) {
  data <- rbind(data, as.timeSeries(asset_retruns_test[i,]))
  colnames(data) <- column_names
  
  spec <- portfolioSpec()
  
  setSolver(spec) <- "solveRquadprog"
  setEstimator(spec) <- 'customEstimator'
  #constraints <- c('LongOnly')
  setTargetReturn(spec) = return_target
  
  portf <- minriskPortfolio(data, spec);
  portfolios <- append(portfolios, portf)
  
  print(paste('Day:', i))
}

daily_portf_return <- NULL
daily_portf_weights <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(daily_portf_weights) <- asset_names

daily_portf_return <- NULL
for (i in 1:n) {
  portf <- portfolios[[i]]
  weights <- getWeights(portf)
  daily_portf_weights[nrow(daily_portf_weights) + 1,] <- weights
  day_returns <- asset_retruns_test[i,]
  daily_portf_return <- append(daily_portf_return, sum(weights * day_returns))
}
sum(daily_portf_return)

# Без криптовалют
asset_names <- c('GSPC', 'SX5E')
asset_retruns <- Returns_with_index[,asset_names]

sample_size <- floor((1 - 1/exp(1)) * nrow(asset_retruns))
train_indexes <- seq_len(sample_size)

asset_retruns_train <- asset_retruns[train_indexes, ]
asset_retruns_test <- asset_retruns[-train_indexes, ]

data <- as.timeSeries(asset_retruns_train)
n <- nrow(asset_retruns_test)
column_names <- colnames(data)

portfolios_no_crypto <- NULL

for (i in 1:n) {
  data <- rbind(data, as.timeSeries(asset_retruns_test[i,]))
  colnames(data) <- column_names
  
  spec <- portfolioSpec()
  setSolver(spec) <- "solveRquadprog"
  setEstimator(spec) <- 'customEstimator'
  setTargetReturn(spec) = return_target
  
  portf <- minriskPortfolio(data, spec);
  portfolios_no_crypto <- append(portfolios_no_crypto, portf)
  
  print(paste('Day:', i))
}

portf_no_crypto_return <- NULL

for (i in 1:n) {
  portf <- portfolios_no_crypto[[i]]
  weights <- getWeights(portf)
  day_returns <- asset_retruns_test[i,]
  portf_no_crypto_return <- append(portf_no_crypto_return, sum(weights * day_returns))
}

sum(portf_no_crypto_return)

plot(cumsum(daily_portf_return), type='l', , col = "red", xlab = 'day', ylab = 'return', ylim=c(-0.45,0.15))
grid()
lines(cumsum(portf_no_crypto_return), type = "l", col = "blue")

plot(daily_portf_weights[['btc']], type="h", lwd=2, col = "red", xlab = 'day', ylab = 'btc weight', bg="lightblue")
grid()

#plot(cumsum(portf_no_rebalance_return), type = "l", col = "blue")
#lines(cumsum(daily_portf_return), type='l', , col = "red")
#lines(cumsum(as.numeric(asset_retruns_test$GSPC)), type='l', , col = "green")

# Создаем портфели с минимизацией риска (короткие продажи разрешены)
asset_names <- c('GSPC', 'SX5E', 'btc')
asset_retruns <- Returns_with_index[,asset_names]

sample_size <- floor((1 - 1/exp(1)) * nrow(asset_retruns))
train_indexes <- seq_len(sample_size)

asset_retruns_train <- asset_retruns[train_indexes, ]
asset_retruns_test <- asset_retruns[-train_indexes, ]

data <- as.timeSeries(asset_retruns_train)

spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
constraints <- c("minW=c(-1,-1,-1)", "maxW=c(1,1,1)")
return_target <- mean(colMeans(asset_retruns_train))
setTargetReturn(spec) = return_target

portf_no_rebalance <- minriskPortfolio(data, spec, constraints);

data <- as.timeSeries(asset_retruns_train)
n <- nrow(asset_retruns_test)
column_names <- colnames(data)

portfolios <- NULL

for (i in 1:n) {
  data <- rbind(data, as.timeSeries(asset_retruns_test[i,]))
  colnames(data) <- column_names
  
  spec <- portfolioSpec()
  
  setSolver(spec) <- "solveRquadprog"
  setEstimator(spec) <- 'customEstimator'
  constraints <- c("minW=c(-1,-1,-1)", "maxW=c(1,1,1)")
  setTargetReturn(spec) = return_target
  
  portf <- minriskPortfolio(data, spec, constraints);
  portfolios <- append(portfolios, portf)
  
  print(paste('Day:', i))
}

daily_portf_return <- NULL
daily_portf_weights <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(daily_portf_weights) <- asset_names

daily_portf_return <- NULL
for (i in 1:n) {
  portf <- portfolios[[i]]
  weights <- getWeights(portf)
  daily_portf_weights[nrow(daily_portf_weights) + 1,] <- weights
  day_returns <- asset_retruns_test[i,]
  daily_portf_return <- append(daily_portf_return, sum(weights * day_returns))
}
sum(daily_portf_return)

# Без криптовалют
asset_names <- c('GSPC', 'SX5E')
asset_retruns <- Returns_with_index[,asset_names]

sample_size <- floor((1 - 1/exp(1)) * nrow(asset_retruns))
train_indexes <- seq_len(sample_size)

asset_retruns_train <- asset_retruns[train_indexes, ]
asset_retruns_test <- asset_retruns[-train_indexes, ]

data <- as.timeSeries(asset_retruns_train)
n <- nrow(asset_retruns_test)
column_names <- colnames(data)

portfolios_no_crypto <- NULL

for (i in 1:n) {
  data <- rbind(data, as.timeSeries(asset_retruns_test[i,]))
  colnames(data) <- column_names
  
  spec <- portfolioSpec()
  
  setSolver(spec) <- "solveRquadprog"
  setEstimator(spec) <- 'customEstimator'
  constraints <- c("minW=c(-1,-1)", "maxW=c(1,1)")
  setTargetReturn(spec) = return_target
  
  portf <- minriskPortfolio(data, spec, constraints);
  portfolios <- append(portfolios, portf)

  portfolios_no_crypto <- append(portfolios_no_crypto, portf)
  
  print(paste('Day:', i))
}

portf_no_crypto_return <- NULL

for (i in 1:n) {
  portf <- portfolios_no_crypto[[i]]
  weights <- getWeights(portf)
  day_returns <- asset_retruns_test[i,]
  portf_no_crypto_return <- append(portf_no_crypto_return, sum(weights * day_returns))
}

sum(portf_no_crypto_return)

plot(cumsum(daily_portf_return), type='l', , col = "red", xlab = 'day', ylab = 'return', ylim=c(-0.45,0.15))
grid()
lines(cumsum(portf_no_crypto_return), type = "l", col = "blue")

plot(daily_portf_weights[['btc']], type="h", lwd=2, col = "red", xlab = 'day', ylab = 'btc weight', bg="lightblue")
grid()

plot(cumsum(portf_no_rebalance_return), type = "l", col = "blue")
lines(cumsum(daily_portf_return), type='l', , col = "red")
lines(cumsum(as.numeric(asset_retruns_test$GSPC)), type='l', , col = "green")

{
# Создаем портфели с минимизацией риска (короткие продажи разрешены) + только крипто
asset_retruns_train <- asset_retruns_train[,-3]
asset_retruns_test <- asset_retruns_test[,-3]

data <- as.timeSeries(asset_retruns_train)

spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
constraints <- c("minW=c(-1,-1)", "maxW=c(1,1)")
return_target <- mean(colMeans(asset_retruns_train))
setTargetReturn(spec) = return_target

portf_no_rebalance <- minriskPortfolio(data, spec, constraints);

data <- as.timeSeries(asset_retruns_train)
n <- nrow(asset_retruns_test)
column_names <- colnames(data)

portfolios <- NULL

for (i in 1:n) {
  data <- rbind(data, as.timeSeries(asset_retruns_test[i,]))
  colnames(data) <- column_names
  
  spec <- portfolioSpec()
  
  setSolver(spec) <- "solveRquadprog"
  setEstimator(spec) <- 'customEstimator'
  constraints <- c("minW=c(-1,-1)", "maxW=c(1,1)")
  setTargetReturn(spec) = return_target
  
  portf <- minriskPortfolio(data, spec, constraints);
  portfolios <- append(portfolios, portf)
  
  print(paste('Day:', i))
}

daily_portf_return <- NULL
for (i in 1:n) {
  portf <- portfolios[[i]]
  weights <- getWeights(portf)
  print(weights)
  day_returns <- asset_retruns_test[i,]
  daily_portf_return <- append(daily_portf_return, sum(weights * day_returns))
}
sum(daily_portf_return)


portf_no_rebalance_return <- NULL
for (i in 1:n) {
  portf <- portf_no_rebalance
  weights <- getWeights(portf)
  day_returns <- asset_retruns_test[i,]
  portf_no_rebalance_return <- append(portf_no_rebalance_return, sum(weights * day_returns))
}
sum(portf_no_rebalance_return)

plot(cumsum(portf_no_rebalance_return), type = "l", col = "blue")
lines(cumsum(daily_portf_return), type='l', , col = "red")
lines(cumsum(as.numeric(asset_retruns_test$btc)), type='l', , col = "green")
lines(cumsum(as.numeric(asset_retruns_test$eth)), type='l', , col = "grey")

# Создаем эффективные портфели (короткие продажи разрешены) + только крипто
data <- as.timeSeries(asset_retruns_train)

spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
constraints <- c("minW=c(-1,-1)", "maxW=c(1,1)")
return_target <- mean(colMeans(asset_retruns_train))
setTargetReturn(spec) = return_target

portf_no_rebalance <- efficientPortfolio(data, spec, constraints);

data <- as.timeSeries(asset_retruns_train)
n <- nrow(asset_retruns_test)
column_names <- colnames(data)

portfolios <- NULL

for (i in 1:n) {
  data <- rbind(data, as.timeSeries(asset_retruns_test[i,]))
  colnames(data) <- column_names
  
  spec <- portfolioSpec()
  
  setSolver(spec) <- "solveRquadprog"
  setEstimator(spec) <- 'customEstimator'
  constraints <- c("minW=c(-1,-1)", "maxW=c(1,1)")
  setTargetReturn(spec) = return_target
  
  portf <- efficientPortfolio(data, spec, constraints);
  portfolios <- append(portfolios, portf)
  
  print(paste('Day:', i))
}

daily_portf_return <- NULL
for (i in 1:n) {
  portf <- portfolios[[i]]
  weights <- getWeights(portf)
  print(weights)
  day_returns <- asset_retruns_test[i,]
  daily_portf_return <- append(daily_portf_return, sum(weights * day_returns))
}
sum(daily_portf_return)


portf_no_rebalance_return <- NULL
for (i in 1:n) {
  portf <- portf_no_rebalance
  weights <- getWeights(portf)
  day_returns <- asset_retruns_test[i,]
  portf_no_rebalance_return <- append(portf_no_rebalance_return, sum(weights * day_returns))
}
sum(portf_no_rebalance_return)

plot(cumsum(daily_portf_return), type='l', col = "red")
lines(cumsum(portf_no_rebalance_return), type = "l", , col = "blue")
lines(cumsum(as.numeric(asset_retruns_test$btc)), type='l', , col = "green")
lines(cumsum(as.numeric(asset_retruns_test$eth)), type='l', , col = "grey")

# Создаем эффективные портфели (короткие продажи разрешены) + 10 активов
asset_names <- c('btc', 'eth', 'ltc', 'AAPL', 'MSFT', 'AMZN')
asset_retruns <- Returns_with_index[,asset_names]

asset_retruns_train <- asset_retruns[train_indexes, ]
asset_retruns_test <- asset_retruns[-train_indexes, ]

data <- as.timeSeries(asset_retruns_train)

spec <- portfolioSpec()
setSolver(spec) <- "solveRquadprog"
constraints <- c("minW=c(-1,-1,-1,-1,-1,-1)", "maxW=c(1,1,1,1,1,1)")
return_target <- mean(colMeans(asset_retruns_train))
setTargetReturn(spec) = return_target

portf_no_rebalance <- efficientPortfolio(data, spec, constraints);

data <- as.timeSeries(asset_retruns_train)
n <- nrow(asset_retruns_test)
column_names <- colnames(data)

portfolios <- NULL

for (i in 1:n) {
  data <- rbind(data, as.timeSeries(asset_retruns_test[i,]))
  colnames(data) <- column_names
  
  spec <- portfolioSpec()
  
  setSolver(spec) <- "solveRquadprog"
  setEstimator(spec) <- 'customEstimator'
  constraints <- c("minW=c(-1,-1,-1,-1,-1,-1)", "maxW=c(1,1,1,1,1,1)")
  setTargetReturn(spec) = return_target
  
  portf <- efficientPortfolio(data, spec, constraints);
  portfolios <- append(portfolios, portf)
  
  print(paste('Day:', i))
}

daily_portf_return <- NULL
for (i in 1:n) {
  portf <- portfolios[[i]]
  weights <- getWeights(portf)
  print(weights)
  day_returns <- asset_retruns_test[i,]
  daily_portf_return <- append(daily_portf_return, sum(weights * day_returns))
}
sum(daily_portf_return)

portf_no_rebalance_return <- NULL
for (i in 1:n) {
  portf <- portf_no_rebalance
  weights <- getWeights(portf)
  day_returns <- asset_retruns_test[i,]
  portf_no_rebalance_return <- append(portf_no_rebalance_return, sum(weights * day_returns))
}
sum(portf_no_rebalance_return)

plot(cumsum(as.numeric(asset_retruns_test$btc)), type='l', col = "green")
lines(cumsum(portf_no_rebalance_return), type = "l", , col = "blue")
lines(cumsum(daily_portf_return), type='l', , col = "red")
lines(cumsum(as.numeric(asset_retruns_test$MSFT)), type='l', , col = "grey")




}

#
# Перекрестные эффекты
#

#ADL(1,1)
asset_names <- c('btc', 'GSPC')
asset_retruns <- Returns_with_index[,asset_names]

data <- as.timeSeries(asset_retruns)

ardl_model <- dynlm(btc ~ L(btc) + L(GSPC, 0:1), data = ts(data))
summary(ardl_model)

ardl_model_2 <- dynlm(GSPC ~ L(GSPC, 1:2) + L(btc, 0:1), data = ts(data))
summary(ardl_model_2)

#ADL(1,1)
asset_names <- c('eth', 'GSPC')
asset_retruns <- Returns_with_index[,asset_names]

data <- as.timeSeries(asset_retruns)

ardl_model <- dynlm(eth ~ L(eth) + L(GSPC, 0:1), data = ts(data))
summary(ardl_model)

ardl_model_2 <- dynlm(GSPC ~ L(GSPC) + L(GSPC, 0:1), data = ts(data))
summary(ardl_model_2)

cor(data)

#ADL(1,1) 
asset_names <- c('eth', 'btc')
asset_retruns <- Returns_with_index[,asset_names]

data <- as.timeSeries(asset_retruns)

ardl_model <- dynlm(btc ~ L(btc) + L(eth, 0:2), data = ts(data))
summary(ardl_model)

ardl_model_2 <- dynlm(eth ~ L(eth) + L(btc, 0:2), data = ts(data))
summary(ardl_model_2)

cor(data)

# Линейная модель 
asset_names <- c('eth', 'btc', 'GSPC')
asset_retruns <- Returns_with_index[,asset_names]
plot(ts(data))

data <- as.timeSeries(asset_retruns)
summary(lm(btc ~ GSPC + eth, data = data))

# Модель BEKK GARCH
asset_names <- c('btc', 'GSPC')
asset_retruns <- Returns_with_index[,asset_names]

data <- as.timeSeries(asset_retruns)
BEKK_model_1 <- BEKK11(as.matrix(data))

vol = BEKK_model_1$Sigma.t

BEKK_model_1$estimates

par(mfcol=c(3,1))
ts.plot(vol[,1], col="blue")
title(main='Volatility btc')
ts.plot(vol[,4], col="blue")
title(main='Volatility GSPC')
ts.plot(vol[,2], col="blue")
title(main='Covariance btc-GSPC')
par(mfcol=c(2,1))

library('VIRF')
?VIRF

# BEKK
asset_names <- c('btc', 'eth')
asset_retruns <- Returns_with_index[,asset_names]

asset_retruns_train <- asset_retruns[train_indexes, ]

data <- as.timeSeries(asset_retruns_train)
BEKK_model_2 <- BEKK11(as.matrix(data))

bekkforecast <- tail(BEKK_model_2$Sigma.t, 1)
dim(bekkforecast) <- c(2, 2)

BEKK_model_3 <- bekk_fit(bekk_spec(model = list(type = "bekk", asymmetric = F)), data = asset_retruns_train)

summary(BEKK_model_3)
plot(BEKK_model_3)
bekk_forecast(BEKK_model_3, n.ahead = 1, ci = 0.95)
bekkforecast

vol = BEKK_model_2$Sigma.t

par(mfcol=c(3,1))
ts.plot(vol[,1])
title(main='Volatility btc')
ts.plot(vol[,4])
title(main='Volatility eth')
ts.plot(vol[,2])
title(main='Covariance btc-eth')
par(mfcol=c(2,1))

# Прогноз BEKK
bekkforecast <- tail(BEKK_model_2$Sigma.t, 1)
dim(bekkforecast) <- c(2, 2)

bekk_forecast(BEKK_model_2, n.ahead = 1, ci = 0.95)


library('mgarchBEKK')


asset_names <- c('btc', 'GSPC')
asset_retruns <- Returns_with_index[,asset_names]

asset_retruns_train <- asset_retruns[train_indexes, ]

data <- as.timeSeries(asset_retruns_train)


# DCC
asset_names <- c('btc', 'eth')
asset_retruns <- Returns_with_index[,asset_names]

asset_retruns_train <- asset_retruns[train_indexes, ]

dt <- as.timeSeries(asset_retruns_train)
dt = getDataPart(dt)
asset_names <- colnames(dt)

dcc_model = ugarchspec(mean.model = list(armaOrder = c(1, 1)), variance.model = list(garchOrder = c(1,1), model = 'sGARCH'), distribution.model = 'norm')
dcc_model_spec = dccspec(uspec = multispec(replicate(ncol(dt), dcc_model)), dccOrder = c(1,1), distribution = 'mvnorm')

dcc_model_fit = dccfit(dcc_model_spec, data = dt)
dcc_model_fit

# Прогноз DCC
dcc_model_cov_fcst <- dccforecast(dcc_model_fit)

dccforecast <- rcov(dcc_model_cov_fcst)
dccforecast <- as.data.frame(dccforecast)
colnames(dccforecast) <- asset_names
rownames(dccforecast) <- asset_names
dccforecast <- data.matrix(dccforecast, rownames.force = NA)
dccforecast

# Сравниваем прогнозы 
bekkforecast
dccforecast

# Граница возможных портфелей
asset_retruns <- Returns_with_index

# compute the tangency portfolio
cml <- tangencyPortfolio(as.timeSeries(asset_retruns))
effectient_frontier <- portfolioFrontier(as.timeSeries(asset_retruns), include.mvl=T)

par(mfrow=c(1,1))
plot(effectient_frontier, which = 1)
plot(cml, which = 3)
singleAssetPoints(effective_frontier)
minvariancePoints(effective_frontier, col='red', pch = 16, cex=1.5)

mv <- minvariancePoints(effective_frontier)
text(x = mv[, 1], y = mv[, 2], 'MV', pos = 2, font = 2, cex = 0.7, col='red')

hidden <- c('AAPL', 'MSFT', 'AMZN', 'GOOGL', 'GOOG', 'TSLA', 'JNJ', 'NVDA', 'FB' , 'UNH')
xy <- singleAssetPoints(effectient_frontier)
xy <-xy[!(rownames(xy) %in% hidden), ]
text(x = xy[, 1], y = xy[, 2], labels = rownames(xy), pos = 2, font = 2, cex = 0.7)
                  
text(x = cml@portfolio@portfolio[["targetRisk"]][["Sigma"]], 
     y = cml@portfolio@portfolio[["targetReturn"]][["mean"]], 
     labels = 'Max SR', pos = 2, font = 2, cex = 0.7, col='SteelBlue')

weightsPlot(effectient_frontier)

tailoredFrontierPlot(effectient_frontier)

cml@portfolio@portfolio[["targetReturn"]][["mean"]]
cml@portfolio@portfolio[["targetRisk"]][["Sigma"]]
