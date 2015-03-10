#library(reshape)
#library(ggplot2)
#library(psych)
#library(RCurl)

get.countries <- function() {
  c("Germany", "France", "Spain")
}

get.data <- function(country) {
  url <- paste("https://raw.githubusercontent.com/intuitics/sample-demandforecasting/master/", paste(country, ".csv", sep = ""), sep = "")
  data <- getURLContent(url, followlocation = T, binary = F, ssl.verifypeer = F)
  read.csv(textConnection(data))
}

has.no.country <- function(country = NULL) {
  is.null(country)
}

get.summary <- function (data) {
  data.summary <- describe(data)
  data.summary <- data.summary[c("price", "marketing.budget", "catalog.pages", "competitor.strength", "total.cost", "revenue", "profit"), c("mean", "sd", "median", "min", "max", "range")]
  data.summary <- cbind(variable = rownames(data.summary), data.summary)
  row.names(data.summary) <- NULL
  data.summary$mean <- round(data.summary$mean, 2)
  data.summary$sd <- round(data.summary$sd, 2)
  data.summary
}

get.column.names <- function(data) {
  col.names <- colnames(data)
  col.names[2:length(col.names)]
}

get.historical.plot <- function (data, plot.column.name) {
  p <- ggplot(data, aes_string(x = "month", y = plot.column.name))
  p <- p + geom_line(color = "blue") + xlab("Month") + ylab(plot.column.name)
  p
}

get.model <- function(data) {
  lm(demand ~ price + marketing.budget + catalog.pages + competitor.strength, data = data)
}

get.r.squared <- function (model) {
  round(summary(model)[["adj.r.squared"]], 4)
}

get.coefficients <- function(model) {
  coefs <- summary(model)[["coefficients"]]
  data.frame(Variable = row.names(coefs), Strength = cut(coefs[, 4], breaks = c(0, 0.01, 0.05, 0.1, 1), labels = c("Very strong", "Strong", "Medium", "Weak")), Coefficient = round(coefs[,1], 2), row.names = c())
}

get.forecast <- function (model, data, forecast.price, forecast.mkt.budget, forecast.catalog.pages, forecast.comp.strength) {
  cost <- data$cost[1]
  
  df <- data.frame(price = forecast.price, marketing.budget = forecast.mkt.budget, catalog.pages = forecast.catalog.pages, competitor.strength = forecast.comp.strength)
  
  pred <- predict(model, df, interval = "prediction")
  costs <- pred[,"fit"] * cost
  revenue <- pred[,"fit"] * df$price
  num.months <- nrow(data)
  
  data.frame(Month = c(num.months + 1), Demand.Avg = round(pred[,"fit"], 0), Demand.Upper = round(pred[,"upr"], 0), Demand.Lower = round(pred[,"lwr"], 0), Cost.Avg = round(costs, 0), Revenue.Avg = round(revenue, 0), Profit.Avg = round(revenue - costs, 0))
}

has.no.forecast <- function (forecast = NULL) {
  is.null(forecast)
}

get.forecast.demand.avg <- function(forecast) {
  forecast$Demand.Avg[1]
}

get.forecast.demand.lower <- function(forecast) {
  forecast$Demand.Lower[1]
}

get.forecast.demand.upper <- function(forecast) {
  forecast$Demand.Upper[1]
}

get.forecast.cost <- function(forecast) {
  forecast$Cost.Avg[1]
}

get.forecast.revenue <- function(forecast) {
  forecast$Revenue.Avg[1]
}

get.forecast.profit <- function(forecast) {
  forecast$Profit.Avg[1]
}

get.forecast.plot <- function(data, forecast) {
  df <- tail(data[c("month", "demand", "profit")], 4)
  df <- rbind(df, setNames(forecast[c("Month", "Demand.Avg", "Profit.Avg")], names(df)))
  df <- melt(df, id.vars = c("month"))
  p <- ggplot(df, aes(x = month, y = value))
  p <- p + facet_grid(variable ~ ., scale="free")
  p <- p + geom_line() + xlab("Month") + ylab("Value")
  p <- p + ggtitle("Last 3 months actual data + 1 month forecast\n(demand in units & profit in $)")
  p
}

#countries <- get.countries()
#country <- countries[1]
#data <- get.data(country)
#summary <- get.summary(data)
#column.names <- get.column.names(data)
#plot.column.name <- column.names[4]
#get.historical.plot(data, plot.column.name)
#model <- get.model(data)
#r.squared <- get.r.squared(model)
#coefficients <- get.coefficients(model)
#forecast.price = 8
#forecast.mkt.budget = 22000
#forecast.catalog.pages = 0.5
#forecast.comp.strength = 9
#forecast <- get.forecast(model, data, forecast.price, forecast.mkt.budget, forecast.catalog.pages, forecast.comp.strength)
#get.forecast.plot(data, forecast)