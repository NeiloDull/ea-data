train <- predict <- function(dataframe, expr) {
  eval(expr)
  dataframe
}
