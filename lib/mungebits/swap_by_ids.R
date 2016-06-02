train <- predict <- function(dataframe, variable, lookup) {
  surveytools2::swap_by_ids(dataframe, variable, lookup)
}
