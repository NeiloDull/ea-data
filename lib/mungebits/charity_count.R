function(dataframe, lookup) {
  dataframe$charity_count <- surveytools2::count_vars(
    dataframe,
    lookup,
    response = "Yes",
    vectorize = TRUE
  )
  dataframe
}
