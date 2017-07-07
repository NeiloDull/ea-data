Ramd::define("variable_names", function(variable_names) {
  data2017_ <- c("data/ea-survey-2017-confidential-no-anon.csv",
                 "data/ea-survey-2017-donations-only-confidential-no-anon.csv") %/>%
                 readr::read_csv
  browser()
  data2017 <- data2017_ %/>% function(df) { plyr::rename(df, variable_names) }
  data2017 <- data2017 %/>% function(df) { df[, intersect(names(df), unlist(variable_names))] }
  data2017 <- data2017 %_>% plyr::rbind.fill
  data2017$ea_id <- data2017$email_address %/>% digest::digest %>% unlist
  data2017$email_address <- NULL
  readr::write_csv(data2017, "data/imsurvey2017-anonymized.csv")
})
