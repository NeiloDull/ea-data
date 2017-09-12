Ramd::define("variable_names", function(variable_names) {
  data2017_ <- c("data/2017/ea-survey-2017-confidential-no-anon.csv",
                 "data/2017/ea-survey-2017-donations-only-confidential-no-anon.csv") %/>%
                 function(x) { suppressWarnings(readr::read_csv(x)) }
  data2017 <- data2017_ %/>% function(df) {
     suppressWarnings(suppressMessages(plyr::rename(df, variable_names))) }
  data2017 <- data2017 %/>% function(df) { df[, intersect(names(df), unlist(variable_names))] }
  data2017[[2]]$sincere <- "Yes (pick this option to have your answers counted)"
  data2017[[2]]$heard_ea <- "Yes"
  data2017[[2]]$is_ea <- "Yes"
  data2017 <- data2017 %_>% plyr::rbind.fill
  names_that_did_not_work <- setdiff(unlist(unname(variable_names)), names(data2017))
  if (length(names_that_did_not_work) > 0) {
    stop("Error: some variables did not import -- ",
      paste0(names_that_did_not_work, collapse = ", "))
  }
  data2017$ea_id <- data2017$email_address %/>% fn(x, if (is.na(x)) { NA } else { digest::digest(x) }) %>% unlist
  data2017$email_address <- NULL
  data2017 <- data2017[seq(3, nrow(data2017)), ] # Remove first two rows (garbled)
  message("Writing out...")
  readr::write_csv(data2017, "data/2017/imsurvey2017-anonymized.csv")
  message("Written...")
})
