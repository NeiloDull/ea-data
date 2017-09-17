Ramd::define("variable_names", function(variable_names) {
  message("Processing...")
  data2015_ <- read.csv("data/2015/confidential-not-anonymous.csv")
  data2015 <- plyr::rename(data2015_, variable_names)
  data2015 <- data2015[, intersect(names(data2015), unlist(variable_names))]
  names_that_did_not_work <- setdiff(unlist(unname(variable_names)), names(data2015))
  if (length(names_that_did_not_work) > 0) {
    stop("Error: some variables did not import -- ",
      paste0(names_that_did_not_work, collapse = ", "))
  }
  for (var in names(data2015)) {
    if (any(data2015[[var]] %in% c("", " "))) {
      data2015[data2015[[var]] %in% c("", " "), ][[var]] <- NA
    }
  }
  hash_email <- function(email, salt) {
    if (is.na(email) || identical(email, "")) { NA }
    else { digest::digest(paste0(email, salt)) }
  }
  email_salt_file <- file("data/email_salt.txt")
  email_salt <- readLines(email_salt_file)
  close(email_salt_file)
  data2015$ea_id <- data2015$email_address %/>% (function(x) hash_email(x, email_salt)) %>% unlist
  data2015$email_address <- NULL
  message("Writing out...")
  readr::write_csv(data2015, "data/2015/imsurvey2015-anonymized.csv")
  message("Written...")
})
