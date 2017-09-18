Ramd::define("variable_names", function(variable_names) {
  message("Processing...")
  csv_path <- "data/2015/confidential-not-anonymous.csv"
  tryCatch({
    data2015_ <- read.csv(csv_path)
  }, error = function(e) {
    stop("Loading 2015 data did not work. Note that this requires a confidential data ",
         "file to be installed at ", csv_path, " which only members of the EA Survey team ",
         "would have access to. However, the 2015 CSV produced by this script is already ",
         "available, so you do not need to be able to run this script to do analysis.")
  })
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
