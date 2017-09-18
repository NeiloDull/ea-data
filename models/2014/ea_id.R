Ramd::define("transform_lookups", function(transform_lookups) {
  message("Processing...")
  csv_path <- "data/2014/confidential-not-anonymous.csv"
  tryCatch({
    data2014_ <- read.csv(csv_path)
  }, error = function(e) {
    stop("Loading 2014 data did not work. Note that this requires a confidential data ",
         "file to be installed at ", csv_path, " which only members of the EA Survey team ",
         "would have access to. However, the 2014 CSV produced by this script is already ",
         "available, so you do not need to be able to run this script to do analysis.")
  })
  variable_names <- transform_lookups$renames
  data2014 <- plyr::rename(data2014_, variable_names)
  data2014 <- data2014[, intersect(names(data2014), unlist(variable_names))]
  names_that_did_not_work <- setdiff(unlist(unname(variable_names)), names(data2014))
  if (length(names_that_did_not_work) > 0) {
    stop("Error: some variables did not import -- ",
      paste0(names_that_did_not_work, collapse = ", "))
  }
  for (var in names(data2014)) {
    if (any(data2014[[var]] %in% c("", " "))) {
      data2014[data2014[[var]] %in% c("", " "), ][[var]] <- NA
    }
  }
  hash_email <- function(email, salt) {
    if (is.na(email) || identical(email, "")) { NA }
    else { digest::digest(paste0(email, salt)) }
  }
  email_salt_file <- file("data/email_salt.txt")
  email_salt <- readLines(email_salt_file)
  close(email_salt_file)
  data2014$ea_id <- data2014$email_address %/>% (function(x) hash_email(x, email_salt)) %>% unlist
  data2014$email_address <- NULL
  message("Writing out...")
  readr::write_csv(data2014, "data/2014/imsurvey2014-anonymized.csv")
  message("Written...")
})
