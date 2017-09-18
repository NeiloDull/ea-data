options("stringsAsFactors" = FALSE)
CONVERSION_AS_OF_DATE <- "2017-08-05" # A single date for currency conversions.

csv_path <- "data/2017/imsurvey2017-anonymized.csv"
tryCatch({
  data <- readr::read_csv(csv_path)
}, error = function(e) {
  stop(csv_path, " could not be loaded. You may need to run the `ea_id.R` ",
       "script first. Note that only EA Survey team members have this data, ",
       "and that the public data is already the data produced by this script.",
       " Running this script is not necessary to do the analysis.")
})

first_pass <- data$currency_donate_1 %/>%
                fn(x, strsplit(x, " - ")[[c(1, 1)]]) %/>%
                function(cc) {
                  if (cc %in% currencyr::currency_codes()) { cc } else { NA_character_ }}

split_currency <- function(currency, sep) {
  strsplit(currency, sep)[[c(1, 1)]] %>% trimws
}
clean_currency <- function(currency) {
  if (grepl("-", currency)) { split_currency(currency, "-") }
  else if (grepl("\\(", currency)) { split_currency(currency, "\\(") }
  else { currency }
}
try_to_get_currency <- function(currency) {
  if (!checkr::is.simple_string(currency)) { return(NA_character_) }
  if (currency %in% currencyr::currency_codes()) { return(currency) }
  attempt <- currencyr::get_code_from_unit(currency)
  if (checkr::is.simple_string(attempt)) { return(attempt) }
  return(NA_character_)
}

second_pass <- data$currency_donate_2 %>% toupper %/>% clean_currency %/>% try_to_get_currency

combine <- function(x, y) { if (is.na(x)) { y } else { x }}
currency_map <- Map(combine, unlist(first_pass), unlist(second_pass)) %>% unlist
data$currency <- currency_map
data$currency_donate_1 <- NULL
data$currency_donate_2 <- NULL

currency_vars <- surveytools2::get_vars(data, c("donate_", "income"), collapse = TRUE)

is_number_string <- function(x) { !is.na(suppressWarnings(as.numeric(x))) }

to_usd <- function(num, current_currency) {
  if (!checkr::is.simple_string(current_currency)) { return(as.numeric(NA)) }
  if (identical(as.numeric(num), 0)) { return(0) }
  if (identical(current_currency, "USD")) { return(as.numeric(num)) }
  if (!is_number_string(num)) { return(as.numeric(NA)) }
  currencyr::convert(as.numeric(num),
                     from = current_currency,
                     as_of = CONVERSION_AS_OF_DATE)$value
}
for (var in currency_vars) {
  message("...Processing ", var)
  data[[paste0(var, "_c")]] <- unname(unlist(Map(to_usd, data[[var]], currency_map)))
}

readr::write_csv(data, "data/2017/imsurvey2017-anonymized-currencied.csv")
message("...Written!")
