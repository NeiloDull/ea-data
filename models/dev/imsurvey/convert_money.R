options("stringsAsFactors" = FALSE)
data <- read.csv("data/imsurvey2015-anonymized.csv")
renames <- source("models/dev/imsurvey/variable_names.R")$value
data <- plyr::rename(data, renames)

currencies <- sort(unique(c(data$currency_lifetime_1,
  data$currency_lifetime_2,
  data$currency_income_1,
  data$currency_lifetime_2,
  data$currency_donate_1,
  data$currency_donate_2)))

first_pass <- sapply(currencies,
  function(currency) {
    Filter(Negate(is.null),
      lapply(currencyr::currency_codes(), function(code) {
        if (grepl(code, currency)) { code } else { NULL }
      })) })

second_pass <- sapply(currencies,
  function(currency) try(get_code_from_unit(currency)))

join <- function(x, y) { if (length(y) == 0) { x } else { y } }
currency_map <- Map(join, first_pass, second_pass)
currency_map[["Reais (Brazil)"]] <- "BRL"
currency_map[["nzd"]] <- "NZD"
currency_map[["NZ$"]] <- "NZD"
currency_map <- as.list(unlist(currency_map))

get_currency <- function(df, dep_var, currency1, currency2) {
  donate_data <- data[, c(dep_var, currency1, currency2)]
  currency_data <- apply(donate_data, 1, function(col) {
    if (is.na(col[[dep_var]])) { return(NA) }
    if (col[[dep_var]] == 0L) { return(0L) }
    if (!checkr::is.empty(col[[currency1]])) {
      return(currency_map[[col[[currency1]]]])
    }
    if (!checkr::is.empty(col[[currency2]])) {
      return(currency_map[[col[[currency2]]]])
    }
    NA
  })
  unlist(Map(function(amount, currency) {
      amount <- suppressWarnings(as.numeric(amount))
      if (isTRUE(amount == 0)) { return(amount) }
      if (isTRUE(!is.na(amount) && !is.na(currency))) { 
        message(paste0("Converting ", amount, " from ", currency, "..."))
        result <- currencyr::convert(amount, from = currency)$value
        message(paste0("...", result))
        result
      } else { NA }
    },
    data[[dep_var]], currency_data))
}

data$donate_2014_c <- get_currency(data, "donate_2014", "currency_donate_1", "currency_donate_2")
data$income_2014_c <- get_currency(data, "income_2014", "currency_income_1", "currency_income_2")
data$donated_lifetime_c <- get_currency(data, "donated_lifetime", "currency_lifetime_1", "currency_lifetime_2")

write.csv(data, "data/imsurvey2015-anonymized-renamed-currencied.csv")
