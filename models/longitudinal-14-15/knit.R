message("...Loading data")
data2014 <- read.csv("data/imsurvey2014-anonymized.csv")
data2015 <- read.csv("data/imsurvey2015-anonymized-renamed-currencied.csv")

message("...Renaming 2014 variables")
lookup <- source("models/dev/imsurvey2014/transform_lookups.R")$value
renames <- lookup$renames
data2014 <- syberiaMungebits2::renamer()$train(data2014, renames)

message("...Converting 2014 currencies")
currency <- lookup$donate_transform
data2014 <- data2014[!is.na(data2014$id),]
data2014 <- resource("lib/mungebits/swap_by_ids")$train(data2014, "donate_2013_c", currency)
data2014$donate_2013_c %<>% as.numeric

message("...Subsetting to shared variables")
shared_variables <- intersect(names(data2014), names(data2015))
data2014 <- data2014[, c(shared_variables, "donate_2013_c")]
data2015 <- data2015[, c(shared_variables, "donate_2014_c")]

message("...Dropping non-logitudinal")
data2014 <- data2014[data2014$ea_id != "",]
data2015 <- data2015[data2015$ea_id != "",]

message("...Merging dataframes")
merged <- merge(data2014, data2015, by = "ea_id")

message("...Writing to CSV")
write.csv(merged, "data/imsurvey-longitudinal.csv")
