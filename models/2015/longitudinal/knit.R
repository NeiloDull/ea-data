message("...Loading data")
data2014 <- read.csv("data/2014/imsurvey2014-anonymized.csv")
data2015 <- read.csv("data/2015/imsurvey2015-anonymized-renamed-currencied.csv")

message("...Renaming 2014 variables")
lookup <- source("models/2014/easurvey/transform_lookups.R")$value
renames <- lookup$renames
data2014 <- plyr::rename(data2014, renames)
data2014 <- data2014[, c(unname(renames), "ea_id")]

message("...Converting 2014 currencies")
currency <- lookup$donate_transform
data2014 <- data2014[!is.na(data2014$id),]
data2014 <- swap_by_ids(data2014, "donate_2013_c", currency)
data2014$donate_2013_c %<>% as.numeric

message("...Subsetting to shared variables")
names_2015 <- source("models/2015/easurvey/variable_names.R")$value
data2015 <- data2015[, c(unlist(unname(names_2015)), "ea_id")]
data2014$veg <- data2014$diet
data2014$cause_import_animal_welfare <- data2014$cause_import_animals
data2014$involved_personal_contact <- data2014$invovled_personal_contact
shared_variables <- intersect(names(data2014), names(data2015))
data2014 <- data2014[, c(shared_variables, "donate_2013_c", "income_2013_c")]
data2015 <- data2015[, c(shared_variables, "donate_2014", "income_2014")]

message("...Dropping non-logitudinal")
data2014 <- data2014[data2014$ea_id != "", ]
data2015 <- data2015[data2015$ea_id != "", ]

message("...Merging dataframes")
merged <- merge(data2014, data2015, by = "ea_id")

message("... ...Found ", nrow(merged), " unique people with ", ncol(merged), " shared variables!")
message("...Writing to CSV")
write.csv(merged, "data/2015/imsurvey-longitudinal.csv")
