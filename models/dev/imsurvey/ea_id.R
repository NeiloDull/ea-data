data2014 <- readr::read_csv("data/EA Survey 2014 CONFIDENTIAL (not anonymous).csv")
data2015 <- readr::read_csv("data/EA Survey 2015 CONFIDENTIAL (not anonymous).csv")

data2014$ea_id <- ""
data2014[data2014[["Your email address"]] != "" & !is.na(data2014[["Your email address"]]), "ea_id"] <- unname(unlist(lapply(unlist(as.list(data2014[data2014[["Your email address"]] != "" & !is.na(data2014[["Your email address"]] != ""), "Your email address"])), digest::digest)))

data2015$ea_id <- ""
data2015[data2015[["Your email address"]] != "" & !is.na(data2015[["Your email address"]]), "ea_id"] <- unname(unlist(lapply(unlist(as.list(data2015[data2015[["Your email address"]] != "" & !is.na(data2015[["Your email address"]] != ""), "Your email address"])), digest::digest)))

data2014[["Your name"]] <- NULL
data2014[["Your email address"]] <- NULL
data2014[["Giving your email address would let you revoke permissions you've given to make things public and edit your answers. It would also be much appreciated."]] <- NULL
data2015[["Your name"]] <- NULL
data2015[["Your email address"]] <- NULL

readr::write_csv(data2014, "data/imsurvey2014-anonymized.csv")
readr::write_csv(data2015, "data/imsurvey2015-anonymized.csv")
