list(
  import = list(file = "data/2017/ssc_survey.csv"),
  data = list(
    "Drop non-EA" = function(df) {
        message(length(na.omit(df$EAID)), " answered EA question")
        df <- dplyr::filter(df, EAID %in% c("Yes", "Sorta"))
        message(NROW(df), " after before dropping non-EA")
        df
    }
  ),
  analyze = list(
      "sex"              = function(df) tab(df, Sex, percent = TRUE)
    , "age"              = function(df) var_summary(df$Age)
    , "student"          = function(df) tab(df, WorkStatus == "Student", percent = TRUE)
    , "USA"              = function(df) tab(df, Country == "United States", percent = TRUE)
    , "white"            = function(df) tab(df, Race == "White (non-Hispanic)", percent = TRUE)
    , "consequentialism" = function(df) tab(df, MoralViews, percent = TRUE)
    , "GWWC"             = function(df) tab(df, GWWC, percent = TRUE)
    , "Donations"        = function(df) var_summary(filter(df, WorkStatus != "Student")$Charity)
    , "Income"           = function(df) var_summary(filter(df, WorkStatus != "Student")$Income)
  )
)
