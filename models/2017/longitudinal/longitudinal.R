
list(import = function() {
       run("2017/easurvey", to = "data")
       df2017 <- df
       names(df2017) <- paste0("s17_", names(df2017))
       run("2015/easurvey", to = "data")
       df2015 <- df
       names(df2015) <- paste0("s15_", names(df2015))
       inner_join(df2015, df2017, by = c("s15_ea_id" = "s17_ea_id"))
     },
     analyze = list(
       # list(write = "data/2017/2017-longitudinal-analysis-tables.txt"),
       list(write = "stdout"), # <-- toggle this to print to the screen.
       list(
         "[check] population aging"   = function(df) var_summary(df$s17_age - as.numeric(df$s15_age), verbose = TRUE)
         , "[check] gender"           = function(df) tab(df, s17_gender, s15_gender)
         , "[check] first heard EA"   = function(df) tab(df, s15_first_heard_EA, s17_first_heard_EA)
         , "summarize 2014 donations" = function(df) var_summary(df$s15_donate_2014_c, verbose = TRUE)
         , "summarize 2015 donations" = function(df) var_summary(df$s17_donate_2015_c, verbose = TRUE)
         , "summarize 2016 donations" = function(df) var_summary(df$s17_donate_2016_c, verbose = TRUE)
         , "summarize 2014 income"    = function(df) var_summary(df$s15_income_2014_c, verbose = TRUE)
         , "summarize 2015 income"    = function(df) var_summary(df$s17_income_2015_individual_c, verbose = TRUE)
         , "summarize 2016 income"    = function(df) var_summary(df$s17_income_2016_individual_c, verbose = TRUE)
         , "keep EA career"           = function(df) tab(df, s15_ea_career, s17_ea_career, na.rm = TRUE)
         , "EA career change"         = function(df) tab(df, s15_career_path, s17_ea_career_type)
         , "heard EA"                 = function(df) tab(df, s15_career_path, s17_ea_career_type)
         , "member EA FB"             = function(df) tab(df, s15_member_ea_fb, s17_member_ea_fb)
         , "member EA Forum"          = function(df) tab(df, s15_member_ea_forum, s17_member_ea_forum)
         , "member GWWC"              = function(df) tab(df, s15_member_gwwc, s17_member_gwwc)
         , "member LW"                = function(df) tab(df, s15_member_lw, s17_member_lw)
         , "member local group"       = function(df) tab(df, s15_member_local_group, s17_member_local_group)
         , "veg"                      = function(df) tab(df, s15_veg, s17_veg)
         , "cause import AR"          = function(df) tab(df, s15_cause_import_animal_welfare, s17_cause_import_animal_welfare)
         , "cause import prioritize"  = function(df) tab(df, s15_cause_import_cause_prioritization, s17_cause_import_cause_prioritization)
         , "cause environment"        = function(df) tab(df, s15_cause_import_environment, s17_cause_import_environment)
         , "cause AI"                 = function(df) tab(df, s15_cause_import_environment, s17_cause_import_environment)
     )))
