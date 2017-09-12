list(import = function() {
       run("2015/easurvey", to = "data")
       df2015 <- df %>% filter(ea_id != "" & !is.na(ea_id))
       names(df2015) <- paste0("s15_", names(df2015))
       run("2014", to = "data")
       df2014 <- df %>% filter(ea_id != "" & !is.na(ea_id))
       names(df2014) <- paste0("s14_", names(df2014))
       inner_join(df2014, df2015, by = c("s14_ea_id" = "s15_ea_id"))
     },
     analyze = list(
       list(write = "data/2015/2015-longitudinal-analysis-tables.txt"),
       # list(write = "stdout"), # <-- toggle this to print to the screen.
       list(
         "N"                          = function(df) dim(df)
         , "[check] population aging" = function(df) var_summary(as.numeric(df$s15_age) - df$s14_age, verbose = TRUE)
         , "[check] gender"           = function(df) tab(df, s15_gender, s14_gender)
         , "[check] first heard EA"   = function(df) tab(df, s14_first_heard_EA, s15_first_heard_EA)
         , "summarize 2013 donations" = function(df) var_summary(df$s14_donate_2013_c, verbose = TRUE)
         , "summarize 2014 donations" = function(df) var_summary(df$s15_donate_2014_c, verbose = TRUE)
         , "summarize 2013 income"    = function(df) var_summary(df$s14_income_2013_c, verbose = TRUE)
         , "summarize 2014 income"    = function(df) var_summary(df$s15_income_2014_c, verbose = TRUE)
         , "EA career change"         = function(df) tab(df, s14_career_path, s15_career_path)
         , "donate 10%? [2013-2014]"  = function(df) tab(df, s14_p_donate_2013_c >= 10, s15_p_donate_2014_c >= 10)
         , "veg"                      = function(df) tab(df, s14_diet, s15_veg)
         , "cause import AR"          = function(df) tab(df, s14_cause_import_animals, s15_cause_import_animal_welfare)
         , "cause import prioritize"  = function(df) tab(df, s14_cause_import_cause_prioritization, s15_cause_import_cause_prioritization)
         , "cause environment"        = function(df) tab(df, s14_cause_import_environmentalism, s15_cause_import_environmentalism)
         , "cause AI"                 = function(df) tab(df, s14_cause_import_ai, s15_cause_import_ai)
         , "cause non-AI far future"  = function(df) tab(df, s14_cause_import_non_ai_far_future, s15_cause_import_non_ai_far_future)
         , "cause poverty"            = function(df) tab(df, s14_cause_import_poverty, s15_cause_import_poverty)
         , "cause rationality"        = function(df) tab(df, s14_cause_import_rationality, s15_cause_import_rationality)
         , "cause politics"           = function(df) tab(df, s14_cause_import_politics, s15_cause_import_politics)
         , "cause meta"               = function(df) tab(df, s14_cause_import_meta, s15_cause_import_meta)
     )))
