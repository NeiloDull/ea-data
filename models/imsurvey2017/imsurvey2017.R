Ramd::define("referrers", "simple_referrers", function(referrer_list, simple_referrer_list) {
  list(
    import = list(file = "data/imsurvey2017-anonymized-currencied.csv"),

    data   = list(
      "Drop insincere"   = function(df) {
          message(NROW(df), " results before dropping insincere")
          df <- dplyr::filter(df, grepl("Yes", sincere))
          message(NROW(df), " results after dropping insincere")
          df
      }
      , "Drop non-EA"    = function(df) {
          message(length(na.omit(df$is_ea)), " answered EA question")
          df <- dplyr::filter(df, grepl("Yes", is_ea))
          message(NROW(df), " after before dropping non-EA")
          df
      }
      , "Create age"     = function(df) { df$age <- 2017 - as.numeric(df$birth_year); df }
      , "Make % inc donate [2015]" = function(df) {
          p <- df$donate_2015_c / df$income_2015_individual_c
          p[is.infinite(p)] <- NA
          df$p_donate_2015 <- p
          df
      }
      , "Make % inc donate [2016]" = function(df) {
          p <- df$donate_2016_c / df$income_2016_individual_c
          p[is.infinite(p)] <- NA
          df$p_donate_2016 <- p
          df
      }
      , "Make referrer_url" = function(df) {
          df$referrer2 <- surveytools2::swap_by_value(df, "referrer_url", referrer_list, grep = TRUE)$referrer_url
          df$referrer2[is.na(df$referrer2)] <- "Missing"
          df$referrer2[!(df$referrer2 %in% unlist(unname(referrer_list)))] <- "Other"
          df
      }
      , "Make simple_referrer" = function(df) {
         df$referrer3 <- surveytools2::swap_by_value(df, "referrer_url", simple_referrer_list, grep = TRUE)$referrer_url
         df$referrer3[is.na(df$referrer3)] <- "Missing"
         df$referrer3[!(df$referrer3 %in% unlist(unname(simple_referrer_list)))] <- "Other"
         df
      }
      , "Make binary cause breakdown" = function(df) {
				for (variable in get_vars(df, "cause_import")) {
					df[[paste0(variable, "_b")]] <- swap_by_value(df, variable,
																						list("I do not think any EA resources should be devoted to this cause" = ":(",
																								 "I do not think this is a priority, but I am glad some EAs are looking into it" = ":(",
																								 "Not Considered / Not Sure" = ":(",
																								 "This cause deserves significant investment but less than the top priorities" = ":(",
																								 "This cause should be a near-top priority" = ":)",
																								 "This cause should be the top priority" = ":)"))[[variable]]
        }
        df
      }
      , "Make gender binary" = function(df) {
          # I know gender is not a binary, but this is still useful for analysis. My apologies.
          df$gender_b <- drop_values(df$gender, c("Other", "Prefer Not to Answer"))
          df
        }
      , "Make diet (veg*n vs. other) binary" = function(df) {
          df$veg_b <- df$veg %in% c("Vegan", "Vegetarian")
          df
        }
      , "Donations by cause area" = function(df) {
					orgs_by_cause <- list("meta" = c("RC", "80K", "CFAR", "CEA", "CS", "EF", "TLYCS"),
																"cause_pri" = c("ACE", "FRI", "GW"),
																"poverty" = c("AMF", "DTW", "END", "GD", "MC", "SCI", "Sightsavers"),
																"animal_rights" = c("faunalytics", "GF", "MFA", "SP", "THL"),
																"far_future" = c("FHI", "MIRI"))

					for (year in c("2015", "2016")) {
						for (cause in names(orgs_by_cause)) {
							out <- paste(orgs_by_cause[[cause]], year, "c", sep = "_") %>%
											 get_vars(df, ., ignore.case = TRUE) %/>%
											 first %/>%
											 (function(x) { df[[x]] }) %_>%
											 fn(x, y, nas_are_zeros(x) + nas_are_zeros(y))
							df[[paste("donate", cause, year, "c", sep = "_")]] <- out
						}
					}
					df
        }
    )

    , analyze = list(
      # list(write = "data/2017-survey-analysis-tables.txt"),
      list(write = "stdout"), # <-- toggle this to print to the screen.
      list(
        "cause_import_animal_welfare"          = function(df) tab(df, cause_import_animal_welfare)
        , "cause_import_cause_prioritization"  = function(df) tab(df, cause_import_cause_prioritization)
        , "cause_import_environmentalism"      = function(df) tab(df, cause_import_environmentalism)
        , "cause_import_ai"                    = function(df) tab(df, cause_import_ai)
        , "cause_import_non_ai_far_future"     = function(df) tab(df, cause_import_non_ai_far_future)
        , "cause_import_poverty"               = function(df) tab(df, cause_import_poverty)
        , "cause_import_rationality"           = function(df) tab(df, cause_import_rationality)
        , "cause_import_politics"              = function(df) tab(df, cause_import_politics)
        , "cause_import_meta"                  = function(df) tab(df, cause_import_meta)
				, "binary cause view"                  = function(df) { for (var in get_vars(df, "cause_import.+_b")) { print(tab_(df, var)) } }
				, "binary cause view x gender"         = function(df) { for (var in get_vars(df, "cause_import.+_b")) { print(tab_(df, list("gender_b", var), freq = FALSE, percent = TRUE)) } }
				, "binary cause view x diet"           = function(df) { for (var in get_vars(df, "cause_import.+_b")) { print(tab_(df, list("veg_b", var), freq = FALSE, percent = TRUE)) } }
        , "diet x cause_import_animal_welfare" = function(df) ctab(df, veg, cause_import_animal_welfare, na.rm = TRUE)
        , "diet x cause_import_animal_welfare 2"    = function(df) tab(df, veg %in% c("Vegan", "Vegetarian"), cause_import_animal_welfare_b, na.rm = TRUE, percent = TRUE)
        , "summarize donations 2015"                = function(df) var_summary(df$donate_2015_c, verbose = TRUE)
        , "summarize donations 2016"                = function(df) var_summary(df$donate_2016_c, verbose = TRUE)
        , "donate in 2015 and 2016?"                = function(df) tab(df, donate_2015_c > 0, donate_2016_c > 0)
        , "median 2015 -> 2016 $ donate increase"   = function(df) { df %>% dplyr::filter(donate_2015_c > 0, donate_2016_c > 0) %>% select(donate_2015_c, donate_2016_c) %>% mutate(increase = donate_2016_c - donate_2015_c) %>% summarise(median(increase)) }
        , "median 2015 -> 2016 income increase"     = function(df) { df %>% dplyr::filter(income_2015_individual_c > 0, income_2016_individual_c > 0) %>% select(income_2015_individual_c, income_2016_individual_c) %>% mutate(increase = income_2016_individual_c - income_2015_individual_c) %>% summarise(median(increase)) }
        , "median 2015 -> 2016 % donate increase"   = function(df) { df %>% dplyr::filter(p_donate_2015 > 0, p_donate_2016 > 0) %>% select(p_donate_2015, p_donate_2016) %>% mutate(increase = p_donate_2016 - p_donate_2015) %>% summarise(median(increase)) }
        , "donations x student"                = function(df) ctab(df, donate_2016_c, student)
        , "summary donations for students"     = function(df) var_summary(dplyr::filter(df, student == "Yes")$donate_2016_c, verbose = TRUE)
        , "summary donations for non-students" = function(df) var_summary(dplyr::filter(df, student == "No")$donate_2016_c, verbose = TRUE)
        , "donation quantile 1"                = function(df) quantile(df$donate_2016_c, probs = seq(0.1, 1, len = 10), na.rm = TRUE)
        , "donation quantile 2"                = function(df) quantile(df$donate_2016_c, probs = seq(0.91, 1, len = 10), na.rm = TRUE)
        , "summarize % donations 2015"         = function(df) var_summary(dplyr::filter(df, income_2015_individual_c > 10000)$p_donate_2015 * 100, verbose = TRUE)
        , "summarize % donations 2016"         = function(df) var_summary(dplyr::filter(df, income_2016_individual_c > 10000)$p_donate_2016 * 100, verbose = TRUE)
        , "% donation quantile"                = function(df) quantile(dplyr::filter(df, income_2016_individual_c > 10000)$p_donate_2016 * 100, probs = seq(0.1, 1, len = 10), na.rm = TRUE)
        , "% donation breakdown"               = function(df) breakdown(dplyr::filter(df, income_2016_individual_c > 10000), "p_donate_2016", c(0.01, 0.02, 0.03, 0.05, 0.1, 0.15, seq(0.2, 0.9, by =  0.1)))
        , "RC Donations 2015 (Total)"          = function(df) var_summary(df$donate_RC_2015_c, verbose = TRUE)
        , "RC Donations 2016 (Total)"          = function(df) var_summary(df$donate_RC_2016_c, verbose = TRUE)
        , "Did donate to RC 2015?"             = function(df) tab(df, donate_RC_2015_c > 0)
        , "Did donate to RC 2016?"             = function(df) tab(df, donate_RC_2016_c > 0)
        , "80K Donations 2015 (Total)"         = function(df) var_summary(df$donate_80K_2015_c, verbose = TRUE)
        , "80K Donations 2016 (Total)"         = function(df) var_summary(df$donate_80K_2016_c, verbose = TRUE)
        , "Did donate to 80K 2015?"            = function(df) tab(df, donate_80K_2015_c > 0)
        , "Did donate to 80K 2016?"            = function(df) tab(df, donate_80K_2016_c > 0)
        , "ACE Donations 2015 (Total)"         = function(df) var_summary(df$donate_ace_2015_c, verbose = TRUE)
        , "ACE Donations 2016 (Total)"         = function(df) var_summary(df$donate_ace_2016_c, verbose = TRUE)
        , "Did donate to ACE 2015?"            = function(df) tab(df, donate_ace_2015_c > 0)
        , "Did donate to ACE 2016?"            = function(df) tab(df, donate_ace_2016_c > 0)
        , "AMF Donations 2015 (Total)"         = function(df) var_summary(df$donate_amf_2015_c, verbose = TRUE)
        , "AMF Donations 2016 (Total)"         = function(df) var_summary(df$donate_amf_2016_c, verbose = TRUE)
        , "Did donate to AMF 2015?"            = function(df) tab(df, donate_amf_2015_c > 0)
        , "Did donate to AMF 2016?"            = function(df) tab(df, donate_amf_2016_c > 0)
        , "CFAR Donations 2015 (Total)"        = function(df) var_summary(df$donate_cfar_2015_c, verbose = TRUE)
        , "CFAR Donations 2016 (Total)"        = function(df) var_summary(df$donate_cfar_2016_c, verbose = TRUE)
        , "Did donate to CFAR 2015?"           = function(df) tab(df, donate_cfar_2015_c > 0)
        , "Did donate to CFAR 2016?"           = function(df) tab(df, donate_cfar_2016_c > 0)
        , "CEA Donations 2015 (Total)"         = function(df) var_summary(df$donate_cea_2015_c, verbose = TRUE)
        , "CEA Donations 2016 (Total)"         = function(df) var_summary(df$donate_cea_2016_c, verbose = TRUE)
        , "Did donate to CEA 2015?"            = function(df) tab(df, donate_cea_2015_c > 0)
        , "Did donate to CEA 2016?"            = function(df) tab(df, donate_cea_2016_c > 0)
        , "CS Donations 2015 (Total)"          = function(df) var_summary(df$donate_cs_2015_c, verbose = TRUE)
        , "CS Donations 2016 (Total)"          = function(df) var_summary(df$donate_cs_2016_c, verbose = TRUE)
        , "Did donate to CS 2015?"             = function(df) tab(df, donate_cs_2015_c > 0)
        , "Did donate to CS 2016?"             = function(df) tab(df, donate_cs_2016_c > 0)
        , "DTW Donations 2015 (Total)"         = function(df) var_summary(df$donate_dtw_2015_c, verbose = TRUE)
        , "DTW Donations 2016 (Total)"         = function(df) var_summary(df$donate_dtw_2016_c, verbose = TRUE)
        , "Did donate to DTW 2015?"            = function(df) tab(df, donate_dtw_2015_c > 0)
        , "Did donate to DTW 2016?"            = function(df) tab(df, donate_dtw_2016_c > 0)
        , "EF Donations 2015 (Total)"          = function(df) var_summary(df$donate_ef_2015_c, verbose = TRUE)
        , "EF Donations 2016 (Total)"          = function(df) var_summary(df$donate_ef_2016_c, verbose = TRUE)
        , "Did donate to EF 2015?"             = function(df) tab(df, donate_ef_2015_c > 0)
        , "Did donate to EF 2016?"             = function(df) tab(df, donate_ef_2016_c > 0)
        , "END Fund Donations 2015 (Total)"    = function(df) var_summary(df$donate_end_2015_c, verbose = TRUE)
        , "END Fund Donations 2016 (Total)"    = function(df) var_summary(df$donate_end_2016_c, verbose = TRUE)
        , "Did donate to END Fund 2015?"       = function(df) tab(df, donate_end_2015_c > 0)
        , "Did donate to END Fund 2016?"       = function(df) tab(df, donate_end_2016_c > 0)
        , "Faunalytics Donations 2015 (Total)" = function(df) var_summary(df$donate_faunalytics_2015_c, verbose = TRUE)
        , "Faunalytics Donations 2016 (Total)" = function(df) var_summary(df$donate_faunalytics_2016_c, verbose = TRUE)
        , "Did donate to Faunalytics 2015?"    = function(df) tab(df, donate_faunalytics_2015_c > 0)
        , "Did donate to Faunalytics 2016?"    = function(df) tab(df, donate_faunalytics_2016_c > 0)
        , "FRI Donations 2015 (Total)"    = function(df) var_summary(df$donate_fri_2015_c, verbose = TRUE)
        , "FRI Donations 2016 (Total)"    = function(df) var_summary(df$donate_fri_2016_c, verbose = TRUE)
        , "Did donate to FRI 2015?"       = function(df) tab(df, donate_fri_2015_c > 0)
        , "Did donate to FRI 2016?"       = function(df) tab(df, donate_fri_2016_c > 0)
        , "FHI Donations 2015 (Total)"    = function(df) var_summary(df$donate_fhi_2015_c, verbose = TRUE)
        , "FHI Donations 2016 (Total)"    = function(df) var_summary(df$donate_fhi_2016_c, verbose = TRUE)
        , "Did donate to FHI 2015?"       = function(df) tab(df, donate_fhi_2015_c > 0)
        , "Did donate to FHI 2016?"       = function(df) tab(df, donate_fhi_2016_c > 0)
        , "GD Donations 2015 (Total)"     = function(df) var_summary(df$donate_gd_2015_c, verbose = TRUE)
        , "GD Donations 2016 (Total)"     = function(df) var_summary(df$donate_gd_2016_c, verbose = TRUE)
        , "Did donate to GD 2015?"        = function(df) tab(df, donate_gd_2015_c > 0)
        , "Did donate to GD 2016?"        = function(df) tab(df, donate_gd_2016_c > 0)
        , "GW Donations 2015 (Total)"     = function(df) var_summary(df$donate_gw_2015_c, verbose = TRUE)
        , "GW Donations 2016 (Total)"     = function(df) var_summary(df$donate_gw_2016_c, verbose = TRUE)
        , "Did donate to GW 2015?"        = function(df) tab(df, donate_gw_2015_c > 0)
        , "Did donate to GW 2016?"        = function(df) tab(df, donate_gw_2016_c > 0)
        , "GF Donations 2015 (Total)"     = function(df) var_summary(df$donate_gf_2015_c, verbose = TRUE)
        , "GF Donations 2016 (Total)"     = function(df) var_summary(df$donate_gf_2016_c, verbose = TRUE)
        , "Did donate to GF 2015?"        = function(df) tab(df, donate_gf_2015_c > 0)
        , "Did donate to GF 2016?"        = function(df) tab(df, donate_gf_2016_c > 0)
        , "MIRI Donations 2015 (Total)"   = function(df) var_summary(df$donate_miri_2015_c, verbose = TRUE)
        , "MIRI Donations 2016 (Total)"   = function(df) var_summary(df$donate_miri_2016_c, verbose = TRUE)
        , "Did donate to MIRI 2015?"      = function(df) tab(df, donate_miri_2015_c > 0)
        , "Did donate to MIRI 2016?"      = function(df) tab(df, donate_miri_2016_c > 0)
        , "MC Donations 2015 (Total)"     = function(df) var_summary(df$donate_mc_2015_c, verbose = TRUE)
        , "MC Donations 2016 (Total)"     = function(df) var_summary(df$donate_mc_2016_c, verbose = TRUE)
        , "Did donate to MC 2015?"        = function(df) tab(df, donate_mc_2015_c > 0)
        , "Did donate to MC 2016?"        = function(df) tab(df, donate_mc_2016_c > 0)
        , "MFA Donations 2015 (Total)"    = function(df) var_summary(df$donate_mfa_2015_c, verbose = TRUE)
        , "MFA Donations 2016 (Total)"    = function(df) var_summary(df$donate_mfa_2016_c, verbose = TRUE)
        , "Did donate to MFA 2015?"       = function(df) tab(df, donate_mfa_2015_c > 0)
        , "Did donate to MFA 2016?"       = function(df) tab(df, donate_mfa_2016_c > 0)
        , "SCI Donations 2015 (Total)"    = function(df) var_summary(df$donate_sci_2015_c, verbose = TRUE)
        , "SCI Donations 2016 (Total)"    = function(df) var_summary(df$donate_sci_2016_c, verbose = TRUE)
        , "Did donate to SCI 2015?"       = function(df) tab(df, donate_sci_2015_c > 0)
        , "Did donate to SCI 2016?"       = function(df) tab(df, donate_sci_2016_c > 0)
        , "Sightsavers Donations 2015 (Total)" = function(df) var_summary(df$donate_sightsavers_2015_c, verbose = TRUE)
        , "Sightsavers Donations 2016 (Total)" = function(df) var_summary(df$donate_sightsavers_2016_c, verbose = TRUE)
        , "Did donate to Sightsavers 2015?"    = function(df) tab(df, donate_sightsavers_2015_c > 0)
        , "Did donate to Sightsavers 2016?"    = function(df) tab(df, donate_sightsavers_2016_c > 0)
        , "SP Donations 2015 (Total)"     = function(df) var_summary(df$donate_sp_2015_c, verbose = TRUE)
        , "SP Donations 2016 (Total)"     = function(df) var_summary(df$donate_sp_2016_c, verbose = TRUE)
        , "Did donate to SP 2015?"        = function(df) tab(df, donate_sp_2015_c > 0)
        , "Did donate to SP 2016?"        = function(df) tab(df, donate_sp_2016_c > 0)
        , "THL Donations 2015 (Total)"    = function(df) var_summary(df$donate_thl_2015_c, verbose = TRUE)
        , "THL Donations 2016 (Total)"    = function(df) var_summary(df$donate_thl_2016_c, verbose = TRUE)
        , "Did donate to THL 2015?"       = function(df) tab(df, donate_thl_2015_c > 0)
        , "Did donate to THL 2016?"       = function(df) tab(df, donate_thl_2016_c > 0)
        , "TLYCS Donations 2015 (Total)"  = function(df) var_summary(df$donate_tlycs_2015_c, verbose = TRUE)
        , "TLYCS Donations 2016 (Total)"  = function(df) var_summary(df$donate_tlycs_2016_c, verbose = TRUE)
        , "Did donate to TLYCS 2015?"     = function(df) tab(df, donate_tlycs_2015_c > 0)
        , "Did donate to TLYCS 2016?"     = function(df) tab(df, donate_tlycs_2016_c > 0)
        , "Meta Cause Donations 2015 (Total)"  = function(df) var_summary(df$donate_meta_2015_c, verbose = TRUE)
        , "Meta Cause Donations 2016 (Total)"  = function(df) var_summary(df$donate_meta_2016_c, verbose = TRUE)
        , "Did donate to Meta Cause 2015?"     = function(df) tab(df, donate_meta_2015_c > 0)
        , "Did donate to Meta Cause 2016?"     = function(df) tab(df, donate_meta_2016_c > 0)
        , "Global Poverty Cause Donations 2015 (Total)"  = function(df) var_summary(df$donate_poverty_2015_c, verbose = TRUE)
        , "Global Poverty Cause Donations 2016 (Total)"  = function(df) var_summary(df$donate_poverty_2016_c, verbose = TRUE)
        , "Did donate to Global Poverty Cause 2015?"     = function(df) tab(df, donate_poverty_2015_c > 0)
        , "Did donate to Global Poverty Cause 2016?"     = function(df) tab(df, donate_poverty_2016_c > 0)
        , "Cause Prioritization Cause Donations 2015 (Total)"  = function(df) var_summary(df$donate_cause_pri_2015_c, verbose = TRUE)
        , "Cause Prioritization Cause Donations 2016 (Total)"  = function(df) var_summary(df$donate_cause_pri_2016_c, verbose = TRUE)
        , "Did donate to Cause Prioritization Cause 2015?"     = function(df) tab(df, donate_cause_pri_2015_c > 0)
        , "Did donate to Cause Prioritization Cause 2016?"     = function(df) tab(df, donate_cause_pri_2016_c > 0)
        , "Animal Welfare Cause Donations 2015 (Total)"  = function(df) var_summary(df$donate_far_future_2015_c, verbose = TRUE)
        , "Animal Welfare Cause Donations 2016 (Total)"  = function(df) var_summary(df$donate_far_future_2016_c, verbose = TRUE)
        , "Did donate to Animal Welfare Cause 2015?"     = function(df) tab(df, donate_far_future_2015_c > 0)
        , "Did donate to Animal Welfare Cause 2016?"     = function(df) tab(df, donate_far_future_2016_c > 0)
        , "Far Future Cause Donations 2015 (Total)"  = function(df) var_summary(df$donate_animal_rights_2015_c, verbose = TRUE)
        , "Far Future Cause Donations 2016 (Total)"  = function(df) var_summary(df$donate_animal_rights_2016_c, verbose = TRUE)
        , "Did donate to Far Future Cause 2015?"     = function(df) tab(df, donate_animal_rights_2015_c > 0)
        , "Did donate to Far Future Cause 2016?"     = function(df) tab(df, donate_animal_rights_2016_c > 0)
        , "age"                           = function(df) tab(df, age)
        , "gender"                        = function(df) tab(df, gender)
        , "race_white"                    = function(df) tab(df, race_white)
        , "race_black"                    = function(df) tab(df, race_black)
        , "race_hispanic"                 = function(df) tab(df, race_hispanic)
        , "race_asian"                    = function(df) tab(df, race_asian)
        , "student"                       = function(df) tab(df, student)
        , "country"                       = function(df) tab(df, country)
        , "city"                          = function(df) tab(df, city)
        , "religion"                      = function(df) tab(df, religion)
        , "diet"                          = function(df) tab(df, veg)
        , "moral philosophy"              = function(df) tab(df, moral_philosophy)
        , "opportunity or obligation"     = function(df) tab(df, ea_opportunity_or_obligation)
        , "employment_status"             = function(df) tab(df, employment_status)
        , "employment field"              = function(df) tab(df, field)
        , "studied_cs"                    = function(df) tab(df, studied_cs)
        , "studied_econ"                  = function(df) tab(df, studied_econ)
        , "studied_engineering"           = function(df) tab(df, studied_engineering)
        , "studied_maths"                 = function(df) tab(df, studied_maths)
        , "studied_medicine"              = function(df) tab(df, studied_medicine)
        , "studied_psych"                 = function(df) tab(df, studied_psych)
        , "studied_phil"                  = function(df) tab(df, studied_phil)
        , "studied_physics"               = function(df) tab(df, studied_physics)
        , "studied_humanities"            = function(df) tab(df, studied_humanities)
        , "studied_social_science"        = function(df) tab(df, studied_social_science)
        , "studied_other_science"         = function(df) tab(df, studied_other_science)
        , "ea_donate"                     = function(df) tab(df, ea_donate)
        , "ea_volunteer"                  = function(df) tab(df, ea_volunteer)
        , "ea_career"                     = function(df) tab(df, ea_career)
        , "ea_career_type"                = function(df) tab(df, ea_career_type)
        , "ea_volunteer_hours"            = function(df) tab(df, ea_volunteer_hours)
        , "ea_paid_hours"                 = function(df) tab(df, ea_paid_hours)
        , "EA year"                       = function(df) tab(df, which_year_EA)
        , "how heard"                     = function(df) tab(df, first_heard_EA)
        , "First heard EA by year"        = function(df) { for (var in unique(df$which_year_EA)) { message(var); print(tab(dplyr::filter_(df, paste0("which_year_EA == '", var, "'")), first_heard_EA, freq = FALSE, percent = TRUE)) }}
        , "LessWrong refer over year"     = function(df) tab(df, which_year_EA, first_heard_EA == "LessWrong", na.rm = TRUE, percent = TRUE)
        , "GWWC refer over year"          = function(df) tab(df, which_year_EA, first_heard_EA == "Giving What We Can", na.rm = TRUE, percent = TRUE)
        , "80K refer over year"           = function(df) tab(df, which_year_EA, first_heard_EA == "80,000 Hours", na.rm = TRUE, percent = TRUE)
        , "SSC refer over year"           = function(df) tab(df, which_year_EA, first_heard_EA == "Slate Star Codex", na.rm = TRUE, percent = TRUE)
        , "Heard EA attributed donations" = function(df) { df %>% group_by(first_heard_EA) %>% summarise(sum(donate_2016_c, na.rm = TRUE)) %>% setNames(., list("first_heard_EA", "sum_donations")) %>% arrange(-sum_donations) }
        , "Heard EA attributed donations" = function(df) { df %>% group_by(first_heard_EA) %>% summarise(sum(donate_2016_c, na.rm = TRUE)) %>% setNames(., list("first_heard_EA", "sum_donations")) %>% arrange(-sum_donations) }
        , "Heard EA attributed donors"    = function(df) tab(df, first_heard_EA, donate_2016_c > 0, percent = TRUE, na.rm = TRUE)
        , "Heard EA attributed career"    = function(df) tab(df, first_heard_EA, ea_career, percent = TRUE, na.rm = TRUE)
        , "involved_TLYCS"                = function(df) tab(df, involved_TLYCS)
        , "involved_local_EA"             = function(df) tab(df, involved_local_EA)
        , "involved_lesswrong"            = function(df) tab(df, involved_lesswrong)
        , "involved_givewell"             = function(df) tab(df, involved_givewell)
        , "involved_online_ea"            = function(df) tab(df, involved_online_ea)
        , "involved_personal_contact"     = function(df) tab(df, involved_personal_contact)
        , "involved_80K"                  = function(df) tab(df, involved_80K)
        , "involved_GWWC"                 = function(df) tab(df, involved_GWWC)
        , "involved EA Global"            = function(df) tab(df, involved_ea_global)
        , "involved Book or Blog"         = function(df) tab(df, involved_book_blog)
        , "involved Swiss"                = function(df) tab(df, involved_swiss)
        , "Involvment attributed"         = function(df) { for (var in get_vars(df, "involved")) { message(var); cat("N: ", sum(df[[var]] == "Yes", na.rm = TRUE), "\n", "Donations: ", sum(df[df[[var]] == "Yes",]$donate_2016_c, na.rm = TRUE), "\n", "Donors: ", sum(df[df[[var]] == "Yes",]$donate_2016_c > 0, na.rm = TRUE), "\n", "Careers: ", sum(df[df[[var]] == "Yes",]$ea_career == "Yes", na.rm = TRUE), "\n") }}
        , "member_ea_fb"                  = function(df) tab(df, member_ea_fb)
        , "member_ea_forum"               = function(df) tab(df, member_ea_forum)
        , "member_gwwc"                   = function(df) tab(df, member_gwwc)
        , "member_lw"                     = function(df) tab(df, member_lw)
        , "member_local_group"            = function(df) tab(df, member_local_group)
        , "would_like_more_involvement"   = function(df) tab(df, would_like_more_involvement)
        , "Insecurity about EA"           = function(df) tab(df, insecurity)
        , "EA Welcoming"                  = function(df) tab(df, ea_welcoming)
        , "ea_nps"                        = function(df) tab(df, ea_nps)
        , "confidence in personal EA"     = function(df) tab(df, confident_future_ea_personal)
        , "confidence in EA movement"     = function(df) tab(df, confident_future_ea_movement)
        , "confidence x confidence"       = function(df) ctab(df, confident_future_ea_personal, confident_future_ea_movement, na.rm = TRUE)
        , "referrer self report"          = function(df) tab(df, referrer_self_report)
        , "referrer URL"                  = function(df) tab(df, referrer2)
        , "referrer URL (narrower)"       = function(df) tab(df, referrer3)
        , "referrer3 x age"               = function(df) ctab(df, age, referrer3, na.rm = TRUE)
        , "referrer3 x donation"          = function(df) ctab(dplyr::filter(df, student == "No"), donate_2016_c, referrer3, na.rm = TRUE)
        , "referrer3 x income"            = function(df) ctab(dplyr::filter(df, student == "No"), income_2016_individual_c, referrer3, na.rm = TRUE)
        , "referrer3 x % income donate"   = function(df) ctab(dplyr::filter(df, student == "No" & income_2016_individual_c > 9999), p_donate_2016, referrer3, na.rm = TRUE)    
        , "referrer3 x poverty"           = function(df) ctab(df, cause_import_poverty == "This cause should be the top priority", referrer3, na.rm = TRUE)
        , "referrer3 x student"           = function(df) ctab(df, student, referrer3, na.rm = TRUE)
        , "referrer3 x veg"               = function(df) ctab(df, veg == "Vegetarian" | veg == "Vegan", referrer3, na.rm = TRUE)
        , "referrer3 x year got involved" = function(df) ctab(df, which_year_EA, referrer3, na.rm = TRUE)
        , "MIRI donations by year"        = function(df) ctab(df %>% dplyr::filter(donate_2016_c > 0), donate_miri_2016_c, which_year_EA, na.rm = TRUE)
        , "AMF donations by year"         = function(df) ctab(df %>% dplyr::filter(donate_2016_c > 0), donate_amf_2016_c, which_year_EA, na.rm = TRUE)
        , "AMF donations by first heard"  = function(df) ctab(df %>% dplyr::filter(donate_2016_c > 0), donate_amf_2016_c, first_heard_EA, na.rm = TRUE)
        , "MIRI donations by first heard" = function(df) ctab(df %>% dplyr::filter(donate_2016_c > 0), donate_miri_2016_c, first_heard_EA, na.rm = TRUE) 
        , "first heard through local grp" = function(df) tab(df, first_heard_EA == "Local EA group")
        , "first heard local x GWWC mem"  = function(df) ctab(df, member_gwwc, first_heard_EA == "Local EA group")
        , "first heard local x donate"    = function(df) ctab(df, donate_2016_c, first_heard_EA == "Local EA group")
        , "would attend"                  = function(df) tab(df, would_attend_local_group)
        , "involved_local_EA x GWWC invo" = function(df) ctab(df, involved_local_EA, involved_GWWC)
        , "involved_local_EA x GWWC mem"  = function(df) ctab(df, involved_local_EA, member_gwwc)
        , "involved_GWWC x GWWC mem"      = function(df) ctab(df, involved_GWWC, member_gwwc)
        , "involved_local_EA x donate"    = function(df) ctab(df, donate_2016_c, involved_local_EA)
        , "member_local x GWWC"           = function(df) ctab(df, member_local_group, member_gwwc)
        , "involved_local_EA x local mem" = function(df) ctab(df, involved_local_EA, member_local_group)
        , "member_local x year"           = function(df) ctab(df, member_local_group, which_year_EA)
        , "member_local x donate"         = function(df) ctab(df, donate_2016_c, member_local_group)
        , "member_gwwc x involved local"  = function(df) ctab(df, member_gwwc, involved_local_EA)
        , "city x member_gwwc"            = function(df) ctab(df, city, member_gwwc, top = 10)
        , "non-students >= 10% donated x GWWC"     = function(df) tab(dplyr::filter(df, income_2016_individual_c > 10000) %>% dplyr::filter(student == "No"), p_donate_2016 * 100 >= 10, member_gwwc)
        , "students >= 10% donated x GWWC"     = function(df) tab(dplyr::filter(df, income_2016_individual_c > 10000) %>% dplyr::filter(student == "Yes"), p_donate_2016 * 100 >= 10, member_gwwc)
        , "GWWC students donating >=1%"   = function(df) ctab(dplyr::filter(df, student == "Yes"), p_donate_2016 >= 0.01, member_gwwc, na.rm = TRUE)
        , "GWWC non-students donating >10%" = function(df) ctab(dplyr::filter(df, student == "No"), p_donate_2016 >= 0.1, member_gwwc, na.rm = TRUE)
        , "GWWC non-students income <$50K donating >10%" = function(df) ctab(dplyr::filter(df, student == "No", income_2016_individual_c < 50000), p_donate_2016 >= 0.1, member_gwwc, na.rm = TRUE)
        , "GWWC non-students income >=$50K donating >10%" = function(df) ctab(dplyr::filter(df, student == "No", income_2016_individual_c >= 50000), p_donate_2016 >= 0.1, member_gwwc, na.rm = TRUE)
      )
    )
  )
})
