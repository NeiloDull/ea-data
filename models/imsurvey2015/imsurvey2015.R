list(
  import = list(file = "data/imsurvey2015-anonymized-renamed-currencied.csv"),
  data   = list(
    "Drop insincere"   = list(select_rows, function(df) grepl("Yes", df$sincere, fixed = TRUE), whole = TRUE)
    , "Drop non-EA"    = list(select_rows, function(df) grepl("Yes", df$is_ea, fixed = TRUE), whole = TRUE)
    , "Drop time vars" = list(function(df) { df[!grepl(".time", names(df), fixed = TRUE)] })
    , "Drop comments"  = list(function(df) { df[!grepl("comment", names(df), fixed = TRUE)] })
    , "Have a plan"    = list(new_variable, function(plan_donate_how_much, already_stated_plan, donate_2014) {
          plan_donate_how_much != "" | already_stated_plan == "Yes" | !is.na(donate_2014)
        }, "have_donation_plan")
    , "Sold EA"        = list(new_variable, function(have_donation_plan, ea_career) {
          (have_donation_plan & !is.na(have_donation_plan) | (ea_career == "Yes" & !is.na(ea_career)))	
        }, "sold_ea")
    , "% inc donate"   = list(new_variable, function(donate_2014_c, income_2014_c) {
          p <- donate_2014_c / income_2014_c
          p[is.infinite(p)] <- NA
          p
        }, "p_donate_2014_c")
    , "Solid EA"       = list(new_variable, function(p_donate_2014_c, ea_career) {
          (p_donate_2014_c > 0.1 | ea_career == "Yes") & !is.na(p_donate_2014_c) & !is.na(ea_career)
        }, "solid_ea")
    , "Radical Giver"  = list(new_variable, function(p_donate_2014_c) {
          p_donate_2014_c >= 0.333
        }, "radical_giver")
    , "is_programmer"  = list(new_variable, function(occupation) {
          ifelse(occupation == "", "", grepl("engineer|programmer|software", occupation, ignore.case = TRUE))
        }, "is_programmer")
    , "referrer_url"   = list(replace_variable, function(referrer_url) {
          ifelse(grepl("utilitarianism-facebook-group", referrer_url), "Utilitarianism FB Group",
          ifelse(grepl("TLY", referrer_url), "TLY",
          ifelse(grepl("survey-site-homepage", referrer_url), "Survey Home",
          ifelse(grepl("SSC", referrer_url), "SSC",
          ifelse(grepl("SHARELW", referrer_url), "LW-SHARE",
          ifelse(grepl("SHARE", referrer_url), "SHARE",
          ifelse(grepl("LW", referrer_url), "LW",
          ifelse(grepl("LocalGroup", referrer_url), "Local Group",
          ifelse(grepl("local-groups-facebook", referrer_url), "Local Group FB",
          ifelse(grepl("LLonE", referrer_url), "LLonE",
          ifelse(grepl("gwwc-members", referrer_url), "GWWC FB Link",
          ifelse(grepl("gwwc-fb", referrer_url), "GWWC FB Group Message",
          ifelse(grepl("fbsample", referrer_url), "FB Random Sample",
          ifelse(grepl("fb-post-2-custom-share", referrer_url), "FB-SHARE",
          ifelse(grepl("fb-post-2", referrer_url), "EAFB",
          ifelse(grepl("ea-fb-group", referrer_url), "EAFB",
          ifelse(grepl("email-to-people", referrer_url), "EA Profile Emails",
          ifelse(grepl("eahub", referrer_url), "EA Hub",
          ifelse(grepl("EAF", referrer_url), "EA Forum",
          ifelse(grepl("eaa-facebook", referrer_url), "EAA FB",
          ifelse(grepl("ea-newsletter", referrer_url), "EA Newsletter",
          ifelse(grepl("ea-hangout-facebook", referrer_url), "EA Hangout FB",
          ifelse(grepl("ACE", referrer_url), "ACE",
          ifelse(is.na(referrer_url), "No Referrer", "Others"))))))))))))))))))))))))
        })	  
	    , "simple referrer" = list(new_variable, function(referrer_url) {
          ifelse(referrer_url == "SSC", "SlateStarCodex",
          ifelse(referrer_url == "FB Random Sample", "FB Random Sample",
          ifelse(grepl("GWWC FB", referrer_url), "GWWC Group Message", "Other")))
        }, "referrer2")
  )

  , analyze = list(
    "num respondents"                 = function(df) num_respondents(df)
    , "first heard about EA"          = function(df) tab(df, first_heard_EA)
    , "EA year"                       = function(df) tab(df, which_year_EA)
    , "how heard x year"              = function(df) ctab(df, first_heard_EA, which_year_EA)
    , "how heard x year (freq)"       = function(df) tab(df, first_heard_EA, which_year_EA)
    , "involved_TLYCS"                = function(df) tab(df, involved_TLYCS)
    , "involved_local_EA"             = function(df) tab(df, involved_local_EA)
    , "involved_lesswrong"            = function(df) tab(df, involved_lesswrong)
    , "involved_givewell"             = function(df) tab(df, involved_givewell)
    , "involved_online_ea"            = function(df) tab(df, involved_online_ea)
    , "involved_personal_contact"     = function(df) tab(df, involved_personal_contact)
    , "involved_80K"                  = function(df) tab(df, involved_80K)
    , "involved_GWWC"                 = function(df) tab(df, involved_GWWC)
    , "member_ea_fb"                  = function(df) tab(df, member_ea_fb)
    , "member_ea_forum"               = function(df) tab(df, member_ea_forum)
    , "member_gwwc"                   = function(df) tab(df, member_gwwc)
    , "member_lw"                     = function(df) tab(df, member_lw)
    , "member_tlycs"                  = function(df) tab(df, member_tlycs)
    , "member_local_group"            = function(df) tab(df, member_local_group)
    , "gender"                        = function(df) tab(df, gender)
    , "age"                           = function(df) tab(df, age)
    , "religion"                      = function(df) tab(df, religion)
    , "student"                       = function(df) tab(df, student)
    , "country"                       = function(df) tab(df, country)
    , "city"                          = function(df) tab(df, city)
    , "cause_import_animal_welfare"   = function(df) tab(df, cause_import_animal_welfare)
    , "cause_import_cause_prioritization"  = function(df) tab(df, cause_import_cause_prioritization)
    , "cause_import_environmentalism"      = function(df) tab(df, cause_import_environmentalism)
    , "cause_import_ai"                    = function(df) tab(df, cause_import_ai)
    , "cause_import_non_ai_far_future"     = function(df) tab(df, cause_import_non_ai_far_future)
    , "cause_import_poverty"          = function(df) tab(df, cause_import_poverty)
    , "cause_import_rationality"      = function(df) tab(df, cause_import_rationality)
    , "cause_import_politics"         = function(df) tab(df, cause_import_politics)
    , "cause_import_meta"             = function(df) tab(df, cause_import_meta)
    , "first heard through local grp" = function(df) tab(df, first_heard_EA == "Local EA group")
    , "first heard local x GWWC mem"  = function(df) ctab(df, member_gwwc, first_heard_EA == "Local EA group")
    , "total donated"                 = function(df) sum(df$donate_2014_c, na.rm = TRUE)
    , "first heard local x donate"    = function(df) ctab(df, donate_2014_c, first_heard_EA == "Local EA group")
    , "would attend"                  = function(df) tab(df, would_attend_local_group)
    , "involved_local_EA x GWWC invo" = function(df) ctab(df, involved_local_EA, involved_GWWC)
    , "involved_local_EA x GWWC mem"  = function(df) ctab(df, involved_local_EA, member_gwwc)
    , "involved_GWWC x GWWC mem"      = function(df) ctab(df, involved_GWWC, member_gwwc)
    , "involved_local_EA x donate"    = function(df) ctab(df, donate_2014_c, involved_local_EA)
    , "member_local x GWWC"           = function(df) ctab(df, member_local_group, member_gwwc)
    , "involved_local_EA x local mem" = function(df) ctab(df, involved_local_EA, member_local_group)
    , "member_local x year"           = function(df) ctab(df, member_local_group, which_year_EA)
    , "member_local x donate"         = function(df) ctab(df, donate_2014_c, member_local_group)
    , "GWWC x donate"                 = function(df) ctab(df, donate_2014_c, member_gwwc)
    , "member_local x donate %"       = function(df) ctab(df, p_donate_2014_c, member_local_group)
    , "GWWC x donate %"               = function(df) ctab(df, p_donate_2014_c, member_gwwc)
    , "GWWC x donate % (inc > $10k)"  = function(df) ctab(filter(df, income_2014_c > 10000), p_donate_2014_c, member_gwwc)
    , "GWWC x donate 10% or more (inc > $10K)"  = function(df) ctab(filter(df, income_2014_c > 10000), p_donate_2014_c > 0.0999999, member_gwwc, na.rm = TRUE) #avoid float-point comparison problem
    , "member_local x student"        = function(df) ctab(df, member_local_group, student)
    , "member_local x welcoming"      = function(df) ctab(df, member_local_group, ea_welcoming)
    , "member_gwwc x welcoming"       = function(df) ctab(df, member_gwwc, ea_welcoming)
    , "member_local x fundraiser"     = function(df) ctab(df, member_local_group, action_fundraiser)
    , "member_gwwc x fundraiser"      = function(df) ctab(df, member_gwwc, action_fundraiser)
    , "member_local x legacy"         = function(df) ctab(df, member_local_group, action_legacy)
    , "member_gwwc x legacy"          = function(df) ctab(df, member_gwwc, action_legacy)
    , "member_local x telling_friends"     = function(df) ctab(df, member_local_group, action_telling_friends)
    , "member_gwwc x telling_friends"      = function(df) ctab(df, member_gwwc, action_telling_friends)
    , "member_local x ea_newsletter"       = function(df) ctab(df, member_local_group, action_ea_newsletter)
    , "member_gwwc x ea_newsletter"        = function(df) ctab(df, member_gwwc, action_ea_newsletter)
    , "donate_80K"                    = function(df) tab(df, donate_80K)
    , "donate_amf"                    = function(df) tab(df, donate_amf)
    , "donate_ace"                    = function(df) tab(df, donate_ace)
    , "donate_cea"                    = function(df) tab(df, donate_cea)
    , "donate_cfar"                   = function(df) tab(df, donate_cfar)
    , "donate_dtw"                    = function(df) tab(df, donate_dtw)
    , "donate_gd"                     = function(df) tab(df, donate_gd)
    , "donate_gw"                     = function(df) tab(df, donate_gw)
    , "donate_gwwc"                   = function(df) tab(df, donate_gwwc)
    , "donate_thl"                    = function(df) tab(df, donate_thl)
    , "donate_leverage"               = function(df) tab(df, donate_leverage)
    , "donate_miri"                   = function(df) tab(df, donate_miri)
    , "donate_phc"                    = function(df) tab(df, donate_phc)
    , "donate_sci"                    = function(df) tab(df, donate_sci)
    , "donate_vo"                     = function(df) tab(df, donate_vo)
    , "MIRI donations by year"        = function(df) ctab(df %>% filter(donate_2014_c > 0), donate_miri, which_year_EA, na.rm = TRUE)
    , "AMF donations by year"         = function(df) ctab(df %>% filter(donate_2014_c > 0), donate_amf, which_year_EA, na.rm = TRUE)
    , "AMF donations by first heard"  = function(df) ctab(df %>% filter(donate_2014_c > 0), donate_amf, first_heard_EA, na.rm = TRUE)
    , "AMF donations by  url"  = function(df) ctab(df %>% filter(donate_2014_c > 0), donate_amf, referrer_url, na.rm = TRUE)    
    , "MIRI donations by first heard"      = function(df) ctab(df %>% filter(donate_2014_c > 0), donate_miri, first_heard_EA, na.rm = TRUE) 
    , "MIRI donations by first heard 2"    = function(df) tab(df %>% filter(donate_2014_c > 0), donate_miri, first_heard_EA, na.rm = TRUE, freq = TRUE, percent = TRUE)    
    , "MIRI donations by first heard"      = function(df) ctab(df %>% filter(donate_2014_c > 0), donate_miri, first_heard_EA, na.rm = TRUE)
    , "summarize donations"           = function(df) var_summary(filter(df, !is.na(donate_2014_c))$donate_2014_c)    
    , "number of >0 donations"        = function(df) sum(df$donate_2014_c > 0, na.rm = TRUE)
    , "donation quantile 1"           = function(df) quantile(df$donate_2014_c, probs = seq(0.1, 1, len = 10), na.rm = TRUE)
    , "donation quantile 2"           = function(df) quantile(df$donate_2014_c, probs = seq(0.91, 1, len = 10), na.rm = TRUE)
    , "donating 10% or over x GWWC"      = function(df) ctab(df, p_donate_2014_c > 0.0999999, member_gwwc)    
    , "summarize donations - fb sample"= function(df) var_summary(filter(df, referrer_url=="No Referrer", !is.na(donate_2014_c))$donate_2014_c)    
    , "summarize donations - student" 	      = function(df) var_summary(filter(df, student == "Yes" & !is.na(student) & !is.na(donate_2014_c))$donate_2014_c)
    , "summarize donations - non-student" 	      = function(df) var_summary(filter(df, student == "No" & student != "NA" & donate_2014_c != "NA")$donate_2014_c)
    , "summarize donations - got involved pre-2013" 	      = function(df) var_summary(filter(df, !((which_year_EA == "2014") | (which_year_EA == "2015")) & donate_2014_c != "NA" & !is.na(which_year_EA))$donate_2014_c)
    , "summarize donations - non-student & Earning to Give" 	      = function(df) var_summary(filter(df, student == "No" & career_path == "Earning to give" & !is.na(career_path) & !is.na(student) & !is.na(donate_2014_c))$donate_2014_c)
    , "summarize % donations"         = function(df) var_summary(df$p_donate_2014_c)
    , "summarize % donations (inc > $10k)"         = function(df) var_summary(filter(df, income_2014_c > 10000 & p_donate_2014_c != "NA")$p_donate_2014_c)    
    , "# people donating 10% or over"    = function(df) sum(df[df$income_2014_c > 10000, "p_donate_2014_c"] > 0.09999999, na.rm = TRUE) #avoid floating point comparison error
    , "donating over 10% x GWWC"      = function(df) ctab(df, p_donate_2014_c > 0.1, member_gwwc)        
    , "% donation quantile 1"         = function(df) quantile(df[df$income_2014_c > 10000, "p_donate_2014_c"], probs = seq(0.1, 1, len = 10), na.rm = TRUE)
    , "% donation quantile 2"         = function(df) quantile(df[df$income_2014_c > 10000, "p_donate_2014_c"], probs = seq(0.91, 1, len = 10), na.rm = TRUE)        
    , "summarize  donations (non-gwwc member, nonstudent, sold_ea, involved-pre-2014)"         = function(df) var_summary(filter(df, income_2014_c > 0, student == "No", !((which_year_EA == "2014") | (which_year_EA == "2015")), sold_ea,  member_gwwc == "Yes",  !is.na(student), !is.na(member_gwwc), !is.na(sold_ea), !is.na(which_year_EA))$donate_2014_c)	
    , "summarize  donations  x member_gwwc (non-student, pre-2014, sold_ea)"         = function(df) ctab(filter(df, income_2014_c > 0, student == "No", !((which_year_EA == "2014") | (which_year_EA == "2015")), sold_ea,  !is.na(student), !is.na(member_gwwc), !is.na(sold_ea), !is.na(which_year_EA)), as.numeric(donate_2014_c), member_gwwc, na.rm=TRUE)
    , "summarize  donations (non-gwwc member, nonstudent, sold_ea)"         = function(df) var_summary(filter(df, income_2014_c > 0, student == "No", sold_ea,  member_gwwc == "Yes",  !is.na(student), !is.na(member_gwwc), !is.na(sold_ea), !is.na(which_year_EA))$donate_2014_c)          
    , "diet"                          = function(df) tab(df, veg)
    , "diet x cause_import_animal_welfare" = function(df) ctab(df, veg, cause_import_animal_welfare, na.rm = TRUE)
    , "why_veg_animals"               = function(df) tab(df, why_veg_animals)
    , "why_veg_health"                = function(df) tab(df, why_veg_health)
    , "why_veg_environment"           = function(df) tab(df, why_veg_environment)
    , "ea career"                     = function(df) tab(df, ea_career)
    , "is_programmer"                 = function(df) tab(df, is_programmer)
    , "subject_economics"             = function(df) tab(df, subject_economics)
    , "subject_engineering"           = function(df) tab(df, subject_engineering)
    , "subject_maths"                 = function(df) tab(df, subject_maths)
    , "subject_medicine"              = function(df) tab(df, subject_medicine)
    , "subject_psychology"            = function(df) tab(df, subject_psychology)
    , "subject_philosophy"            = function(df) tab(df, subject_philosophy)
    , "subject_physics"               = function(df) tab(df, subject_physics)
    , "subject_humanities"            = function(df) tab(df, subject_humanities)
    , "subject_social_science"        = function(df) tab(df, subject_social_science)
    , "subject_sciences"              = function(df) tab(df, subject_sciences)
    , "subject_vocational"            = function(df) tab(df, subject_vocational)
    , "opportunity or obligation"     = function(df) tab(df, ea_opportunity_or_obligation)
    , "act now or later"              = function(df) tab(df, act_now_or_later)
    , "act now or later x age"        = function(df) tab(df, age, act_now_or_later)
    , "moral philosophy"              = function(df) tab(df, moral_philosophy)
    , "confidence in personal EA"     = function(df) tab(df, confident_future_ea_personal)
    , "confidence in EA movement"     = function(df) tab(df, confident_future_ea_movement)
    , "confidence x confidence"       = function(df) ctab(df, confident_future_ea_personal, confident_future_ea_movement, na.rm = TRUE)
    , "confidence x confidence (raw)" = function(df) tab(df, confident_future_ea_personal, confident_future_ea_movement)
    , "EA topic"                      = function(df) tab(df, topic_ea)
    , "EA Welcoming"                  = function(df) tab(df, ea_welcoming)
    , "EA Welcoming x gender"         = function(df) ctab(df, gender, ea_welcoming, na.rm = TRUE)
    , "EA Welcoming x age"            = function(df) ctab(df, age, ea_welcoming, na.rm = TRUE)
    , "EA Welcoming x religion"       = function(df) ctab(df, religion == "Atheist, agnostic or non-religious", ea_welcoming, na.rm = TRUE)
    , "EA Welcoming x ethics"         = function(df) ctab(df, moral_philosophy == "Consequentialism (utilitarian)", ea_welcoming, na.rm = TRUE)
    , "Insecurity about EA"           = function(df) tab(df, insecurity)
    , "opportunity-oblication x inse" = function(df) ctab(df, insecurity, ea_opportunity_or_obligation, na.rm = TRUE)
    , "referrer URL"                  = function(df) tab(df, referrer_url)
    , "referrer self-report"          = function(df) tab(df, referrer_self_report)
    , "referrer x referrer"           = function(df) ctab(df, referrer_url, referrer_self_report)
    , "referrer URL x gender"         = function(df) ctab(df, gender, referrer_url)
    , "referrer URL x age"            = function(df) ctab(df, age, referrer_url)
    , "referrer URL x ethics"         = function(df) ctab(df, moral_philosophy, referrer_url)
    , "referrer self-report x gender" = function(df) ctab(df, gender, referrer_self_report)
    , "referrer self-report x age"    = function(df) ctab(df, age, referrer_self_report)
    , "referrer self-report x ethics" = function(df) ctab(df, moral_philosophy, referrer_self_report)
    , "referrer2"                     = function(df) tab(df, referrer2)
    , "referrer2 x age"               = function(df) ctab(df, age, referrer2, na.rm = TRUE)
    , "referrer2 x donation"          = function(df) ctab(filter(df, student == "No"), donate_2014_c, referrer2, na.rm = TRUE)
    , "referrer2 x income"            = function(df) ctab(filter(df, student == "No"), income_2014_c, referrer2, na.rm = TRUE)
    , "referrer2 x % income donate"   = function(df) ctab(filter(df, student == "No" & income_2014_c > 9999), p_donate_2014_c, referrer2, na.rm = TRUE)    
    , "referrer2 x poverty"           = function(df) ctab(df, cause_import_poverty == "This cause is the top priority", referrer2, na.rm = TRUE)
    , "referrer2 x student"           = function(df) ctab(df, student, referrer2, na.rm = TRUE)
    , "referrer2 x veg"               = function(df) ctab(df, veg == "Vegetarian" | veg == "Vegan", referrer2, na.rm = TRUE)
    , "referrer2 x year got involved" = function(df) ctab(df, which_year_EA, referrer2, na.rm = TRUE)
    , "solid EA x city"               = function(df) ctab(df, solid_ea, city)
    , "Radical Givers"                = function(df) tab(df, radical_giver)
    , "Radical Givers give how much?" = function(df) ctab(df, donate_2014_c, radical_giver)
    , "Radical Givers give II"        = function(df) sum(filter(df, radical_giver)$donate_2014_c)
  )
)
