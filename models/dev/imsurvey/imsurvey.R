define("variable_names", function(variable_names) {
  list(
    #TODO: Longitudinal study
    import = list(file = "data/imsurvey2015-anonymized-renamed-currencied.csv"),
    data   = list(
      "Rename variables" = list(renamer, variable_names)
      , "Drop time vars" = list(function(df) { df[!grepl(".time", names(df), fixed = TRUE)] })
      , "Drop insincere" = list(select_rows, function(df) grepl("Yes", df$sincere, fixed = TRUE), whole = TRUE)
      , "Drop non-EA"    = list(select_rows, function(df) df$is_ea == "Yes", whole = TRUE)
      # TODO: Publish comments
      , "Drop comments"  = list(function(df) { df[!grepl("comment", names(df), fixed = TRUE)] })
      , "Have a plan"    = list(new_variable, function(plan_donate_how_much, already_stated_plan, donate_2014) {
        plan_donate_how_much != "" | already_stated_plan == "Yes" | !is.na(donate_2014)
      }, "have_donation_plan")
      , "Sold EA"        = list(new_variable, function(have_donation_plan, ea_career) {
        have_donation_plan | ea_career == "Yes"
      }, "sold_ea")
      , "% inc donate"   = list(new_variable, function(donate_2014_c, income_2014_c) {
        p <- donate_2014_c / income_2014_c
        p[is.infinite(p)] <- NA
        p
      }, "p_donate_2014_c")
    )

    , analyze = list(
      "first heard about EA"            = function(df) table(df$heard_ea)
      , "involved_local_EA"             = function(df) table(df$involved_local_EA)
      , "would attend"                  = function(df) table(df$would_attend_local_group)
      , "involved_local_EA x GWWC invo" = function(df) comparison_table(df, "involved_local_EA", "involved_GWWC", type = "categorical")
      , "involved_local_EA x GWWC mem"  = function(df) comparison_table(df, "involved_local_EA", "member_gwwc", type = "categorical")
      , "involved_GWWC x GWWC mem"      = function(df) comparison_table(df, "involved_GWWC", "member_gwwc", type = "categorical")
      , "involved_local_EA x local mem" = function(df) comparison_table(df, "involved_local_EA", "member_local_group", type = "categorical")
      , "member_local x year"           = function(df) comparison_table(df, "member_local_group", "which_year_EA", type = "categorical")
      , "member_local x GWWC"           = function(df) comparison_table(df, "member_local_group", "member_gwwc", type = "categorical")
      , "member_local x donate"         = function(df) comparison_table(df, "donate_2014_c", "member_local_group", type = "continuous")
      , "GWWC x donate"                 = function(df) comparison_table(df, "donate_2014_c", "member_gwwc", type = "continuous")
      , "member_local x donate %"       = function(df) comparison_table(df, "p_donate_2014_c", "member_local_group", type = "continuous")
      , "GWWC x donate %"               = function(df) comparison_table(df, "p_donate_2014_c", "member_gwwc", type = "continuous")
      , "member_local x student"        = function(df) comparison_table(df, "member_local_group", "student", type = "categorical")
      , "member_local x welcoming"      = function(df) comparison_table(df, "member_local_group", "ea_welcoming", type = "categorical")
      , "member_gwwc x welcoming"       = function(df) comparison_table(df, "member_gwwc", "ea_welcoming", type = "categorical")
      , "member_local x fundraiser"     = function(df) comparison_table(df, "member_local_group", "action_fundraiser", type = "categorical")
      , "member_gwwc x fundraiser"      = function(df) comparison_table(df, "member_gwwc", "action_fundraiser", type = "categorical")
      , "member_local x legacy"         = function(df) comparison_table(df, "member_local_group", "action_legacy", type = "categorical")
      , "member_gwwc x legacy"          = function(df) comparison_table(df, "member_gwwc", "action_legacy", type = "categorical")
      , "member_local x telling_friends"   = function(df) comparison_table(df, "member_local_group", "action_telling_friends", type = "categorical")
      , "member_gwwc x telling_friends"    = function(df) comparison_table(df, "member_gwwc", "action_telling_friends", type = "categorical")
      , "member_local x ea_newsletter"     = function(df) comparison_table(df, "member_local_group", "action_ea_newsletter", type = "categorical")
      , "member_gwwc x ea_newsletter"      = function(df) comparison_table(df, "member_gwwc", "action_ea_newsletter", type = "categorical")
      # cause areas
      # social movements
      # cause area X social movements
      # involvement
      # cause area X involvement
      # membership
      # cause area X membership
      # student
      # career
      # amount donated
      # income
      # percent donated
      # cause area X amount donated
      # where donated
      # cause area X where donated
      # where donated X GWWC membership
      # occupation
      # subject
      # ea career
      # career path (of ea careers)
      # ea career x saying 80K was influential
      # amount donated x ETG
      # percent donated x ETG
      # diet
      # diet reasons
      # diet x animal welfare support
      # diet reasons x animal welfare support
      # age
      # gender
      # country
      # city
      # religion
      # politics
      # confidence in personal EA
      # confidence in EA movement
      # confidence x confidence
      # ea_welcoming
      # confidence x ea_welcoming
      # ea_welcoming x gender
      # ea_welcoming x age
      # ea_welcoming x cause area
      # ea_welcoming x involvement
      # insecurity
      # ea_welcoming x insecurity
    )
  )
})
