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
      "first heard about EA"            = function(df) tab(df, first_heard_EA)
      , "first heard through local grp" = function(df) tab(df, first_heard_EA == "Local EA group")
      , "first heard local x GWWC mem"  = function(df) ctab(df, member_gwwc, first_heard_EA == "Local EA group")
      , "first heard local x donate"    = function(df) ctab(df, donate_2014_c, first_heard_EA == "Local EA group")
      , "involved_local_EA"             = function(df) tab(df, involved_local_EA)
      , "would attend"                  = function(df) tab(df, would_attend_local_group)
      , "involved_local_EA x GWWC invo" = function(df) ctab(df, involved_local_EA, involved_GWWC)
      , "involved_local_EA x GWWC mem"  = function(df) ctab(df, involved_local_EA, member_gwwc)
      , "involved_GWWC x GWWC mem"      = function(df) ctab(df, involved_GWWC, member_gwwc)
      , "involved_local_EA x donate"    = function(df) ctab(df, donate_2014_c, involved_local_EA)
      , "member_local"                  = function(df) tab(df, member_local_group)
      , "member_local x GWWC"           = function(df) ctab(df, member_local_group, member_gwwc)
      , "involved_local_EA x local mem" = function(df) ctab(df, involved_local_EA, member_local_group)
      , "member_local x year"           = function(df) ctab(df, member_local_group, which_year_EA)
      , "member_local x donate"         = function(df) ctab(df, donate_2014_c, member_local_group)
      , "GWWC x donate"                 = function(df) ctab(df, donate_2014_c, member_gwwc)
      , "member_local x donate %"       = function(df) ctab(df, p_donate_2014_c, member_local_group)
      , "GWWC x donate %"               = function(df) ctab(df, p_donate_2014_c, member_gwwc)
      , "member_local x student"        = function(df) ctab(df, member_local_group, student)
      , "member_local x welcoming"      = function(df) ctab(df, member_local_group, ea_welcoming)
      , "member_gwwc x welcoming"       = function(df) ctab(df, member_gwwc, ea_welcoming)
      , "member_local x fundraiser"     = function(df) ctab(df, member_local_group, action_fundraiser)
      , "member_gwwc x fundraiser"      = function(df) ctab(df, member_gwwc, action_fundraiser)
      , "member_local x legacy"         = function(df) ctab(df, member_local_group, action_legacy)
      , "member_gwwc x legacy"          = function(df) ctab(df, member_gwwc, action_legacy)
      , "member_local x telling_friends"   = function(df) ctab(df, member_local_group, action_telling_friends)
      , "member_gwwc x telling_friends"    = function(df) ctab(df, member_gwwc, action_telling_friends)
      , "member_local x ea_newsletter"     = function(df) ctab(df, member_local_group, action_ea_newsletter)
      , "member_gwwc x ea_newsletter"      = function(df) ctab(df, member_gwwc, action_ea_newsletter)
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
