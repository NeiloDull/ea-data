define("variable_names", function(variable_names) {
  list(
    import = list(file = "data/imsurvey2015-anonymized-renamed-currencied.csv"),
    data   = list(
      "Rename variables" = list(renamer, variable_names)
      , "Drop time vars" = list(function(df) { df[!grepl(".time", names(df), fixed = TRUE)] })
      , "Drop insincere" = list(select_rows, function(df) grepl("Yes", df$sincere, fixed = TRUE), whole = TRUE)
      , "Drop non-EA"    = list(select_rows, function(df) df$is_ea == "Yes", whole = TRUE)
      # Construct big five index
      # Publish comments
      , "Drop comments"  = list(function(df) { df[!grepl("comment", names(df), fixed = TRUE)] })
    )

    , analyze = list(
      "first heard about EA"            = function(df) df$heard_ea
      , "involved_local_EA"             = function(df) df$involved_local_ea
      , "would attend"                  = function(df) df$would_attend_local_group
      , "involved_local_EA x GWWC invo" = function(df) table(df$involved_local_ea, df$involved_gwwc)
      , "involved_local_EA x GWWC mem"  = function(df) table(df$involved_local_ea, df$member_gwwc)
      , "involved_GWWC x GWWC mem"      = function(df) table(df$involved_gwwc, df$member_gwwc)
      , "involved_local_EA x local mem" = function(df) table(df$involved_local_ea, df$member_local_group)
      , "member_local x year"           = function(df) table(df$member_local_group, df$which_year_EA)
      , "member_local x GWWC"           = function(df) table(df$member_local_group, df$member_gwwc)
      , "member_local x donate"         = function(df) tapply(df$donated_2014_c, df$member_local_group, mean)
      , "GWWC x donate"                 = function(df) tapply(df$donated_2014_c, df$member_gwwc, mean)
      , "member_local x donate %"       = function(df) tapply(df$donated_2014_c / df$income_2014_c, df$member_local_group, mean)
      , "GWWC x donate %"               = function(df) tapply(df$donated_2014_c / df$income_2014_c, df$member_gwwc, mean)
      , "member_local x student"        = function(df) table(df$member_local_group, df$student)
      , "member_local x welcoming"      = function(df) table(df$member_local_group, df$welcoming)
      , "member_GWWC x welcoming"       = function(df) table(df$member_GWWC, df$welcoming)
      , "member_local x fundraiser"     = function(df) table(df$member_local_group, df$action_fundraiser)
      , "member_GWWC x fundraiser"      = function(df) table(df$member_GWWC, df$action_fundraiser)
      , "member_local x legacy"         = function(df) table(df$member_local_group, df$action_legacy)
      , "member_GWWC x legacy"          = function(df) table(df$member_GWWC, df$action_legacy)
      , "member_local x telling_friends"   = function(df) table(df$member_local_group, df$action_telling_friends)
      , "member_GWWC x telling_friends"    = function(df) table(df$member_GWWC, df$action_telling_friends)
      , "member_local x ea_newsletter"     = function(df) table(df$member_local_group, df$action_ea_newsletter)
      , "member_GWWC x ea_newsletter"      = function(df) table(df$member_GWWC, df$action_ea_newsletter)
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
      # welcoming
      # confidence x welcoming
      # welcoming x gender
      # welcoming x age
      # welcoming x cause area
      # welcoming x involvement
      # insecurity
      # welcoming x insecurity
    )
#    export = list(R = "model")  # export data
  )
})
