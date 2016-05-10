define("variable_names", function(variable_names) {
  list(
    import = list(file = "data/imsurvey2015-anonymized-renamed-currencied.csv"),
    data   = list(
      "Rename variables" = list(renamer, variable_names) #,
      # Rename variables
      # Make codebook
      # Drop insincere anwsers
      # Drop non-EAs
      # Currency conversion
      # Construct big five index
      # Publish comments
      # Drop comments
    ) #,
#    analyze = list(
      # first heard about EA
      # which_year_EA
      # cause areas
      # social movements
      # cause area X social movements
      # involvement
      # cause area X involvement
      # membership
      # cause area X membership
      # student
      # would join local group
      # local group X student
      # career
      # amount donated
      # income
      # percent donated
      # amount donated X GWWC membership
      # percent donated X GWWC membership
      # amount donated X local group
      # percent donated X local group
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
#    ),
#    export = list(R = "model")  # export data
  )
})
