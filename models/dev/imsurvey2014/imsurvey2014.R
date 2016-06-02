define("transform_lookups", "transform_functions", function(transform_lookup, transform_functions) {
  list(
    import = list(
      multi = list(
        pieces = c("imdata_base", "imdata_more", "imdata_ace"),
        with = resource_name,
        bind = TRUE
      )
    ),
    data = list(
      "Rename variables sensibly"       = list(renamer, transform_lookup$renames)
      ,"Select only relevant variables" = list(select_variables, transform_lookup$desired_variables)
      ,"Make age numeric"               = list(column_transformation(as.numeric), 'age')
      ,"Unlist ids"                     = list(replace_variable, function(id) unlist(id))
      ,"Drop NA ids"                    = list(select_rows, function(dataframe) { !is.na(dataframe$id) }, whole = TRUE) 
      ,"Clean values in meta-ethics"    = list(value_replacer, 'metaethics', list(list('Consequentialist/utilitarian', 'consequentialist'), list('Deontology', 'deontology'), list('Virtue ethics', 'virtue'), list('Other', 'other'), list('No opinion, or not familiar with these terms', 'other')))
      ,"Make reducitarian variable"     = list(new_variable, function(diet) ifelse(diet == 'Meat-eating', FALSE, TRUE), 'reducitarian')
      ,"Make veg binary variable"       = list(new_variable, function(diet) ifelse(diet == 'Vegetarian', TRUE, ifelse(diet == 'Vegan', TRUE, FALSE)), 'veg')
      ,"Clean up atheist response"      = list(value_replacer, 'religion', list(list('Atheist, agnostic or non-religious', 'atheist')))
      ,"Clean up group responses"       = list(value_replacer, 'group', transform_lookup$clean_group)
      ,"Impute TLYCS factors"           = list(impute, surveytools2::swap_multiple_ids, 'factors_TLYCS', c(13, 31, 79, 110, 146, 367, 374, 383, 534, 577), 'Yes')
      ,"Impute GiveWell factors"        = list(evaldf, quote(dataframe[[dataframe$id == 271, 'factors_givewell']] <- 'Yes'))
      ,"Impute online factors"          = list(impute, surveytools2::swap_multiple_ids, 'factors_online', c(361, 374, 606), 'Yes')
      ,"Consolidate sublovation"        = list(value_replacer, transform_lookup$clean_city)
      ,'Clean up career responses'      = list(value_replacer, list(list('Direct charity/non-profit work', 'Direct'), list('Earning to Give', 'ETG'), list('None of these', 'None'), list('Research', 'Research'), list('Other', 'None')))
      ,'Number of charities donated'    = list(charity_count, transform_lookup$charity_vars)
      ,'Impute career responses'        = list(swap_by_ids, 'career', transform_lookup$career_transform)
      ,'Impute donate responses'        = list(swap_by_ids, 'donate2013', transform_lookup$donate_transform)
      ,'Impute income responses'        = list(swap_by_ids, 'income2013', transform_lookup$income_transform)
      ,'Make p_inc_donate'              = list(new_variable, transform_functions$make_p_inc_donate, 'p_inc_donate')
      ,"Make income numeric"            = list(column_transformation(as.numeric), 'income2013')
      ,"Make donations numeric"         = list(column_transformation(as.numeric), 'donate2013')
      ,"Make p_inc_donate numeric"      = list(column_transformation(as.numeric), 'p_inc_donate')
      ,"Earning to give?"               = list(new_variable, function(income2013, p_inc_donate) { ifelse(income2013 >= 60000 & p_inc_donate >= 0.1, TRUE, FALSE) }, 'is.ETG')
      ,"Trim referrer"                  = list(replace_variable, function(referrer) { r <- referrer; r[grepl('\\?', r)] <- sapply(strsplit(r[grepl('\\?', r)], '\\?'), '[', 2); r[nchar(r) > 12] <- ""; r[substring(r, 1, 1) == 't' & !is.na(r)] <- "t"; r })
      ,'Clean referrer'                 = list(value_replacer, 'referrer', transform_lookup$referrer)
      ,"Trim referrer2"                 = list(replace_variable, function(referrer2) { referrer2[nchar(referrer2) < 5] <- ""; referrer2 })
      ,'Clean referrer2'                = list(value_replacer, 'referrer', transform_lookup$referrer2)
      ,'Impute ACE as a referrer'       = list(evaldf, quote({ dataframe[grepl('A', dataframe$id), 'referrer'] <- 'ACE'; dataframe[grepl('A', dataframe$id), 'referrer2'] <- 'ACE' }))
      ,'In random FB sample?'           = list(new_variable, function(id) id %in% transform_lookup$random_sample_ids, 'in_random_fb_sample')
    ),
    export = list(R = list('imdata', .type = 'data'))
  )
})
