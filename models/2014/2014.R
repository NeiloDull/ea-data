define("transform_lookups", function(transform_lookup) {
  list(
    import = list(file = "data/2014/imsurvey2014-anonymized.csv")
    , data = list(
      "Rename variables sensibly"       = function(df) {
        df <- plyr::rename(df, transform_lookup$renames)
        if (length(setdiff(transform_lookup$renames, names(df))) > 0) {
          stop(paste0(setdiff(transform_lookup$renames, names(df)), collapse = ", "),
               " variables were not created!")
        }
        df <- df[, transform_lookup$renames]
        df
      }
      ,"Make age numeric"               = function(df) { df$age %<>% as.numeric; df }
      ,"Unlist ids"                     = function(df) { df$id %<>% unlist; df }
      ,"Drop NA ids"                    = function(df) { df[!is.na(df$id), ] }
      ,"Clean values in meta-ethics"    = function(df) { swap_by_value(df, "metaethics", list("Consequentialist/utilitarian" = "consequentialist", "Deontology" = "deontology", "Virtue ethics" = "virtue", "Other" = "other", "No opinion, or not familiar with these terms" = "other")) }
      ,"Make reducitarian variable"     = function(df) { df$reducetarian <- ifelse(df$diet == 'Meat-eating', FALSE, TRUE); df }
      ,'Make veg binary variable'       = function(df) { df$veg <- ifelse(df$diet == "Vegetarian", TRUE, ifelse(df$diet == "Vegan", TRUE, FALSE)); df }
      ,"Clean up atheist response"      = function(df) { swap_by_value(df, "religion", list('Atheist, agnostic or non-religious' = 'atheist')) }
      ,"Clean up group responses"       = function(df) { swap_by_value(df, 'group', transform_lookup$clean_group) }
      ,"Impute TLYCS factors"           = function(df) { swap_multiple_ids(df, 'involved_TLYCS', c(13, 31, 79, 110, 146, 367, 374, 383, 534, 577), 'Yes') }
      ,"Impute online factors"          = function(df) { swap_multiple_ids(df, 'factors_online', c(361, 374, 606), 'Yes') }
      ,"Consolidate sublocation"        = function(df) { swap_by_value(df, "city", transform_lookup$clean_city) }
      ,"Clean up career responses"      = function(df) { swap_by_value(df, "career", list("Direct charity/non-profit work" = "Direct", "Earning to Give" = "ETG", "None of these" =  "None", "Other" = "None")) }
      ,"Number of charities donated"    = function(df) {
          charity_vars <- head(get_vars(df, "donate"), -1)
          df$charity_count <- count_vars(df, charity_vars, response = "Yes", vectorize = TRUE)
          df
      }
      ,"Impute career responses"        = function(df) swap_by_ids(df, "career", transform_lookup$career_transform)
      ,"Impute donate responses"        = function(df) swap_by_ids(df, "donate_2013_c", transform_lookup$donate_transform)
      ,"Impute income responses"        = function(df) swap_by_ids(df, "income_2013_c", transform_lookup$income_transform)
      ,"Make income numeric"            = function(df) { df$income_2013_c %<>% as.numeric; df }
      ,"Make donations numeric"         = function(df) { df$donate_2013_c %<>% as.numeric; df }
      ,"Make p_inc_donate"              = function(df) {
          p <- df$donate_2013_c / df$income_2013_c * 100
          p <- ifelse(is.infinite(p) | p == "NaN", NA, p)
          df$p_donate_2013_c <- p
          df
      }
      ,"Earning to give?"               = function(df) { df$is.ETG <- ifelse(df$income_2013_c >= 60000 & df$p_donate_2013_c >= 0.1, TRUE, FALSE); df }
      ,"Trim referrer_url"              = function(df) {
        r <- df$referrer_url
        r[grepl('\\?', r)] <- sapply(strsplit(r[grepl('\\?', r)], '\\?'), '[', 2); r[nchar(r) > 12] <- ""
        r[substring(r, 1, 1) == 't' & !is.na(r)] <- "t"
        df$referrer_url <- r
        df
 }
      ,"Clean referrer_url"             = function(df) { swap_by_value(df, "referrer_url", transform_lookup$referrer) }
      ,"Impute ACE as a referrer"       = function(df) { df[grepl("A", df$id), "referrer_url"] <- "ACE"; df }
      ,"In random FB sample?"           = function(df) { df$in_random_fb_sample <- df$id %in% transform_lookup$random_sample_ids; df }
    )
  )
})
