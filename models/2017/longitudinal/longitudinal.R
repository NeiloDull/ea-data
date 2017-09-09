run("2017/easurvey", to = "data")
df2017 <- df
run("2015/easurvey", to = "data")
df2015 <- df
run("2014", to = "data")
df2014 <- df

ids_in_all_three <- intersect(intersect(df2014$ea_id, df2015$ea_id), df2017$ea_id)
ids_in_2015_and_2017 <- intersect(df2015$ea_id, df2017$ea_id)
