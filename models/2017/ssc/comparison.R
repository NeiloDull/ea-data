run("2017/easurvey", to = "data")
easurvey <- filter(df, referrer3 == "SSC")
run("2017/ssc", to = "data")
ssc <- df
# TODO: Figure out better statistical testing
print(t.test(na.rm(easurvey$gender_b) == "Male", na.rm(ssc$Sex) == "Male"))
print(ks.test(na.rm(easurvey$age), na.rm(ssc$Age)))
print(t.test(na.rm(easurvey$student) == "Yes", na.rm(ssc$WorkStatus) == "Student"))
print(t.test(na.rm(easurvey$country) == "United States", na.rm(ssc$Country) == "United States"))
print(t.test(na.rm(easurvey$race_white) == "Yes", na.rm(ssc$Race) == "White (non-Hispanic)"))
print(t.test(grepl("Consequentialism", na.rm(easurvey$moral_philosophy)), grepl("consequentialism", na.rm(ssc$MoralViews))))
print(t.test(na.rm(easurvey$member_gwwc) == "Yes", swap_by_value(ssc, "GWWC", list(" " = NA))$GWWC == "Yes"))
print(ks.test(filter(easurvey, student == "No")$donate_2016_c, filter(ssc, WorkStatus != "Student")$Charity))
print(ks.test(filter(easurvey, student == "No")$income_2016_individual_c, filter(ssc, WorkStatus != "Student")$Income))

