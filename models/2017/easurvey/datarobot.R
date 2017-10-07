# Connect to DataRobot
tryCatch(ConnectToDataRobot(configPath = "~/.config/datarobot/drconfig.yaml"),
         error = function(e) {
           stop("This script cannot be run unless you have a DataRobot license and your ",
                "DataRobot API configuration file set up. (Unfortunately, DataRobot licenses ",
                "are not available without paying money or being a DataRobot employee.)")
         })

# Get data out of EA Survey Syberia (and hack around active binding)
run("2017/easurvey", to = "data")
data <- df

# Some additional munging
data$ea_welcoming_b <- ifelse(is.na(data$ea_welcoming), NA,
                              data$ea_welcoming %in% c("Welcoming", "Very welcoming"))
data$ea_social_b <- ifelse(is.na(data$ea_social), NA,
                           data$ea_social %in% c("41-60%", "61-80%", "81-90%", "90-100%"))
data$ea_nps_promoter <- ifelse(is.na(data$ea_nps), NA,
                               data$ea_nps %in% c("10 (Completely likely)", "9", "8"))
data$ea_nps_detractor <- ifelse(is.na(data$ea_nps), NA,
                                data$ea_nps %in% c("6", "5", "4", "3", "2",
                                                   "1", "0 (Not at all likely)"))
data <- swap_by_value(data, "act_now_or_later", list("Other" = NA))
data <- swap_by_value(data, "ea_opportunity_or_obligation",
                      list("Moral Duty" = "Obligation", "Other" = NA))
data <- swap_by_value(data, "topic_ea", list("None of the Above/Other" = NA))

# Define variables
targets <- c("ea_donate", "ea_welcoming_b", "ea_career", "member_gwwc",
            "ea_nps_promoter", "ea_nps_detractor")
features <- c(get_vars(data, "_comment"),
              get_vars(data, "cause_import.+_b"),
              "left",
              "veg_b",
              "gender_b",
              "num_top_cause_priorities",
              "referrer3",
              "age",
              "income_2015_individual_c",
              "income_2016_individual_c",
              "ea_social_b",
              "metaethics",
              "moral_philosophy",
              "topic_ea",
              "ea_opportunity_or_obligation",
              get_vars(data, "race"),
              "religion",
              "city",
              "country",
              get_vars(data, "studied"),
              setdiff(get_vars(data, "member"), "member_gwwc"),
              "which_year_EA",
              get_vars(data, "involved"),
              "first_heard_EA",
              get_vars(data, "movement"),
              "ea_nps_promoter",
              "ea_nps_detractor")

# Check definitions
mismatches <- setdiff(c(targets, features), names(data))
if (length(mismatches) > 0) {
  stop("Some variables aren't in the data: ", paste0(mismatches, collapse = ", "))
}

for (target in targets) {
  message("Running ", target, "...")
  # Remove variables that are too entangled with targets
  if ((target %in% c("ea_welcoming_b", "ea_nps_promoter", "ea_nps_detractor"))) {
    drops <- c("ea_welcoming", "ea_welcoming_comment", "ea_nps_promoter", "ea_nps_detractor",
              "member_local_group")
  }
  else if (identical(target, "ea_career")) {
    drops <- c("ea_social_b", "ea_career_comment")
  }
  else if (identical(target, "member_gwwc")) {
    drops <- "involved_gwwc"
  }

  # Subset to features
  data2 <- data[, setdiff(c(features, target), drops)]

  # Make DataRobot model
  dr_project <- SetupProject(data2, projectName = paste0("EASurvey-", target))
  SetTarget(dr_project, target, mode = AutopilotMode$Quick)
  UpdateProject(dr_project, workerCount = 10)
  WaitForAutopilot(dr_project)
}
