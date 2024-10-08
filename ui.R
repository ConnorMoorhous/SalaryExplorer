htmlTemplate("index.html",
  content = uiOutput("content"),
  dateSelect = dateRangeInput("dateRange",
                                "Date",
                                min = min(dat$date),
                                max = max(dat$date),
                                start = min(dat$date),
                                end = max(dat$date)
                                ),
  titleSelect = pickerInput("title",
                      "Title",
                      choices = unique(dat$title),
                      selected = unique(dat$title),
                      options = list(`actions-box` = T,
                                    `selected-text-format` = "count > 2",
                                    `count-selected-text` = "{0} Titles"
                      ),
                      multiple = TRUE),
  educationSelect = pickerInput("education",
                      "Education",
                      choices = unique(dat$education_level),
                      selected = unique(dat$education_level),
                      options = list(`actions-box` = T,
                                    `selected-text-format` = "count > 1",
                                    `count-selected-text` = "{0} Education Levels"
                      ),
                      multiple = TRUE),
  genderSelect = pickerInput("gender",
                      "Gender",
                      choices = unique(dat$gender),
                      selected = unique(dat$gender),
                      options = list(`actions-box` = T,
                                    `selected-text-format` = "count > 1",
                                    `count-selected-text` = "{0} Genders"
                      ),
                      multiple = TRUE),
  raceSelect = pickerInput("race",
                      "Race",
                      choices = unique(dat$race),
                      selected = unique(dat$race),
                      options = list(`actions-box` = T,
                                    `selected-text-format` = "count > 1",
                                    `count-selected-text` = "{0} Races"
                      ),
                      multiple = TRUE)
)