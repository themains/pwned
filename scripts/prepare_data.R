library(dplyr)
library(tidyr)
library(readr)

profiles <- read_csv("data/YGOV1058_profile.csv", show_col_types = FALSE)
raw_events <- read_csv("data/YGOV1058_pwned.csv", show_col_types = FALSE)
stopifnot(
  nrow(profiles) == n_distinct(profiles$id),
  !anyNA(profiles$id),
  all(raw_events$id %in% profiles$id),
  !anyNA(raw_events[c("id", "Name", "IsVerified", "IsSpamList")])
)

# One email was queried per person. Remove only identical source records.
events <- raw_events |>
  distinct() |>
  rename(
    breach_name = Name, domain = Domain,
    is_verified = IsVerified, is_spam_list = IsSpamList
  ) |>
  select(id, breach_name, domain, is_verified, is_spam_list)
stopifnot(!anyDuplicated(events[c("id", "breach_name")]))

event_counts <- events |>
  summarise(
    all_exposures = n(),
    nonspam_exposures = sum(is_verified & !is_spam_list),
    .by = id
  )

people <- profiles |>
  left_join(event_counts, by = "id", relationship = "one-to-one") |>
  mutate(
    across(c(all_exposures, nonspam_exposures), ~ replace_na(.x, 0L)),
    age = 2018 - birthyr,
    age_group = cut(
      age, breaks = c(18, 25, 35, 50, 65, 100), include.lowest = TRUE,
      labels = c("18 to 25", "26 to 35", "36 to 50", "51 to 65", "66 to 100")
    ),
    education = factor(educ, levels = 1:6, labels = c(
      "No HS", "HS Grad.", "Some College", "2-year College Degree",
      "4-year College Degree", "Postgrad Degree"
    )),
    race = factor(race, levels = 1:8, labels = c(
      "White", "Black", "Hispanic/Latino", "Asian", "Native American",
      "Mixed Race", "Other", "Middle Eastern"
    )),
    sex = factor(gender, levels = c(2, 1), labels = c("Female", "Male"))
  ) |>
  select(id, age, age_group, education, race, sex, all_exposures, nonspam_exposures)

stopifnot(
  nrow(people) == nrow(profiles),
  !anyNA(people),
  sum(people$all_exposures) == nrow(events),
  sum(people$nonspam_exposures) == sum(events$is_verified & !events$is_spam_list)
)
