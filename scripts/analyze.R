library(purrr)

outcomes <- people |>
  pivot_longer(ends_with("_exposures"), names_to = "outcome", values_to = "exposures")

totals <- outcomes |>
  summarise(
    respondents = n(), records = sum(exposures),
    exposed = sum(exposures > 0), share_exposed = mean(exposures > 0),
    mean = mean(exposures), median = median(exposures),
    .by = outcome
  )

group_stats <- map_dfr(c("age_group", "education", "sex", "race"), \(variable) {
  outcomes |>
    group_by(outcome, category = .data[[variable]]) |>
    summarise(respondents = n(), mean = mean(exposures),
              se = sd(exposures) / sqrt(n()), .groups = "drop") |>
    mutate(category = as.character(category), demographic = variable, .before = category)
})

demographics <- group_stats |>
  filter(outcome == "all_exposures") |>
  transmute(demographic, category, respondents, share = respondents / nrow(people))

models <- list(
  race = lm(all_exposures ~ race, data = people),
  sex = lm(all_exposures ~ sex, data = people),
  educ = lm(all_exposures ~ education, data = people),
  age = lm(all_exposures ~ splines::ns(age, 2), data = people)
)
coefficients <- imap_dfr(models, \(model, variable) {
  bind_rows(
    broom::tidy(model, conf.int = TRUE) |> mutate(inference = "OLS"),
    broom::tidy(lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC3")),
                conf.int = TRUE) |> mutate(inference = "HC3")
  ) |>
    mutate(model = variable, .before = term)
})

sex_contrasts <- map_dfr(c("all_exposures", "nonspam_exposures"), \(outcome) {
  broom::tidy(t.test(
    people[[outcome]][people$sex == "Female"],
    people[[outcome]][people$sex == "Male"]
  )) |>
    mutate(outcome = outcome, contrast = "Female minus male", .before = estimate)
})

domain_counts <- events |>
  filter(!is.na(domain), domain != "") |>
  count(domain, sort = TRUE)

cps <- read_csv("data/cps_2018.csv", show_col_types = FALSE) |>
  select(category = Var, cps = proportion) |>
  filter(category != "Totals")
cps_labels <- tibble::tribble(
  ~category, ~cps_category,
  "66 to 100", "66 to 80+",
  "White", "White alone",
  "Black", "Black or African American alone",
  "Native American", "American Indian and Alaska Native alone",
  "No HS", "No high school diploma",
  "HS Grad.", "High school or equivalent",
  "Some College", "Some college, less than 4-yr degree",
  "2-year College Degree", "Some college, less than 4-yr degree",
  "4-year College Degree", "Bachelor's degree or higher",
  "Postgrad Degree", "Bachelor's degree or higher"
)
cps_shares <- demographics |>
  left_join(cps_labels, by = "category", relationship = "many-to-one") |>
  mutate(category = coalesce(cps_category, category)) |>
  summarise(yougov = sum(share), .by = category)
cps_comparison <- cps |>
  left_join(cps_shares, by = "category", relationship = "one-to-one") |>
  mutate(difference = cps - yougov)
