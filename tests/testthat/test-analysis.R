test_that("both outcomes preserve every queried person and measured zero", {
  expect_equal(nrow(people), 5000L)
  expect_setequal(people$id, profiles$id)
  expect_false(anyDuplicated(people$id) > 0)
  expect_false(anyNA(people))
  expect_equal(sum(people$all_exposures == 0), 858L)
  expect_equal(sum(people$nonspam_exposures == 0), 1411L)
  expect_equal(sum(people$all_exposures > 0 & people$nonspam_exposures == 0), 553L)
  expect_true(all(people$nonspam_exposures <= people$all_exposures))
  expect_equal(totals$respondents, c(5000L, 5000L))
})

test_that("counts represent unique person-breach events", {
  expect_equal(nrow(raw_events) - nrow(distinct(raw_events)), 28L)
  expect_equal(nrow(events), 14951L)
  expect_equal(sum(people$all_exposures), 14951L)
  expect_equal(sum(people$nonspam_exposures), 9304L)
  expect_equal(mean(people$all_exposures > 0), .8284)
  expect_equal(mean(people$nonspam_exposures > 0), .7178)
  expect_equal(mean(people$all_exposures), 2.9902)
  expect_equal(mean(people$nonspam_exposures), 1.8608)
  expect_false(anyDuplicated(events[c("id", "breach_name")]) > 0)
  qualifying_ids <- events$id[events$is_verified & !events$is_spam_list]
  independent_counts <- tabulate(match(qualifying_ids, people$id), nbins = nrow(people))
  expect_equal(people$nonspam_exposures, independent_counts)
})

test_that("labels match source codes and all observed adults have age groups", {
  for (code in 1:8) {
    expected <- c("White", "Black", "Hispanic/Latino", "Asian", "Native American",
                  "Mixed Race", "Other", "Middle Eastern")[[code]]
    expect_true(all(as.character(people$race[profiles$race == code]) == expected))
  }
  expect_equal(sum(people$age == 18), 95L)
  expect_equal(sum(people$age_group == "18 to 25"), 740L)
  expect_true(all(people$age_group[people$age == 18] == "18 to 25"))
  expect_equal(demographics$share[demographics$category == "Female"], .5138)
  expect_equal(demographics$share[demographics$category == "White"], .6446)
})

test_that("CPS comparison matches labels and subtracts unrounded shares", {
  expect_equal(cps_comparison$yougov[cps_comparison$category == "Male"], .4862)
  expect_equal(cps_comparison$yougov[cps_comparison$category == "Female"], .5138)
  expect_equal(cps_comparison$yougov[cps_comparison$category == "18 to 25"], .148)
  expect_equal(cps_comparison$difference, cps_comparison$cps - cps_comparison$yougov)
  expect_true(is.na(cps_comparison$yougov[cps_comparison$category == "Asian alone"]))
})

test_that("reported contrasts agree with independent group summaries", {
  for (outcome in c("all_exposures", "nonspam_exposures")) {
    result <- sex_contrasts[sex_contrasts$outcome == outcome, ]
    expected <- mean(people[[outcome]][people$sex == "Female"]) -
      mean(people[[outcome]][people$sex == "Male"])
    expect_equal(result$estimate, expected)
    expect_lt(result$conf.low, result$estimate)
    expect_gt(result$conf.high, result$estimate)
  }
  expect_true(all(vapply(models, nobs, integer(1)) == 5000L))
  expect_equal(unname(coef(models$sex)[["sexMale"]]),
               -sex_contrasts$estimate[sex_contrasts$outcome == "all_exposures"])
})
