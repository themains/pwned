library(ggplot2)
library(knitr)

options(knitr.kable.NA = "")
demographic_labels <- c(age_group = "Age", education = "Education", sex = "Sex", race = "Race")
table_specs <- tibble::tribble(
  ~outcome, ~file, ~label, ~caption,
  "all_exposures", "tab2_freq_se_by_group", "socdem_dat",
  "Recorded Exposures by Socioeconomic Factors",
  "nonspam_exposures", "tab4_freq_se_by_validate_group", "socdem_verified_dat",
  "Verified, Nonspam Exposures by Socioeconomic Factors"
)
for (i in seq_len(nrow(table_specs))) {
  table_data <- map_dfr(names(demographic_labels), \(variable) {
    bind_rows(
      tibble::tibble(category = demographic_labels[[variable]], mean = NA_real_, se = NA_real_),
      group_stats |>
        filter(outcome == table_specs$outcome[[i]], demographic == variable) |>
        select(category, mean, se)
    )
  })
  writeLines(kable(
    table_data, format = "latex", booktabs = TRUE, digits = 2,
    col.names = c("", "Mean", "SE"), caption = table_specs$caption[[i]],
    label = table_specs$label[[i]]
  ), paste0("tabs/", table_specs$file[[i]], ".tex"))
}

sample_table <- map_dfr(names(demographic_labels), \(variable) {
  bind_rows(
    tibble::tibble(category = demographic_labels[[variable]], share = NA_real_),
    demographics |> filter(demographic == variable) |> select(category, share)
  )
})
writeLines(kable(sample_table, format = "latex", booktabs = TRUE, digits = 2,
                 col.names = c("", "Proportion"), caption = "YouGov Sample Characteristics",
                 label = "yg_dat"), "tabs/sample.tex")
writeLines(kable(domain_counts |> filter(n > 100), format = "latex", booktabs = TRUE,
                 col.names = c("Domain", "Records"), caption = "Most Frequently Implicated Domains",
                 label = "domain_dat"), "tabs/domains.tex")
writeLines(kable(cps_comparison, format = "latex", booktabs = TRUE, digits = 2,
                 col.names = c("", "CPS", "YouGov", "Difference"),
                 caption = "Comparison Between YouGov and CPS 2018", label = "cps_yg"),
           "tabs/tabsi_yg_cps.tex")

model_labels <- c(race = "Race/Ethnicity", sex = "Sex", educ = "Education", age = "Age")
for (variable in names(models)) {
  model_table <- coefficients |>
    filter(model == variable, inference == "OLS") |>
    mutate(term = sub("^(race|sex|education)", "", term),
           term = sub("splines::ns(age, 2)", "Age spline ", term, fixed = TRUE),
           p.value = trimws(format.pval(p.value, digits = 3, eps = .001))) |>
    select(term, estimate, std.error, conf.low, conf.high, p.value)
  writeLines(kable(
    model_table, format = "latex", booktabs = TRUE, digits = 3,
    col.names = c("Term", "Estimate", "SE", "Lower", "Upper", "p"),
    caption = paste("Recorded Exposures by", model_labels[[variable]]),
    label = paste0(variable, "_breaches")
  ), paste0("tabs/", variable, "_pwned.tex"))
}

plot_theme <- theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "#e1e1e1", linetype = "dotted"),
    panel.grid.minor = element_blank(), legend.position = "bottom",
    legend.key = element_blank(), legend.key.width = unit(1, "cm"),
    axis.title = element_text(size = 10, color = "#555555"),
    axis.text = element_text(size = 10, color = "#555555"),
    axis.title.x = element_text(vjust = 1, margin = margin(10, 0, 0, 0)),
    axis.title.y = element_text(vjust = 1),
    axis.ticks = element_line(color = "#e1e1e1", linetype = "dotted", linewidth = .2),
    axis.text.x = element_text(vjust = .3), plot.margin = unit(c(.5, .75, .5, .5), "cm")
  )
presentation_theme <- plot_theme +
  theme(
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black", colour = "#bcbcbc", linewidth = .2),
    panel.grid.major = element_line(color = "#bcbcbc", linetype = "dotted", linewidth = .2),
    axis.title = element_text(size = 10, color = "#dadada"),
    axis.text = element_text(size = 10, color = "#cacaca"),
    axis.ticks = element_line(color = "#c1c1c1", linetype = "dotted", linewidth = .2)
  )

for (variable in c("age", "education", "race", "sex")) {
  for (presentation in c(FALSE, TRUE)) {
    point_color <- if (presentation) "#fdbc00" else "#777777"
    interval_color <- if (presentation) "#fecd00" else "#A7A7A7"
    if (variable == "age") {
      plot <- ggplot(people, aes(age, all_exposures)) +
        geom_point(alpha = .05, color = if (presentation) "#bcbcbc" else "black") +
        geom_smooth(method = "loess", formula = y ~ x,
                    color = if (presentation) "#fdbc00" else "#3366FF") +
        scale_x_continuous("Age", limits = c(18, 100), breaks = seq(20, 100, 10)) +
        ylab("Number of Recorded Exposures")
    } else {
      plot_data <- group_stats |>
        filter(outcome == "all_exposures", demographic == variable) |>
        mutate(category = factor(category, levels = levels(people[[variable]])))
      plot <- ggplot(plot_data, aes(category, mean)) +
        geom_point(color = point_color) +
        geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                      width = .03, color = interval_color,
                      linetype = if (presentation) "solid" else "dotted") +
        labs(x = "", y = "Average Number of Recorded Exposures") +
        coord_flip()
    }
    plot <- plot + if (presentation) presentation_theme else plot_theme
    stem <- paste0("figs/", if (variable == "education") "educ" else variable, "_pwned")
    if (presentation) {
      ggsave(paste0(stem, "_present.pdf"), plot,
             width = if (variable == "age") 3.5 else 4.5,
             height = if (variable == "age") 3.5 else 3.9)
    } else {
      ggsave(paste0(stem, ".pdf"), plot, width = 7, height = 7)
      ggsave(paste0(stem, ".png"), plot, width = 7, height = 7)
    }
  }
}

catalog <- jsonlite::fromJSON("data/breaches.json")
all_totals <- totals |> filter(outcome == "all_exposures")
nonspam_totals <- totals |> filter(outcome == "nonspam_exposures")
all_means <- group_stats |> filter(outcome == "all_exposures")
nonspam_means <- group_stats |> filter(outcome == "nonspam_exposures")
numbers <- c(
  sampleSize = nrow(people), eventCount = nrow(events),
  duplicateCount = nrow(raw_events) - nrow(events),
  exposedPercent = sprintf("%.2f", 100 * all_totals$share_exposed),
  meanExposures = sprintf("%.2f", all_totals$mean), medianExposures = all_totals$median,
  nonspamCount = nonspam_totals$records,
  nonspamPercent = sprintf("%.2f", 100 * nonspam_totals$share_exposed),
  nonspamMean = sprintf("%.2f", nonspam_totals$mean),
  verifiedPercent = sprintf("%.0f", 100 * mean(events$is_verified)),
  spamPercent = sprintf("%.2f", 100 * mean(events$is_spam_list)),
  domainCount = nrow(domain_counts), breachCount = n_distinct(events$breach_name),
  missingDomainCount = sum(is.na(events$domain) | events$domain == ""),
  topDomainCount = sum(domain_counts$n > 100),
  topDomainEvents = sum(domain_counts$n[domain_counts$n > 100]),
  catalogCount = nrow(catalog), catalogDomains = n_distinct(catalog$Domain),
  catalogAccounts = format(sum(catalog$PwnCount), big.mark = ",", scientific = FALSE),
  femaleMaleRatio = sprintf(
    "%.2f", mean(people$all_exposures[people$sex == "Female"]) /
      mean(people$all_exposures[people$sex == "Male"])
  )
)
for (category in c("No HS", "Postgrad Degree", "Black", "White", "Hispanic/Latino", "Asian",
                   "Female", "Male")) {
  macro <- c("No HS" = "noHighSchool", "Postgrad Degree" = "postgraduate", Black = "black",
             White = "white", "Hispanic/Latino" = "hispanic", Asian = "asian",
             Female = "female", Male = "male")[[category]]
  numbers[paste0(macro, "Mean")] <- sprintf("%.2f", all_means$mean[all_means$category == category])
  numbers[paste0(macro, "NonspamMean")] <- sprintf(
    "%.2f", nonspam_means$mean[nonspam_means$category == category]
  )
}
count_names <- c(
  "sampleSize", "eventCount", "nonspamCount", "missingDomainCount", "topDomainEvents"
)
numbers[count_names] <- format(as.integer(numbers[count_names]), big.mark = ",", trim = TRUE)
writeLines(paste0("\\newcommand{\\", names(numbers), "}{", numbers, "}"), "tabs/numbers.tex")

for (section in c("age_sex", "race_education")) {
  slide_cps <- cps_comparison |>
    mutate(section = if_else(row_number() < match("Race", category),
                             "age_sex", "race_education")) |>
    filter(.data$section == .env$section, !is.na(yougov) | is.na(cps)) |>
    select(-section)
  writeLines(kable(slide_cps, format = "latex", booktabs = TRUE, digits = 2,
                   col.names = c("", "CPS", "YouGov", "Difference")),
             paste0("tabs/present_cps_", section, ".tex"))
}
for (section in c("age_education", "sex_race")) {
  variables <- if (section == "age_education") c("age_group", "education") else c("sex", "race")
  slide_means <- map_dfr(variables, \(variable) {
    bind_rows(
      tibble::tibble(category = demographic_labels[[variable]], mean = NA_real_, se = NA_real_),
      nonspam_means |> filter(demographic == variable) |> select(category, mean, se)
    )
  })
  writeLines(kable(slide_means, format = "latex", booktabs = TRUE, digits = 2,
                   col.names = c("", "Mean", "SE")),
             paste0("tabs/present_nonspam_", section, ".tex"))
}
slide_domains <- domain_counts |>
  filter(n > 100) |>
  mutate(domain = case_when(
    domain == "rivercitymediaonline.com" ~ paste0("\\alert<3>{", domain, "}"),
    domain %in% c("linkedin.com", "myspace.com", "adobe.com", "tumblr.com", "dropbox.com") ~
      paste0("\\alert<2>{", domain, "}"),
    .default = domain
  ))
writeLines(kable(slide_domains, format = "latex", booktabs = TRUE, escape = FALSE,
                 col.names = c("Domain", "Records")), "tabs/present_domains.tex")
