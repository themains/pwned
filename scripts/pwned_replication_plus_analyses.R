# Replicating + Additional Analyses
# s.e. + plots + regressions
# -------

# Set dir.
setwd(githubdir)
setwd("pwned/")

# Load dat
yg <- read.csv("data/YGOV1058_profile.csv")
pwn <- read.csv("data/YGOV1058_pwned.csv")

# load libs
library(tidyverse)
library(splines)
library(stargazer)
library(xtable)

# Join or die
fin_dat <- yg %>% left_join(pwn)

# lowercase names
names(fin_dat) <- tolower(names(fin_dat))

# Total rows
nrow(fin_dat)

# Number breached
length(unique(pwn$id))

# Create a vector of 1/0 using NAs from join so that NA is 0 and else is 1
sum(is.na(pwn$PwnCount))
fin_dat$pwn <- ifelse(is.na(fin_dat$pwncount), 0, 1)

# Descriptive stuff
counts <- fin_dat %>%
    group_by(id) %>%
    summarize(n = sum(pwn))

# Mean/median/range
summary(counts$n)

# Group by gender
counts <- fin_dat %>%
    group_by(id, gender) %>%
    summarize(n = sum(pwn)) %>%
    group_by(gender) %>%
    summarize(n = mean(n))

# Everything else should work. Let's go to filtering
fin_small_dat <- subset(fin_dat, (is.na(isverified) | isverified == T) & (is.na(isspamlist) | isspamlist == F))

counts <- fin_small_dat %>%
    group_by(id, gender) %>%
    summarize(n = sum(pwn)) %>%
    group_by(gender) %>%
    summarize(n = mean(n))

## ID level pwn
pwn_id <- fin_dat[, c('id', 'pwn')] %>% 
   group_by(id) %>%
   summarize(total_pwn = sum(pwn))

# Inner join to profile
fin_id_dat <- pwn_id %>%
    inner_join(fin_dat)

# Unique
fin_id_dat <- fin_id_dat[!duplicated(fin_id_dat$id), ]

# Means and s.e. by race, sex, age, and education

## Recoding

fin_id_dat$educ <- car::recode(fin_id_dat$educ, "1 = 'No HS';
                                             2 = 'HS Grad.';
                                             3 = 'Some College'; 
                                             4 = '2-year College Degree'; 
                                             5 = '4-year College Degree'; 
                                             6 = 'Postgrad Degree'",
                                             as.factor = T, 
                                             levels = c("No HS", "HS Grad.", "Some College", "2-year College Degree", "4-year College Degree", "Postgrad Degree"))

fin_id_dat$race <- car::recode(fin_id_dat$race, "1 = 'White';
                                             2 = 'Black';
                                             3 = 'Hispanic/Latino'; 
                                             4 = 'Asian'; 
                                             5 = 'Native American'; 
                                             6 = 'Middle Eastern';
                                             7 = 'Mixed Race'; 
                                             8 = 'Other'",
                                             as.factor = T, 
                                             levels = c("White", "Black", "Hispanic/Latino", "Asian", "Native American", "Middle Eastern", "Mixed Race", "Other"))

fin_id_dat$gender <- car::recode(fin_id_dat$gender, "1 = 'Male';
                                             2 = 'Female'",
                                             as.factor = T, 
                                             levels = c("Female", "Male"))

fin_id_educ <- fin_id_dat %>%
    group_by(educ) %>%
    summarise(mean_pwn = mean(total_pwn), se = sd(total_pwn)/sqrt(n()))

fin_id_race <- fin_id_dat %>%
    group_by(race) %>%
    summarise(mean_pwn = mean(total_pwn), se = sd(total_pwn)/sqrt(n()))

fin_id_sex <- fin_id_dat %>%
    group_by(gender) %>%
    summarise(mean_pwn = mean(total_pwn), se = sd(total_pwn)/sqrt(n()))

# Recode Age
fin_id_dat$age = 2018 - fin_id_dat$birthyr
fin_id_dat$agecat = cut(fin_id_dat$age, breaks = c(18, 25, 35, 50, 65, 100), right = T, ordered_result = T)

fin_id_age <- fin_id_dat %>%
    group_by(agecat) %>%
    summarise(mean_pwn = mean(total_pwn), se = sd(total_pwn)/sqrt(n()))

## Output a Table
fin_res <- data.frame("Sociodemographics" = NA, mean = NA, se = NA)
fin_res[1, 1] <- "Age"
fin_res[2:(nrow(fin_id_age) + 1), 1] <- as.character(fin_id_age$agecat)
fin_res[2:(nrow(fin_id_age) + 1), 2:3] <- fin_id_age[, 2:3]
fin_res[7, 1] <- "Missing"
fin_res[8, 1] <- ""

fin_res[9, 1] <- "Education"
fin_res[10:(nrow(fin_id_educ) + 9), 1] <- as.character(fin_id_educ$educ)
fin_res[10:(nrow(fin_id_educ) + 9), 2:3] <- fin_id_educ[, 2:3]
fin_res[16, 1] <- ""

fin_res[17, 1] <- "Sex"
fin_res[18:(nrow(fin_id_sex) + 17), 1] <- as.character(fin_id_sex$gender)
fin_res[18:(nrow(fin_id_sex) + 17), 2:3] <- fin_id_sex[, 2:3]
fin_res[20, 1] <- ""

fin_res[21, 1] <- "Race"
fin_res[22:(nrow(fin_id_race) + 21), 1] <- as.character(fin_id_race$race)
fin_res[22:(nrow(fin_id_race) + 21), 2:3] <- fin_id_race[, 2:3]

names(fin_res)[1] <- ""
print(xtable(fin_res,
               label = "table:socdem_dat",
               caption = "Frequency of Account Breaches By Socio-economic Factors"), 
               caption.placement = "top",
               size = "\\small",
               include.rownames = F,
               heading_command = NULL,
               sanitize.text.function = function(x) {x},
               table.placement = "!htb",
               file = "tabs/tab2_freq_se_by_group.tex")

# Do some plots
# Custom ggplot theme
cust_theme <- theme_minimal() +
   theme(panel.grid.major = element_line(color = "#e1e1e1",  linetype = "dotted"),
      panel.grid.minor = element_blank(),
      legend.position  = "bottom",
      legend.key       = element_blank(),
      legend.key.width = unit(1, "cm"),
      axis.title   = element_text(size = 10, color = "#555555"),
      axis.text    = element_text(size = 10, color = "#555555"),
      axis.title.x = element_text(vjust = 1, margin = margin(10, 0, 0, 0)),
      axis.title.y = element_text(vjust = 1),
      axis.ticks   = element_line(color = "#e1e1e1", linetype = "dotted", size = .2),
      axis.text.x  = element_text(vjust = .3),
      plot.margin = unit(c(.5, .75, .5, .5), "cm"))

ggplot(fin_id_dat, aes(age, total_pwn)) +
  geom_point(alpha = .05) +
  geom_smooth(method = "loess") +
  scale_x_continuous("Age", limits = c(18, 100), breaks = seq(20, 100, 10), labels = seq(20, 100, 10)) +
  ylab("Number of Accounts Breached") + 
  cust_theme
ggsave("figs/age_pwned.pdf")
ggsave("figs/age_pwned.png")

ggplot(fin_id_educ, aes(x = educ, y = mean_pwn)) + 
  geom_point(stat = "identity", color = "#777777") +
  geom_errorbar(aes(ymin = mean_pwn - 1.96*se, ymax = mean_pwn + 1.96*se), width = .03, color = "#A7A7A7", linetype = "dotted") + 
  ylab("Average Number of Accounts Breached") +
  xlab("") + 
  cust_theme +
  coord_flip()
ggsave("figs/educ_pwned.pdf")
ggsave("figs/educ_pwned.png")

ggplot(fin_id_race, aes(x = race, y = mean_pwn)) + 
  geom_point(stat = "identity", color = "#777777") +
  geom_errorbar(aes(ymin = mean_pwn - 1.96*se, ymax = mean_pwn + 1.96*se), width = .03, color = "#A7A7A7", linetype = "dotted") + 
  ylab("Average Number of Accounts Breached") +
  xlab("") +
  cust_theme +
  coord_flip()
ggsave("figs/race_pwned.pdf")
ggsave("figs/race_pwned.png")

ggplot(fin_id_sex, aes(x = gender, y = mean_pwn)) + 
  geom_point(stat = "identity", color = "#777777") +
  geom_errorbar(aes(ymin = mean_pwn - 1.96*se, ymax = mean_pwn + 1.96*se), width = .03, color = "#A7A7A7", linetype = "dotted") + 
  ylab("Average Number of Accounts Breached") +
  xlab("") +
  cust_theme +
  coord_flip()
ggsave("figs/sex_pwned.pdf")
ggsave("figs/sex_pwned.png")

# Run some regressions
educ_lm <- lm(total_pwn ~ educ, data = fin_id_dat)
race_lm <- lm(total_pwn ~ race, data = fin_id_dat)
sex_lm  <- lm(total_pwn ~ gender, data = fin_id_dat)
age_lm  <- lm(total_pwn ~ as.factor(agecat), data = fin_id_dat)
age_lm2 <- lm(total_pwn ~ ns(age, 2), data = fin_id_dat)

stargazer(educ_lm, 
          title = "Number of Breaches by Education",
          digits = 2,
          label = "tab:educ_breaches",
          initial.zero = FALSE,
          dep.var.labels = "Number of Breaches",
          omit.stat = c("LL", "ser", "f"),
          no.space = TRUE,
          covariate.labels = c("HS Grad.", "Some College", "2-year College Degree", "4-year College Degree", "Postgrad Degree"),
          out = "tabs/educ_pwned.tex")

stargazer(race_lm, 
          title = "Number of Breaches by Race/Ethnicity",
          digits = 2,
          label = "tab:race_breaches",
          initial.zero = FALSE,
          dep.var.labels = "Number of Breaches",
          omit.stat = c("LL", "ser", "f"),
          no.space = TRUE,
          covariate.labels = c("Black", "Hispanic/Latino", "Asian", "Native American", "Middle Eastern", "Mixed Race", "Other"),
          out = "tabs/race_pwned.tex")

stargazer(sex_lm, 
          title = "Number of Breaches by Sex",
          digits = 2,
          label = "tab:sex_breaches",
          initial.zero = FALSE,
          dep.var.labels = "Number of Breaches",
          omit.stat = c("LL", "ser", "f"),
          no.space = TRUE,
          covariate.labels = "Male",
          out = "tabs/sex_pwned.tex")

stargazer(age_lm2, 
          title = "Number of Breaches by Age",
          digits = 2,
          label = "tab:age_breaches",
          initial.zero = FALSE,
          dep.var.labels = "Number of Breaches",
          omit.stat = c("LL", "ser", "f"),
          no.space = TRUE,
          out = "tabs/age_pwned.tex")

## Spam List

# Everything else should work. Let's go to filtering
fin_dat <- subset(fin_dat, (is.na(isverified) | isverified == T) & (is.na(isspamlist) | isspamlist == F))

## ID level pwn
pwn_id <- fin_dat[, c('id', 'pwn')] %>% 
   group_by(id) %>%
   summarize(total_pwn = sum(pwn))

# Inner join to profile
fin_id_dat <- pwn_id %>%
    inner_join(fin_dat)

# Unique
fin_id_dat <- fin_id_dat[!duplicated(fin_id_dat$id), ]

# Means and s.e. by race, sex, age, and education

## Recoding

fin_id_dat$educ <- car::recode(fin_id_dat$educ, "1 = 'No HS';
                                             2 = 'HS Grad.';
                                             3 = 'Some College'; 
                                             4 = '2-year College Degree'; 
                                             5 = '4-year College Degree'; 
                                             6 = 'Postgrad Degree'",
                                             as.factor = T, 
                                             levels = c("No HS", "HS Grad.", "Some College", "2-year College Degree", "4-year College Degree", "Postgrad Degree"))

fin_id_dat$race <- car::recode(fin_id_dat$race, "1 = 'White';
                                             2 = 'Black';
                                             3 = 'Hispanic/Latino'; 
                                             4 = 'Asian'; 
                                             5 = 'Native American'; 
                                             6 = 'Middle Eastern';
                                             7 = 'Mixed Race'; 
                                             8 = 'Other'",
                                             as.factor = T, 
                                             levels = c("White", "Black", "Hispanic/Latino", "Asian", "Native American", "Middle Eastern", "Mixed Race", "Other"))

fin_id_dat$gender <- car::recode(fin_id_dat$gender, "1 = 'Male';
                                             2 = 'Female'",
                                             as.factor = T, 
                                             levels = c("Female", "Male"))

fin_id_educ <- fin_id_dat %>%
    group_by(educ) %>%
    summarise(mean_pwn = mean(total_pwn), se = sd(total_pwn)/sqrt(n()))

fin_id_race <- fin_id_dat %>%
    group_by(race) %>%
    summarise(mean_pwn = mean(total_pwn), se = sd(total_pwn)/sqrt(n()))

fin_id_sex <- fin_id_dat %>%
    group_by(gender) %>%
    summarise(mean_pwn = mean(total_pwn), se = sd(total_pwn)/sqrt(n()))

# Recode Age
fin_id_dat$age = 2018 - fin_id_dat$birthyr
fin_id_dat$agecat = cut(fin_id_dat$age, breaks = c(18, 25, 35, 50, 65, 100), right = T, ordered_result = T)

fin_id_age <- fin_id_dat %>%
    group_by(agecat) %>%
    summarise(mean_pwn = mean(total_pwn), se = sd(total_pwn)/sqrt(n()))

## Output a Table
fin_res <- data.frame("Sociodemographics" = NA, mean = NA, se = NA)
fin_res[1, 1] <- "Age"
fin_res[2:(nrow(fin_id_age) + 1), 1] <- as.character(fin_id_age$agecat)
fin_res[2:(nrow(fin_id_age) + 1), 2:3] <- fin_id_age[, 2:3]
fin_res[7, 1] <- "Missing"
fin_res[8, 1] <- ""

fin_res[9, 1] <- "Education"
fin_res[10:(nrow(fin_id_educ) + 9), 1] <- as.character(fin_id_educ$educ)
fin_res[10:(nrow(fin_id_educ) + 9), 2:3] <- fin_id_educ[, 2:3]
fin_res[16, 1] <- ""

fin_res[17, 1] <- "Sex"
fin_res[18:(nrow(fin_id_sex) + 17), 1] <- as.character(fin_id_sex$gender)
fin_res[18:(nrow(fin_id_sex) + 17), 2:3] <- fin_id_sex[, 2:3]
fin_res[20, 1] <- ""

fin_res[21, 1] <- "Race"
fin_res[22:(nrow(fin_id_race) + 21), 1] <- as.character(fin_id_race$race)
fin_res[22:(nrow(fin_id_race) + 21), 2:3] <- fin_id_race[, 2:3]

names(fin_res)[1] <- ""
print(xtable(fin_res,
               label = "table:socdem_verified_dat",
               caption = "Frequency of Verified, Non-SpamList Account Breaches By Socioeconomic Factors."), 
               caption.placement = "top",
               size = "\\small",
               include.rownames = F,
               heading_command = NULL,
               sanitize.text.function = function(x) {x},
               table.placement = "!htb",
               file = "tabs/tab4_freq_se_by_validate_group.tex")