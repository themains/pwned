# Just replicating to be sure

setwd(githubdir)
setwd("pwned/")

# Load dat
yg <- read.csv("data/YGOV1058_profile.csv")
pwn <- read.csv("data/YGOV1058_pwned.csv")

# load libs
library(tidyverse)

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
fin_id_dat$agecat = cut(2018 - fin_id_dat$birthyr, breaks = c(18, 25, 35, 50, 65, 100), right = T, ordered_result = T)

fin_id_age <- fin_id_dat %>%
    group_by(agecat) %>%
    summarise(prop = n()/5000, mean_pwn = mean(total_pwn), se = sd(total_pwn)/sqrt(n()))

# Do some plots
ggplot(fin_id_dat, aes(birthyr, total_pwn)) +
  geom_point() +
  geom_smooth()

# Run some regressions
summary(lm(total_pwn ~ as.factor(educ), data = fin_id_dat))
summary(lm(total_pwn ~ as.factor(race), data = fin_id_dat))
summary(lm(total_pwn ~ as.factor(gender), data = fin_id_dat))
summary(lm(total_pwn ~ as.factor(agecat), data = fin_id_dat))
summary(lm(total_pwn ~ ns(I(2018 - birthyr), 2), data = fin_id_dat))

