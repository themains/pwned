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

