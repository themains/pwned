## YG/CPS Comparison

# set dir.
setwd(githubdir)
setwd("pwned/")

# load libs
library(tidyverse)
library(xtable)

# Load dat
cps <- read.csv("data/cps_2018.csv")
yg <- read.csv("data/YGOV1058_profile.csv")

# Recode
yg$educ <- car::recode(yg$educ, "1 = 'No high school diploma';
                                 2 = 'High school or equivalent';
                                 3 = 'Some college, less than 4-yr degree'; 
                                 4 = 'Some college, less than 4-yr degree'; 
                                 5 = 'Bachelors degree or higher'; 
                                 6 = 'Bachelors degree or higher'",
                                 as.factor = T, 
                                 levels = c("No high school diploma", "High school or equivalent", "Some college, less than 4-yr degree", "Bachelors degree or higher"))

yg$race <- car::recode(yg$race, "1 = 'White alone';
                                 2 = 'Black or African American alone';
                                 3 = 'Hispanic/Latino'; 
                                 4 = 'Asian alone'; 
                                 5 = 'American Indian and Alaska Native alone'; 
                                 6 = 'Asian alone';
                                 7 = 'Two or more races'; 
                                 8 = 'Other'",
                                 as.factor = T, 
                                 levels = c("White alone", "Black or African American alone", "Hispanic/Latino", "Asian alone", "American Indian and Alaska Native alone", "Two or more races", "Other"))

yg$gender <- car::recode(yg$gender, "1 = 'Male';
                                     2 = 'Female'",
                                     as.factor = T, 
                                     levels = c("Female", "Male"))

# Recode Age
yg$age = 2018 - yg$birthyr
yg$agecat = cut(yg$age, breaks = c(18, 25, 35, 50, 65, 100), right = T, ordered_result = T)

# Get the table ready
cps$yg <- NA
cps[8:9, "yg"] <- table(yg$gender)/nrow(yg)
cps[2:6, "yg"] <- table(yg$agecat)/nrow(yg)
cps[18:21, "yg"] <- table(yg$educ)/nrow(yg)
cps[11:13, "yg"] <- (table(yg$race)/nrow(yg))[c(1, 2, 5)]

# Formatting
cps$yg <- round(cps$yg, 2)
cps$proportion <- round(cps$proportion, 2)
cps$diff <- round(cps$proportion - cps$yg, 2)

# Rename
names(cps)[1] <- ""
names(cps)[3] <- "cps"

# select
cps <- cps[, c(1, 3, 4, 5)]
cps <- cps[-22, ]

# Print
print(xtable(cps,
               label = "table:cps_yg",
               caption = "Comparison Between YouGov and CPS 2018"), 
               caption.placement = "top",
               size = "\\small",
               include.rownames = F,
               heading_command = NULL,
               sanitize.text.function = function(x) {x},
               table.placement = "!htb",
               file = "tabs/tabsi_yg_cps.tex")
