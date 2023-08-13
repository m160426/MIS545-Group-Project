# MIS545 Project

# install libraries
# install.packages("tidyverse")
# install.packages("dummies", repos = NULL, type="source")

# load libraries
library(tidyverse)
library(dummies)

# set wd?

# read csv file
# Lot - F
# Variety - F
# Fields - F
# Customer - C 
# Selected - C/L (file has Yes/No, read as C, convert to logical)
# Ranking - I
# Greeness - I
# Shatter - I
# Rating - I
# Evergreen - I
# Woody - I
# Nutty - I
# Earthy - I
# Spicy - I
# Vegetal - I
# Floral - I
# Grassy - I
# Herbal - I
# Fruity - I
# Stonefruit - I 
# Citrus - I
# Tropical - I
# OG - I
# Cheesy - I 
# Plastic - I
# Diesel - I
# Notes - C
varietyFeedback <- read_csv(file = "PerraultFarmsFeedback.csv",
                            col_types = "fffcciiiiiiiiiiiiiiiiiiiiic",
                            col_names = TRUE
                            )

# Convert Selected to logical
varietyFeedback <- varietyFeedback %>%
  mutate(Selected = ifelse(Selected == "No", FALSE, TRUE))

# Display tibble
print(varietyFeedback)

# Display structure
str(varietyFeedback)

# Display summary
summary(varietyFeedback)

# Remove variety's with less than 10 entries
varietyFeedback <- varietyFeedback %>%
  group_by(Variety) %>%
  filter(n() >= 10 | is.na(Variety)) %>%
  ungroup()

# Dummy code Variety
varietyFeedbackDataFrame <- data.frame(varietyFeedback)
varietyFeedback <- as_tibble(dummy.data.frame(data = varietyFeedbackDataFrame, 
                                         names = "Variety"))

# Normalize?

# Remove Lot? Field? Customer?


