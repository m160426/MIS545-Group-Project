# MIS545 Project
# set seed as 545 when needed

# install libraries
# install.packages("tidyverse")
# install.packages("dummies", repos = NULL, type="source")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")

# load libraries
library(tidyverse)
library(dummies)
library(corrplot)
library(olsrr)
library(smotefamily)

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

varietyFeedback %>%
  count(Variety) %>%
  print()

# Remove variety's with less than 15 entries
varietyFeedback <- varietyFeedback %>%
  group_by(Variety) %>%
  filter(n() >= 15 | is.na(Variety)) %>%
  ungroup()

# Remove Notes, Customers, Fields, Lot
varietyFeedback <- varietyFeedback %>% select(-Notes)
varietyFeedback <- varietyFeedback %>% select(-Customer)
varietyFeedback <- varietyFeedback %>% select(-Fields)
varietyFeedback <- varietyFeedback %>% select(-Lot)

# Dummy code Variety
varietyFeedbackDataFrame <- data.frame(varietyFeedback)
varietyFeedback <- as_tibble(dummy.data.frame(data = varietyFeedbackDataFrame, 
                                         names = "Variety"))

# Normalize?

# Run three queries
summary(varietyFeedback)

# How many of each Rating
varietyFeedback %>%
  count(Rating) %>%
  print()

# How many were Selected vs not Selected
varietyFeedback %>%
  count(Selected) %>%
  print()

# How many that were detected as Spicy were selected?
varietyFeedback %>%
  filter(Spicy > 0 & Selected == TRUE) %>%
  count() %>%
  print()

# Recreate the displayAllHistograms()
displayAllHistograms <- function(tibbleDataset) {
  tibbleDataset %>%
    keep(is.numeric) %>%
    gather() %>%
    ggplot() + geom_histogram(mapping = aes(x = value,
                                            fill = key),
                              color = "black") +
    facet_wrap (~ key, scales = "free") +
    theme_minimal ()
}

# Call the displayAllHistograms() for Rating and Ranking
displayAllHistograms(select(varietyFeedback, Rating, Ranking))

# Scatter plot of Ranking vs Rating?

# Display a correlation plot 
corrplot(cor(varietyFeedback),
         method = "number",
         type = "lower")

# Split data into training and testing
set.seed(545)
sampleSetLR <- sample(nrow(varietyFeedback),
                      round(nrow(varietyFeedback) * .75),
                      replace = FALSE)
varietyFeedbackLRTraining <- varietyFeedback[sampleSetLR, ]
varietyFeedbackLRTesting <- varietyFeedback[-sampleSetLR, ]

# Class imbalance in regards to Selected vs not selected?
summary(varietyFeedbackLRTraining$Selected)
mean(varietyFeedbackLRTraining$Selected)
classImbalanceMagnitude <- 215 / 167


# Deal with class imbalance
varietyFeedbackLRTrainingSmoted <-
  tibble(BLSMOTE(X = data.frame(varietyFeedbackLRTraining),
               target = varietyFeedbackLRTraining$Selected,
               dupSize = 2)$data)

summary(varietyFeedbackLRTrainingSmoted$Selected)

# Convert Selected back to logical
varietyFeedbackLRTrainingSmoted <- varietyFeedbackLRTrainingSmoted %>%
  mutate(Selected = as.logical(Selected))
summary(varietyFeedbackLRTrainingSmoted)

# Remove class function
varietyFeedbackLRTrainingSmoted <- varietyFeedbackLRTrainingSmoted %>%
  select(-class)

# Generage Logistic Regression Model
varietyFeedbackLRModel <- glm(data = varietyFeedbackLRTrainingSmoted,
                              family = binomial,
                              formula = Selected ~ .)

# Display output of LR model
summary(varietyFeedbackLRModel)

# Calculate odds ratios for significant variables
# increase in rank (lower rank the better) means decreases odds of it 
# being selected
exp(coef(varietyFeedbackLRModel)["Ranking"])

# increase in Rating means higher chance of being selected
exp(coef(varietyFeedbackLRModel)["Rating"])

# increase in Citrus smell means lower odds of being selected
exp(coef(varietyFeedbackLRModel)["Citrus"])

# Use the model to predict outcomes in the testing dataset
varietyFeedbackLRPrediction <- predict(varietyFeedbackLRModel,
                                       varietyFeedbackLRTesting,
                                       type = "response")

# treat anything below or equal to .5 as a 0 and anything above .5 as a 1
varietyFeedbackLRPrediction <-
  ifelse(varietyFeedbackLRPrediction >= 0.5, 1, 0)

# Display prediction model
print(varietyFeedbackLRPrediction)

# create confusion matrix
varietyFeedbackLRConfusionMatrix <- table(varietyFeedbackLRTesting$Selected,
                                          varietyFeedbackLRPrediction)

# display CM
print(varietyFeedbackLRConfusionMatrix)

# calculate false positive rate
# Precentage of the time the model predicted a variety would be selected but
# it wasnt
varietyFeedbackLRConfusionMatrix[1, 2] /
  (varietyFeedbackLRConfusionMatrix[1, 2] +
     varietyFeedbackLRConfusionMatrix[1, 1])

# Calculate false negative rate
# Precentage of the time our model predicted a variety would NOT be selected but
# was
varietyFeedbackLRConfusionMatrix[2, 1] /
  (varietyFeedbackLRConfusionMatrix[2, 1] +
     varietyFeedbackLRConfusionMatrix[2, 2])

# Calcuate total predictive accuracy
sum(diag(varietyFeedbackLRConfusionMatrix)) / nrow(varietyFeedbackLRTesting)

# Test for Multicollinearity
ols_vif_tol(varietyFeedbackLRModel)



                     