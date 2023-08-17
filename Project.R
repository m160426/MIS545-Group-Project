# MIS545 Project
# set seed as 545 when needed

# install libraries
# install.packages("tidyverse")
# install.packages("dummies", repos = NULL, type="source")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")
# install.packages("rpart.plot")
# install.packages("neuralnet")

# load libraries
library(tidyverse)
library(dummies)
library(corrplot)
library(olsrr)
library(smotefamily)
library(class)
library(rpart)
library(rpart.plot)
library(neuralnet)

# set wd?
#setwd("/Users/beau9/Documents/MIS545-Project")

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

# Remove entries with rating and ranking of 0 to remove NA
varietyFeedback <- varietyFeedback %>%
  filter((Rating > 0) & (Ranking > 0))

# Remove Notes, Customers, Fields, Lot, Evergree, Nutty, Fruity
varietyFeedback <- varietyFeedback %>% select(-Notes)
varietyFeedback <- varietyFeedback %>% select(-Customer)
varietyFeedback <- varietyFeedback %>% select(-Fields)
varietyFeedback <- varietyFeedback %>% select(-Lot)
varietyFeedback <- varietyFeedback %>% select(-Evergreen)
varietyFeedback <- varietyFeedback %>% select(-Nutty)
varietyFeedback <- varietyFeedback %>% select(-Fruity)

# Dummy code Variety
varietyFeedbackDataFrame <- data.frame(varietyFeedback)
varietyFeedback <- as_tibble(dummy.data.frame(data = varietyFeedbackDataFrame, 
                                         names = "Variety"))

# Run three queries
# Which variety was selected the most?
varietyFeedback %>%
  filter(VarietySimcoe > 0 & Selected == TRUE) %>%
  count()

varietyFeedback %>%
  filter(VarietyCitra >0 & Selected == TRUE) %>%
  count()

varietyFeedback %>%
  filter(VarietyMosaic > 0 & Selected == TRUE) %>%
  count()

varietyFeedback %>%
  filter(VarietyPahto >0 & Selected == TRUE) %>%
  count()

varietyFeedback %>%
  filter(VarietySabro >0 & Selected == TRUE) %>%
  count()

print("Simcoe was selected the most (53 times)")
print("Pahto was selected only once")

# How many were Selected vs not Selected
varietyFeedback %>%
  count(Selected) %>%
  print()
selectionPercentage <- 117 / (276 + 117)
sprintf("Selected Percentage: %f", selectionPercentage)

# How many that were detected as Spicy were selected?
varietyFeedback %>%
  filter(Spicy > 0 & Selected == TRUE) %>%
  count() %>%
  print()
print("6 varieties were selected that were detected as spicy")

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

# Call the displayAllHistograms()
displayAllHistograms(varietyFeedback)

# Normalize data?

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


# Logistic Regression -----------------------------------------------------
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

# Multicollinearity?

# Pairwise Correlation?





                     




# K-Nearest Neighbors -----------------------------------------------------




# Seperate tibble into two
varietyFeedbackLabels <- varietyFeedback %>% select(Selected)
varietyFeedbackKNN <- varietyFeedback %>% select(-Selected)

# Set seed and split data
sampleSetKNN <- sample(nrow(varietyFeedbackKNN),
                       round(nrow(varietyFeedbackKNN) * 0.75),
                       replace = FALSE)

# Put 75% of records into Training
varietyFeedbackKNNTraining <- varietyFeedbackKNN[sampleSetKNN, ]
varietyFeedbackKNNTrainingLabels <- varietyFeedbackLabels[sampleSetKNN, ]

# Put 25% of records into Testing
varietyFeedbackKNNTesting <- varietyFeedbackKNN[-sampleSetKNN, ]
varietyFeedbackKNNTestingLabels <- varietyFeedbackLabels[-sampleSetKNN, ]

# Create Matrix of k-values with their predictive accuracy
kValueMatrix <- matrix(data = NA,
                       nrow = 0,
                       ncol = 2)

# Assign column names to the matrix
colnames(kValueMatrix) <- c("k value", "Predictive Accuracy")

# Loop through values of k to determine best fitting model
for (kValue in 1:nrow(varietyFeedbackKNNTraining)) {
  # Only calculate if it is odd
  if(kValue %% 2 != 0) {
    # Generate the model
    varietyFeedbackKNNPrediction <- knn(train = varietyFeedbackKNNTraining,
                                        test = varietyFeedbackKNNTesting,
                                        cl = 
                                      varietyFeedbackKNNTrainingLabels$Selected,
                                        k = kValue)
    
    # Generate confusion matrix
    varietyFeedbackKNNConfusionMatrix <- table(
      varietyFeedbackKNNTestingLabels$Selected, varietyFeedbackKNNPrediction)
    
    # Calculate the predictive accuracy
    varietyFeedbackKNNPredictiveAccuracy <- sum(diag(
      varietyFeedbackKNNConfusionMatrix)) / nrow(varietyFeedbackKNNTesting)
    
    # Add new row to the kValueMatrix
    kValueMatrix <- rbind(kValueMatrix, c(kValue, 
                                          varietyFeedbackKNNPredictiveAccuracy))
  }
}

# Display kValueMatrix
print(kValueMatrix)

# Best k value is 29
varietyFeedbackKNNPrediction <- knn(train = varietyFeedbackKNNTraining,
                                    test = varietyFeedbackKNNTesting,
                                    cl = 
                                      varietyFeedbackKNNTrainingLabels$Selected,
                                    k = 29)

# Display Prediction
summary(varietyFeedbackKNNPrediction)

# Generate confusion matrix
varietyFeedbackKNNConfusionMatrix <- table(
  varietyFeedbackKNNTestingLabels$Selected, varietyFeedbackKNNPrediction)

# Display confusion matrix
print(varietyFeedbackKNNConfusionMatrix)

# Calculate the predictive accuracy
varietyFeedbackKNNPredictiveAccuracy <- sum(diag(
  varietyFeedbackKNNConfusionMatrix)) / nrow(varietyFeedbackKNNTesting)

# Display predictive accuracy
print(varietyFeedbackKNNPredictiveAccuracy)

# Naive Bayes -------------------------------------------------------------




# Decision Tree -----------------------------------------------------------
# Split Data into training and testing
sampleSetDT <- sample(nrow(varietyFeedback),
                      round(nrow(varietyFeedback) * .75),
                      replace = FALSE)
varietyFeedbackDTTraining <- varietyFeedback[sampleSetDT, ]

# Calculate Selected probability in training dataset
summary(varietyFeedbackDTTraining$Selected)
print(86/295)

# Put remaining records in testing tibble
varietyFeedbackDTTesting <- varietyFeedback[-sampleSetDT, ]

# Train decision tree model
varietyFeedbackDTModel <- rpart(formula = Selected ~ .,
                                method = "class",
                                cp = .02,
                                data = varietyFeedbackDTTraining)

# Display DT plot
rpart.plot(varietyFeedbackDTModel)

# Predict classes for each record in the testing dataset
varietyFeedbackDTPrediction <- predict(varietyFeedbackDTModel,
                                       varietyFeedbackDTTesting,
                                       type = "class")

# Display predictions
print(varietyFeedbackDTPrediction)

# Evaluate model by forming a confusion matrix
varietyFeedbackDTConfusionMatrix <- table(varietyFeedbackDTTesting$Selected,
                                          varietyFeedbackDTPrediction)

# Display Confusion Matrix
print(varietyFeedbackDTConfusionMatrix)

# Calculate Predictive Accuracy
varietyFeedbackDTPredictiveAccuracy <- sum(diag(
  varietyFeedbackDTConfusionMatrix)) / nrow(varietyFeedbackDTTesting)

# Display Predictive Accuracy
print(varietyFeedbackDTPredictiveAccuracy)

# Generate Model with lower complexity parameter
# This doesnt change DT plot at all, meaning no other complexity to uncover?





# Neural Network ----------------------------------------------------------
# Scale features from 0 to 1
varietyFeedbackNeural <- varietyFeedback
varietyFeedbackNeural <- varietyFeedbackNeural %>%
  mutate(RankingScaled = (Ranking - min(Ranking)) / 
           (max(Ranking) - min(Ranking))) %>% 
  mutate(RatingScaled = (Rating - min(Rating)) / 
           (max(Rating) - min(Rating))) %>%
  mutate(GreenessScaled = (Greeness - min(Greeness)) / 
           (max(Greeness) - min(Greeness)))%>%
  mutate(ShatterScaled = (Shatter - min(Shatter)) / 
           (max(Shatter) - min(Shatter))) %>%
  mutate(WoodyScaled = (Woody - min(Woody)) / 
           (max(Woody) - min(Woody))) %>%
  mutate(EarthyScaled = (Earthy - min(Earthy)) / 
           (max(Earthy) - min(Earthy))) %>%
  mutate(SpicyScaled = (Spicy - min(Spicy)) / 
           (max(Spicy) - min(Spicy))) %>%
  mutate(VegetalScaled = (Vegetal - min(Vegetal)) / 
           (max(Vegetal) - min(Vegetal))) %>%
  mutate(FloralScaled = (Floral - min(Floral)) / 
           (max(Floral) - min(Floral))) %>%
  mutate(GrassyScaled = (Grassy - min(Grassy)) / 
           (max(Grassy) - min(Grassy))) %>%
  mutate(HerbalScaled = (Herbal - min(Herbal)) / 
           (max(Herbal) - min(Herbal))) %>%
  mutate(StonefruitScaled = (Stonefruit - min(Stonefruit)) / 
           (max(Stonefruit) - min(Stonefruit))) %>%
  mutate(CitrusScaled = (Citrus - min(Citrus)) / 
           (max(Citrus) - min(Citrus))) %>%
  mutate(TropicalScaled = (Tropical - min(Tropical)) / 
           (max(Tropical) - min(Tropical))) %>%
  mutate(OGScaled = (OG - min(OG)) / 
           (max(OG) - min(OG))) %>%
  mutate(CheesyScaled = (Cheesy - min(Cheesy)) / 
           (max(Cheesy) - min(Cheesy))) %>%
  mutate(PlasticScaled = (Plastic - min(Plastic)) / 
           (max(Plastic) - min(Plastic))) %>%
  mutate(DieselScaled = (Diesel - min(Diesel)) / 
           (max(Diesel) - min(Diesel)))
  
#  Split Data into training and testing
sampleSetNN <- sample(nrow(varietyFeedbackNeural),
                      round(nrow(varietyFeedbackNeural) * .75),
                      replace = FALSE)
varietyFeedbackNeuralTraining <- varietyFeedbackNeural[sampleSetNN, ]
varietyFeedbackNeuralTesting <- varietyFeedbackNeural[-sampleSetNN, ]  
  
# Generate Neural Net
varietyFeedbackNeuralNet <- neuralnet(
  formula = Selected ~ 
    VarietySimcoe +
    VarietyCitra +
    VarietyMosaic +
    VarietyPahto +
    VarietySabro +  
    RankingScaled +
    RatingScaled +
    GreenessScaled +
    ShatterScaled +
    WoodyScaled +
    EarthyScaled +
    SpicyScaled +
    VegetalScaled +
    FloralScaled +
    GrassyScaled +
    HerbalScaled +
    StonefruitScaled +
    CitrusScaled +
    TropicalScaled +
    OGScaled +
    CheesyScaled +
    PlasticScaled +
    DieselScaled,
  data = varietyFeedbackNeuralTraining,
  hidden = 3,
  act.fct = "logistic",
  linear.output = FALSE)

# Display nerual network results
print(varietyFeedbackNeuralNet)

# Visualize the nerual net
plot(varietyFeedbackNeuralNet)

# Generate probabilities
varietyFeedbackNeuralNetProbability <- compute(varietyFeedbackNeuralNet,
                                               varietyFeedbackNeuralTesting)
  
# Display predictions
print(varietyFeedbackNeuralNetProbability$net.result)
  
# Convert into 0/1 predictions
varietyFeedbackNeuralNetPrediction <- 
  ifelse(varietyFeedbackNeuralNetProbability$net.result > 0.05, 1, 0)

# Display Predictions
print(varietyFeedbackNeuralNetPrediction)

# Evaluate model by forming a confusion matrix
varietyFeedbackNeuralNetConfusionMatrix <-
  table(varietyFeedbackNeuralTesting$Selected,
        varietyFeedbackNeuralNetPrediction)

# Display the confusion matrix
print(varietyFeedbackNeuralNetConfusionMatrix)

# Calculate the models Predictive Accuracy
neuralNetPredictiveAccuracy <- 
  sum(diag(varietyFeedbackNeuralNetConfusionMatrix)) /
  nrow(varietyFeedbackNeuralTesting)

# Display the predictive accuracy
print(neuralNetPredictiveAccuracy)





