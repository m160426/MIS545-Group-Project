# MIS545 Project
# Group 2 - Beau, Jeffrey, Julia, Keyur
# ProjectGroup2.R

# install libraries
# install.packages("tidyverse")
# install.packages("dummies", repos = NULL, type="source")
# install.packages("corrplot")
# install.packages("olsrr")
# install.packages("smotefamily")
# install.packages("rpart.plot")
# install.packages("neuralnet")
# install.packages("e1071")
# install.packages("tidybins")

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
library(e1071)
library(tidybins)

# set wd - Commented out to ensure execution on any computer
# setwd("/Users/beau9/Documents/MIS545-Project")

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
                            col_names = TRUE)

# Convert Selected to logical
varietyFeedback <- varietyFeedback %>%
  mutate(Selected = ifelse(Selected == "No", FALSE, TRUE))

# Display tibble
print(varietyFeedback)

# Display structure
str(varietyFeedback)

# Display summary
summary(varietyFeedback)

# Remove entries with rating and ranking of 0 to remove NA
varietyFeedback <- varietyFeedback %>%
  filter((Rating > 0) & (Ranking > 0))

# Remove variety's with less than 15 entries
varietyFeedback <- varietyFeedback %>%
  group_by(Variety) %>%
  filter(n() >= 15 | is.na(Variety)) %>%
  ungroup()

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
# Which variety had the highest selection rate?
varietyFeedback %>%
  filter(VarietySimcoe > 0 & Selected == TRUE) %>%
  count() / 154

varietyFeedback %>%
  filter(VarietyCitra > 0 & Selected == TRUE) %>%
  count() / 74

varietyFeedback %>%
  filter(VarietyMosaic > 0 & Selected == TRUE) %>%
  count() / 146

varietyFeedback %>%
  filter(VarietySabro > 0 & Selected == TRUE) %>%
  count() / 17

# Which variety smelled the most Citrusy?
varietyFeedback %>%
  filter(VarietySimcoe > 0) %>%
  tally(Citrus) / 154

varietyFeedback %>%
  filter(VarietyCitra > 0) %>%
  tally(Citrus) / 74

varietyFeedback %>%
  filter(VarietyMosaic > 0) %>%
  tally(Citrus) / 146

varietyFeedback %>%
  filter(VarietySabro > 0) %>%
  tally(Citrus) / 17

# How many that were detected as Citrusy were selected?
varietyFeedback %>%
  filter(Citrus > 0 & Selected == TRUE) %>%
  count()
varietyFeedback %>%
  filter(Citrus > 0) %>%
  count()
print(71/250)
print(paste("If a variety was detected as Citrusy it was",
      "selected 28.4% of the time"))

# Normalize data, avoid normalizing dummy variables
calculateZScore <- function(x) {
  (x - mean(x)) / sd(x)
}
varietyFeedback <- varietyFeedback %>%
  mutate_at(vars(!contains("Variety") & !contains("Selec")), calculateZScore)

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

# Display a correlation plot 
corrplot(cor(varietyFeedback),
         method = "circle",
         type = "lower")

# Pairwise correlation
print(cor(varietyFeedback$VarietySimcoe, varietyFeedback$VarietyMosaic))
print(cor(varietyFeedback$Ranking, varietyFeedback$Rating))


# Logistic Regression -----------------------------------------------------
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
                 dupSize = 4)$data)

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

# Increase in Tropical decreases odds of being selected
exp(coef(varietyFeedbackLRModel)["Tropical"])

# Increase in Plastic smell increases odds of being selected
exp(coef(varietyFeedbackLRModel)["Plastic"])

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
# Precentage of the time our model predicted a variety would NOT be selected
# but was
varietyFeedbackLRConfusionMatrix[2, 1] /
  (varietyFeedbackLRConfusionMatrix[2, 1] +
     varietyFeedbackLRConfusionMatrix[2, 2])

# Calcuate total predictive accuracy
sum(diag(varietyFeedbackLRConfusionMatrix)) / nrow(varietyFeedbackLRTesting)

# Multicollinearity? The glm model does not work due to ratings dataset
ols_vif_tol(varietyFeedbackLRModel)

# Pairwise Correlation?
cor(varietyFeedback$VarietySimcoe,varietyFeedback$VarietyCitra)

# K-Nearest Neighbors -----------------------------------------------------
# Seperate tibble into two
varietyFeedbackLabels <- varietyFeedback %>% select(Selected)
varietyFeedbackKNN <- varietyFeedback %>% select(-Selected)

# Set seed and split data
set.seed(545)
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

# Best k value is 19
varietyFeedbackKNNPrediction <- knn(train = varietyFeedbackKNNTraining,
                                    test = varietyFeedbackKNNTesting,
                                    cl = 
                                    varietyFeedbackKNNTrainingLabels$Selected,
                                    k = 17)

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
# Bin data
varietyFeedbackBinned <- varietyFeedback %>%
  mutate(RankingBinGroup = ntile(Ranking, 3))

# Loop through all columns and replace with bin means
for (varietyNames in colnames(varietyFeedback)){
  colNameToWrite <- paste0(varietyNames, "Binned")
    if((as.name(varietyNames) != "RankingBinGroup") && 
       (as.name(varietyNames) != "Selected")){
      varietyFeedbackBinned <- varietyFeedbackBinned %>%
        group_by(RankingBinGroup) %>%
        mutate(!!colNameToWrite := mean(!!as.name(varietyNames)))
      varietyFeedbackBinned <- varietyFeedbackBinned %>% 
        select(-as.name(varietyNames))
    }
}
varietyFeedbackBinned <- varietyFeedbackBinned %>%
  as_tibble() %>%
  select(-RankingBinGroup)

# Splitting data 75/25
set.seed(545)
sampleSet <- sample(nrow(varietyFeedbackBinned),
                    round(nrow(varietyFeedbackBinned) * 0.75),
                    replace = FALSE)

# Put records in training 75%
varietyFeedbackBinnedTraining <- varietyFeedbackBinned[sampleSet, ]

# Put records in testing 25%
varietyFeedbackBinnedTesting <- varietyFeedbackBinned[-sampleSet, ]

# Train the naive bayes model
varietyFeedbackNBModel <- naiveBayes(formula = Selected ~ .,
                        data = varietyFeedbackBinnedTraining,
                        laplace = 1)

# Build probabilities for each record in testing dataset
varietyFeedbackNBProb <- predict(varietyFeedbackNBModel,
                             varietyFeedbackBinnedTesting,
                             type = "raw")

# Display probabilities in console
print(varietyFeedbackNBProb)

# Build probabilities on class
varietyFeedbackNBPred <- predict(varietyFeedbackNBModel,
                          varietyFeedbackBinnedTesting,
                          type = "class")

# Display probabilities in console
print(varietyFeedbackNBPred)

# Form confusion matrix
varietyFeedbackNBConfusionMatrix <- table(
  varietyFeedbackBinnedTesting$Selected,
                             varietyFeedbackNBPred)
print(varietyFeedbackNBConfusionMatrix)

# Calculate predictive accuracies
varietyFeedbackNBAccuracy <- sum(diag(varietyFeedbackNBConfusionMatrix)) / 
  nrow(varietyFeedbackBinnedTesting)

# Display
print(varietyFeedbackNBAccuracy)

# Decision Tree -----------------------------------------------------------
# Split Data into training and testing
set.seed(545)
sampleSetDT <- sample(nrow(varietyFeedback),
                      round(nrow(varietyFeedback) * .75),
                      replace = FALSE)
varietyFeedbackDTTraining <- varietyFeedback[sampleSetDT, ]
# Remove ranking to create different decision trees
# Comment out to create 95.9% accurate DT
varietyFeedbackDTTraining <- varietyFeedbackDTTraining %>%
  select(-Ranking)

# Calculate Selected probability in training dataset
summary(varietyFeedbackDTTraining$Selected)
print(86/295)

# Put remaining records in testing tibble
varietyFeedbackDTTesting <- varietyFeedback[-sampleSetDT, ]
# Remove ranking to create different decision trees
# Comment out to create 95.9% accurate DT
varietyFeedbackDTTesting <- varietyFeedbackDTTesting %>%
  select(-Ranking)

# Train decision tree model
varietyFeedbackDTModel <- rpart(formula = Selected ~ .,
                                method = "class",
                                cp = .01,
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
# Train decision tree model
varietyFeedbackDTModel <- rpart(formula = Selected ~ .,
                                method = "class",
                                cp = .005,
                                data = varietyFeedbackDTTraining)

# Display DT plot
rpart.plot(varietyFeedbackDTModel)

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
set.seed(545)
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
  
# Convert into 0/1 predictions
varietyFeedbackNeuralNetPrediction <- 
  ifelse(varietyFeedbackNeuralNetProbability$net.result > 0.05, 1, 0)

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