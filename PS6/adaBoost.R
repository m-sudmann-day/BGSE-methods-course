
#############################################################################
# cTree.R
#
# Matthew Sudmann-Day
# Barcelona GSE Data Science
#
# Implements a classification tree that supports a variety of cost functions:
# cross-entropy, misclassification error, and Gini index.
#
# Uses R packages:
#   formula.tools
#
# Public functions: cTree(), cTreePredict(), cTreeShow()
#############################################################################

# Activate this code if you do not have the prerequisite libraries.
#install.packages("formula.tools")

#############################################################################
# A private enumeration of cost functions.

#############################################################################
# Public function cTree() builds a classification tree based on the formula,
# data, and hyperparameters passed by the caller.
#
# Parameters:
#   formula: an R-style formula to describe the label and the independent
#     columns in the data
#   data: a data frame containing the training data
#   depth: the maximum depth of split nodes in the tree, excludes leaf nodes
#   minPoints: the minimum number of training observations permitted in a
#     single classification region
#   costFnc: the cost function to measure the fragmentation of the labels
#     (permitted values are "Entropy" (default), "ME", and "Gini")
#
# Calls cTreeAux() to perform the recursive portion of the algorithm.
#
# Returns:
#   a classification tree, consisting of a recursive structure of lists
#   that describe the split and leaf nodes of the tree

adaBoost <- function(formula, data, depth, noTrees, test=NULL)
{
  require(assertthat)
  require(rpart)
  
  # Generate a model frame object to sort out the environment of the formula and
  # to allow us to add a weights column to the data without changing the meaning
  # of the formula in case the formula contains a '.' for all other variables.
  mf <- model.frame(formula, data)
  formula2 <- as.formula(mf)
  
  # Pull the true labels from the model frame.
  labels <- mf[,1]

  # Get a vector of unique labels.  Assert that it be of length 2.
  uniqueLabels <- unique(levels(factor(labels)))
  assert_that(length(uniqueLabels) == 2)

  # Initialize the weights, alpha (our model "scores"), and G (our list of models).
  N <- nrow(data)
  w <- rep(1/N, N)
  alpha <- rep(NA, noTrees)
  G <- list()
  
  # Loop to create the number of trees we need.
  for (m in 1:noTrees)
  {
    # Copy the weights vector into the model frame for use by rpart.
    mf$w<-w

    # Create a classification tree model using rpart.
    G[[m]] <- rpart(formula2, data=mf, weights=w, method="class",
                    control=rpart.control(maxdepth=depth))

    # Apply that model back to the training data to get predictions.
    preds <- predict(G[[m]], newdata=mf, type="class")
    preds <- as.numeric(levels(preds)[preds])
    
cat(paste(mean(preds==labels), "\n"))
    # Generate a vector of misclassifications.
    misclass <- ifelse(preds != labels, 1, 0)

    # Using the Adaboost.M1 algorithm, update the observation weights
    # based on our misclassifications.
    err <- sum(w * misclass)/sum(w)
    alpha[m] <- log((1-err)/err)
    w <- w * exp(alpha[m] * misclass)
  }

  if (is.null(test))
  {
    test <- data
  }

  # Initialize a vector of meta predictions.
  metaPreds <- rep(0, nrow(test))

  # Loop through all the trees generating predictions from each model.
  for (m in 1:noTrees)
  {
    # Generate predictions from model m on our test data.
    preds <- predict(G[[m]], newdata=test, type="class")

    # Map these predictions into {-1, 1}.
    posNegPreds <- ifelse(preds == uniqueLabels[1], -1, 1)

    # Update the meta predictions.
    metaPreds <- metaPreds + (alpha[m] * posNegPreds)
  }

  # Map these meta-predictions back from {-1, 1} to their original values.
  metaPreds <- ifelse(metaPreds < 0, uniqueLabels[1], uniqueLabels[2])
  
  # Return our results.
  return(list(predLabels=metaPreds))
}

data <- read.csv("../PS5/Spam/spambase.data")
formula <- factor(spam) ~ .#word_freq_make+word_freq_address+word_freq_all+word_freq_3d+word_freq_over
preds <- adaBoost(formula, data, 30, 10)
mean(as.numeric(preds$predLabels)==data$spam)

require(caTools)
data <- read.csv("../PS5/Spam/spambase.data")
spl <- sample.split(0.5)
train <- data[spl,]
test <- data[!spl,]
preds <- adaBoost(factor(spam) ~ ., train, 30, 10, test)
mean(as.numeric(preds$predLabels)==test$spam)

require(caTools)
data <- read.csv("../PS5/Spam/spambase.data")
spl <- sample.split(data, 0.5)
train <- data[spl,]
test <- data[!spl,]
formula=factor(spam) ~ .
data=train
depth=30
noTrees=10
nrow(test)
nrow(train)
