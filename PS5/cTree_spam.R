
#############################################################################
# cTree.R
#
# Matthew Sudmann-Day
# Barcelona GSE Data Science
#
# Performs a comparison of test and training errors between the classification
# tree algorithm in the script cTree.R and the library function party::ctree().
#
# Uses R packages:
#   formula.tools
#   party
#   caTools
#
# Public function: cTreeTest()
#############################################################################

# Activate this code if you do not have the prerequisite libraries.
#install.packages("formula.tools")
#install.packages("party")
#install.packages("caTools")

#############################################################################
# Public function cTreeTest() evaluates the test errors and training errors of
# this cTree algorithm, and compares it against party::ctree().  It loops through
# a number of maximum depths and produces a plot based on the errors measured.
#
# Parameters:
#   formula: an R-style formula to describe the label and the independent
#     columns in the data
#   data: a data frame containing the data from which both training and test
#     sets will be extracted
#   file: the filename or path to which a PDF of the results will be written
#   costFnc: the cost function to measure the fragmentation of the labels
#     (permitted values are "Entropy" (default), "ME", and "Gini") to be applied
#     to the cTree function only.  party::ctree() will use its own default.
#   minPoints: the minimum number of training observations permitted in a
#     single classification region (default=5).  This applies to cTree only.
#     party::cTree() will use its own default.
#
# Returns:  nothing
cTreeTest <- function(formula, data, file, trainRatio=0.7, costFnc="Entropy", minPoints=5)
{
  require(formula.tools)
  require(party)
  require(caTools)
  
  labelColumn <- get.vars(lhs(formula))
  
  split <- sample.split(data, trainRatio)
  train <- data[split,]
  test <- data[!split,]

  results <- data.frame(depth=NA, trainErrors=NA, testErrors=NA, comparisonTrainErrors=NA, comparisonTestErrors=NA)
  
  for (depth in (1:3)*5)
  {
    tree <- cTree(formula, data, depth, minPoints, costFnc)
    actualDepth <- tree$depth
    
    # if (actualDepth < depth)
    # {
    #   break
    # }
    
    preds <- cTreePredict(tree, train)
    trainErrors <- 1-mean(train$spam == preds$predLabels)
    
    preds <- cTreePredict(tree, test)
    testErrors <- 1-mean(test$spam == preds$predLabels)
    
    comparisonTree <- ctree(formula, train, controls=ctree_control(maxdepth=depth))
    
    preds <- predict(comparisonTree, train)
    comparisonTrainErrors <- 1-mean(train$spam == preds)
    
    preds <- predict(comparisonTree, test)
    comparisonTestErrors <- 1-mean(test$spam == preds)
    
    results <- rbind(results, c(depth, trainErrors, testErrors, comparisonTrainErrors, comparisonTestErrors))
    
    print(results)
    return(results)
    save(results, file="results2.RData")
  }
}


data <- read.csv("Spam/spambase.data")

data = rbind(head(data,100),tail(data,100))

x<-cTreeTest(spam ~ ., data, "cTree.pdf", trainRatio=0.2)

tree <- cTree(spam ~ ., data, 2, 10, "Entropy")



mean(data$spam==cTreePredict(tree,data)$predLabels)

cTreeTest(spam ~ ., data)



# data <- data.frame(stringsAsFactors=FALSE, rbind(
#   c('slashdot','USA','yes',18,'None'),
#   c('google','France','yes',23,'Premium'),
#   c('digg','USA','yes',24,'Basic'),
#   c('kiwitobes','France','yes',23,'Basic'),
#   c('google','UK','no',21,'Premium'),
#   c('(direct)','New Zealand','no',12,'None'),
#   c('(direct)','UK','no',21,'Basic'),
#   c('google','USA','no',24,'Premium'),
#   c('slashdot','France','yes',19,'None'),
#   c('digg','USA','no',18,'None'),
#   c('google','UK','no',18,'None'),
#   c('kiwitobes','UK','no',19,'None'),
#   c('digg','New Zealand','yes',12,'Basic'),
#   c('slashdot','UK','no',21,'None'),
#   c('google','UK','yes',18,'Basic'),
#   c('kiwitobes','France','yes',19,'Basic')))
# colnames(data) <- c('referer', 'country', 'read_faq', 'pages_visited', 'subscription')
# tree <- cTree(subscription ~ ., data, 2, 5)
# mean(data$subscription==cTreePredict(tree,data)$predLabels)
