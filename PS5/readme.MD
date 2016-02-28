# Implementation of a Classification Tree

Implements a classification tree that supports a variety of cost functions: cross-entropy, misclassification error, and Gini index.

## cTree.R

Defines functions for training a classification tree, and for using that tree to generate predictions.

#### Public functions:
	cTree()
	cTreePredict()
	cTreeShow()

## cTree_spam.R

Defines a method for testing the classification tree against the 'party' library, and demonstrating the change in training and test errors as tree depth increases.

When run directly, the script defines the above function, loads the 'spam' dataset, runs the function, and generates the results in a PDF.

#### Packages required:
	formula.tools
	caTools
	party
