# Discriminant Functions demonstrated using Shiny

This application generates a dataset containing three classifications of loan applications: Approved, Denied, and Undecided.  It separates these classifications with two separate discriminant functions and generates a graph showing the data
points and boundary lines.  The first boundary line splits denied loans from all others because, presumably, it is better to deny a loan that should be approved than it is to approve a loan that should be denied.
Second, the remaining points are split with another boundary that separates approved from undecided loans.  Note that this boundary line does not extend beyond the first boundary line so as to avoid creating ambiguous
regions.

The user can manipulate the mean and standard deviation of the two independent variables (PI ratio and solvency), see the clusters and the boundary lines change in real time, and also see the results updated on a confusion matrix.

Deployment is done in a manner typical for Shiny websites.

