# ui.R
#
# Matthew Sudmann-Day
# Barcelona GSE Data Science
#
# The UI component for the matching server.R.  See the comments in server.R.
#

library(shiny)

header <- headerPanel('Classification of Loan Applications')

inputs1 <- inputPanel(
  h3("Denied"),
  sliderInput('deniedMeanSolvency', 'Mean Solvency', 6, min = 0, max = 20),
  sliderInput('deniedMeanPIRatio', 'Mean PI Ratio', 150, min = 0, max = 250),
  sliderInput('deniedStDevSolvency', 'St. Dev. Solvency', 3, min = 0, max = 10),
  sliderInput('deniedStDevPIRatio', 'St. Dev. PI Ratio', 20, min = 0, max = 60)
)

inputs2 <- inputPanel(
  h3("Approved"),
  sliderInput('approvedMeanSolvency', 'Mean Solvency', 10, min = 0, max = 20),
  sliderInput('approvedMeanPIRatio', 'Mean PI Ratio', 100, min = 0, max = 250),
  sliderInput('approvedStDevSolvency', 'St. Dev. Solvency', 2, min = 0, max = 10),
  sliderInput('approvedStDevPIRatio', 'St. Dev. PI Ratio', 30, min = 0, max = 60)
)

inputs3 <- inputPanel(
  h3("Undecided"),
  sliderInput('undecidedMeanSolvency', 'Mean Solvency', 5, min = 0, max = 20),
  sliderInput('undecidedMeanPIRatio', 'Mean PI Ratio', 80, min = 0, max = 250),
  sliderInput('undecidedStDevSolvency', 'St. Dev. Solvency', 2, min = 0, max = 10),
  sliderInput('undecidedStDevPIRatio', 'St. Dev. PI Ratio', 25, min = 0, max = 60)
)

inputsRow <- fluidRow(
  column(4, inputs1),
  column(4, inputs2),
  column(4, inputs3))

plot <- plotOutput('plot')

table <- tableOutput('table')

br <- uiOutput('br')

page <- fluidPage(
  header,
  inputsRow,
  tags$br(),
  plot,
  tags$br(),
  table,
  tags$style(type="text/css", "#table td:first-child {font-weight:bold;}")
)

shinyUI(page)
