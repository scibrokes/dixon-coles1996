library(shiny)
library('fbRanks')
library('XML')
library('plyr')
<<<<<<< HEAD:shiny/ui.R
load('scores.Rda')
=======
load('~/scores.Rda')
>>>>>>> origin/master:ui.R
#scores <- read.csv('scores.csv')

# Define UI for dataset viewer application
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("Dixon & Coles 1996"),

  # Sidebar with controls to provide a caption, select a dataset, and 
  # specify the number of observations to view. Note that changes made
  # to the caption in the textInput control are updated in the output
  # area immediately as you type
  sidebarPanel(
    textInput("caption", "Caption:", "Data Summary"),
    selectInput("dataset", "Choose a dataset:", 
                choices = c("matches", "teams", "result")),
    numericInput("obs", "Number of observations to view:", 10)
  ),
  
  # Show the caption, a summary of the dataset and an HTML table with
  # the requested number of observations
  mainPanel(
    h3(textOutput("caption")), 
    verbatimTextOutput("summary"), 
    tableOutput("view")
  )
))
