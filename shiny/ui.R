library('shiny')
library('fbRanks')
library('XML')
library('plyr')
library('dplyr')

#load('C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/data/scores.Rda')
scores <- read.csv('C:/Users/Scibrokes Trading/Documents/GitHub/englianhu/Dixon-Coles1996/data/scores.csv')
#scores <- create.fbRanks.dataframes(scores)

shinyUI(fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #ad1d28;
      }
      body {
        background-color: #fbfef1;
      }
    "))
  ),

  headerPanel("Dixon & Coles 1996"),

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
