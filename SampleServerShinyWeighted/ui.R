# Shiny ui for LM

library(shiny)

source('global.R')

# Define UI for the application

shinyUI(pageWithSidebar(

  # Application title
  titlePanel("REPROTHON 2021 - random sample server"),

  sidebarPanel(

      br(),
      actionButton("update" ,"Get a New Sample", icon("refresh"),
                 class = "btn btn-primary"),
      br(),
      br(),
      helpText("\n\n Once you have drawn a new sample, please process the question and enter the outcome in the", a(href= 'https://forms.gle/rT9bkKdV48ChRhjU7', "results form"), "before pressing the button again."),
      hr(),
      helpText("\n\n For more guidance, check out the How_to_Guide and Tutorials at", a(href= 'https://aberdeenstudygroup.github.io/studyGroup/Reprothon2021/', "https://aberdeenstudygroup.github.io/studyGroup/Reprothon2021/"), "."),
      hr(),
      br(),
      hr(),
      # h6(helpText("\n\n The source code for this app available at [insert link here]"),
      hr()

  ),


  mainPanel(
    tabsetPanel(
      tabPanel(title = 'Output',
           		h3(textOutput("T1")),
              br(),
           		uiOutput("sampleID"),
              br(),
              helpText(textOutput("T2"))
      )
    )

  )

))
