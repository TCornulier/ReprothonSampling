# Shiny server for sampling from Stackoverflow R questions
library(shiny)

source('global.R')

# Define server logic
shinyServer(function(input, output) {

    inputVal <- reactive({
        list(needupdating= input$update)
    })

	observeEvent(input$update, {
        output$sampleID<- renderUI({
            new.sample<- fn.sample(inputVal())
            url<- h3(a(new.sample, href= new.sample, target= "_blank"))
            HTML(paste(url))
        })

        output$T1<- renderText("Your Fresh New Sample has landed!")

        output$T2<- renderText("(link will open in a new tab)")

    })

	# observeEvent(input$update, {
    #     output$sampleID<- renderText({
    #         print(fn.sample(inputVal()))
    #     })
    # })

})

