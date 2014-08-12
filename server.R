
require(shiny)
require(shinyIncubator)
require(ggplot2)

source("schelling.R")

NUM_GEN <- 20

shinyServer(function(input,output,session) {

    sim.output <- reactive({
        if (input$run <= 0) return(NULL)
        isolate({
            withProgress(session, {
                run.sim(probRed=input$lotsRed/100,
                    probBlue=input$lotsBlue/100,
                    thresh=input$intolerance,
                    width=input$dimension,height=input$dimension,
                    numGen=NUM_GEN+1)
            }, min=0, max=NUM_GEN+1)
        })
    })

    output$plotSlider <- renderUI({
        sliderInput("animation","generation",
            min=1,max=NUM_GEN,step=1,value=1,
            animate=animationOptions(interval=200,loop=FALSE))
    })

    output$thePlot <- renderPlot({
        the.plots <- sim.output()$plots
        return(the.plots[[input$animation]])
    })
})
