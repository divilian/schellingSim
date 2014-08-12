
require(shiny)
require(shinyIncubator)
require(ggplot2)
require(scales)

source("schelling.R")

NUM_GEN <- 30

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
        if (input$run <= 0) return(NULL)
        sliderInput("animation","generation",
            min=1,max=NUM_GEN,step=1,value=1,
            animate=animationOptions(interval=200,loop=FALSE))
    })

    output$map <- renderPlot({
        the.plots <- sim.output()$plots
        return(the.plots[[input$animation]])
    })

    output$migrationPlot <- renderPlot({
        num.swaps <- sim.output()$perc.swaps
        ggplot(data=data.frame(gen=1:length(num.swaps),num.swaps)) +
            geom_line(aes(x=gen,y=num.swaps)) +
            xlab("Generation") +
            ylab("Migration") +
            ggtitle(paste0("Migration over time\n",
                "(Percentage of residents who moved in each generation)")) +
            scale_y_continuous(labels=percent)

    })
})
