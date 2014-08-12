
require(shiny)
require(shinyIncubator)


shinyUI(fluidPage(

    progressInit(),

    tags$head(
        tags$link(rel="stylesheet", type="text/css",
            href="schelling.css")
    ),

    titlePanel("Schelling Segregation Simulator"),

    sidebarLayout(

        sidebarPanel(h3("Simulation Parameters"),
          flowLayout(
            numericInput("dimension","City dimension (height & width)",
                min=5,max=50,step=1,value=40),
            numericInput("lotsRed","% lots red",min=0,max=100,value=30),
            numericInput("lotsBlue","% lots blue",min=0,max=100,value=30)
          ),
            sliderInput("intolerance","Intolerance",min=0,max=1,step=.01,
                value=.4),
            actionButton("run","Run!")
        ),

        mainPanel(
            verticalLayout(
                plotOutput("thePlot",width="600px",height="600px"),
                uiOutput("plotSlider")
            )
        )
    )
))
