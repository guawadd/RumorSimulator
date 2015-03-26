library("shiny")

shinyUI(fluidPage(
    titlePanel("Rumor Source Detection Simulator"),
    
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload graph data", accept="text/txt"),
        
        actionButton("resetSrc", "Reset rumor source"),
        checkboxInput("infectedOnly", "Show infected network only"),
        sliderInput("size", "Rumor size", min=1, max=1, value=1, step=1),
        
        tags$hr(),
        
        uiOutput("control"),
        
        tags$hr(),
        
        uiOutput("warnings"),
        
        tags$head(tags$style("#warnings{color: red;
                                 font-weight: bold;
                                 }"
            )
          )
        ),
      
      mainPanel(
        tabsetPanel(
          id = "plotArea",
          tabPanel("Rumor Spreading", plotOutput("spreading", width="1000px", height="1000px")),
          tabPanel("Source Inferring", plotOutput("inferring", width="1000px", height="1000px"))
          )
      )
    )
))