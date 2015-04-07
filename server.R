library("shiny")
library("igraph")

set.seed(666)

source("helper.R")

shinyServer(function(input, output, session) {
    # reactive utilities
    rValues <- reactiveValues()
    rValues$rumorSource <- NULL
    rValues$warnings <- NULL
    
    getGraphData <- reactive({
      fileInfo <- input$file
      if( is.null(fileInfo) )
        return(NULL)
      
      g <- loadGraph( fileInfo$datapath )
      if( vcount(g) > 1000 )
        return(NULL)
      return(g)
    })
    getPlotData <- reactive({
      if( is.null(rValues$rumorSource) || is.null(input$size) )
        return(NULL)
      
      g <- getGraphData()
      if(is.null(g))
        return(NULL)
      
      loadPlotData_Exp(g, rValues$rumorSource, input$size)
    })
    
    # dynamic UI
    observe({
      input$resetSrc
      
      g <- getGraphData()
      if(!is.null(g))
        rValues$rumorSource <- genSource(g)
    })
    observe({
      g <- getGraphData()
      
      if(!is.null(g))
        updateSliderInput(session, "size", min=1, max=vcount(g), value=1, step=1)
    })
    
    # output render
    output$control <- renderUI({
      if( input$plotArea == "Rumor Spreading" )
        sliderInput("iteration", "Iteration", min=1, max=input$size, value=1, step=1, ticks=FALSE,
                    animate=animationOptions(interval=1000, loop=TRUE))
      else
          checkboxGroupInput("centers", "Centrality measures", 
                             c("Betweeenness centrality" = 1,
                               "Closeness centrality" = 2,
                               "Degress centrality" = 3,
                               "Eccentricity" = 4,
                               "Eigenvector centrality" = 5,
                               "Kleinberg authority score" = 6,
                               "Rumor centrality" = 7,
                               "Subgraph centrality" = 8))
        
    })
    output$spreading <- renderPlot({
      if( is.null(input$file) )
        return(NULL)

      rSource <- "#000000"
      healthy <- "#1e90ff"
      infected <- "#ff0000"
      
      p <- getPlotData()
      rValues$warnings <- NULL
      if(is.null(p))
      {
        rValues$warnings <- "Loaded graph contains more than 1000 vertices! Please reset graph file!"
        return(NULL)
      }
      if( !p$g$connected )
        rValues$warnings <- "Loaded graph is not connected, therefore the maximum (not maximal) connected component is used instead."
      
      iter <- input$iteration
      
      vColor <- rep(healthy, vcount(p$g))
      vColor[p$inf[1]] <- rSource
      if(iter > 1)
        vColor[p$inf[2:iter]] <- infected
      
      eType <- 1
      if( input$infectedOnly )
      {
        vColor[-p$inf[1:iter]] <- NA
        eType <- ifelse(E(p$g) %in% E(p$g)[p$inf[1:iter] %--% p$inf[1:iter]], 1, 0)
      }
      
      plot(p$g, 
           vertex.size=3,
           vertex.label=NA,
           vertex.color=vColor,
           vertex.frame.color=NA,
           edge.lty=eType)
      
      legend("topleft", 
             legend=c("healthy","infected","true source"), 
             col=c(healthy, infected, rSource),
             pch=19,
             pt.cex=2,
             border="white")
    })
    
    output$inferring <- renderPlot({
      if( is.null(input$file) )
        return(NULL)
      
      centerToCalc <- input$centers
      progress <- shiny::Progress$new(session, min=1, max=length(centerToCalc)+3)
      on.exit(progress$close())
      progressCnt <- 2
      progress$set(message = "Initialization",
                   detail = "Initialize graph data...",
                   value = progressCnt)
      progressCnt <- progressCnt + 1
      
      healthy <- "#1e90ff"
      infected <- "#ff0000"
      
      centerColor <- c("black", "darkmagenta", "darkgreen", "darkorange", "deeppink", "darkkhaki", "darkgrey", "darkred", "darkslateblue")
      p <- getPlotData()
      rValues$warnings <- NULL
      if(is.null(p))
      {
        rValues$warnings <- "Loaded graph contains more than 1000 vertices! Please reset graph file!"
        return(NULL)
      }
      if( !p$g$connected )
        rValues$warnings <- "Loaded graph is not connected, therefore the maximum (not maximal) connected component is used instead."
      
      vColor <- rep(healthy, vcount(p$g))
      vFrame <- NA
      eType <- 1
      if(length(p$inf) > 1)
      {
        vColor[p$inf[-1]] <- infected
        if( input$infectedOnly )
        {
          vColor[p$inf] <- "white"
          vColor[-p$inf] <- NA
          vFrame <- rep(NA, vcount(p$g))
          vFrame[p$inf] <- "black"
          eType <- ifelse(E(p$g) %in% E(p$g)[p$inf %--% p$inf], 1, 0)
        }
      }
      GN <- induced.subgraph(p$g, p$inf)
      
      group <- list()
      group$center <- vector(mode="numeric", length=1)
      group$legend <- vector(mode="character", length=1)
      
      group$center[1] <- p$inf[1]
      group$legend[1] <- "true source"
      
      if("1" %in% centerToCalc)
      {
        progress$set(message = "Calculation in progress",
                     detail = "Calculate betweenness centrality...",
                     value = progressCnt)
        progressCnt <- progressCnt + 1
        betweenness <- betweenness(GN, directed=FALSE)
        betweennessC <- V(GN)$id[which(betweenness==max(betweenness))]
        group <- assignGroup(group, betweennessC, "betweenness center")
      }
      
      if("2" %in% centerToCalc)
      {
        progress$set(message = "Calculation in progress",
                     detail = "Calculate closeness centrality...",
                     value = progressCnt)
        progressCnt <- progressCnt + 1
        closeness <- closeness(GN)
        closenessC <- V(GN)$id[which(closeness==max(closeness))]
        group <- assignGroup(group, closenessC, "closeness center")
      }
      
      if("3" %in% centerToCalc)
      {
        progress$set(message = "Calculation in progress",
                     detail = "Calculate degree centrality...",
                     value = progressCnt)
        progressCnt <- progressCnt + 1
        degree <- degree(GN)
        degreeC <- V(GN)$id[which(degree==max(degree))]
        group <- assignGroup(group, degreeC, "degree center")
      }
      
      if("4" %in% centerToCalc)
      {
      progress$set(message = "Calculation in progress",
                   detail = "Calculate eccentricity...",
                   value = progressCnt)
      progressCnt <- progressCnt + 1
      eccentricity <- eccentricity(GN)
      eccentricityC <- V(GN)$id[which(eccentricity==min(eccentricity))]
      group <- assignGroup(group, eccentricityC, "jordan center")
      }
      
      if("5" %in% centerToCalc)
      {
        progress$set(message = "Calculation in progress",
                     detail = "Calculate eccentricity...",
                     value = progressCnt)
        progressCnt <- progressCnt + 1
        evcent <- evcent(GN)$vector
        evcentC <- V(GN)$id[which(evcent==max(evcent))]
        group <- assignGroup(group, evcentC, "eigenvector center")
      }
      
      if("6" %in% centerToCalc)
      {
        progress$set(message = "Calculation in progress",
                     detail = "Calculate kleinberg centrality...",
                     value = progressCnt)
        progressCnt <- progressCnt + 1
        kleinberg <- authority.score(GN)$vector
        kleinbergC <- V(GN)$id[which(kleinberg==max(kleinberg))]
        group <- assignGroup(group, kleinbergC, "kleinberg authority center")
      }
      
      if("7" %in% centerToCalc)
      {
        progress$set(message = "Calculation in progress",
                     detail = "Calculate rumor centrality...",
                     value = progressCnt)
        progressCnt <- progressCnt + 1
        if( vcount(GN)>150 )
        {
          rValues$warnings <- paste(isolate(rValues$warnings), "The size of rumor graph is larger than 150, rumor centrality will not be calculated due to technical reason.", sep="<br>")
        }
        else
        {
          rumor <- rumor.centrality(p$g,GN)
          rumorC <- V(GN)$id[which(rumor==max(rumor))]
          group <- assignGroup(group, rumorC, "rumor center")
        }
      }
      
      if("8" %in% centerToCalc)
      {
        progress$set(message = "Calculation in progress",
                     detail = "Calculate subgraph centrality...",
                     value = progressCnt)
        progressCnt <- progressCnt + 1
        subgraph <- subgraph.centrality(GN)
        subgraphC <- V(GN)$id[which(subgraph==max(subgraph))]
        group <- assignGroup(group, subgraphC, "subgraph center")
      }
      progress$set(message = "Plotting graph",
                   detail = "Plotting various centers",
                   value = progressCnt)
      
      vColor[group$center] <- centerColor[1:length(group$center)]
  
      plot(p$g, 
           vertex.size=3,
           vertex.label=NA,
           vertex.color=vColor,
           vertex.frame.color=vFrame,
           edge.lty=eType)
      
      legend("topleft",
             inset=-.05,
             legend=group$legend, 
             col=centerColor[1:length(group$center)],
             pch=19,
             pt.cex=2,
             border="white")
    })
    
    output$warnings <- renderUI({
        HTML(paste("<p style='color:red;font-weight:bold'>", rValues$warnings, "</p>", sep=""))
    })
})