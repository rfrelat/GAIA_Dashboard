# Define background calculation and plots for application
shinyServer(function(input, output, session) {
  
  #close the session when the app closes
  session$onSessionEnded(stopApp)
  
  # Tab 2: Country data
  output$mapPH <- renderLeaflet({    
    map_ph(PH[[input$countSSA]], pal = palph)
  })
  
  output$alluPH <- render_parcats({
    a <- alluvial_parcats(tab2b, input$countSSA, 
                     pal = palph)
    a$x$layout <- list(margin=list(b=10,l=30,t=20, r=30),
                       paper_bgcolor='transparent')
    return(a)
  })
  
  output$tablePH <- renderDT({
    DT::datatable(summaryPH(tab2, input$countSSA),
                  options = list(pageLength = 3,
                  lengthMenu = c(3, 5, 10, 20)))}
  )
  
  output$downloadPH <- downloadHandler(
    filename = function() {
      paste("GAIA_PHdata-", input$countSSA, ".csv", sep="")
    },
    content = function(file) {
      write.csv(summaryPH(tab2, input$countSSA), file)
    }
  )
  
  # Tab 3: Lime requirement
  output$mapLime <- renderLeaflet({ 
    if (input$selLime=="Cochrane"){
      p <- map_lime(cochrane[[input$country]], 
                    gadm36[[input$country]], 
                    met = input$selLime, pal = pallime)
    } else {
      p <- map_lime(kamprath[[input$country]], gadm36[[input$country]], 
                    pal = pallime, met = input$selLime)
    }
    return(p)
  })
  
  output$donutLime <- renderPlotly({           
    fig <- donut_lime(tab3, input$country, input$selLime)
  })
  
  output$tableLime <- renderDT({
    DT::datatable(summaryLime(tab3, input$country),
                  options = list(pageLength = 3,
                                 lengthMenu = c(3, 5, 10, 20)))}
  )
  
  output$downloadLime <- downloadHandler(
    filename = function() {
      paste("GAIA_Limedata-",input$country, ".csv", sep="")
    },
    content = function(file) {
      write.csv(summaryLime(tab3, input$country), file)
    }
  )
  
  # Tab 4: Return on investment
  # UI: select province based on country
  output$province <- renderUI({
    tabS <- tab3[tab3$country %in% input$country,]
    selectInput('selProvince', h5('Select the province'), 
                c("All", sort(unique(tabS$province))),
                selected = "All")
  })
  
  output$mapROI <- renderLeaflet({  
    lay <- paste0("hp_", input$selPrice, "_roi_resid")
    map_roi(raster(roi[[input$country]], layer=lay), 
                  gadm36[[input$country]], pal = palroi)
  })
  
  output$barROI <- renderPlotly({
    tabS <- tab4[[input$selPrice]]
    if (input$selProvince=="All"){
      tabS <- tabS[tabS$country %in% input$country,]
    } else {
      tabS <- tabS[tabS$province %in% input$selProvince,]
    }
    barroi(tabS, pal = palroi)
  })
  
  output$tableROI <- renderDT({
    DT::datatable(summaryROI(tab4[[input$selPrice]], input$country,
                             input$selProvince),
                  options = list(pageLength = 3,
                                 lengthMenu = c(3, 5, 10, 20)))}
  )
  
  output$downloadROI <- downloadHandler(
    filename = function() {
      paste("GAIA_ROIdata-",input$selPrice,"USD-",input$country, ".csv", sep="")
    },
    content = function(file) {
      write.csv(summaryROI(tab4[[input$selPrice]], input$country), file)
    }
  )
})
