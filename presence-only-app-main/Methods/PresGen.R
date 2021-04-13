observeEvent(input$gen_pres,{
  
  #cut intensity in stripes small enough to have integral smaller than max 32bit integer
  rv$parts <- ceiling(integral(rv$itsy)/2000000000)
  #inititate empty ppp
  rv$pres2 <- ppp()
  #generate poisson processes for each stripe and add them to the ppp
  for(i in 0:(rv$parts-1)){
    rv$pointer <- (rv$itsy$yrange[2]-rv$itsy$yrange[1])/rv$parts
    rv$stripe <- owin (xrange = c(rv$itsy$xrange[1],rv$itsy$xrange[2]), yrange = c(rv$itsy$yrange[1]+i*rv$pointer,rv$itsy$yrange[1]+(i+1)*rv$pointer))
    rv$pres2 <- superimpose.ppp(rv$pres2,rpoispp(as.im(rv$itsy[rv$stripe])))
  }
  
  rv$data_pres <- raster::extract(rv$absref,coords(rv$pres2)) %>% data.frame(.,type = 1)
  output$distPlot2 <- renderPlot({image(rv$itsy); points(coords(rv$pres2),col="green",cex=2,pch=20)},width = "auto")
  output$countsPres <- renderText({paste(npoints(rv$pres2),"samples generated")})
})

#generate absences
observeEvent(input$gen_abs,{
  rv$absrefnew <- rv$absref
  if(!is.null(rv$pres2) & !is.na(input$num_abs) & input$num_abs>0 ){
    
    rv$abs      <- randomPoints(rv$absref,n = input$num_abs, p=coords(rv$pres2))
    rv$data_abs <- raster::extract(rv$absref,rv$abs) %>% data.frame(.,type = 0)
    output$distPlot2 <- renderPlot({image(rv$itsy);
      points(coords(rv$pres2),col="green",cex=2,pch=20);
      points(rv$abs,col="red",cex=2,pch=20)},width = "auto")
    updateNumericInput(session,"LR_abs",value = input$num_abs)
  }
})

#change calculation variables to the selected subset, restore default if unselected
#subset = selected subset as owin, whole data is default
#trimmedsubs = data trimmed by the subset as image, intensity as image is default
#extentcut = calculation variable to cut the rasterstack data
#absrefnew = subsetted data as rasterstack, whole data is default
#trimmedsubspres/abs = list of subsetted data at the presence/absence points, default are all points
observeEvent(input$subselect_Global,{
  rv$subset <- owin(xrange = c(as.integer(input$subset_selected_Global$xmin),as.integer(input$subset_selected_Global$xmax)), yrange = c(as.integer(input$subset_selected_Global$ymin),as.integer(input$subset_selected_Global$ymax)))
  rv$trimmedsubs <- rv$itsy[rv$subset]
  rv$extentcut <- extent(c(as.integer(input$subset_selected_Global$xmin),as.integer(input$subset_selected_Global$xmax),as.integer(input$subset_selected_Global$ymin),as.integer(input$subset_selected_Global$ymax)))
  rv$absrefnew <- crop(rv$absref,rv$extentcut)
  rv$trimmedsubspres <- raster::extract(rv$absrefnew,coords(rv$pres2)) %>% data.frame(.,type = 1) %>% na.omit()
  rv$trimmedsubsabs <- raster::extract(rv$absrefnew,rv$abs) %>% data.frame(.,type = 0) %>% na.omit()
})
observeEvent(input$unselect_Global,{
  rv$subset <- owin(xrange = c(as.integer(rv$itsy$xrange[1]),as.integer(rv$itsy$xrange[2])), yrange = c(as.integer(rv$itsy$yrange[1]),as.integer(rv$itsy$yrange[2])))
  rv$trimmedsubs <- rv$itsy
  rv$absrefnew <- rv$absref
  rv$trimmedsubspres <-rv$data_pres
  rv$trimmedsubsabs <- rv$data_abs
})

#create reference point
observeEvent(input$xloc|input$yloc|input$set_ref,{
  
  if(!is.null(rv$itsy) & !is.na(min(input$xloc,input$yloc)) & min(input$xloc,input$yloc)>0 ){
    
    output$refPt <- renderPlot({image(rv$itsy);
      points(rv$itsy$xcol[input$xloc],rv$itsy$yrow[input$yloc],col="green",pch=20,cex = 3)
    },width = "auto")
    
  }
})
