#Generate absences

observeEvent(input$LR_abs,{
  if(!is.null(rv$pres2) & !is.na(input$LR_abs) & input$LR_abs>0 ){
    
    rv$abs      <- randomPoints(rv$absrefnew,n = input$LR_abs, p=coords(rv$pres2))
    rv$data_abs <- raster::extract(rv$absref,rv$abs) %>% data.frame(.,type = 0)
    rv$trimmedsubsabs <- raster::extract(rv$absrefnew,rv$abs) %>% data.frame(.,type = 0) %>% na.omit()
    output$distPlot2 <- renderPlot({image(rv$itsy);
      points(coords(rv$pres2),col="green",cex=2,pch=20);
      points(rv$abs,col="red",cex=2,pch=20)},width = "auto")
    updateNumericInput(session,"num_abs",value = input$LR_abs)
  }
})
#rv$ignoreLR <- FALSE
#observeEvent(input$subselect_LR,{
#  rv$extentcutLR <- extent(c(as.integer(input$subset_selected_LR$xmin),as.integer(input$subset_selected_LR$xmax),as.integer(input$subset_selected_LR$ymin),as.integer(input$subset_selected_LR$ymax)))
#  rv$ignoreLR <- TRUE
#  rv$subsetLR <- owin(xrange = c(as.integer(input$subset_selected_LR$xmin),as.integer(input$subset_selected_LR$xmax)), yrange = c(as.integer(input$subset_selected_LR$ymin),as.integer(input$subset_selected_LR$ymax)))
#})

#run lr
observeEvent(input$LR_fit,{
  #if(class(rv$subset) != "owin"){
   #rv$subsetLR <- as.owin(rv$itsy)
    #rv$trimmedsubsLR <- rv$itsy
  #}
  #else if(typeof(input$subset_selected_Global) != "list" || (rv$ignoreME == TRUE)){
    #rv$subsetLR <- rv$subsetLR
    #rv$trimmedsubsLR <- rv$itsy[rv$subsetLR]
  #}
  #else{
    #rv$subsetLR <- rv$subset
    #rv$trimmedsubsLR <- rv$trimmedsubs
  #}
  if(!is.null(rv$data_abs)){
    rv$pointlist <- rbind(coords(rv$pres2),rv$abs)
    withProgress(message = 'Model fitting', value = 0, {
      incProgress(1/4)
      glm1       <- glm(eval(parse(text=input$LR_model)),family = binomial(link = "logit"),data = rbind(rv$data_pres,rv$data_abs), subset = inside.owin(rv$pointlist[,1],rv$pointlist[,2],rv$subset))
      incProgress(1/4)
      pred       <- predict(rv$absref, glm1)
      pred       <- as.im(apply(values(pred,format="matrix"),2,rev))
      incProgress(1/4)
      output$LR_predict <- renderPlot({par(mfrow=c(1,4));
        plot(rv$itsy,main = "original");
        plot(rv$trimmedsubs,main = "original subset");
        plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original");
        plot(pred-pred[input$yloc,input$xloc],main = "prediction")},width = "auto");
      output$LR_output  <- renderPrint({summary(glm1)})
      incProgress(1/4)
      
      
      
      Sys.sleep(0.5)
      
    })}
  
})