#Generate absences

observeEvent(input$RF_abs,{
  if(!is.null(rv$pres2) & !is.na(input$CT_abs) & input$CT_abs>0 ){
    
    rv$abs      <- randomPoints(rv$absrefnew,n = input$RF_abs, p=coords(rv$pres2))
    rv$data_abs <- raster::extract(rv$absref,rv$abs) %>% data.frame(.,type = 0)
    rv$trimmedsubsabs <- raster::extract(rv$absrefnew,rv$abs) %>% data.frame(.,type = 0) %>% na.omit()
    output$distPlot2 <- renderPlot({image(rv$itsy);
      points(coords(rv$pres2),col="green",cex=2,pch=20);
      points(rv$abs,col="red",cex=2,pch=20)},width = "auto")
  }
})
#rv$ignoreRF <- FALSE
#observeEvent(input$subselect_RF,{
#  rv$extentcutRF <- extent(c(as.integer(input$subset_selected_RF$xmin),as.integer(input$subset_selected_RF$xmax),as.integer(input$subset_selected_RF$ymin),as.integer(input$subset_selected_RF$ymax)))
#  rv$ignoreRF <- TRUE
#  rv$subset <- owin(xrange = c(as.integer(input$subset_selected_RF$xmin),as.integer(input$subset_selected_RF$xmax)), yrange = c(as.integer(input$subset_selected_RF$ymin),as.integer(input$subset_selected_RF$ymax)))
#})


#run rf
observeEvent(input$RF_fit,{
  if(class(rv$subset) != "owin"){
    rv$subset <- as.owin(rv$itsy)
  }
  if(!is.null(rv$data_abs)){
    rv$pointlist <- rbind(coords(rv$pres2),rv$abs)
    withProgress(message = 'Model fitting', value = 0, {
      
      
      rf1      <- randomForest(eval(parse(text=input$RF_model)),data = rbind(rv$data_pres,rv$data_abs),subset = inside.owin(rv$pointlist[,1],rv$pointlist[,2],rv$subset), importance = T,ntree= input$RF_ntrees, nodesize = input$nodesize, maxnodes = input$maxnodes)
      pred_rf  <- predict(rv$absref, rf1)
      pred_rf  <- as.im(apply(values(pred_rf,format="matrix"),2,rev))
      logit_rf <- log(pred_rf/(1-pred_rf))
      
      incProgress(1/2)
      output$RF_predict <- renderPlot({par(mfrow=c(1,3));
        plot(rv$itsy,main = "original");
        plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original - reference");
        plot(logit_rf-logit_rf[input$yloc,input$xloc],main = "prediction")},width = "auto")
      output$RF_output  <- renderPrint({print(rf1$forest)})
      
      
      incProgress(1/2)
      Sys.sleep(0.5)
      
    })}
  
})