#Generate absences

observeEvent(input$ME_abs,{
  if(!is.null(rv$pres2) & !is.na(input$ME_abs) & input$ME_abs>0 ){
    
    rv$abs      <- randomPoints(rv$absrefnew,n = input$ME_abs, p=coords(rv$pres2))
    rv$data_abs <- raster::extract(rv$absref,rv$abs) %>% data.frame(.,type = 0)
    rv$trimmedsubsabs <- raster::extract(rv$absrefnew,rv$abs) %>% data.frame(.,type = 0) %>% na.omit()
    output$distPlot2 <- renderPlot({image(rv$itsy);
      points(coords(rv$pres2),col="green",cex=2,pch=20);
      points(rv$abs,col="red",cex=2,pch=20)},width = "auto")
  }
})
#rv$ignoreME <- FALSE
#observeEvent(input$subselect_ME,{
#  rv$extentcutME<- extent(c(as.integer(input$subset_selected2$xmin),as.integer(input$subset_selected2$xmax),as.integer(input$subset_selected2$ymin),as.integer(input$subset_selected2$ymax)))
#  rv$ignoreME <- TRUE
#  rv$subsetME <- owin(xrange = c(as.integer(input$subset_selected2$xmin),as.integer(input$subset_selected2$xmax)), yrange = c(as.integer(input$subset_selected2$ymin),as.integer(input$subset_selected2$ymax)))
#})

#run maxent

observeEvent(input$ME_fit,{
  #subsetter(ME)
  #if(class(rv$subset) != "owin"){
    #rv$trimmedsubspres <- rv$data_pres
    #rv$trimmedsubsabs <- rv$data_abs
    #rv$absrefnew <- rv$absref
  #}
  #else if(typeof(input$subset_selected_Global) != "list" || (rv$ignoreME == TRUE)){
    
    #rv$absrefnew <- crop(rv$absref,extentcut)
    #rv$trimmedsubspres <- raster::extract(rv$absrefnew,coords(rv$pres2)) %>% data.frame(.,type = 1) %>% na.omit()
    #rv$trimmedsubsabs <- raster::extract(rv$absrefnew,rv$abs) %>% data.frame(.,type = 0) %>% na.omit()
  #}
  
  
  if(!is.null(rv$data_abs)){
    
    withProgress(message = 'Model fitting', value = 0, {
      
      ME.x      <- model.frame(eval(parse(text=input$ME_model)),data = rbind(rv$trimmedsubspres,rv$trimmedsubsabs))
      #ME.p      <- model.frame(eval(parse(text=input$ME_model))[-3],data = rbind(rv$data_pres,rv$data_abs))[,1]
      
      #first path application folder, second path plot folder inside the app folder
      ME        <- maxent(ME.x[-1],ME.x[1], removeDuplicates = input$dupl, nbg = input$bckgrnd, path = "C:/Users/Marc/Downloads/presence-only-app-main", args = c(paste("jackknife=", toString(input$jackknife),sep=""), paste("randomseed=" ,toString(input$rndmseed),sep="") , paste("allowpartialdata=" ,toString(input$prtldata),sep=""), paste("randomtestpoints=" ,toString(input$testpoints),sep=""), paste("betamultiplier=" ,toString(input$betamult),sep=""), paste("replicates=" ,toString(input$replicates),sep=""), paste("maximumiterations=" ,toString(input$maxits),sep=""), paste("convergencethreshold=" ,toString(exp(-input$prtldata)),sep=""), paste("replicatetype=" ,toString(input$replicatetype),sep=""), paste("linear=" ,toString(input$linear),sep=""), paste("quadratic=" ,toString(input$quadratic),sep=""), paste("product=" ,toString(input$product),sep=""), paste("threshold=" ,toString(input$threshold),sep=""), paste("hinge=" ,toString(input$hinge),sep=""),"responsecurves=TRUE"))
      addResourcePath(prefix = "plots",directoryPath = "C:/Users/Marc/Downloads/presence-only-app-main/plots")
      pred_ME   <- predict(rv$absrefnew,ME)
      pred_MEfull <- predict(rv$absref,ME)
      
      
      
      pred_ME  <- as.im(apply(values(pred_ME,format="matrix"),2,rev))
      pred_MEfull  <- as.im(apply(values(pred_MEfull,format="matrix"),2,rev))
      logit_ME <- log(pred_ME/(1-pred_ME))
      logit_MEfull <- log(pred_MEfull/(1-pred_MEfull))
      
      incProgress(1/2)
      output$ME_predict <- renderPlot({par(mfrow=c(1,5));
        plot(rv$itsy,main = "original");
        plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original - reference");
        plot(logit_ME-logit_ME[input$yloc,input$xloc],main = "subset prediction");
        plot(logit_MEfull-logit_MEfull[input$yloc,input$xloc],main = "prediction");
        plot(ME,main = "variables");
      },width = "auto")
      htmlfile  <- list.files("C:/Users/Marc/Downloads/presence-only-app-main",pattern = "maxent.html",full.names = T)
      output$ME_output  <- renderUI(includeHTML(htmlfile[1]))
      
      
      
      incProgress(1/2)
      Sys.sleep(0.5)
      
    })}
  
})