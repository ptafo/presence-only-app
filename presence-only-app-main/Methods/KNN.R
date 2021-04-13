#Generate absences

observeEvent(input$KNN_abs,{
  if(!is.null(rv$pres2) & !is.na(input$KNN_abs) & input$KNN_abs>0 ){
    
    rv$abs      <- randomPoints(rv$absrefnew,n = input$KNN_abs, p=coords(rv$pres2))
    rv$data_abs <- raster::extract(rv$absref,rv$abs) %>% data.frame(.,type = 0)
    rv$trimmedsubsabs <- raster::extract(rv$absrefnew,rv$abs) %>% data.frame(.,type = 0) %>% na.omit()
    output$distPlot2 <- renderPlot({image(rv$itsy);
      points(coords(rv$pres2),col="green",cex=2,pch=20);
      points(rv$abs,col="red",cex=2,pch=20)},width = "auto")
  }
})
#rv$ignoreKNN <- FALSE
#observeEvent(input$subselect_KNN,{
#  rv$extentcutKNN <- extent(c(as.integer(input$subset_selected_KNN$xmin),as.integer(input$subset_selected_KNN$xmax),as.integer(input$subset_selected_KNN$ymin),as.integer(input$subset_selected_KNN$ymax)))
#  rv$ignoreKNN <- TRUE
#  rv$subsetKNN <- owin(xrange = c(as.integer(input$subset_selected_KNN$xmin),as.integer(input$subset_selected_KNN$xmax)), yrange = c(as.integer(input$subset_selected_KNN$ymin),as.integer(input$subset_selected_KNN$ymax)))
#})

#run knn
observeEvent(input$KNN_fit,{
  #subsetter(KNN)
  #if(class(rv$subset) != "owin"){
    #rv$trimmedsubspres <- rv$data_pres
    #rv$trimmedsubsabs <- rv$data_abs
    #rv$absrefnew <- rv$absref
    #rv$trimmedsubs <- as.owin(rv$itsy)
  #}
  #else if(typeof(input$subset_selected_Global) != "list" || (rv$ignoreKNN == TRUE)){
    #rv$absrefnew <- crop(rv$absref,extentcut)
    #rv$trimmedsubspres <- raster::extract(rv$absrefnew,coords(rv$pres2)) %>% data.frame(.,type = 1) %>% na.omit()
    #rv$trimmedsubsabs <- raster::extract(rv$absrefnew,rv$abs) %>% data.frame(.,type = 0) %>% na.omit()
    #rv$trimmedsubs <- rv$subset
  #}
  if(!is.null(rv$data_abs)){
    
    withProgress(message = 'Model fitting', value = 0, {
      
      pred_KNN  <- rv$absrefnew[[1]]
      pred_KNNfull  <- rv$absref[[1]]
      
      #tmp       <- as.matrix(model.frame(eval(parse(text=input$KNN_model))[-2],data.frame(values(envS)),na.action = NULL ))
      tmp       <- as.matrix(values(rv$absrefnew)[,input$KNN_response])
      tmpfull       <- as.matrix(values(rv$absref)[,input$KNN_response])
      tmpNNA    <- apply(!is.na(tmp), 1, all)
      tmpNNAfull    <- apply(!is.na(tmpfull), 1, all)
      train     <- as.matrix(rbind(rv$trimmedsubspres,rv$trimmedsubsabs)[,input$KNN_response])
      test      <- as.matrix(tmp[tmpNNA,])
      testfull      <- as.matrix(tmpfull[tmpNNAfull,])
      if(input$KNN_scale){
        train <- scale(train)
        test  <- scale(test)
        testfull  <- scale(testfull)
      }
      
      
      # KNN  <- class::knn(train = model.frame(eval(parse(text=input$KNN_model)),rbind(rv$data_pres,rv$data_abs))[-1],
      #                       test = tmp[tmpNNA,],
      #                       cl = model.frame(eval(parse(text=input$KNN_model)),rbind(rv$data_pres,rv$data_abs))[,
      #                                                                                1],
      #                       k=input$KNN_K,
      #                       prob = 1)
      
      KNN  <- class::knn(train = train,
                         test = test,
                         cl = rbind(rv$trimmedsubspres,rv$trimmedsubsabs)[, "type"],
                         k=input$KNN_K,
                         prob = 1)
      KNNfull  <- class::knn(train = train,
                             test = testfull,
                             cl = rbind(rv$trimmedsubspres,rv$trimmedsubsabs)[, "type"],
                             k=input$KNN_K,
                             prob = 1)
      
      p  <- attributes(KNN)
      pfull  <- attributes(KNNfull)
      p$prob[KNN == min(p$levels)] <- 1- p$prob[KNN == min(p$levels)]
      pfull$prob[KNNfull == min(pfull$levels)] <- 1- pfull$prob[KNNfull == min(pfull$levels)]
      values(pred_KNN)[tmpNNA] <- p$prob
      values(pred_KNNfull)[tmpNNAfull] <- pfull$prob
      
      pred_KNN  <- as.im(apply(values(pred_KNN,format="matrix"),2,rev))
      pred_KNNfull  <- as.im(apply(values(pred_KNNfull,format="matrix"),2,rev))
      logit_KNN <- log(pred_KNN/(1-pred_KNN))
      logit_KNNfull <- log(pred_KNNfull/(1-pred_KNNfull))
      
      incProgress(1/2)
      output$KNN_predict <- renderPlot({par(mfrow=c(1,4));
        plot(rv$itsy,main = "original");
        plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original - reference");
        plot(logit_KNN-logit_KNN[input$yloc,input$xloc],main = "subset prediction");
        plot(logit_KNNfull-logit_KNNfull[input$yloc,input$xloc],main = "prediction")
      },width = "auto")
      
      
      
      incProgress(1/2)
      Sys.sleep(0.5)
      
    })}
  
})