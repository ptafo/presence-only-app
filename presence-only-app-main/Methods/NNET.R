
#Generate absences

observeEvent(input$NNET_abs,{
  if(!is.null(rv$pres2) & !is.na(input$NNET_abs) & input$NNET_abs>0 ){
    
    rv$abs      <- randomPoints(rv$absrefnew,n = input$NNET_abs, p=coords(rv$pres2))
    rv$data_abs <- raster::extract(rv$absref,rv$abs) %>% data.frame(.,type = 0)
    rv$trimmedsubsabs <- raster::extract(rv$absrefnew,rv$abs) %>% data.frame(.,type = 0) %>% na.omit()
    output$distPlot2 <- renderPlot({image(rv$itsy);
      points(coords(rv$pres2),col="green",cex=2,pch=20);
      points(rv$abs,col="red",cex=2,pch=20)},width = "auto")
  }
})
#rv$ignoreNNET <- FALSE
#observeEvent(input$subselect_NNET,{
#  rv$extentcutNNET <- extent(c(as.integer(input$subset_selected_NNET$xmin),as.integer(input$subset_selected_NNET$xmax),as.integer(input$subset_selected_NNET$ymin),as.integer(input$subset_selected_NNET$ymax)))
#  rv$ignoreNNET <- TRUE
#  rv$subsetNNET <- owin(xrange = c(as.integer(input$subset_selected_NNET$xmin),as.integer(input$subset_selected_NNET$xmax)), yrange = c(as.integer(input$subset_selected_NNET$ymin),as.integer(input$subset_selected_NNET$ymax)))
#})

#check if softmax or censored is selected, if yes use factor function on data later
rv$methodCheck <- FALSE
observeEvent(input$NNET_method,{
  if(input$NNET_method == "softmax" || input$NNET_method == "censored" ){
    rv$methodCheck <- TRUE 
  }
  else{
    rv$methodCheck <- FALSE
  }
})
observeEvent(input$NNET_fit,{
  #subsetter(NNET)
  #if(class(rv$subset) != "owin"){
    #rv$trimmedsubspres <- rv$data_pres
    #rv$trimmedsubsabs <- rv$data_abs
    #rv$absrefnew <- rv$absref
    #rv$trimmedsubs <- as.owin(rv$itsy)
  #}
  #else if(typeof(input$subset_selected_Global) != "list" || (rv$ignoreNNET == TRUE)){
    #rv$absrefnew <- crop(rv$absref,extentcut)
    #rv$trimmedsubspres <- raster::extract(rv$absrefnew,coords(rv$pres2)) %>% data.frame(.,type = 1) %>% na.omit()
    #rv$trimmedsubsabs <- raster::extract(rv$absrefnew,rv$abs) %>% data.frame(.,type = 0) %>% na.omit()
    #rv$trimmedsubs <- rv$subset
  #}
  
  #run nnet
  
  if(!is.null(rv$data_abs)){
    
    withProgress(message = 'Model fitting', value = 0, {
      if(rv$methodCheck){
        rv$NNETinput <- rbind(as.data.frame(raster::extract(factor(rv$absrefnew),coords(rv$pres2)) %>% data.frame(.,type = 1) %>% na.omit()),as.data.frame(raster::extract(factor(rv$absrefnew),rv$abs) %>% data.frame(.,type = 0) %>% na.omit()))
      }
      else{
        rv$NNETinput <- rbind(rv$trimmedsubspres,rv$trimmedsubsabs)
      }
      pred_NNET  <- rv$absrefnew[[1]]
      pred_NNETfull  <- rv$absref[[1]]
      tmp       <- as.matrix(values(rv$absrefnew))
      
      NNET      <- nnet(eval(parse(text=input$NNET_model)),data = rv$NNETinput ,size =input$NNET_size, rang =input$NNET_rang,
                        decay = input$NNET_decay, maxit =input$NNET_maxit, lineout = (input$NNET_method == "lineout"), entropy = (input$NNET_method == "entropy"), softmax = (input$NNET_method == "softmax"), censored = (input$NNET_method == "censored"))
      
      values(pred_NNET)<- predict(NNET,tmp)
      values(pred_NNETfull)<- predict(NNET,as.matrix(values(rv$absref)))
      
      pred_NNET  <- as.im(apply(values(pred_NNET,format="matrix"),2,rev))
      pred_NNETfull  <- as.im(apply(values(pred_NNETfull,format="matrix"),2,rev))
      logit_NNET <- log(pred_NNET/(1-pred_NNET))
      logit_NNETfull <- log(pred_NNETfull/(1-pred_NNETfull))
      
      incProgress(1/2)
      output$NNET_predict <- renderPlot({par(mfrow=c(1,4));
        plot(rv$itsy,main = "original");
        plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original - reference");
        plot(logit_NNET-logit_NNET[input$yloc,input$xloc],main = "subset prediction");
        plot(logit_NNETfull-logit_NNETfull[input$yloc,input$xloc],main = "subset prediction");
      },width = "auto")
      
      
      
      incProgress(1/2)
      Sys.sleep(0.5)
      
    })}
  
})