#Generate absences

observeEvent(input$CT_abs,{
  if(!is.null(rv$pres2) & !is.na(input$CT_abs) & input$CT_abs>0 ){
    
    rv$abs      <- randomPoints(rv$absrefnew,n = input$CT_abs, p=coords(rv$pres2))
    rv$data_abs <- raster::extract(rv$absref,rv$abs) %>% data.frame(.,type = 0)
    rv$trimmedsubsabs <- raster::extract(rv$absrefnew,rv$abs) %>% data.frame(.,type = 0) %>% na.omit()
    output$distPlot2 <- renderPlot({image(rv$itsy);
      points(coords(rv$pres2),col="green",cex=2,pch=20);
      points(rv$abs,col="red",cex=2,pch=20)},width = "auto")
  }
})
#rv$ignoreCT <- FALSE
#observeEvent(input$subselect_CT,{
#  rv$extentcutCT <- extent(c(as.integer(input$subset_selected_CT$xmin),as.integer(input$subset_selected_CT$xmax),as.integer(input$subset_selected_CT$ymin),as.integer(input$subset_selected_CT$ymax)))
#  rv$ignoreCT <- TRUE
#  rv$subset <- owin(xrange = c(as.integer(input$subset_selected_CT$xmin),as.integer(input$subset_selected_CT$xmax)), yrange = c(as.integer(input$subset_selected_CT$ymin),as.integer(input$subset_selected_CT$ymax)))
#})

#run ct
observeEvent(input$CT_fit,{
  #if(class(rv$subset) != "owin"){
    #rv$trimmedsubspres <- rv$data_pres
    #rv$trimmedsubsabs <- rv$data_abs
    #rv$absrefnew <- rv$absref
    #rv$trimmedsubs <- as.owin(rv$itsy)
  #}
  #else if(typeof(input$subset_selected_Global) != "list" || (rv$ignoreCT == TRUE)){
    #rv$absrefnew <- crop(rv$absref,rv$extentcut)
    #rv$trimmedsubspres <- raster::extract(rv$absrefnew,coords(rv$pres2)) %>% data.frame(.,type = 1) %>% na.omit()
    #rv$trimmedsubsabs <- raster::extract(rv$absrefnew,rv$abs) %>% data.frame(.,type = 0) %>% na.omit()
    #rv$trimmedsubs <- rv$subset
  #}
  rv$pointsdata <- rbind(rv$trimmedsubspres,rv$trimmedsubsabs)
  if(!is.null(rv$data_abs)){
    
    withProgress(message = 'Model fitting', value = 0, {
      
      
      CT1      <- rpart(eval(parse(text=input$CT_model)),  method="class", data=rv$pointsdata,control = rpart.control(minsplit =1,minbucket=1, cp=input$CT_cp))
      pruneCT <- prune(CT1, cp =  CT1$cptable[which.min(CT1$cptable[,"xerror"]),"CP"])
      print(CT1$cptable[which.min(CT1$cptable[,"xerror"]),"CP"])
      pred_CT  <- 1-predict(rv$absref, CT1)
      pred_pruneCT <- 1-predict(rv$absref, pruneCT)
      pred_CT  <- as.im(apply(values(pred_CT,format="matrix"),2,rev))
      pred_pruneCT  <- as.im(apply(values(pred_pruneCT,format="matrix"),2,rev))
      logit_CT <- log(pred_CT/(1-pred_CT))
      logit_pruneCT <- log(pred_pruneCT/(1-pred_pruneCT))
      
      incProgress(1/2)
      output$CT_predict <- renderPlot({par(mfrow=c(1,4));
        plot(rv$itsy,main = "original");
        plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original - reference");
        plot(logit_CT-logit_CT[input$yloc,input$xloc],main = "prediction");
        plot(logit_pruneCT-logit_pruneCT[input$yloc,input$xloc],main = "best prune prediction");},width = "auto")
      output$CT_output  <- renderPlot({par(mfrow=c(1,4));
        plot(CT1, uniform=TRUE,
             main="Classification Tree")
        text(CT1, use.n=TRUE, all=TRUE, cex=2);}
      )
      
      # rpart.control(cp = input$CT_cp)
      
      incProgress(1/2)
      Sys.sleep(0.5)
      
    })}
  
})
