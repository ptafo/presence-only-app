rv$functions <- list()


#add custom function to ipp method
observeEvent(input$addfctn,{
  rv$functions <- c(rv$functions,input$fctn)
})
#rv$ignoreIPP <- FALSE
#observeEvent(input$subselect,{
#  rv$extentcutIPP <- extent(c(as.integer(input$subset_selected$xmin),as.integer(input$subset_selected$xmax),as.integer(input$subset_selected$ymin),as.integer(input$subset_selected$ymax)))
#  rv$ignoreIPP <- TRUE
#  rv$subsetIPP <- owin(xrange = c(as.integer(input$subset_selected$xmin),as.integer(input$subset_selected$xmax)), yrange = c(as.integer(input$subset_selected$ymin),as.integer(input$subset_selected$ymax)))
#})

#run ipp
observeEvent(input$IPP_fit,{
  #subsetter(IPP)
  #if(class(rv$subset) != "owin"){
   # rv$trimmedsubsIPP <- rv$itsy
  #}
  #else if(typeof(input$subset_selected_Global) != "list" || rv$ignoreIPP == TRUE) {
   # rv$trimmedsubsIPP <- rv$itsy[rv$subset]
  #}
  #else{
  #  rv$trimmedsubsIPP <- rv$trimmedsubs
  #}
  withProgress(message = 'Model fitting', value = 0, {   
    rv$ipp.fit            <- ppm(eval(parse(text=input$IPP_model)),data = rv$mydata,nd=input$IPP_grid, correction = input$correction, rboard = input$corrsize, use.gam = input$gam, method = input$method, forcefit = input$force, emend = input$emend, clipwin = rv$subset)
    rv$fullfit            <- predict(rv$ipp.fit, as.owin(rv$itsy))
    incProgress(1/2)
    output$IPP_predict <- renderPlot({par(mfrow=c(1,4));
      plot(rv$itsy,main = "original");
      plot(as.im(trimmedsubsIPP),main = "original subset");
      plot(predict(rv$ipp.fit),main = "subset prediction");
      plot(rv$fullfit,main = "prediction")
    },width = "auto")
    output$IPP_output  <- renderPrint({print(rv$ipp.fit)})
    output$IPP_quad  <- renderPrint({rv$ipp.fit$Q})
    incProgress(1/2)
    Sys.sleep(0.5)
  })})