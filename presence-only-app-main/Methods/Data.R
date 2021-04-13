
#check hich kind of data has been sselected and initiate calculation variables and create correlationplo, boxplot and image plots
# mydata = selected data as images
#absref = selected data as rasterstack
observeEvent(input$dataset,
             {
               if(input$dataset == "Load Excel Data..."){
                 my_data <- read_excel(file.choose())
                 rv$mydata <<- as.im(as.data.frame(my_data))
                 rv$absref <<- raster::stack(lapply(rv$mydata,raster))
                 attach(rv$mydata)
                 output$result <- renderText({
                   paste("Your")
                 })
                 correlation <- cor(as.data.frame(rv$absref), use="pairwise.complete.obs")
                 output$corrplot <- renderPlot({ggcorrplot(correlation,tl.srt = 270)},res = 100)
                 output$boxPlot <- renderPlot({ggplot(stack(as.data.frame(raster::stack(lapply(rv$mydata,raster)))), aes(x=ind,y=log(values)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))},res = 100)
                 output$distPlot <- renderPlot({plot(raster::stack(lapply(rv$mydata,raster)))},res = 100)
                 rv$absref <<- raster::stack(lapply(rv$mydata,raster))
               }
               else if(input$dataset == "Load TIFF File..."){
                 rv$mydata <<- raster::stack(choose.files())
                 rv$absref <<- rv$mydata
                 output$result <- renderText({
                   paste("Your")
                 })
                 correlation <- cor(as.data.frame(rv$mydata), use="pairwise.complete.obs")
                 output$corrplot <- renderPlot({ggcorrplot(correlation,tl.srt = 270)},res = 100)
                 output$boxPlot <- renderPlot({ggplot(stack(as.data.frame(rv$mydata)), aes(x=ind,y=log(values)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))},res = 100)
                 output$distPlot <- renderPlot({plot(rv$mydata)},res = 100)
               }
               else if(input$dataset =="Hessen Data"){
                 rv$mydata <<- geo_Hessen_im
                 rv$absref <<- geo_Hessen_stack
                 correlation <- cor(as.data.frame(rv$absref), use="pairwise.complete.obs")
                 output$corrplot <- renderPlot({ggcorrplot(correlation,tl.srt = 270)},res = 100)
                 output$boxPlot <- renderPlot({ggplot(stack(as.data.frame(rv$absref)), aes(x=ind,y=log(values)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))},res = 100)
                 output$distPlot <- renderPlot({plot(geo_Hessen)},res = 100)
                 
               }
               else{
                 rv$mydata <<- envI
                 rv$absref <<- envS
                 output$result <- renderText({
                   paste("The" , input$dataset)
                 })
                 correlation <- cor(as.data.frame(rv$absref), use="pairwise.complete.obs")
                 output$corrplot <- renderPlot({ggcorrplot(correlation,tl.srt = 270)},res = 100)
                 output$boxPlot <- renderPlot({ggplot(stack(as.data.frame(envS)), aes(x=ind ,y=log(values)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))},res = 100)
                 output$distPlot <- renderPlot({plot(envS)},res = 100)
               }
             })



# input ui

observeEvent(input$plus,{
  speicher <- paste(input$input,"+")
  updateTextInput(session, "input", value=speicher)
})
observeEvent(input$minus,{
  speicher <- paste(input$input,"-")
  updateTextInput(session, "input", value=speicher)
})
observeEvent(input$multiplication,{
  speicher <- paste(input$input,"*")
  updateTextInput(session, "input", value=speicher)
})
observeEvent(input$division,{
  speicher <- paste(input$input,"/")
  updateTextInput(session, "input", value=speicher)
})
# intensity function
observeEvent(input$vsleItsy,
             {rv$itsy <- eval(parse(text=input$input))
             #initiate a subset equal to the whole data when intenity is created
             rv$subset <- owin(xrange = c(as.integer(rv$itsy$xrange[1]),as.integer(rv$itsy$xrange[2])), yrange = c(as.integer(rv$itsy$yrange[1]),as.integer(rv$itsy$yrange[2])))
             #change stepsize for the large Hessen dataset
             if(input$dataset =="Hessen Data"){
               rv$itsy$xstep <- 0.5
               rv$itsy$ystep <- 0.5
             }
             #plots for the different functions, global allows a selection for a subset
             output$distPlot1 <- renderPlot({image(rv$itsy)},width = "auto")
             output$distPlot2 <- renderPlot({image(rv$itsy)},width = "auto")
             output$subsetselect <- renderPlot({plot.owin(as.owin(rv$itsy[rv$subset])) ; points(coords(rv$pres2),col="green",cex=0.5,pch=20)},width = "auto")
             output$subsetselect2 <- renderPlot({plot.owin(as.owin(rv$itsy[rv$subset])) ; points(coords(rv$pres2),col="green",cex=0.5,pch=20);points(rv$abs,col="red",cex=0.5,pch=20)},width = "auto")
             output$subsetselect_LR <- renderPlot({plot.owin(as.owin(rv$itsy[rv$subset])) ; points(coords(rv$pres2),col="green",cex=0.5,pch=20);points(rv$abs,col="red",cex=0.5,pch=20)},width = "auto")
             output$subsetselect_CT <- renderPlot({plot.owin(as.owin(rv$itsy[rv$subset])) ; points(coords(rv$pres2),col="green",cex=0.5,pch=20);points(rv$abs,col="red",cex=0.5,pch=20)},width = "auto")
             output$subsetselect_RF <- renderPlot({plot.owin(as.owin(rv$itsy[rv$subset])) ; points(coords(rv$pres2),col="green",cex=0.5,pch=20);points(rv$abs,col="red",cex=0.5,pch=20)},width = "auto")
             output$subsetselect_Global <- renderPlot({plot.owin(as.owin(rv$itsy)) ; points(coords(rv$pres2),col="green",cex=0.5,pch=20);points(rv$abs,col="red",cex=0.5,pch=20)},width = "auto")
             output$subsetselect_NNET <- renderPlot({plot.owin(as.owin(rv$itsy[rv$subset])) ; points(coords(rv$pres2),col="green",cex=0.5,pch=20);points(rv$abs,col="red",cex=0.5,pch=20)},width = "auto")
             output$subsetselect_KNN <- renderPlot({plot.owin(as.owin(rv$itsy[rv$subset])) ; points(coords(rv$pres2),col="green",cex=0.5,pch=20);points(rv$abs,col="red",cex=0.5,pch=20)},width = "auto")
             })