library("ggplot2")
library(ggcorrplot)
library(tidyverse)
library("readxl")
library(tiff)
library(shiny)
library(spatstat)
library(raster)
library(dplyr)
library(ppmlasso)
library(RColorBrewer)
library(plyr)
library(rpart)
library(randomForest)
library(dismo)
library(nnet)

#########################################################
## BlueMountains 
#########################################################



# load the data and adjust to grid
data("BlueMountains")
BM <- BlueMountains 

# save environment data as images and as stacks

envI  <- as.im(BM$env)
envS  <- raster::stack(lapply(envI,raster))
variablen <- reactiveValues()
attach(envI)
setwd("C:/Users/Marc/Downloads/presence-only-app-main/Daten_Hessen")
geo_list  <- list.files(getwd(),pattern = ".tif",full.names = T)
geo       <- raster::stack(geo_list)
referenz <- focal(geo$Hessen100fact_soil, w=matrix(1/9, nc=3, nr=3),fun=mean,na.rm=1)
geo$Hessen100fact_LRT <- focal(geo$Hessen100fact_LRT, w=matrix(1/9, nc=3, nr=3),fun=mean,na.rm=1)
geo$Hessen100fact_LRT <- focal(geo$Hessen100fact_LRT, w=matrix(1/9, nc=3, nr=3),fun=mean,na.rm=1)
geo$Hessen100fact_LRT <- focal(geo$Hessen100fact_LRT, w=matrix(1/9, nc=3, nr=3),fun=mean,na.rm=1)
geo$Hessen100fact_LRT <- focal(geo$Hessen100fact_LRT, w=matrix(1/9, nc=3, nr=3),fun=mean,na.rm=1)
geo$Hessen100fact_LRT <- focal(geo$Hessen100fact_LRT, w=matrix(1/9, nc=3, nr=3),fun=mean,na.rm=1)
geo_Hessen <- mask(geo,referenz)
geo_Hessen_stack <- stack(geo_Hessen)
geo_Hessen_im <- as.im(as.data.frame(geo_Hessen_stack,xy=1))

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel(""),
    
    # Sidebar with a slider input for number of bins 
    
    tabsetPanel(
        tabPanel("Data",
                 
                 #Define Dataset Selection
                 selectInput("dataset", "Choose a database:",
                             list(`Examples` = list("BlueMountains", "Load Excel Data..." , "Load TIFF File...", "Hessen Data")
                                )),
                 
                 h2(textOutput("result") , "Dataset consists of ... "),
                 plotOutput("boxPlot",height= "400px"),
                 plotOutput("corrplot",height= "400px"),
                 plotOutput("distPlot",height = "800px"),
                 h1(),
                 h1("Generate intensity function"),
                 fluidRow(
                     column(10,
                            textInput("input","Intensity function",width = '100%',value = "exp(-1-rv$mydata$FC/10)"),
                            sidebarPanel(
                                actionButton("plus","+"),
                                actionButton("minus","-"),
                                actionButton("multiplication","x"),
                                actionButton("division","/"),
                                selectInput("dataset", "Choose a variable:",
                                            list(`Variables` = ("a")
                                            )),
                            ),
                            actionButton("vsleItsy","Visualize intensity")),
                     plotOutput("distPlot1",width = "100%",height = "1000px")
                 ),
                 #dataTableOutput("POP")%>% withSpinner(color="#0dc5c1"),
                 
        ),
        tabPanel("Generate samples",h1("Generate samples"),sidebarLayout(
            sidebarPanel(
                width = 2,
                h3("presence"),
                actionButton("gen_pres","generate presence samples"),
                textOutput("countsPres"),
                h3("pseudo absence"),
                 numericInput("num_abs","counts",value = 100,min = 1),
                 actionButton("gen_abs","generate pseudo-absences")
                ),
            mainPanel(plotOutput("distPlot2",width = "100%",height = "1000px")))
            ),
        tabPanel("Set reference",h1("set reference"),sidebarLayout(
            sidebarPanel(
                width = 2,
                h3("reference point"),
                fluidRow(
                    column(6, sliderInput("xloc","x",value = 1,min = 1,max=201 ,width='100%')),
                    column(6, sliderInput("yloc","y",value = 1,min = 1,max=301,width='100%'))),
                actionButton("set_ref","set reference")
            ),
            mainPanel(plotOutput("refPt",width = "100%",height = "1000px")))
        ),
        tabPanel("IPP",h1("Fitting with IPP"),sidebarLayout(
            sidebarPanel(
                width = 2,
                textInput("IPP_model","Model trend",width = '100%',value = "rv$pres2~."),
                sliderInput("IPP_grid","Grid ",value = 10,min = 10,max=300,step = 10),
                selectInput("correction", "Boarder correction:",
                            list(`Examples` = list("border", "Ripley" , "isotropic", "periodic","translate","none")
                            ), selected = "border"),
                textInput("corrsize", "Correction Size",width = '100%',value = "2"),
                textInput("fctn", "New function",width = '100%',value = ""),
                actionButton("addfctn","Add function"),
                checkboxInput("gam", label = "Use gam instead of glm?", value = FALSE, width = NULL),
                checkboxInput("force", label = "Forceinput?", value = TRUE, width = NULL),
                checkboxInput("emend", label = "Emend?", value = FALSE, width = NULL),
                selectInput("method", "Fitting method",
                            list(`Examples` = list("mpl", "logi" , "VBlogi", "ho")
                            ), selected = "mpl"),
                actionButton("IPP_fit","FIT MODEL")),
            mainPanel(h3("Prediction"),
                      plotOutput("IPP_predict",width = "100%",height = "1000px"),
                      h3("Quadrature scheme"),
                      verbatimTextOutput("IPP_quad"),
                      h3("Summary"),
                      verbatimTextOutput("IPP_output")))
        ),
        tabPanel("IPP(Lasso)",h1("Fitting with IPP"),sidebarLayout(
            sidebarPanel(
                width = 2,
                h3("Counts absence"),
                actionButton("gen_pres_tmp1","generate presence samples"),
                textOutput("countsPres_tmp1"),
                h3("Model fitting"),
                numericInput("num_abs_tmp1","counts",value = 10),
                actionButton("gen_abs_tmp1","generate pseudo-absences"),
                h3("Model evaluation"),
                h3("Model prediction")),
            mainPanel(plotOutput("distPlot2_tmp1",width = "100%",height = "1000px")))
        ),
        tabPanel("logistic regression",h1("Logistic regression"),sidebarLayout(
            sidebarPanel(
                width = 2,
                textInput("LR_model","Model trend",width = '100%',value = "type~."),
                sliderInput("LR_abs","pseudo-absence",value = 10,min = 10,max=1000,step = 10),
                actionButton("LR_fit","FIT MODEL")
                ),
            mainPanel(h3("Prediction"),
                      plotOutput("LR_predict",width = "100%",height = "1000px"),
                      h3("Quadrature scheme"),
                      verbatimTextOutput("LR_quad"),
                      h3("Summary"),
                      verbatimTextOutput("LR_output")))
        ),
        tabPanel("logistic regression(Lasso)",h1("logistic regression"),sidebarLayout(
            sidebarPanel(
                width = 2,
                h3("Counts absence"),
                actionButton("gen_pres_tmp11","generate presence samples"),
                textOutput("countsPres_tmp11"),
                h3("Model fitting"),
                numericInput("num_abs_tmp11","counts",value = 10),
                actionButton("gen_abs_tmp11","generate pseudo-absences"),
                h3("Model evaluation"),
                h3("Model prediction")),
            mainPanel(plotOutput("distPlot2_tmp11",width = "100%",height = "1000px")))
        ),
        tabPanel("Maxent",h1("Maximum Entropy"),sidebarLayout(
            sidebarPanel(
                width = 2,
                textInput("ME_model","Model trend",width = '100%',value = "type~."),
                sliderInput("ME_abs","pseudo-absence",value = 10,min = 10,max=10000,step = 10),
                actionButton("ME_fit","FIT MODEL")
            ),
            mainPanel(h3("Prediction"),
                      plotOutput("ME_predict",width = "100%",height = "1000px"),
                      h3("Quadrature scheme"),
                      verbatimTextOutput("ME_quad"),
                      h3("Summary"),
                      verbatimTextOutput("ME_output")))
        ),
        tabPanel("Random Forest",h1("Random forest"),sidebarLayout(
            sidebarPanel(
                width = 2,
                textInput("RF_model","Model trend",width = '100%',value = "type~."),
                sliderInput("RF_abs","pseudo-absence",value = 100,min = 0,max=1000,step = 100),
                sliderInput("RF_ntrees","number of trees",value = 50,min = 0,max=1000,step = 100),
                actionButton("RF_fit","FIT MODEL")
            ),
            mainPanel(h3("Prediction"),
                      plotOutput("RF_predict",width = "100%",height = "1000px"),
                      h3("Quadrature scheme"),
                      verbatimTextOutput("RF_quad"),
                      h3("Summary"),
                      verbatimTextOutput("RF_output")))),
        tabPanel("Boosted regression trees",h1("Boosted regression trees")),
        tabPanel("Classification tree",h1("Classification tree"),sidebarLayout(
            sidebarPanel(
                width = 2,
                textInput("CT_model","Model trend",width = '100%',value = "type~."),
                sliderInput("CT_abs","pseudo-absence",value = 100,min = 0,max=2000,step = 100),
                numericInput("CT_cp","Complexity parameter",value = 0.01),
                actionButton("CT_fit","FIT MODEL")
            ),
            mainPanel(h3("Prediction"),
                      plotOutput("CT_predict",width = "100%",height = "1000px"),
                      h3("Quadrature scheme"),
                      verbatimTextOutput("CT_quad"),
                      h3("Summary"),
                      verbatimTextOutput("CT_output")))),
        tabPanel("KNN",h1("K-nearest neighbor"),sidebarLayout(
            sidebarPanel(
                width = 2,
                checkboxGroupInput("KNN_response","response",choices =names(envI),selected = names(envI) ,width = '100%'),
                checkboxInput("KNN_scale","Normalize",width = '100%',value = FALSE),
                textInput("KNN_model","Model trend",width = '100%',value = "type~."),
                sliderInput("KNN_abs","pseudo-absence",value = 100,min = 0,max=2000,step = 100),
                sliderInput("KNN_K","K",value = 10,min = 1,max=500,step = 1),
                actionButton("KNN_fit","FIT MODEL")
            ),
            mainPanel(h3("Prediction"),
                      plotOutput("KNN_predict",width = "100%",height = "1000px"),
                      h3("Quadrature scheme"),
                      verbatimTextOutput("KNN_quad"),
                      h3("Summary"),
                      verbatimTextOutput("KNN_output")))),
        tabPanel("NNET",h1("Neural Networks"),sidebarLayout(
            sidebarPanel(
                width = 2,
                textInput("NNET_model","Model trend",width = '100%',value = "type~."),
                numericInput("NNET_size","size",value = 3,width = '100%'),
                numericInput("NNET_rang","rang",value = 0.1,width = '100%'),
                numericInput("NNET_decay","decay",value = 5e-6,width = '100%'),
                numericInput("NNET_maxit","maxit",value = 200,width = '100%'),
                sliderInput("NNET_abs","pseudo-absence",value = 100,min = 0,max=2000,step = 100),
                actionButton("NNET_fit","FIT MODEL")
            ),
            mainPanel(h3("Prediction"),
                      plotOutput("NNET_predict",width = "100%",height = "1000px"),
                      h3("Quadrature scheme"),
                      verbatimTextOutput("NNET_quad"),
                      h3("Summary"),
                      verbatimTextOutput("NNET_output"))))
        )
    
    
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    #Define Excel dataset import logic
    rv         <- reactiveValues()
    
    observeEvent(input$dataset,
                 {
                     if(input$dataset == "Load Excel Data..."){
                         my_data <- read_excel(file.choose())
                         rv$mydata <<- as.im(as.data.frame(my_data))
                         attach(rv$mydata)
                         output$result <- renderText({
                             paste("Your")
                         })
                         correlation <- cor(as.data.frame(rv$mydata), use="pairwise.complete.obs")
                         output$corrplot <- renderPlot({ggcorrplot(correlation,tl.srt = 270)},res = 100)
                         output$boxPlot <- renderPlot({ggplot(stack(as.data.frame(raster::stack(lapply(rv$mydata,raster)))), aes(x=ind,y=log(values)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))},res = 100)
                         output$distPlot <- renderPlot({plot(raster::stack(lapply(rv$mydata,raster)))},res = 100)
                         rv$absref <<- raster::stack(lapply(rv$mydata,raster))
                     }
                     else if(input$dataset == "Load TIFF File..."){
                         rv$mydata <<- raster::stack(choose.files())
                         output$result <- renderText({
                             paste("Your")
                         })
                         correlation <- cor(as.data.frame(rv$mydata), use="pairwise.complete.obs")
                         output$corrplot <- renderPlot({ggcorrplot(correlation,tl.srt = 270)},res = 100)
                         output$boxPlot <- renderPlot({ggplot(stack(as.data.frame(rv$mydata)), aes(x=ind,y=log(values)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))},res = 100)
                         output$distPlot <- renderPlot({plot(rv$mydata)},res = 100)
                         rv$absref <<- rv$mydata
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
                         output$result <- renderText({
                             paste("The" , input$dataset)
                         })
                         correlation <- cor(as.data.frame(rv$mydata), use="pairwise.complete.obs")
                         output$corrplot <- renderPlot({ggcorrplot(correlation,tl.srt = 270)},res = 100)
                         output$boxPlot <- renderPlot({ggplot(stack(as.data.frame(envS)), aes(x=ind ,y=log(values)))+geom_boxplot()+ theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))},res = 100)
                         output$distPlot <- renderPlot({plot(envS)},res = 100)
                         rv$absref <<- envS
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
                 output$distPlot1 <- renderPlot({image(rv$itsy)},width = "auto")
                 output$distPlot2 <- renderPlot({image(rv$itsy)},width = "auto")
                 })
    
    # generate presence
    observeEvent(input$gen_pres,{
        
                 rv$pres2     <- rpoispp(rv$itsy)
                 rv$data_pres <- raster::extract(rv$absref,coords(rv$pres2)) %>% data.frame(.,type = 1)
                 output$distPlot2 <- renderPlot({image(rv$itsy); points(coords(rv$pres2),col="green",cex=2,pch=20)},width = "auto")
                 output$countsPres <- renderText({paste(npoints(rv$pres2),"samples generated")})
                 })
   
    observeEvent(input$gen_abs,{
        
        if(!is.null(rv$pres2) & !is.na(input$num_abs) & input$num_abs>0 ){
        
                    rv$abs      <- randomPoints(rv$absref,n = input$num_abs, p=coords(rv$pres2))
                    rv$data_abs <- raster::extract(rv$absref,rv$abs) %>% data.frame(.,type = 0)
                    output$distPlot2 <- renderPlot({image(rv$itsy);
                                                    points(coords(rv$pres2),col="green",cex=2,pch=20);
                                                    points(rv$abs,col="red",cex=2,pch=20)},width = "auto")
                    updateNumericInput(session,"LR_abs",value = input$num_abs)
        }
                 })
    
    observeEvent(input$xloc|input$yloc|input$set_ref,{
        
        if(!is.null(rv$itsy) & !is.na(min(input$xloc,input$yloc)) & min(input$xloc,input$yloc)>0 ){
            
            output$refPt <- renderPlot({image(rv$itsy);
                                        points(rv$itsy$xcol[input$xloc],rv$itsy$yrow[input$yloc],col="green",pch=20,cex = 3)
                },width = "auto")
            
        }
    })
    

    
    
    
    # Model fitting
    
    #################
    ## IPP
    #################

    rv$functions <- list()
    
    observeEvent(input$addfctn,{
        rv$functions <<- c(rv$functions,input$fctn)
    })
    observeEvent(input$IPP_fit,{
        
    rv$ipp.fit            <<- ppm(eval(parse(text=input$IPP_model)),data = envI,nd=input$IPP_grid, correction = input$correction, rboard = input$corrsize, use.gam = input$gam, method = input$method, forcefit = input$force, emend = input$emend)
    output$IPP_predict <- renderPlot({par(mfrow=c(1,3));
                                      plot(rv$itsy,main = "original");
                                      plot(predict(rv$ipp.fit),main = "prediction")},width = "auto")
    output$IPP_output  <- renderPrint({print(rv$ipp.fit)})
    output$IPP_quad  <- renderPrint({rv$ipp.fit$Q})
    })
    
    
    ########################
    ## Logistic regression
    ########################

    observeEvent(input$LR_abs,{
        updateNumericInput(session,"num_abs",value = input$LR_abs)
    })
    
    observeEvent(input$LR_fit,{
        
        if(!is.null(rv$data_abs)){
            
            withProgress(message = 'Model fitting', value = 0, {
        incProgress(1/4)
        glm1       <- glm(eval(parse(text=input$LR_model)),family = binomial(link = "logit"),data = rbind(rv$data_pres,rv$data_abs))
        incProgress(1/4)
        pred       <- predict(envS, glm1)
        pred       <- as.im(apply(values(pred,format="matrix"),2,rev))
        incProgress(1/4)
        output$LR_predict <- renderPlot({par(mfrow=c(1,3));
            plot(rv$itsy,main = "original");
            plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original");
            plot(pred-pred[input$yloc,input$xloc],main = "prediction")},width = "auto")
        output$LR_output  <- renderPrint({summary(glm1)})
        incProgress(1/4)
        
        

                Sys.sleep(0.5)
            
        })}

    })
    
    ########################
    ## Random Forest
    ########################

    
    observeEvent(input$RF_abs,{
        updateNumericInput(session,"num_abs",value = input$RF_abs)
    })
    
    observeEvent(input$RF_fit,{
        
        if(!is.null(rv$data_abs)){
            
            withProgress(message = 'Model fitting', value = 0, {
                
               
                rf1      <- randomForest(eval(parse(text=input$RF_model)),data = rbind(rv$data_pres,rv$data_abs),importance = T,ntree= input$RF_ntrees)
                pred_rf  <- predict(envS, rf1)
                pred_rf  <- as.im(apply(values(pred_rf,format="matrix"),2,rev))
                logit_rf <- log(pred_rf/(1-pred_rf))
                
                incProgress(1/2)
                output$RF_predict <- renderPlot({par(mfrow=c(1,3));
                    plot(rv$itsy,main = "original");
                    plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original - reference");
                    plot(logit_rf-logit_rf[input$yloc,input$xloc],main = "prediction")},width = "auto")
                
                
                
                incProgress(1/2)
                Sys.sleep(0.5)
                
            })}
        
    })
    
    ########################
    ## Classification tree
    ########################
 
    observeEvent(input$CT_abs,{
        updateNumericInput(session,"num_abs",value = input$CT_abs)
    })
    
    observeEvent(input$CT_fit,{
        
        if(!is.null(rv$data_abs)){
            
            withProgress(message = 'Model fitting', value = 0, {
                
                
                CT1      <- rpart(eval(parse(text=input$CT_model)),  method="class", data=rbind(rv$data_pres,rv$data_abs),control = rpart.control(cp = input$CT_cp))
                pred_CT  <- 1-predict(envS, CT1)
                pred_CT  <- as.im(apply(values(pred_CT,format="matrix"),2,rev))
                logit_CT <- log(pred_CT/(1-pred_CT))

                incProgress(1/2)
                output$CT_predict <- renderPlot({par(mfrow=c(1,3));
                    plot(rv$itsy,main = "original");
                    plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original - reference");
                    plot(logit_CT-logit_CT[input$yloc,input$xloc],main = "prediction")},width = "auto")
                
                
                
                incProgress(1/2)
                Sys.sleep(0.5)
                
            })}
        
    })
    
    ########################
    ## KNN
    ########################
    

    
    
    observeEvent(input$KNN_abs,{
        updateNumericInput(session,"num_abs",value = input$KNN_abs)
    })
    
    observeEvent(input$KNN_fit,{
        
        if(!is.null(rv$data_abs)){
            
            withProgress(message = 'Model fitting', value = 0, {
                
                pred_KNN  <- envS$FC
                #tmp       <- as.matrix(model.frame(eval(parse(text=input$KNN_model))[-2],data.frame(values(envS)),na.action = NULL ))
                tmp       <- as.matrix(values(envS)[,input$KNN_response])
                
                tmpNNA    <- apply(!is.na(tmp), 1, all)
                train     <- as.matrix(rbind(rv$data_pres,rv$data_abs)[,input$KNN_response])
                test      <- as.matrix(tmp[tmpNNA,])
                if(input$KNN_scale){
                    train <- scale(train)
                    test  <- scale(test)
                }

                
                # KNN  <- class::knn(train = model.frame(eval(parse(text=input$KNN_model)),rbind(rv$data_pres,rv$data_abs))[-1],
                #                       test = tmp[tmpNNA,],
                #                       cl = model.frame(eval(parse(text=input$KNN_model)),rbind(rv$data_pres,rv$data_abs))[,
                #                                                                                1],
                #                       k=input$KNN_K,
                #                       prob = 1)
                
                KNN  <- class::knn(train = train,
                                   test = test,
                                   cl = rbind(rv$data_pres,rv$data_abs)[, "type"],
                                   k=input$KNN_K,
                                   prob = 1)
                
                p  <- attributes(KNN)
                p$prob[KNN == min(p$levels)] <- 1- p$prob[KNN == min(p$levels)]
                values(pred_KNN)[tmpNNA] <- p$prob
                
                pred_KNN  <- as.im(apply(values(pred_KNN,format="matrix"),2,rev))
                logit_KNN <- log(pred_KNN/(1-pred_KNN))
                
                incProgress(1/2)
                output$KNN_predict <- renderPlot({par(mfrow=c(1,3));
                    plot(rv$itsy,main = "original");
                    plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original - reference");
                    plot(logit_KNN-logit_KNN[input$yloc,input$xloc],main = "prediction")},width = "auto")
                
                
                
                incProgress(1/2)
                Sys.sleep(0.5)
                
            })}
        
    })
    
    
    
    ########################
    ## NNET
    ########################
    
    
    
    
    observeEvent(input$NNET_abs,{
        updateNumericInput(session,"num_abs",value = input$NNET_abs)
    })
    
    observeEvent(input$NNET_fit,{
        
        if(!is.null(rv$data_abs)){
            
            withProgress(message = 'Model fitting', value = 0, {
                
                pred_NNET  <- envS$FC
                tmp       <- as.matrix(values(envS))
                
                NNET      <- nnet(eval(parse(text=input$NNET_model)),data = rbind(rv$data_pres,rv$data_abs),size =input$NNET_size, rang =input$NNET_rang,
                                  decay = input$NNET_decay, maxit =input$NNET_maxit)
               
                values(pred_NNET)<- predict(NNET,tmp)
                
                pred_NNET  <- as.im(apply(values(pred_NNET,format="matrix"),2,rev))
                logit_NNET <- log(pred_NNET/(1-pred_NNET))
                
                incProgress(1/2)
                output$NNET_predict <- renderPlot({par(mfrow=c(1,3));
                    plot(rv$itsy,main = "original");
                    plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original - reference");
                    plot(logit_NNET-logit_NNET[input$yloc,input$xloc],main = "prediction")},width = "auto")
                
                
                
                incProgress(1/2)
                Sys.sleep(0.5)
                
            })}
        
    })
    
    ########################
    ## MAXENT
    ########################
    
    
    
    
    observeEvent(input$ME_abs,{
        updateNumericInput(session,"num_abs",value = input$ME_abs)
    })
    
    observeEvent(input$ME_fit,{
        
        if(!is.null(rv$data_abs)){
            
            withProgress(message = 'Model fitting', value = 0, {
                
                ME.x      <- model.frame(eval(parse(text=input$ME_model)),data = rbind(rv$data_pres,rv$data_abs))
                #ME.p      <- model.frame(eval(parse(text=input$ME_model))[-3],data = rbind(rv$data_pres,rv$data_abs))[,1]
                ME        <- maxent(ME.x[-1],ME.x[1])
                pred_ME   <- predict(envS,ME)



                pred_ME  <- as.im(apply(values(pred_ME,format="matrix"),2,rev))
                logit_ME <- log(pred_ME/(1-pred_ME))

                incProgress(1/2)
                output$ME_predict <- renderPlot({par(mfrow=c(1,3));
                    plot(rv$itsy,main = "original");
                    plot(log(rv$itsy)-log(rv$itsy)[input$yloc,input$xloc],main = "log original - reference");
                    plot(logit_ME-logit_ME[input$yloc,input$xloc],main = "prediction")},width = "auto")
                
                
                
                incProgress(1/2)
                Sys.sleep(0.5)
                
            })}
        
    })
    
    
    
    
    
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
