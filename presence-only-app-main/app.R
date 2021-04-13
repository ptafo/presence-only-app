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
library(bit64)

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
geo_Hessen_frame <- as.data.frame(geo_Hessen_stack)
geo_Hessen_rein <- na.omit(geo_Hessen_frame)
geo_Hessen_im <- as.im(as.data.frame(geo_Hessen,xy=1))

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
                 actionButton("gen_abs","generate pseudo-absences"),
                plotOutput("subsetselect_Global",width = "100%",height = "500px", brush = "subset_selected_Global"),
                actionButton("subselect_Global","Add subset"),
                actionButton("unselect_Global","Remove subset")
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
                width = 5,
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
                plotOutput("subsetselect",width = "100%",height = "500px"),
                #actionButton("subselect","Add subset"),
                textOutput("tester"),
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
                sliderInput("LR_abs","pseudo-absence",value = 10,min = 10,max=10000,step = 10),
                plotOutput("subsetselect_LR",width = "100%",height = "500px"),
                #actionButton("subselect_LR","Add subset"),
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
                sliderInput("bckgrnd","Background Points",value = 10,min = 10,max=10000,step = 10),
                sliderInput("testpoints","Random Test Points Percentage",value = 0,min = 0,max=99,step = 1),
                numericInput("betamult","Betamultiplier",value = 1,min = 1),
                numericInput("replicates","Replicates",value = 1,min = 1),
                selectInput("replicatetype", "Replicatetype:",
                            list(`Examples` = list("crossvalidate", "bootstrap" , "subsample"))),
                numericInput("maxits","Maximum Iterations",value = 500,min = 1),
                numericInput("cvrgncthresh","Convergencethreshold(log(-x))",value = 5,min = 1),
                checkboxInput("dupl", label = "Remove Duplicates?", value = TRUE, width = NULL),
                checkboxInput("jackknife", label = "Jackknife?", value = FALSE, width = NULL),
                checkboxInput("rndmseed", label = "New random seed each run?", value = FALSE, width = NULL),
                checkboxInput("prtldata", label = "Allow partial data?", value = FALSE, width = NULL),
                checkboxInput("linear", label = "Allow linear features?", value = TRUE, width = NULL),
                checkboxInput("quadratic", label = "Allow quadratic features?", value = TRUE, width = NULL),
                checkboxInput("product", label = "Allow product features?", value = TRUE, width = NULL),
                checkboxInput("threshold", label = "Allow threshold features?", value = TRUE, width = NULL),
                checkboxInput("hinge", label = "Allow hinge features?", value = TRUE, width = NULL),
                plotOutput("subsetselect2",width = "100%",height = "500px"),
                #actionButton("subselect2","Add subset"),
                actionButton("ME_fit","FIT MODEL")
            ),
            mainPanel(h3("Prediction"),
                      plotOutput("ME_predict",width = "100%",height = "1000px"),
                      h3("Quadrature scheme"),
                      verbatimTextOutput("ME_quad"),
                      h3("Summary"),
                      htmlOutput("ME_output")))
        ),
        tabPanel("Random Forest",h1("Random forest"),sidebarLayout(
            sidebarPanel(
                width = 2,
                textInput("RF_model","Model trend",width = '100%',value = "type~."),
                sliderInput("RF_abs","pseudo-absence",value = 100,min = 0,max=10000,step = 100),
                sliderInput("RF_ntrees","number of trees",value = 50,min = 0,max=1000,step = 100),
                numericInput("nodesize","min terminal node size",value = 1),
                numericInput("maxnodes","max terminal nodes",value = 100),
                plotOutput("subsetselect_RF",width = "100%",height = "500px"),
                #actionButton("subselect_RF","Add subset"),
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
                sliderInput("CT_abs","pseudo-absence",value = 100,min = 0,max=10000,step = 100),
                numericInput("CT_cp","Complexity parameter",value = 0.01),
                plotOutput("subsetselect_CT",width = "100%",height = "500px"),
                #actionButton("subselect_CT","Add subset"),
                actionButton("CT_fit","FIT MODEL")
            ),
            mainPanel(h3("Prediction"),
                      plotOutput("CT_predict",width = "100%",height = "1000px"),
                      h3("Quadrature scheme"),
                      verbatimTextOutput("CT_quad"),
                      h3("Summary"),
                      plotOutput("CT_output",width = "500%",height = "1000px")))),
        tabPanel("KNN",h1("K-nearest neighbor"),sidebarLayout(
            sidebarPanel(
                width = 2,
                checkboxGroupInput("KNN_response","response",choices =names(envI),selected = names(envI) ,width = '100%'),
                checkboxInput("KNN_scale","Normalize",width = '100%',value = FALSE),
                textInput("KNN_model","Model trend",width = '100%',value = "type~."),
                sliderInput("KNN_abs","pseudo-absence",value = 100,min = 0,max=10000,step = 100),
                sliderInput("KNN_K","K",value = 10,min = 1,max=500,step = 1),
                plotOutput("subsetselect_KNN",width = "100%",height = "500px"),
                #actionButton("subselect_KNN","Add subset"),
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
                radioButtons(
                    "NNET_method",
                    "Fitting Method",
                    choices = c("None selected","lineout","entropy","softmax","censored"),
                    selected = NULL,
                    inline = FALSE,
                    width = NULL,
                    choiceNames = NULL,
                    choiceValues = NULL
                ),
                sliderInput("NNET_abs","pseudo-absence",value = 100,min = 0,max=10000,step = 100),
                plotOutput("subsetselect_NNET",width = "100%",height = "500px"),
                #actionButton("subselect_NNET","Add subset"),
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
    
    rv         <- reactiveValues()
    
    
    subsetter <- function(method){
        arg_name <- deparse(substitute(method))
        var_subset <- paste("rv$subset", arg_name, sep="")
        var_trimmedsubs <- paste("trimmedsubs", arg_name, sep="")
        var_extentcut <- paste("extentcut", arg_name, sep="")
        var_absrefnew <- paste("absrefnew", arg_name, sep="")
        var_trimmedsubspres <- paste("trimmedsubspres", arg_name, sep="")
        var_trimmedsubsabs<- paste("trimmedsubsabs", arg_name, sep="")
        var_ignore <- paste("rv$ignore", arg_name, sep="")
        if(class(rv$subset) != "owin" && class(var_subset) != "owin"){
            assign(var_trimmedsubs, rv$itsy, envir=.GlobalEnv)
            assign(var_absrefnew , rv$absref, envir=.GlobalEnv)
            assign(var_trimmedsubspres, rv$data_pres, envir=.GlobalEnv)
            assign(var_trimmedsubsabs, rv$data_abs, envir=.GlobalEnv)
        }
        else if(typeof(input$subset_selected_Global) != "list" || var_ignore == TRUE) {
            assign(var_trimmedsubs, rv$itsy[eval(parse(var_subset))], envir=.GlobalEnv)
            assign(var_absrefnew , crop(rv$absref,eval(parse(var_extentcut))), envir=.GlobalEnv)
            assign(var_trimmedsubspres, raster::extract(eval(parse(var_absrefnew)),coords(rv$pres2)) %>% data.frame(.,type = 1) %>% na.omit(), envir=.GlobalEnv)
            assign(var_trimmedsubsabs, raster::extract(eval(parse(var_absrefnew)),rv$abs) %>% data.frame(.,type = 0) %>% na.omit(), envir=.GlobalEnv)
        }
        else{
            assign(var_trimmedsubs, rv$trimmedsubs, envir=.GlobalEnv)
            assign(var_absrefnew , rv$absrefnew, envir=.GlobalEnv)
            assign(var_trimmedsubspres, rv$trimmedsubspres, envir=.GlobalEnv)
            assign(var_trimmedsubsabs, rv$trimmedsubsabs, envir=.GlobalEnv)
        }
    }
    
    #Data Selection
    
    source("./Methods/Data.R",local = TRUE) 
    
    
    # generate presence
    

    source("./Methods/PresGen.R",local = TRUE) 
    
    
    # Model fitting
    
    #################
    ## IPP
    #################

    source("./Methods/IPP.R",local = TRUE) 
    
    
    ########################
    ## Logistic regression
    ########################

    source("./Methods/LR.R",local = TRUE) 
    
    ########################
    ## Random Forest
    ########################

    source("./Methods/RF.R",local = TRUE) 
    
    
    ########################
    ## Classification tree
    ########################
 
    source("./Methods/CT.R",local = TRUE) 
    
    ########################
    ## KNN
    ########################
    

    source("./Methods/KNN.R",local = TRUE) 
    
    
    
    
    
    ########################
    ## NNET
    ########################
    
    source("./Methods/NNET.R",local = TRUE) 
    
    
   
    
    ########################
    ## MAXENT
    ########################
    
    
    source("./Methods/MAXENT.R",local = TRUE) 
    
    
    
    
    
    
    
    
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
