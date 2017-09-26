library("shinythemes")
library("shinyBS")
library("highcharter")
# library("knitr")


#rmdfiles <- c("help.rmd")
#sapply(rmdfiles, knit, quiet = T)

shinyUI(
  fluidPage(
    theme = shinytheme("spacelab"),
    sidebarPanel(width=3,


                 ################################# About (start) #####################################

                 conditionalPanel(condition="input.tabs1=='About'"
                 ),

                 conditionalPanel(condition="input.tabs1=='Authors'"
                 ),
                 
                 conditionalPanel(condition="input.tabs1=='Citation'"
                                                                  
                 ),

                 #conditionalPanel(condition="input.tabs1=='Help'"

                 # HTML('<img src="images/help.png" width=300 height=200>'),



                 #),

                 ################################# About (end) #####################################
            
                 ########## Analysis (start) #####################################


                 conditionalPanel(condition="input.tabs1=='Analysis'",

                                  h4("Risk factors"),
                                  sliderInput(inputId = "iim", label = "IIM", min = 30, max = 75, value = 50, step = 1, ticks = T, animate = T),
                                  sliderInput(inputId = "tgl", label = "TGL", min = 100, max = 140, value = 120, step = 1, ticks = T, animate = T),
                                  selectizeInput(inputId = "mov1", label = "MOV1", choices = c("No", "Yes"), multiple = FALSE, selected = "Yes"),
                                  
                                  br(),
                                  checkboxInput(inputId = "advanced", label = "Advanced Results", value = FALSE),
                                    conditionalPanel(condition="input.advanced",
                                                     
                                         checkboxInput(inputId = "lp", label = "Linear predictor", value = FALSE),
                                         checkboxInput(inputId = "cav", label = "Central adjustment values", value = FALSE),            
                                         checkboxInput(inputId = "t", label = "Terms", value = FALSE),
                                         checkboxInput(inputId = "ct", label = "Combined terms", value = FALSE)
                                         
        
                                    ),
                                  
                                  br(),
                                  actionButton(inputId = "predict", label = "Predict Risk", icon = icon("play", lib = "glyphicon"))
                                  
                                  
                                  
                 )


                 ########## Analysis (end) #####################################

    ),




    mainPanel(

      navbarPage(title = "Nomogram", id="tabs1",

                 tabPanel("About",

                          #br(),
                          h4('Create nomogram and predict risk probability')


                          ),

                 tabPanel("Analysis",
                                  tabsetPanel(

                                            tabPanel(title = 'Risk Prediction',value = 1,
                                                   # DT::dataTableOutput("nData"),
                                                   h4(textOutput(outputId = "table1")),
                                                   h4(uiOutput("predictRisk")),
                                                   
                                                   br(),
                                                   
                                                   h4(textOutput(outputId = "table11")),
                                                   h4(uiOutput("lp")),
                                                     
                                                   br(),
                                                   
                                                  h4(textOutput(outputId = "table2")),
                                                  DT::dataTableOutput("adf"),
                                                  
                                                  br(),
                                                  
                                                  h4(textOutput(outputId = "table3")),
                                                  DT::dataTableOutput("terms"),
                                                  
                                                  br(),   
                                                  
                                                  h4(textOutput(outputId = "table4")),
                                                  DT::dataTableOutput("cterms")
                                                   
                                           ),
                                           
                                           tabPanel(title = 'Nomogram',value = 2,
                                                    
                                                    plotOutput("nomogram")
                                                    
                                                    
                                           ),
                                           
                                           tabPanel(title = 'Validation',value = 3,
                                                    
                                                    verbatimTextOutput("validationRes")
                                                    
                                                    
                                           ),
                                           
                                           tabPanel(title = 'Calibration',value = 4,
                                                    
                                                    plotOutput("calibrationRes")
                                                    
                                                    
                                           )
                                    
                                           
                                           )


                 ),

                 #tabPanel("Help",

                 #shiny::includeMarkdown("help.md"),
                 #   HTML('<p>Please click here to reach the <a href="help.html" target="_blank"> <b>help page</b></a> </p>')

                 #uiOutput('help'),

                 #),

                 tabPanel("Authors"
                 ),
                 
                 tabPanel("Citation"
                 )
                 


                          )



    )
)
  )#)




