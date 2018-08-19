# 28/05/2018: Shiny app to compute  elements for the Flatness project
# 28/06/2018: Adding production function sliders
# Use https://stackoverflow.com/questions/39040018/how-does-one-use-special-symbols-in-a-choices-list-as-names
# 10/07/2018: Version 0.3. Prices on top of page 
# 11/07/2018: Introduction of graps


library(shiny)

# Define UI for application 
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Sensitivity of Optimal Production Choices to Risk Preferences "),
  tags$h5("Christophe Bontemps, Douadia Bougherara and Celine Nauges (2018)"),
  tags$h5("Version 0.6"),
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
   
    sidebarPanel(
      HTML("<h3><font color='#2874A6'> Output./InputPrice ratio </font></h3>"),
      br(),
      sliderInput("p.level",
                  "Output price (EUR/q) ",
                  min = 1,
                  max = 110,
                  step = 1,
                  value = 11),
      tags$p("Input price is normalized to 1"),
      
      
      HTML("<h3><font color='#2874A6'> Production technology </font></h3>"),
      br(),
      selectInput("Prod", 
                  label = "Production function:",
                  c("Just & Pope" = "JP",
                    "State contingent" = "SC")
      ),
      conditionalPanel(condition = "input.Prod == 'JP'",
     sliderInput("beta.f",
                 "F lin. coef. (\u03B2.f)",
                 min = 0,
                 max = 30,
                 step = 1,
                 value = 15),              
                       
       sliderInput("alpha.f",
                   "F exp. coef. (\u03B1.f)",
                   min = 0,
                   max = 0.5,
                   step = 0.01,
                   value = 0.3),
      sliderInput("beta.g",
                   "F lin. coef. (\u03B2.g)",
                   min = 0,
                   max = 50,
                   step = 1,
                   value = 30),       
       
       sliderInput("alpha.g",
                   "G exp. coef. (\u03B1.g)",
                   min = -0.25,
                   max = 0,
                   step = 0.01,
                   value =  -0.10)
      ),
      HTML("<h3><font color='#2874A6'> Risk Preferences </font></h3>"),
      br(),
      selectInput("Risk", 
                  label = "Risk Model:",
                  c("Expected Utility (CRRA)" = "CRRA",
                    "Cumulative Prospect theory" = "CPT")
      ),
      conditionalPanel(condition = "input.Risk == 'CRRA'", 
      sliderInput("r.level",
                  "risk aversion parameter:",
                  min = 0,
                  max = 4,
                  step = 0.5,
                  value = 1)
      ),
      conditionalPanel(condition = "input.Risk == 'CPT'", 
                       sliderInput("w0",
                                   "Reference point",
                                   min = -2,
                                   max = 2,
                                   step = 0.5,
                                   value = 0),
                       sliderInput("aplus",
                                   "Positive reflection (a+)",
                                   min = 0,
                                   max = 1,
                                   step = 0.01,
                                   value = 0.88),
                       sliderInput("amoins",
                                   "Negative reflection (a-)",
                                   min = 0,
                                   max = 1,
                                   step = 0.01,
                                   value = 0.88),
                       
                       sliderInput("lambda",
                                   "Loss aversion (\u03BB)",
                                   min = 0,
                                   max = 5,
                                   step = 0.5,
                                   value = 2.25),
                       
                       tags$h5("Weighting parameters"),
                       br(),
                       sliderInput("gammaplus",
                                   "Curvature in the gain domain (\u03B3 +)",
                                   min = 0,
                                   max = 1,
                                   step = 0.01,
                                   value = 0.61),
                       sliderInput("gammamoins",
                                   "Curvature in the loss domain (\u03B3 -)",
                                   min = 0,
                                   max = 1,
                                   step = 0.01,
                                   value = 0.69)
                       
      ),
      
      HTML("<h3><font color='#2874A6'> Prices </font></h3>"),
      br()
      
      
       
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Results Table", tableOutput("FullTable"),
        HTML(
          paste("x* is the optimum input level",'<br/>',
         "DeltaCE = ( CE(x*0) - CE(x*) ) / CE(x*0) , reported in % ",'<br/>', "")
        ),
        textOutput(""),
        textOutput("ModelProd")
        ),
       
        tabPanel("Production functions", plotOutput("ProdGraph")
                  ),
        
        tabPanel("Expected Profit", plotOutput("ProfGraph")
        ),
        
        tabPanel("Certain equivalent", plotOutput(""),
                  paste("Graph to appear here soon... ")
        )
      )
    )
)
))
