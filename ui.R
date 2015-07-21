library(shiny)
countries=read.csv("countries2.csv",header=FALSE)
test=as.data.frame(cbind(as.character(countries[,5]),0:214))
test$V1=factor(test$V1,levels(test$V1)[c(212,1:211,213:215)])

shinyUI(
pageWithSidebar(
  
  #     # Application title
  #     titlePanel("Earthquake mapping (Data from USGS.gov)"),
  
  # Sidebar with a slider input for the number of bins
  headerPanel("Earthquake mapping (Data from USGS.gov)"),
  sidebarPanel(
    sliderInput("date_range", 
                label = h4("Year range: "),
                min = 1950, max = 2015, value = c(1950, 2015)),
    numericInput("min_mag", 
                 label = h4("Min magnitude: "), 
                 value = 7.5),
    selectInput("region", label = h4("Country"), 
                choices = split(test$V2,test$V1),selected=0)
  ),
  
  
  
  
  # Show a plot of the generated distribution
  mainPanel(textOutput("text1"),
            uiOutput('mymap'))
))