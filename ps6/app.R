#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)

tuition <- read_delim("schools.csv")
rows<-nrow(tuition)
rows
cols<-ncol(tuition)
cols
school_type<-unique(tuition$Type)
schooltype_location<- tuition %>% 
  group_by(Type) %>% 
  summarise(ave=mean(Value))
# Define UI for application that draws a histogram
ui <- fluidPage(
  tabsetPanel(
      tabPanel("About",
               tags$h1("This shiny app will show data about",strong("college tuition prices around the US.")),
               tags$h2("U.S. college tuition prices from 2013 to 2021"),
               tags$p("This data shows",rows,em("observations of data"), ",and",cols,em("variables"))),
      tabPanel("Graphs",
               sidebarLayout(
                 sidebarPanel(
                   tags$p("This graph shows the average tution costs per State."),
                   tags$p("You can view costs by State by changing the input."),
                   radioButtons("color_input", "Select color:",
                                choices = c("Red", "Purple", "Blue", "Green"),
                                selected = "Red"),
                   checkboxGroupInput("States",
                                      "Select State",
                                      choices=unique(tuition$State),
                                      selected=unique(tuition$State))
                   
                 ),
                 mainPanel(
                   plotOutput("Plot")
                 )
               )),
      tabPanel("Tables",
               sidebarLayout(
                 sidebarPanel(
                   tags$p("This table shows the average tuition costs by Private, In-State Public, and Out-of-State Public Schools"),
                   tags$p("You can view costs by Type of School by changing the input."),
                   radioButtons("type_of_school","Select School Type",
                                choices=c(school_type),
                                selected=head(school_type,n=1))
                 ),
                 mainPanel(
                   tableOutput("private_public")
                 )
               ))
    )
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$Plot <- renderPlot({
    tuition %>% 
      filter(State %in% input$States) %>% 
      group_by(State) %>% 
      summarise(ave_data=mean(Value)) %>% 
      ggplot(aes(State,ave_data,fill=as.factor(State)))+
      geom_col(position="dodge")+
      labs(title="Average Tuition Costs by State",
           x="State",
           y="tuition cost")+
      scale_fill_discrete(name="State")
      
  })
  output$private_public <-renderPlot({
    tuition %>% 
      filter(Type %in% input$type_of_school) %>% 
      group_by(Type) %>% 
      summarise(ave=mean(Value))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
#https://eshabangur.shinyapps.io/ps6-shinyapp/
