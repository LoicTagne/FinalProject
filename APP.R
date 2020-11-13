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

Kt <- read_csv("Klay Thompson Stats copy.csv", 
               col_types = cols(G = col_skip(), Age = col_skip(), 
                                Tm = col_skip(), X6 = col_skip(), 
                                GS = col_skip(), 
                                ORB = col_double(), 
                                PF = col_skip(), GmSc = col_skip(), 
                                `+/-` = col_skip())) %>%
    rename(W_L = "X8") %>%
    rename(OPP = "Opp") %>%
    rename(FGP = "FG%") %>%
    rename(FTP = "FT%") %>%
    rename(TPP = "3P%") %>%
    rename(TP = "3P") %>%
    rename(Game = "Rk") %>%
    rename(TPA = "3PA") %>%
    mutate(FTP = FTP * 100) %>%
    mutate(TPP = TPP * 100) %>%
    mutate(FGP = FGP * 100) %>%
    mutate(RSLT = c(rep("W", 24), "L", rep("W", 5), "L", rep("W", 7), "L", "W", "L", rep("W", 11), "L", rep("W", 7), "L", rep("W", 7), "L", rep("W", 6), "L", "W", "L", rep("W", 4)))

# Define UI for application that draws a histogram
ui <-  
navbarPage("Final Project Title" , 

           # Application title
           tabPanel("Model",
                    fluidPage(
               
               # Sidebar with a slider input for number of bins 
               titlePanel("Klay Thompson's 2015-2016 Minutes"),     
               
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                       plotOutput("Klay")) ,
                    )), 
          

tabPanel("Discussion",
         titlePanel("Discussion Title"),
         mainPanel(plotOutput("Klay2") ,  
         
         h4("Tour of the modeling choices you made and 
              an explanation of why you made them"))) ,
tabPanel("About", 
         titlePanel("About"),
         h3("Project Background and Motivations"),
         p("Hello, this is where I talk about my project. I decided to look
               at Klay Thompson's and Stephen Curry's statistics from the 2015-
               2016 NBA season"),
         h3("About Me"),
         p("My name is Loic Tagne and I study Government and Economics. 
             You can reach me at loictagne@college.harvard.edu.")))      










# Define server logic required to draw a histogram
server <- function(input, output) {

  
output$distPlot2 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
})


output$Klay <- renderPlot({
    Kt %>%
        select(MP, Game) %>%
        mutate_if(is.numeric, funs(replace_na(., 0))) %>%
        ggplot(data = Kt, mapping = aes(x = Game, y = MP)) +
        geom_line(color = "blue") +
        geom_vline(lty = "dashed", color = "red", xintercept = 58) +
        theme_bw() +
        labs(x = "Game Number",
             y = "Minutes Played (mins:sec)",
             title = "Klay Thompson's Minutes",
             subtitle = "Did Klay Thompson Play Too Many Minutes During the Regular Season?",
             caption = "Klay Thompson was inactive for Game 11 (BRK) and Game 24 (BOS)\n The red line indicates when the Warriors clinched a playoff berth")
})


output$Klay2 <- renderPlot({
    Kt %>%
        select(MP, Game) %>%
        mutate_if(is.numeric, funs(replace_na(., 0))) %>%
        ggplot(data = Kt, mapping = aes(x = Game, y = MP)) +
        geom_line(color = "blue") +
        geom_vline(lty = "dashed", color = "red", xintercept = 58) +
        theme_bw()
})
}







# Run the application 
shinyApp(ui = ui, server = server)
