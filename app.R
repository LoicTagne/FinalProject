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
library(shinythemes)
library(plotly)
library(ggplot2)
library(dplyr)
library(gt)
library(gtsummary)
library(rstanarm)
library(rlang)




Kt <- read_csv("Klay Thompson Stats copy.csv", 
               col_types = cols(G = col_skip(), Age = col_skip(), 
                                Tm = col_skip(), X6 = col_skip(), 
                                GS = col_skip(), 
                                ORB = col_double(), 
                                PF = col_skip(), GmSc = col_skip(), 
                                `+/-` = col_skip())) %>%
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
    mutate(RSLT = c(rep("W", 24), "L", rep("W", 5), "L", rep("W", 7), "L", "W",
                    "L", rep("W", 11), "L", rep("W", 7), "L", rep("W", 7), "L", 
                    rep("W", 6), "L", "W", "L", rep("W", 4))) %>%
    mutate(PLYR = "Klay Thompson")


SC <- read_csv("steph copy.csv", col_types = cols(Score = col_skip(), 
                                                      PF = col_skip(), 
                                                      TO = col_skip(), 
                                                      Dates = col_skip(), 
                                                      Type = col_skip())) %>%
    slice(544:595, 597:626) %>%
    mutate(Game = 1:82) %>%
    rename(OPP = "Opponent") %>%
    rename(MP = "Minutes") %>%
    rename(FG = "Successful Shots") %>%
    rename(FGA = "Total Shots") %>%
    mutate(FGP = FG/FGA * 100) %>%
    rename(TP = "3 Points Succesful") %>%
    rename(TPA = "Total 3 Points") %>%
    mutate(TPP = TP/TPA * 100) %>%
    rename(FT = "Successful FT") %>%
    rename(FTA = "Total FT") %>%
    mutate(FTP = FT/FTA * 100) %>%
    rename(TRB = "REB") %>%
    rename(RSLT = "Result") %>%
    mutate(PLYR = "Stephen Curry") %>%
    mutate(MRGN = `Score GS` - `Score Opponent`)

SPLASH <- bind_rows(Kt, SC)

stephcurry <- SPLASH %>%
    slice(83:164) %>%
    slice(1:10, 12:23, 25:30, 33:58, 60:82) 

klaythompson <- SPLASH %>%
    slice(1:82) %>%
    slice(1:10, 12:23, 25:30, 33:58, 60:82)

SPLASH3011 <- rbind(stephcurry, klaythompson)


AD <- read_csv("AD2019-2020.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>%
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>%
  rename(TP = "3P") %>%
  slice(1:10, 12:27, 29, 31:37, 43:56, 58, 60:67, 69, 70, 72) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "LA") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO)

CJ <- read_csv("CJ2018-2019.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>%
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>%
  rename(TP = "3P") %>%
  slice(1:24, 26:50, 52:69, 80, 81) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "DC") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO)

Dame <- read_csv("Dame2018-2019.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>%
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>%
  rename(TP = "3P") %>%
  slice(1:24, 26:50, 52:69, 80, 81) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "DC") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO)

Harden <- read_csv("Harden2019-2020.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>%
  rename(TP = "3P") %>%
  slice(1:6, 8:12, 14:25, 27:32, 34, 35, 37, 38, 40:44, 47:49, 51, 53:56, 
        58:61, 63:67) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "JR") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO)

Kobe <- read_csv("Kobe2000-2001.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  mutate(MRGN = c(10,-5,9,5,-10,-10,6,16,2,-1,8,20,7,15,17,-33,9,2,11,-3,-8,24,
                  -4,-10,22,3,2,5,5,-5,37,2,11,-23,3,-8,1,13,-11,-11,12,-1,6,
                  -13,6,6,2,3,-15,5,-1,10,2,7,6,-6,10,15,12,12,-4,-12,5,6,12,5,
                  -2,-7,13,12,-21,-24,9,-1,8,12,4,5,26,17,5,17)) %>%
  slice(1:9, 11:24, 26:41, 48:52, 56:61, 64:68, 74, 79:82) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "KS") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, 
         TRB, BLK, DUO, MRGN)

LBJ <- read_csv("LBJ2019-2020.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  slice(1:10, 12:27, 29, 31:37, 43:56, 58, 60:67, 69, 70, 72) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "LA") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, 
         TRB, BLK, DUO)

Malone <- read_csv("Malone96-97.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "KJ") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO)

MJ <- read_csv("MJ95-96.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  mutate(MRGN = c(14,22,9,18,4,-6,19,15,6,9,5,-5,3,6,6,7,19,12,9,19,20,9,13,9,
                  14,-6,27,2,14,24,29,26,27,7,12,3,15,20,20,22,11,11,20,15,-6,
                  -10,4,13,3,3,8,26,5,-9,20,21,23,32,9,21,-32,17,21,4,4,22,21,
                  -1,31,21,18,8,34,4,-1,13,30,26,6,31,-1,10)) %>%
  slice(1:61, 67:82) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "MS") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO, MRGN)

Pippen <- read_csv("Pippen95-96.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  mutate(MRGN = c(14,22,9,18,4,-6,19,15,6,9,5,-5,3,6,6,7,19,12,9,19,20,9,13,9,
                  14,-6,27,2,14,24,29,26,27,7,12,3,15,20,20,22,11,11,20,15,-6,
                  -10,4,13,3,3,8,26,5,-9,20,21,23,32,9,21,-32,21,-1,31,21,18,8,
                  34,4,-1,13,30,26,6,31,-1,10)) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "MS") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO, MRGN)

Shaq <- read_csv("Shaq2000-2001.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  mutate(MRGN = c(10,-5,9,5,-10,-10,6,16,2,-1,8,20,7,15,17,-33,9,2,11,-3,-8,24,
                  -4,-10,22,3,2,5,5,-5,37,2,11,-23,3,-8,1,13,-11,-11,12,-1,6,
                  -13,6,6,2,3,-15,5,-1,10,2,7,6,-6,10,15,12,12,-4,-12,5,6,12,5,
                  -2,-7,13,12,-21,-24,9,-1,8,12,4,5,26,17,5,17)) %>%
  slice(1:9, 11:24, 26:41, 48:52, 56:61, 64:68, 74, 79:82) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "KS") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO, MRGN)

Stockton <- read_csv("Stockton96-97.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "KJ") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, 
         TRB, BLK, DUO)

Westbrook <- read_csv("Westbrook2019-2020.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  slice(1:6, 8:12, 14:25, 27:32, 34, 35, 37, 38, 40:44, 47:49, 51, 53:56, 
        58:61, 63:67) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "JR") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO)

Kawhi <- read_csv("Kawhi.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  slice(15:18, 20:24, 26:27, 29:31, 33:36, 49:52, 54, 55, 57:68, 71) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "KP") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO)

PG <- read_csv("PG.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  slice(15:18, 20:24, 26:27, 29:31, 33:36, 49:52, 54, 55, 57:68, 71) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "KP") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO)

Simmons <- read_csv("Simmons.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  slice(1:3, 5:26, 28:36, 38:42, 44:49, 51:58, 67:70, 72, 73, 75, 79) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "JB") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO)

Embiid <- read_csv("Embiid.csv") %>%
  rename(OPP = "Opp") %>%
  rename(TPP = "3P%") %>%
  rename(TPA = "3PA") %>%
  rename(Game = "Rk") %>% 
  rename(FGP = "FG%") %>%
  rename(FTP = "FT%") %>% 
  rename(TP = "3P") %>%
  slice(1:3, 5:26, 28:36, 38:42, 44:49, 51:58, 67:70, 72, 73, 75, 79) %>%
  mutate(BSR = BLK + TRB + AST) %>%
  mutate(DUO = "JB") %>%
  select(FG, FGA, FGP, TPA, TPP, TP, PTS, Game, FTA, FT, FTP, AST, STL, TRB, 
         BLK, DUO)

SC30 <- Steph <- SC %>%
  mutate(DUO = "SK") %>%
  select(-OPP)


KT11 <- Kt %>%
  mutate(DUO = "SK") %>%
  select(-OPP)

a <- left_join(x=LBJ, y=AD, by =c("DUO", "Game")) 
b <- left_join(x=Dame, y=CJ, by =c("DUO", "Game")) 
c <- left_join(x=Kawhi, y=PG, by =c("DUO", "Game")) 
d <- left_join(x=SC30, y=KT11, by =c("DUO", "Game")) 
e <- left_join(x=Malone, y=Stockton, by =c("DUO", "Game")) 
f <- left_join(x=Kobe, y=Shaq, by=c("DUO", "Game")) 
g <- left_join(x=Harden, y=Westbrook, by =c("DUO", "Game")) 
h <- left_join(x=MJ, y=Pippen, by =c("DUO", "Game")) 
i <- left_join(x=Embiid, y=Simmons, by =c("DUO", "Game")) 


Duos <- bind_rows(a, b, c ,d, e, f, g, h, i)















ui <-
 navbarPage(theme = shinytheme("cosmo"),
  "The Splash Brothers",
    tabPanel("About",
      fluidPage(
      fixedRow(
      column(10,
      h1("Background"),
      p("The 2015-2016 NBA Season was one of the most memorable seasons in the 
         league’s history. 
         After nearly two decades, the historic 72-10 record by the 1995-96 
         Chicago Bulls, a feat that initially seemed to be insurmountable, 
         had been shattered by the 2015-2016 Golden State Warriors. 
         The Golden State Warriors ended the 2015-2016 NBA Season with a 
         73-9 record. Ultimately, unlike the 1995-96 Chicago Bulls, the Golden 
         State Warriors were unable to capture a championship in their 
         record-breaking season. 
         However, their season was still incredibly remarkable and requires 
         further analysis of the players that were at the core of the Golden 
         State Warriors’ success: Stephen Curry and Klay Thompson. 
         For many, this NBA backcourt is known as “The Splash Brothers” 
         and are regarded as the greatest shooting backcourt in NBA history. 
         In that season, Stephen Curry made 402 three pointers, which is an 
         NBA record for the most three pointers in a single season. 
         Also, Klay Thompson made 276 three pointers, which ranks ninth for 
         the most three pointers in a single season. Stephen Curry would also 
         go on to become the first-ever unanimous MVP (Most-Valuable Player) 
         in NBA history! 
         Together, “The Splash Brothers” have won three NBA championships and, 
         according to many, have many more to come."),
      
        h1("Data"),
    p("As for data, it was collected from ", a("basketball-reference.com", 
      href = "https://www.basketball-reference.com/players/t/thompkl01.html"),". 
 Also, I found data from", a("kaggle.com", href = "https://www.kaggle.com/"), ". 
 These datasets were downloaded as csv files and read in using the read_csv 
 function.", 
        p("You can find the code for this project on ",
        a("GitHub", ref = "https://github.com/LoicTagne/FinalProject")), 
        
        h1("Creator"),
        p(strong("Loic Tagne")),
        p("My name is Loic Tagne and I am an undergraduate student at Harvard 
        College studying Government with a secondary in Economics.
        I can be reached at ", a("loictagne@college.harvard.edu", 
                              href = "mailto: loictagne@college.harvard.edu")),
                                   
                            ),
                            
                            
column(10, style = "margin-top: 3%", img(src = "SK.jpg", width="125%", 
                                         height="100%"), 
       p("Source:", 
       a("Sportscasting", 
         href = "https://www.sportscasting.com/could-steph-curry-and-klay-thompson-play-together-again-this-season/"), 
         align="right"), 
         align= "center"
                            ))))),


                        
tabPanel("Season Statistics", 
 tabsetPanel(
   tabPanel("2015-2016",
   fixedRow(
   column(4, style = "margin-top: 7%",
   radioButtons("PLYRSelect30", "Player: ",
   choices = list("Stephen Curry"="Stephen Curry", 
                  "Klay Thompson"="Klay Thompson")
                                                                      
                                                         ), 
   selectInput("SeasonStat30", "Select a Statistic:",
   choices = list("Field Goals"="FG", 
                  "Field Goals Attempted"="FGA", "Field Goal Percentage"="FGP",
                  "Three Pointers"="TP", "Three Pointers Attempted"="TPA", 
                  "Three Point Percentage"="TPP", "Free Throws"="FT", 
                  "Total Rebounds"="TRB", "Assists"="AST", "Steals"="STL", 
                  "Blocks"="BLK", "Points"="PTS", "Free Throws Attempted"="FTA", 
                  "Free Throw Percentage"="FTP"),
                  selected = "FG"
                                                         ),
                  align="center"
                                                  ),
    column(8, plotlyOutput("Season30", height = "100%"), align="center")
                                              ),
    fixedRow(
    column(8, plotlyOutput("SKTOTAL"), style = "margin-top:5%", 
           align="center"
                                                  ), 
    column(4, style = "margin-top:11%", 
    p("The following graphs have two important takeaways. First, it is clear 
       that Stephen Curry is the higher volume player in the duo. This makes 
       sense as Stephen Curry plays point guard and Klay Thompson plays shooting 
       guard. As a result, most of the offence is facilitated through Stephen 
       Curry. However, Klay Thompson is the better overall defensive player. 
       Secondly, it is evident that the “Splash Brothers” score the majority of 
       their points from beyond the arc. This should come as no surprise as they 
       are the greatest shooting backcourt in NBA history. It will interesting 
       to see how their production is related to one another and how this 
       compares to other historic NBA duos."), 
                                                         align = "center"
                                                  ),
  column(8, style = "margin-top: 10%", plotlyOutput("kbreak") , align= "center"
                                                  ),
  column(8, style = "margin-top: 10%", plotlyOutput("sbreak"), align = "center")
                                                  
                                              )
                                     ))),
                                 
                                 
tabPanel("Model", 
  tabsetPanel(
    tabPanel("Statistics",
    fixedRow(
    column(4, style = "margin-top: 7%",
    radioButtons("DUOSelect", "Duo: ",
                  choices = list("Stephen Curry & Klay Thompson"="SK", 
                                 "Lebron James & Anthony Davis"="LA",
                                 "Damian Lillard & CJ McCollum" = "DC", 
                                 "Joel Embiid & Ben Simmons" = "JB",
                                 "James Harden & Russell Westbrook" = "JR", 
                                 "Kawhi Leonard & Paul George" = "KP",
                                 "Kobe Bryant & Shaquille O'Neal" = "KS", 
                                 "Karl Malone & John Stockton" = "KJ", 
                                 "Michael Jordan & Scottie Pippen" = "MS")
                                                                           
                                                              ), 
     selectInput("DUOSTATX", "Select a Statistic for Player A:
                                                          
                                Stephen Curry, 
                                Lebron James, 
                                Damian Lillard, 
                                Joel Embiid, 
                                James Harden, 
                                Kawhi Leonard, 
                                Kobe Bryant, 
                                Karl Malone, 
                                Michael Jordan"
                                                              ,
                choices = list("Field Goals"="FG.x", 
                               "Field Goals Attempted"="FGA.x", 
                               "Field Goal Percentage"="FGP.x",
                               "Three Pointers"="TP.x", 
                               "Three Pointers Attempted"="TPA.x", 
                               "Three Point Percentage"="TPP.x", 
                               "Free Throws"="FT.x", "Total Rebounds"="TRB.x", 
                               "Assists"="AST.x", "Steals"="STL.x", 
                               "Blocks"="BLK.x", "Points"="PTS.x", 
                               "Free Throws Attempted"="FTA.x", 
                               "Free Throw Percentage"="FTP.x"),
                          selected = "FG.x"
                                                              ), 
     selectInput("DUOSTATY", "Select a Statistic for Player B:
                                                              
                              Klay Thompson,
                              Anthony Davis,
                              CJ McCollum,
                              Ben Simmons,
                              Russell Westbrook
                              Paul George,
                              Shaquille O'Neal,
                              John Stockton,
                              Scottie Pippen"
                                                                                            ,
                  choices = list("Field Goals"="FG.y", 
                                 "Field Goals Attempted"="FGA.y", 
                                 "Field Goal Percentage"="FGP.y",
                                 "Three Pointers"="TP.y", 
                                 "Three Pointers Attempted"="TPA.y", 
                                 "Three Point Percentage"="TPP.y", 
                                 "Free Throws"="FT.y", "Total Rebounds"="TRB.y", 
                                 "Assists"="AST.y", "Steals"="STL.y", 
                                 "Blocks"="BLK.y", "Points"="PTS.y", 
                                 "Free Throws Attempted"="FTA.y", 
                                 "Free Throw Percentage"="FTP.y"),
                            selected = "FG.y"),
                 align="center"
                                                       ), 
    column(8, style = "margin-top: 3%", img(src = "duo.jpg", width="100%", 
                                            height="100%"), 
          p("Source:", 
          a("Statmuse", 
 href = "https://blog.statmuse.com/greatest-nba-duos-of-all-time-1d9a761a46a4"), 
          align="right"), 
      align= "center"
                                                       ),
    column(10, plotlyOutput("DUO", height = "100%"), align="center"),
    column(6, verbatimTextOutput("stan"), height = "100%", 
           style = "margin-top:5%", align="center"),
    column(6, verbatimTextOutput("stan2"), height = "100%", 
           style = "margin-top:5%", align="center"),
    column(12, style = "margin-top:3%", 
          p("The stan_glm output provides a plethora of data. The two outputs is 
          for any given duo and Stephen Curry and Klay Thompson for comparison 
          (the one with 80 observations). For the most part, the emphasis should 
          be placed on the ‘Estimates’ portion. The value in the ‘(Intercept)’ 
            row under the ‘mean’ column represents the statistic Player B would 
            have if Player A had zero of the other given statistic. The value 
            in the ‘xduo’ row under the ‘mean’ column is the slope of the blue 
            regression line shown in the graph above. In other words, for every 
            one additional increase of the Player A statistical value, it will 
            increase/decrease the Player B statistical value by the value shown 
            in the ‘xduo’ row/‘mean’ column. These models should provide a good 
            understanding of the relationship between the performances of the 
            players in a specific duo. Of course, basketball has five players 
            per team. But, in each of these cases, these duos were/are the 
            primary contributors of their respective teams. In addition, the 
            'mean_ppd' row/'mean' column value provides an average of Player 
            B's average in the specified statisical category. Lastly, you will 
            notice for certain duos, primarily the past NBA duos, that there is 
            minimal data surrounding three pointers (made), three point 
            percentage, and three pointers attempted. This is due to the fact 
            that the NBA has been completely transcended in recent years. 
            In the past, three pointers did not play a salient role. However, 
            in today’s NBA, all players at every position are expected to be 
            able to shoot three pointers."), 
          align = "center"))
                                                     
                                                     
                                                    
                                                  
                                                       
                                                       
                                                     ))))   
                                 
                                 
                            
               
               
                            
                        
               

# Define server logic required to draw a histogram
server <- function(input, output) {

output$Season30 <- renderPlotly({ 
 stat30 <- SPLASH %>%
  filter(PLYR == input$PLYRSelect30) %>%
  select(-c(`Score GS`, `Score Opponent`, MRGN, RSLT, MP)) %>%
  pivot_longer(cols = FG:PTS, values_to = "Statistic", names_to = "stat") %>%
  filter(stat == input$SeasonStat30) %>%
  mutate(stat = recode(stat, "FG"="Field Goals", "FGA"="Field Goals Attempted", 
                       "FGP"="Field Goals Attempted", "TP"="Three Pointers", 
                       "TPA"="Three Pointers Attempted", 
                       "TPP"="Three Point Percentage", 
                       "FT"="Free Throws", "TRB"="Total Rebounds", 
                       "AST"="Assists", 
                       "STL"="Steals", "BLK"="Blocks", "PTS"="Points", 
                       "FTP"="Free Throw Percentage", 
                       "FTA"="Free Throws Attempted")) %>%
            drop_na(Statistic)
        
        
            
    plus <-  ggplot(data = stat30, mapping = aes(label = OPP)) +
        geom_point(aes(x = Game, y = Statistic)) +
        geom_line(aes(x = Game, y = Statistic)) +
        geom_hline(lty = "dashed", color = "red", 
                   yintercept = mean(stat30$Statistic)) +
        labs(y = stat30$stat) 
          
      
      
       ggplotly(plus)        
           
        

   })
    

    
    
output$SKTOTAL <- renderPlotly({
  
  SCtotal <- SPLASH %>%
    filter(PLYR == "Stephen Curry") %>%
    select(FG, TP, STL, BLK, AST, TRB, FT, PTS, Game, PLYR, FGA, TPA, FTA) %>%
    drop_na() %>%
    summarize(FG = sum(FG), TP = sum(TP), STL = sum(STL), BLK = sum(BLK), 
              AST = sum(AST), TRB = sum(TRB), FT = sum(FT), PTS = sum(PTS), 
              TPA = sum(TPA), FGA = sum(FGA), FTA = sum(FTA)) %>%
    pivot_longer(names_to = "Category",
                 values_to = "Totals",
                 cols = FG:FTA) %>%
    mutate(Player = "Stephen Curry")
  
 
  
  
  KTtotal <- SPLASH %>%
    filter(PLYR == "Klay Thompson") %>%
    select(FG, TP, STL, BLK, AST, TRB, FT, PTS, Game, PLYR, TPA, FGA, FTA) %>%
    drop_na() %>%
    summarize(FG = sum(FG), TP = sum(TP), STL = sum(STL), 
              BLK = sum(BLK), AST = sum(AST), TRB = sum(TRB), 
              FT = sum(FT), PTS = sum(PTS), TPA = sum(TPA), 
              FGA = sum(FGA), FTA = sum(FTA)) %>%
    pivot_longer(names_to = "Category",
                 values_to = "Totals",
                 cols = FG:FTA) %>%
    mutate(Player = "Klay Thompson")
  
  
  
  TOTAL <- rbind(SCtotal, KTtotal)
  
  
  
xo <-   ggplot(data = TOTAL) +
    geom_bar(mapping = aes(x = Category, y = Totals, fill = Player), 
             stat = "identity", 
             position = "dodge") +
    theme_classic() +
    labs(x = "Statistical Category", 
         title = "Season Totals for Stephen Curry and Klay Thompson")
         

ggplotly(xo)
  })   

    
    
output$kbreak <- renderPlotly ({
  
  kbreak <- SPLASH %>%
    slice(1:82) %>%
    slice(1:10, 12:23, 25:30, 33:58, 60:82) %>%
    select(Game, FG, TP, FT, PTS, OPP) %>%
    rename(FTpts = FT) %>%
    mutate(TPpts = TP * 3) %>%
    mutate(FGpts = (FG - TP) * 2) %>% 
    select(FGpts, TPpts, FTpts, Game, OPP) %>%
    rename(`Field Goal Points` = FGpts) %>%
    rename(`Free Throw Points` = FTpts) %>%
    rename(`Three Pointer Points` = TPpts) %>%
    pivot_longer(names_to = "Breakdown",
                 values_to = "Pts",
                 cols = `Field Goal Points`:`Free Throw Points`)
  
 k <- ggplot(data = kbreak, mapping = aes(label = OPP)) +
    geom_bar(mapping = aes(x = Game, y = Pts, fill = Breakdown), 
             stat = "identity") +
    theme_classic() +
    labs(y = "Points", title = "Klay Thompson's Point Breakdown Each Game", 
         subtitle = "How do the Splash Brothers Get their Points?", 
         caption = "The data shows the 77 games that both Klay Thompson\nand 
         Stephen Curry were active")
 
 ggplotly(k)
 
})
    
output$sbreak <- renderPlotly({ 
  
  sbreak <- SPLASH %>%
  slice(83:164) %>%
  slice(1:10, 12:23, 25:30, 33:58, 60:82) %>%
  select(Game, FG, TP, FT, PTS, OPP) %>%
  rename(FTpts = FT) %>%
  mutate(TPpts = TP * 3) %>%
  mutate(FGpts = (FG - TP) * 2) %>% 
  select(FGpts, TPpts, FTpts, Game, OPP) %>%
  rename(`Field Goal Points` = FGpts) %>%
  rename(`Free Throw Points` = FTpts) %>%
  rename(`Three Pointer Points` = TPpts) %>%
  pivot_longer(names_to = "Breakdown",
               values_to = "Pts",
               cols = `Field Goal Points`:`Free Throw Points`) 

g1 <- ggplot(data = sbreak, mapping = aes(label = OPP)) +
  geom_bar(mapping = aes(x = Game, y = Pts, fill = Breakdown), 
           stat = "identity") +
  theme_classic() +
  labs(y = "Points", title = "Stephen Curry's Point Breakdown Each Game", 
       subtitle = "How do the Splash Brothers Get their Points?", 
       caption = "The data shows the 77 games that both Klay Thompson\nand 
       Stephen Curry were active")


ggplotly(g1)
  
  
})    
    
    
output$DUO <- renderPlotly ({
  
  g2 <- Duos %>%
    filter(DUO == input$DUOSelect)
  
  
  g22 <- ggplot(data = g2, mapping = aes_string(x = input$DUOSTATX, 
                                                y = input$DUOSTATY)) +
    geom_point() +
    geom_smooth(se = FALSE, method = lm) +
    labs(x = input$DUOSTATX, 
         y = input$DUOSTATY)
  
  ggplotly(g22)
  

  
  
})


output$stan <- renderPrint ({
  
  g23 <- Duos %>%
    filter(DUO == input$DUOSelect) 
  
  
  x <- input$DUOSTATX
  y <- input$DUOSTATY
  
  
  
  xduo <- case_when(
    x == "FG.x" ~ g23$FG.x,
    x == "FGA.x" ~ g23$FGA.x,
    x == "FGP.x" ~ g23$FGP.x,
    x == "TP.x" ~ g23$TP.x,
    x == "TPA.x" ~ g23$TPA.x,
    x == "TPP.x" ~ g23$TPP.x,
    x == "FT.x" ~ g23$FT.x,
    x == "TRB.x" ~ g23$TRB.x,
    x == "AST.x" ~ g23$AST.x,
    x == "STL.x" ~ g23$STL.x,
    x == "BLK.x" ~ g23$BLK.x,
    x == "PTS.x" ~ g23$PTS.x,
    x == "FTA.x" ~ g23$FTA.x,
    x == "FTP.x" ~ g23$FTP.x
    )
  
  yduo <- case_when (
    y == "FG.y" ~ g23$FG.y,
    y == "FGA.y" ~ g23$FGA.y,
    y == "FGA.y" ~ g23$FGA.y,
    y == "TP.y" ~ g23$TP.y,
    y == "TPA.y" ~ g23$TPA.y,
    y == "TPP.y" ~ g23$TPP.y,
    y == "FT.y" ~ g23$FT.y,
    y == "TRB.y" ~ g23$TRB.y,
    y == "AST.y" ~ g23$AST.y,
    y == "STL.y" ~ g23$STL.y,
    y == "BLK.y" ~ g23$BLK.y,
    y == "PTS.y" ~ g23$PTS.y,
    y == "FTA.y" ~ g23$FTA.y,
    y == "FTP.y" ~ g23$FTP.y
  )
  
    
  modelduo <- stan_glm(formula = yduo ~ xduo,
                       refresh = 0,
                       data = g23)
  
  print(summary(modelduo))
    
    
    
  
})


output$stan2 <- renderPrint ({
  
  g24 <- Duos %>%
    filter(DUO == "SK") 
  
  
  x <- input$DUOSTATX
  y <- input$DUOSTATY
  
  
  
  xduo <- case_when(
    x == "FG.x" ~ g24$FG.x,
    x == "FGA.x" ~ g24$FGA.x,
    x == "FGP.x" ~ g24$FGP.x,
    x == "TP.x" ~ g24$TP.x,
    x == "TPA.x" ~ g24$TPA.x,
    x == "TPP.x" ~ g24$TPP.x,
    x == "FT.x" ~ g24$FT.x,
    x == "TRB.x" ~ g24$TRB.x,
    x == "AST.x" ~ g24$AST.x,
    x == "STL.x" ~ g24$STL.x,
    x == "BLK.x" ~ g24$BLK.x,
    x == "PTS.x" ~ g24$PTS.x,
    x == "FTA.x" ~ g24$FTA.x,
    x == "FTP.x" ~ g24$FTP.x
  )
  
  yduo <- case_when (
    y == "FG.y" ~ g24$FG.y,
    y == "FGA.y" ~ g24$FGA.y,
    y == "FGA.y" ~ g24$FGA.y,
    y == "TP.y" ~ g24$TP.y,
    y == "TPA.y" ~ g24$TPA.y,
    y == "TPP.y" ~ g24$TPP.y,
    y == "FT.y" ~ g24$FT.y,
    y == "TRB.y" ~ g24$TRB.y,
    y == "AST.y" ~ g24$AST.y,
    y == "STL.y" ~ g24$STL.y,
    y == "BLK.y" ~ g24$BLK.y,
    y == "PTS.y" ~ g24$PTS.y,
    y == "FTA.y" ~ g24$FTA.y,
    y == "FTP.y" ~ g24$FTP.y
  )
  
  
  modelduo2 <- stan_glm(formula = yduo ~ xduo,
                       refresh = 0,
                       data = g24)
  
  print(summary(modelduo2))



})



}

# Run the application 
shinyApp(ui = ui, server = server)
