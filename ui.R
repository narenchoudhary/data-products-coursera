library(shiny)
library(ggplot2)
library(magrittr)
library(reshape2)
library(dplyr)

radioButtons_block <- radioButtons(
    "action_type",
    label = strong(strong(h2("Choose a comparison option:"))),
    choices = c(
        "All players" = "all",
        "Two players" = "two",
        "One player" = "one"
    ),
    selected = "all"
)

fluidRow_header <- fluidRow(
    style = "background-image: url('bg1.jpg'); padding: 20px; margin: 0px;",
    column(
        width = 3,
        img(src="bo1.png")
        
    ),
    column(
        width = 6,
        h1(
            "FIFA Ballon d'Or 2015",
            style = "font-family: 'Open Sans';
                        text-align: center; 
                        color: #FFD700;
                        "
        )
    ),
    column(
        width = 3
    )
)

fluidRow_introduction <- fluidRow(
    column(
        width = 3,
        wellPanel(
            style="font-size: 20px;",
            h2("FIFA"),
            p(
                "The", 
                strong("Fédération Internationale de Football Association"), 
                "is the governing body of association football, futsal and beach football."
            ),
            br(),
            p("FIFA is headquartered in Zürich. 
              Currently, 209 men's national football teams are affiliated to FIFA."),
            p("FIFA, along with IFAB, organizes FIFA World Cup and oversees other confederation tournaments."),
            br(),
            img(src="logo.jpg", height = 100, width = 120),
            br(),
            p(span("FIFA", style = "color: blue"), 
              "logo is registered trademark of ", 
              a("FIFA", href = "http://www.fifa.com/"), ".")
        )
    ),
    column(
        width = 9,
        h1("Overview"),
        p("The FIFA Ballon d'Or is an annual association football award given to the world's 
          best male player by the sport's governing body, FIFA, and the French publication 
          France Football since 2010. It is awarded based on votes from international media 
          representatives and national team coaches and captains, who select the player they 
          deem to have performed the best in the previous calendar year.", style="font-size: 18px;"),
        p("Each voter is allotted", strong("three votes") ," worth ", strong("five points"), ",", strong("three points"), ",",
          strong("one points"), "and the three finalists are ordered based on total number of points.", style="font-size: 18px;"),
        br(),
        p("In FIFA Ballon d'Or 2015, voters casted their votes in favor of 23 players. This application visualizes the distribution of votes among those 23 nominees.
            Individual statistics of a particular player, comparison with another nominees 
            and comparison with all the nominees can be made in this application.", style="font-size: 18px;"),
        p(
            "Voting data was released by",
            a("FIFA.com", href="http://www.fifa.com/ballon-dor/official-documents/index.html"),
            "on Jan 12, 2015.",
            style="font-size: 18px;"
        ),
        h1("How to use this app?"),
        tags$ol(
            tags$li("Select option from sidebar below.", style="font-size: 18px;"),
            tags$li("Each options renders a widget for selecting inputs.", style="font-size: 18px;"),
            tags$li("1st Option, i.e. All Players, allows selecting range of all vote 
                    positions in which players are required. For eg. setting minimum of ", strong("First Choice (5 pts)"), 
                    "at 50 will only show Cristiano Ronaldo and Lionel Messi. More than 50 voters have chosen these two
                    players as their first choice.",style="font-size: 18px;"),
            tags$li("Second option allows selecting two players and comparing their votes."),
            tags$li("Last options presents vote statistics of the player selected in the dialog.")
        ),
        p(strong("Note:")," App is working slowly on", span("shinyapps.io", style="color:blue"), ". So first few operations may take 5-10 seconds.")
    )
)

fluidRow_footer <- fluidRow(
    style="background-image: url('bg5.jpg'); padding:20px; margin-top: 20px;",
    column(
        width = 2,
        p()
    ),
    column(
        width = 8,
        align = "center",
        p(strong("Created for JHU's Cousera Data Products Course Project")),
        a(img(src="github.png", height= 20, width = 20), href="https://www.github.com/narenchoudhary")
    ),
    column(
        width = 2,
        p()
    )
)

fluidRow_plot <- fluidRow(
    column(
        width = 3,
        wellPanel(
            radioButtons_block,
            helpText(paste("Note: ", "Select an option from these 3 options. First 
                           option will compare all nominated candidates. Two players can
                           be compared with second options. Individual player stats can be
                           viewed using last option."), style="font-size: 15px;"),
            uiOutput(
                "radioButtonControls"
            )
        )
    ),
    column(
        width = 9,
        plotOutput("barplot", width = "100%", height = "800px")
    )
)

fluidPage_block <- fluidPage(
    style="font-size: 20px;",
    theme="slider.css",
    title = "FIFA Ballon d'Or 2015",
    titlePanel(
        fluidRow_header
    ),
    fluidRow_introduction,
    fluidRow_plot,
    fluidRow_footer
)

# Define UI for the application
shinyUI(
    fluidPage_block
)