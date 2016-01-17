library(shiny)
library(ggplot2)
library(dplyr)
library(magrittr)

#Data
playerwise_men <- readRDS("data/playerwise_men.rds")

# Widgets
min_1pts <- sliderInput(
    "slider1",
    label = h3("3rd Choice (1 point)"),
    min = min(playerwise_men$Third),
    max = max(playerwise_men$Third) + 20,
    value = c(min(playerwise_men$Third), max(playerwise_men$Third) + 20)
)

min_3pts <- sliderInput(
    "slider3",
    label = h3("2nd Choice (3 points)"),
    min = min(playerwise_men$Second),
    max = max(playerwise_men$Second) + 20,
    value = c(min(playerwise_men$Second), max(playerwise_men$Second) + 20)
)

min_5pts <- sliderInput(
    "slider5",
    label = h3("1st Choice (5 points)"),
    min = min(playerwise_men$First),
    max = max(playerwise_men$First) + 20,
    value = c(min(playerwise_men$First), max(playerwise_men$First) + 20)
)

# Selecting widget for one player
playerselect_block <- selectInput(
    "select_player",
    label = h3("Select Player"),
    choices = as.character(playerwise_men$player_name),
    selected = "Messi Lionel"
)

playercomparison1_block <- selectInput(
    "select_comp1",
    label = h3("Select 1st Player"),
    choices = as.character(playerwise_men$player_name),
    selected = "Messi Lionel"
)

playercomparison2_block <- selectInput(
    "select_comp2",
    label = h3("Select 2nd Player"),
    choices = as.character(playerwise_men$player_name),
    selected = "Cristiano Ronaldo"
)

# Functions
all_plot <- function(plot_data_all){
    totals <- plot_data_all %>% group_by(player_name) %>% summarize(total = sum(value))
    # Plot
    ggplot(plot_data_all, aes(x=player_name, y=value, fill = variable)) + 
        geom_bar(stat="identity") + 
        geom_text(aes(player_name, total + 10, label = total, fill = NULL), data = totals, size = 7)+
        theme_bw() + theme(
            axis.text.x = element_text(size=15), 
            axis.text.y = element_text(size = 15),
            #legend.position = "bottom",
            legend.title = element_text(size = rel(2)),
            legend.key = element_rect(color = "black"),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 18),
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            plot.title = element_text(size=rel(2))
            
        ) + 
        scale_y_continuous(breaks = seq(0, 475, 25)) +
        scale_fill_brewer(name = "Vote\nPositions", palette = "Set1") +
        labs(title=expression(underline("Comparison of nominated players")), x = "Players", y = "Votes") + coord_flip()
}

one_plot <- function(r_data){
    ggplot(r_data, aes(x=vote_group, y = vote_value, fill = vote_group)) + 
        geom_bar(stat="identity", color="black") + 
        geom_text(aes(y=vote_value/2, label = vote_value), size = 8) +
        theme_bw() +
        theme(
            axis.text.x = element_text(size=17), 
            axis.text.y = element_text(size = 17),
            #legend.key = element_rect(color = "black"),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 18),
            #legend.position = "top",
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(2)),
            plot.title = element_text(size = rel(2))
        ) +
        scale_fill_brewer("Vote\nPositions", palette = "Set1") +
        scale_y_continuous(breaks = seq(0, 475, 25)) +
        labs(title = expression(underline("Vote Distribution")), x = "Vote Positions", y = "Vote Count")
}

two_plot <- function(r_data){
    ggplot(r_data, aes(x=player_name, y = value, fill=variable)) + 
        geom_bar(stat="identity", position = "dodge", color = "black") + 
        theme_bw() +
        theme(
            axis.text.x = element_text(size=17), 
            axis.text.y = element_text(size = 17),
            #legend.key = element_rect(color = "black"),
            legend.text = element_text(size = 15),
            legend.title = element_text(size = 18),
            #legend.position = "top",
            axis.title.y = element_text(size = rel(1.5)),
            axis.title.x = element_text(size = rel(1.5)),
            legend.title = element_text(size = rel(2)),
            plot.title = element_text(size = rel(2))
        ) + labs(title=expression(underline("Comparison between two nominees")), x = "Players", y = "PositionWise Vote Count") + 
        geom_text(aes(y=value/2, label=value), position = position_dodge(width=1), size = 5) + 
        scale_y_continuous(breaks=seq(0, 350, 25)) + 
        scale_fill_brewer("Vote\nPositions", palette = "Set1") + coord_flip()
}

shinyServer(function(input, output) {
    # Output UI elements based on radio option selected
    output$radioButtonControls <- renderUI({
        if(input$action_type == "one"){
            wellPanel(
                playerselect_block
            )
        }else if(input$action_type == "two"){
            wellPanel(
                playercomparison1_block,
                playercomparison2_block
            )
        }else{
            wellPanel(
                min_5pts,
                min_3pts,
                min_1pts
            )
        }
    })
    
    # Reactive data generation for all option
    plot_data_all <- reactive({
        filterd_data <- filter(playerwise_men, First >= input$slider5[1], First <= input$slider5[2]) %>% 
                filter(Second >= input$slider3[1], Second <= input$slider3[2]) %>% 
                filter(Third >= input$slider1[1], Third <= input$slider1[2])
        melt(filterd_data) %>% filter(variable != "total")
    })
    
    # Reactive data generation for one option
    plot_data_one <- reactive({
        vote_group <- c("First", "Second", "Third")
        vote_value_vec <- playerwise_men[playerwise_men$player_name == input$select_player, ]
        vote_value <- as.numeric(as.vector(vote_value_vec[,2:4]))
        data.frame(vote_group = vote_group, vote_value = vote_value)
    })
    
    # Reactive data generation for two option
    plot_data_two <- reactive({
        r_data <- filter(playerwise_men, player_name == input$select_comp2 | player_name == input$select_comp1)
        melt(r_data) %>% filter(variable != "total")
    })
    
    output$barplot <- renderPlot({
        if(input$action_type == "one"){
            one_plot(plot_data_one())
        }else if(input$action_type == "two"){
            two_plot(plot_data_two())
        }else{
            all_plot(plot_data_all())
        }
    })
})