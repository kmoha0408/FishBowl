#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(miniUI)
library(tidyverse)
library(lubridate)


# Define UI for application that draws a histogram
ui <- miniPage(
    
    includeCSS("styles.css"),
    # Application title
    miniTitleBar(span(strong("Virtual Game: Fish Bowl"), style = "color: navy")),

    miniTabstripPanel(
        miniTabPanel("Instructions", icon = icon("list-alt"), 
            br(),
            uiOutput("Inst")),
        
        miniTabPanel("Set Up", icon = icon("user-friends"),
            br(),
            write_csv(data.frame(name = character(), score = numeric()), "team1.csv"),
            write_csv(data.frame(name = character(), score = numeric()), "team2.csv"),
            write_csv(data.frame(words = character()), "bowl.csv"),
            uiOutput("Setup")),

        miniTabPanel("Game", icon = icon("play"),
            br(),
            uiOutput("GameInfo")),
        
        miniTabPanel("Score", icon = icon("chalkboard"),
            br(),
            uiOutput("Score"))
    )


)




# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    
    ## Reactive Data Values that are continuously updated and used
    remaining_words <- reactiveValues(data = NULL)
    team1_players <- reactiveValues(data = NULL)
    team2_players <- reactiveValues(data = NULL)
    player_up <- reactiveValues(name = NULL)
    word2guess <- reactiveValues(data = NULL)
    timer <- reactiveVal(60)
    active <- reactiveVal(FALSE)
    
    team_counter <- reactiveValues(data = 1)
    step_counter <- reactiveValues(begin = 1)
    
    current_round <- reactiveValues(r = 1)
    score <- reactiveValues(s1 = numeric(), s2 = numeric())
    total <- reactiveValues(team1 = 0, team2 = 0)
    
    
    ## Actions to happen after submit button is pressed
    observeEvent(input$sub1, {
        
        # Show a text response that the words have been added to the bowl
        showModal(modalDialog(paste0(input$name, ", your words have been added to the bowl! Go to game tab."), footer = NULL))
        
        # Collect all the submitted words
        all_inputs <- reactiveValuesToList(input)
        word_index <- str_detect(names(reactiveValuesToList(input)), "word")
        keep_words <- unlist(all_inputs[word_index])
        
        # Save the name, and team in members file and words in the bowl file
        write.table(data.frame(words = keep_words), file = "bowl.csv", append = TRUE, row.names = FALSE, col.names = FALSE, quote = TRUE, sep = ",")
        
        if(input$teamchoice == "Team 1") {
            write.table(data.frame(name = input$name, score = 0), file = "team1.csv", append = TRUE, row.names = FALSE, col.names = FALSE, quote = TRUE, sep = ",")
            team1_players$data <- read.csv("team1.csv", stringsAsFactors = FALSE)$name
            score$s1 <- read.csv("team1.csv", stringsAsFactors = FALSE)$score
            total$team1 <- sum(score$s1)
        } else {
            write.table(data.frame(name = input$name, score = 0), file = "team2.csv", append = TRUE, row.names = FALSE, col.names = FALSE, quote = TRUE, sep = ",")
            team2_players$data <- read.csv("team2.csv", stringsAsFactors = FALSE)$name
            score$s2 <- read.csv("team2.csv", stringsAsFactors = FALSE)$score
            total$team2 <- sum(score$s2)
        }

        
        Sys.sleep(2)
        removeModal()
        
        # Reset all values to blanks
        updateTextInput(session, "name", value = "")
        updateTextInput(session, "word1", value = "")
        updateTextInput(session, "word2", value = "")
        updateTextInput(session, "word3", value = "")
        updateTextInput(session, "word4", value = "")
        updateTextInput(session, "word5", value = "")
        updateTextInput(session, "word6", value = "")
        #updateTextInput(session, "word7", value = "")
        #updateTextInput(session, "word8", value = "")
        updateRadioButtons(session, "teamchoice", selected = "Team 1")
        
    })
    
    
    ## Let the player know what round it is
    output$CurrentRound <- reactive(
        if(current_round$r == 1){
            "Round 1: Catch Phrase"
        } else if (current_round$r == 2) {
            "Round 2: Charades"
        } else {
            "Round 3: Password"
        }
    )
        
    ## (Step 1) Phrases after entering all the players
    output$ScoreExpl <- reactive(
        "View players on each team and score from the Score Tab"
    )
    
    output$AllPlayIn <- reactive(
        "Wait for all players to enter in the game then press 'All Players In'"
    )
    
    output$WordsInBowl <- reactive(
        paste0("There are ", length(remaining_words$data), " words in the bowl!")
        
    )
    
    ## Re-fill the bowl with the full list of words
    observeEvent({input$load_files}, {
        
        if(is.logical(read.csv("bowl.csv")$words)) {
            showModal(modalDialog(paste0("There are no words in the bowl. Go back to setup menu."), footer = NULL))
            Sys.sleep(2)
            removeModal()
        } else {
            
            remaining_words$data <- read.csv("bowl.csv", stringsAsFactors = FALSE)$words
            
            if(step_counter$begin == 5) {
                step_counter$begin <- 1
                team1_players$data <- read.csv("team1.csv", stringsAsFactors = FALSE)$name
                team2_players$data <- read.csv("team2.csv", stringsAsFactors = FALSE)$name
            }
            
            step_counter$begin <- step_counter$begin + 1
        }
        
        
    })
    
    ## (Step 2) Team information and starting instruction
    output$TEAM1LIST <- reactive(
        paste0("Team 1: ", paste(print(team1_players$data), collapse = ", ")) 
    )
    
    output$TEAM2LIST <- reactive(
        paste0("Team 2: ", paste(print(team2_players$data), collapse = ", ")) 
    )
    
    output$STARTGAME <- reactive(
        "When everyone is ready, choose a team to start then press start round"
    )
    
    observeEvent(input$start_round, {
        
        # Get the first player up
        if(input$teamfirst == "Team 1") {
            team_counter$data <- 1
            player_up$name <- sample(x = team1_players$data, size = 1)
            team1_players$data <- team1_players$data[which(team1_players$data != player_up$name)]
            
        } else {
            team_counter$data <- 2
            player_up$name <- sample(x = team2_players$data, size = 1)
            team2_players$data <- team2_players$data[which(team2_players$data != player_up$name)]
            
        }
        
        step_counter$begin <- step_counter$begin + 1
        
    })
    
    ## (Step 3) Show the current player and draw a word so long one is left
    
    output$CURRENT_PLAYER <- reactive(
        paste0("It is ", print(player_up$name), "'s turn!")
    )
    
    output$TurnInstruction <- reactive(
        "When you are ready, the 60 second clock will start and the word/phrase will appear."
    )
    
    observeEvent(input$begin_turn, {
        
        word2guess$data <- sample(remaining_words$data, size = 1)
        
        active(TRUE)
        
        step_counter$begin <- step_counter$begin + 1
        
    })
    
    ## (Step 4) The game screen to show the timer, word to guess, correct, and pass buttons
    output$timeleft <- renderText(
        paste0("Time left: ", seconds_to_period(timer()))
    )
    
    observe({
        invalidateLater(1000, session)
        isolate({
            if(active()) {
                timer(timer() - 1)
                if(timer() < 1) {
                    active(FALSE)
                    showModal(modalDialog(paste0(print(player_up$name), ", your time is up!"), footer = NULL))
                    Sys.sleep(2)
                    removeModal()
                    timer(60)
                    step_counter$begin <- 3
                    
                    if(team_counter$data == 1) {
                        
                        if(length(team2_players$data) == 0) {team2_players$data <- read.csv("team2.csv", stringsAsFactors = FALSE)$name}
                        
                        player_up$name <- sample(x = team2_players$data, size = 1)
                        team2_players$data <- team2_players$data[which(team2_players$data != player_up$name)]
                        team_counter$data <- 2
                        
                    } else {
                        
                        if(length(team1_players$data) == 0) {team1_players$data <- read.csv("team1.csv", stringsAsFactors = FALSE)$name}
                        
                        player_up$name <- sample(x = team1_players$data, size = 1)
                        team1_players$data <- team1_players$data[which(team1_players$data != player_up$name)]
                        team_counter$data <- 1
                    
                    }
                }
            }
        })
    })
    
    output$guess_word <- reactive(
        "Your team needs to guess: " 
    )
    
    output$this_word <- reactive(
        paste0(print(word2guess$data))
    )
    
    observeEvent(input$correct, {
        
        if(length(remaining_words$data) != 0) {
            word2guess$data <- sample(remaining_words$data, size = 1)
            remaining_words$data <- remaining_words$data[which(remaining_words$data != word2guess$data)]
            
            if(team_counter$data == 1){
                
                index_player <- which(read.csv("team1.csv", stringsAsFactors = FALSE)$name == player_up$name)
                score$s1[index_player] <- score$s1[index_player]  + 1
                total$team1 <- sum(score$s1)
                
            } else {
                index_player <- which(read.csv("team2.csv", stringsAsFactors = FALSE)$name == player_up$name)
                score$s2[index_player] <- score$s2[index_player]  + 1
                total$team2 <- sum(score$s2)
            }
            
        
        } else {
            showModal(modalDialog(paste0(print(player_up$name), ", the bowl is empty!"), footer = NULL))
            Sys.sleep(2)
            removeModal()
            active(FALSE)
            timer(60)
            if(current_round$r != 3) {
                step_counter$begin <- 5
                current_round$r <- current_round$r + 1
            } else {
                step_counter$begin <- 6
            }
            
        }
    })
    
    observeEvent(input$pass, {
        
        remaining_words$data <- c(remaining_words$data, word2guess$data)
        word2guess$data <- sample(remaining_words$data, size = 1)
        remaining_words$data <- remaining_words$data[which(remaining_words$data != word2guess$data)]

        
    })
    
    ## (step 5) Round done
    output$good_job <- reactive(
        paste0("Good job everyone! You have finished round ", print(current_round$r))
    )
    
    output$next_round <- reactive(
        "To start next round, click Start New Round!"
    )
    
    ## (Step 6) End of Game
    
    output$end_game <- reactive(
        "Thanks for playing Fish Bowl! Go to the score tab to see who won!"
    )
    
    output$NewGame <- reactive(
        "If you would like to start a new game, click below and go back to the setup menu."
    )
    
    observeEvent(input$new_game, {
        
        list.files()[str_detect(list.files(), "csv")] %>% map(file.remove)
        
        write_csv(data.frame(name = character(), score = numeric()), "team1.csv")
        write_csv(data.frame(name = character(), score = numeric()), "team2.csv")
        write_csv(data.frame(words = character()), "bowl.csv")
        
        remaining_words$data <- NULL
        team1_players$data <- NULL
        team2_players$data <- NULL
        player_up$name <- NULL
        word2guess$data <- NULL
        timer(60)
        active(FALSE)
        
        team_counter$data <- 1
        step_counter$begin <- 1
        
        current_round$r <- 1
        score$s1 <- numeric()
        score$s2 <- numeric()
        total$team1 <- 0
        total$team2 <- 0
        
        
        
    })
    
    
    ### SCORE SHEET
    
    observe({
        
        output$table1 <- renderTable({
            data.frame(Name = read.csv("team1.csv", stringsAsFactors = FALSE)$name, Score = score$s1)
        })
        
        output$table2 <- renderTable({ 
            data.frame(Name = read.csv("team2.csv", stringsAsFactors = FALSE)$name, Score = score$s2)
        })
 
        
        output$Team1Points <- reactive(paste0("Team 1 Total Points: ", total$team1))
        output$Team2Points <- reactive(paste0("Team 2 Total Points: ", total$team2))
        
        })
        
        
        
    
    
    #################################################################
    
    ## Instructions for how to play
    output$Inst <- renderUI({
        
        verticalLayout(
            
            fillRow(
                br(),
                tags$h1(tags$b("Welcome to Virtual Fish Bowl!"), style = "color: orange"),
                br(),
                flex = c(1.5, 2, 1.5)),
            
            br(), br(), br(), hr(),
            
            fillRow(
                br(),
                verticalLayout(
                    br(),
                    br(),
                    tags$h3(tags$b("How to Set Up the Game"), style = "color: green"),
                    br(),
                    
                    tags$h5("1. Divide into two teams."),
                    tags$h5("2. Write your own 8 clues to add to the Fishbowl by clicking the Setup Tab."),
                    tags$h5(strong("The clues can literally be anything!")),
                    
                    br(),
                    
                    tags$h3(tags$b("How to Play"), style = "color: blue"),
                    tags$h4(tags$em("When it's your turn")),
                    
                    tags$h5("1. Stand in front of your video so your team can see and hear you"),
                    tags$h5("2. You will have 1 minute to give your team as many clues to guess the word/phrase."),
                    tags$h5("3. If you get it correct, press 'I Got it!!' to move on to the next word." ),
                    tags$h5("4. If you don't know the word, press 'Pass' to skip the word." ),
                    tags$h5("5. Go until timer runs out or bowl is empty." )
                    
                ),
                
                verticalLayout(
                    br(),
                    br(),
                    tags$h3(tags$b("Rounds for Virtual Fish Bowl"), style = "color: maroon"),
                    br(),
                    
                    tags$h5("1. Catch Phrase: Give clues only using words and sentences. Motions are not allowed"),
                    tags$h5("3. Charades: Gives clues using only motions. Words or phrases are not allowed" ),
                    tags$h5("2. Password: Give clues using only one word. Words and phrases are not allowed")
                    
                    
                ), flex = c(0.25, 2, 2)))
    })
    
    ## Set up menu to submit words
    output$Setup <- renderUI({
        
        verticalLayout(
            
            
            fillRow(
                br(),
                tags$h1(tags$b("Fish Bowl Setup Menu"), style = "color: green"),
                br(),
                flex = c(2, 3, 1)
            ),
            
            br(), br(), br(), hr(), br(),
            
            fillRow(
                br(),
                
                verticalLayout(
                    textInput(inputId = "name", label = "Your Name:"),
                    br(),
                    radioButtons("teamchoice", "What team are you?", c("Team 1", "Team 2"))),
                
                verticalLayout(
                    textInput(inputId = "word1", label = "Enter First word:"),
                    textInput(inputId = "word2", label = "Enter Second word:"),
                    textInput(inputId = "word3", label = "Enter Third word:")),
                
                verticalLayout(
                    textInput(inputId = "word4", label = "Enter Fourth word:"),
                    textInput(inputId = "word5", label = "Enter Fifth word:"),
                    textInput(inputId = "word6", label = "Enter Sixth word:"),
                    #textInput(inputId = "word7", label = "Enter Seventh word:"),
                    #textInput(inputId = "word8", label = "Enter Eighth word:"),
                    actionButton("sub1", "Submit Words", style = "color: white;
                                                                background-color: navy")), 
                flex = c(0.5,2,2,2))
        )
        
    })
    
    
    ## Output during the game
    output$GameInfo <- renderUI({
        verticalLayout(
            
            fillRow(
                br(),
                tags$h1(tags$b("It's time to play Fish Bowl!"), style = "color: maroon"),
                br(),
                flex = c(2, 3, 1)
            ),
            
            br(), br(), br(), hr(),
            
            fillRow(
                br(),
                tags$h3(textOutput("CurrentRound"), style = "color: purple"),
                br(),
                flex = c(2, 2, 1)
            ),
            
            br(), br(), br(),
            
            if(step_counter$begin == 1) {
                
                fillRow(
                    br(),
                    verticalLayout(
                        tags$h4(strong(textOutput("ScoreExpl"))),
                        br(),
                        tags$h5(em(textOutput("AllPlayIn"))),
                        br(),
                        actionButton("load_files", "All Players In", style = "color: white;
                                                    background-color: navy")
                    ), flex = c(1, 2))
                
            },
            
            if(step_counter$begin == 2) {
                fillRow(
                    br(),
                    verticalLayout(
                        tags$h4(strong(textOutput("WordsInBowl"))),
                        br(),
                        tags$h4(textOutput("TEAM1LIST"), style = "color: green"),
                        br(),
                        tags$h4(textOutput("TEAM2LIST"), style = "color: navy"),
                        br(),
                        tags$h4(textOutput("STARTGAME")),
                        br(),
                        radioButtons("teamfirst", "Which teams turn is it?", c("Team 1", "Team 2")),
                        br(),
                        actionButton("start_round", "Start Round!", style = "color: white;
                                                    background-color: navy"),
                        br()
                    ), flex = c(1, 2))
            },
            
            if(step_counter$begin == 3) {
                fillRow(
                    br(),
                    verticalLayout(
                        tags$h4(strong(textOutput("CURRENT_PLAYER"))),
                        br(),
                        tags$h4(textOutput("TurnInstruction")),
                        br(),
                        actionButton("begin_turn", "Begin Turn", style = "color: white;
                                                    background-color: navy")
                    ),
                flex = c(1,2))
            },
            
            if(step_counter$begin == 4) {
              fillRow(
                  br(),
                  verticalLayout(
                      tags$h3(strong(textOutput("timeleft")), style = "color: red"),
                      br(),
                      tags$h4(textOutput("guess_word")),
                      br(),

                      tags$h2(strong(textOutput("this_word")), style = "color: navy"),
                      
                      br(), br(), br(), br(),
                      
                      tagList(
                          actionButton("correct", " I got it! ", style = "color: white;
                                                background-color: green"),
                          actionButton("pass", "  Pass  ", style = "color: white;
                                                background-color: red"))),
                  flex = c(1, 2)
              )  
            },
            
            if(step_counter$begin == 5) {
                fillRow(
                    br(),
                    verticalLayout(
                        tags$h4(strong(textOutput("goodjob"))),
                        br(),
                        tags$h4(textOutput("next_round")),
                        br(),
                        actionButton("load_files", "Start New Round!", style = "color: white;
                                                    background-color: navy")),
                    flex = c(1, 2))  
            },
            
            if(step_counter$begin == 6) {
                fillRow(
                    br(),
                    verticalLayout(
                        tags$h4(strong(textOutput("end_game"))),
                        br(),
                        tags$h4(textOutput("NewGame")),
                        br(),
                        actionButton("new_game", "Start a New Game!", style = "color: white;
                                                    background-color: navy")),
                    flex = c(1, 2))  
            }
            
            
        )
    })
    
    
    
    ## Displaying the current score
    output$Score <- renderUI({
        
        verticalLayout(
            fillRow(
                br(),
                tags$h1(tags$b("Score Sheet"), style = "color: blue"),
                br(),
                flex = c(2, 2, 1)
            ),
            
            br(), br(), br(), hr(), br(),
            
            fillRow(
                br(),
                verticalLayout(
                    tableOutput("table1"),
                    br(),
                    tags$h4(textOutput("Team1Points"), style = "color: green")),
                br(),
                verticalLayout(
                    tableOutput("table2"),
                    br(),
                    tags$h4(textOutput("Team2Points"), style = "color: navy")),
                flex = c(0.5, 1, 0.2, 1)))
    })
    
}





# Run the application 
shinyApp(ui = ui, server = server)
