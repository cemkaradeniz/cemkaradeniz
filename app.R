library(shiny)
library(dplyr)

# User interface code starts here
ui <- fluidPage(
  h3("GW MBA Basketball Season Record"),
  
  # Sidebar code starts here
  sidebarLayout(
    
    sidebarPanel(
     
      
      # Input: Select a file 
      fileInput("file", "Choose csv file",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      # Input: Select home or away games for the team 
      selectInput("homeoraway",
                  label = "Select home or away games:",
                  choices =c("H"= "home","A"="away"),selected = "home"),
      
      # Input: Select the result of the game: W(Won) or L(Lost)
      selectInput("wonorlost", label = "Select games Won(W) or Lost(L): ",
                  choices = c("W" = "won", "L" = "lost"),selected = "won")
    ),
    # Sidebar code ends here
    
    # Main panel code starts here
    mainPanel(
      tabsetPanel(
        
        #DataTable tab
        tabPanel("Data Table", 
                 tableOutput("contents")),
        tabPanel("Summary",
                 verbatimTextOutput("summary")),
        
        #Plot tab
        tabPanel("Plot",
                 plotOutput("plot_opponentrating") # Plotting the opponent's rating(strength)
        ) 
      )
    )
  )
  # Main panel code ends here
)
# User interface code ends here

library(shiny)
# Server side code starts here
server <- function(input, output){
  
  # Creating a server logic to plot a data table
  output$contents <- renderTable({
    
    # Creating server logic to read selected file  
    req(input$file)
    df <- read.csv(input$file$datapath)
    
    # Creating different subsets based on the combinations that the user puts in place 
    # wonorlost and homeoraway
    df1 <- df %>% filter((WonLost == "W") & (HomeAway == "H"))
    df2 <- df %>% filter((WonLost == "W") & (HomeAway == "A"))
    df3 <- df %>% filter((WonLost == "L") & (HomeAway == "H"))
    df4 <- df %>% filter((WonLost == "L") & (HomeAway == "A"))
    
    
    # Assigning each combination to the subsets created
    if((input$homeoraway == "home") && (input$wonorlost == "won")) 
    {
      return(df1)
    }
    
    else if ((input$homeoraway == "away") && (input$wonorlost == "won")) 
    {
      return(df2)
    }
    
    else if ((input$homeoraway == "home") && (input$wonorlost == "lost")) 
      
    {
      return(df3)
    }
    
    else
    {
      return(df4)
    }
  }
  )
  output$summary <- renderPrint({
    summary(df$Rating)
  })
  # This function summarizes the opponent's ratings and prints it for the user
  output$plot_opponentrating <-renderPlot(
    
    {
      # Reading the data file from input
      req(input$file)
      df <- read.csv(input$file$datapath)
      
      # Matching each combination and plotting
      if((input$homeoraway == "home") && (input$wonorlost == "won")) 
      {
        hist(subset(df$Rating, ((df$WonLost == "W") & (df$HomeAway == "H"))), xlab = "Opponent's Rating",  
             ylab = "count",breaks=20,main="")
      }
      
      else if ((input$homeoraway == "home") && (input$wonorlost == "lost")) 
      {
        hist(subset(df$Rating, ((df$WonLost == "L") & (df$HomeAway == "H"))), xlab = "Opponent's Rating",  
             ylab = "count",breaks=20,main="")
      }
      
      else if ((input$homeoraway == "away") && (input$wonorlost == "won")) 
      {
        hist(subset(df$Rating, ((df$WonLost == "W") & (df$HomeAway == "A"))), xlab = "Opponent's Rating",  
             ylab = "count",breaks=20,main="")
      }
      
      else
      {
        hist(subset(df$Rating, ((df$WonLost == "L") & (df$HomeAway == "A"))), xlab = "Opponent's Rating",  
             ylab = "count",breaks=20,main="")
      }
      
    }
  )
  
}
# Returning ui and server to shiny
shinyApp(ui, server)


