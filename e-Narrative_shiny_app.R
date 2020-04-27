library(tidyverse)
library(ggplot2)
library(openxlsx)
library(shiny)


# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("e-Narratives"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Input: Select a file ----
      fileInput("file1", "Choose file",
                multiple = T,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
     
      
      selectizeInput("dist", "Subject:",
                     c("1" = "1",
                       "2" = "2",
                       "3" = "3",
                       "4" = "4"))
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
      # Output: Data file ----
      tabPanel("Description",
               #downloadButton('downloadPlot', 'Download'),
               verbatimTextOutput("Description")
      ),
      tabPanel("MH",
               #downloadButton('downloadPlot', 'Download'),
               tableOutput("MH")
      ),tabPanel("DAR",
                 #downloadButton('downloadPlot', 'Download'),
                 tableOutput("DAR")
      ),
      tabPanel("Death",
               #downloadButton('downloadPlot', 'Download'),
               tableOutput("Death")
      ),
      tabPanel("Adverse events",
               #downloadButton('downloadPlot', 'Download'),
               plotOutput("AE"), height = "250px"
      ),
      
      tabPanel("Concomitant medication",
               #downloadButton('downloadPlot', 'Download'),
               plotOutput("Conmeds"), height = "250px"
      ),
      
      tabPanel("Comments",
               textInput("inputtext", label="", value=" ")
      )
      
      
      )
    )
    
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  d <- reactive({
    dist <- switch(input$dist,
                   "1" = 1,
                   "2" = 2,
                   "3" = 3,
                   "4" = 4)
    
    #dist(input$dist)
  })
  
  output$Description <- renderText({
    
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath) 
        # df <-  read.xlsx(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    df1 <- df
    
     paste("Study No: XYZ1235",
            "TREATMENT: ABC 10mg", 
           unique(paste("Subject No:", df1$Subject[df1$Subject==input$dist])) , "\n\n",
           unique(paste("SAE:", df1$Task[df1$Project == "SAE" & df1$Subject==input$dist])), 
           unique(paste("AED:", df1$Task[df1$Project == "AED" & df1$Subject==input$dist])),"\n\n",
           unique(paste("First Dose:", df1$End[ df1$Category == "DAR" & df1$Subject==input$dist][1])),
           unique(paste("Last Dose:", df1$Project[df1$Category == "DAR" &  df1$Subject==input$dist][2])), sep ="\n")
    
  })
  
  output$MH <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath) 
        # df <-  read.xlsx(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    df1 <- df
    
    names(df1) <- c("Subject","Reported Term","Preferred Term","Date of diagnosis", "Ongoing/ Active at study entry","Category")
    return(
      subset (df1[df1$Subject == input$dist,], Category %in% c("MH"), 
              select = c("Subject","Reported Term","Preferred Term","Date of diagnosis", "Ongoing/ Active at study entry")))
    
    
  })
  
  output$DAR <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read.csv(input$file1$datapath) 
       # df <-  read.xlsx(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    df1 <- df
    
    names(df1) <- c("Subject","Study drug name","Start Day","End Day", "Dose (mg)","Category")
    return(
      subset (df1[df1$Subject == input$dist,], Category %in% c("DAR"), 
              select = c("Subject","Study drug name","Start Day","End Day", "Dose (mg)")))
    
    
  })
  
 
  
  output$AE <- renderPlot({ 
    
    
    tasks <- read.csv(input$file1$datapath)
    #tasks <- read.xlsx(input$file1$datapath)
    
   
    
    tasks.long <- tasks %>%
      filter(Category == "AE") %>%
      gather(date.type, task.date, -c(Subject, Project, Task, Category)) %>%
      arrange(date.type, task.date) %>%
      mutate(Task = factor(Task, levels=rev(unique(Task)), ordered=TRUE))
    
    # Calculate where to put the dotted lines that show up every three entries
    x.breaks <- seq(length(tasks$Task) + 0.5 - 3, 0, by=-3)
    
    tasks.long$Subject <- as.numeric(tasks.long$Subject)
    tasks.long$Project <- as.character(tasks.long$Project)
    tasks.long$Task <- as.character(tasks.long$Task)
    tasks.long$Category <- as.character(tasks.long$Category)
    tasks.long$task.date <- as.numeric(tasks.long$task.date)
   
    
    for (i in unique (tasks.long$Subject)){
   xx <-  ggplot(tasks.long[tasks.long$Subject == input$dist, ], aes(x=Task, y=task.date, colour=Project)) +
    
      geom_line(size=1) +
     # geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") +
      guides(colour=guide_legend(title=NULL)) +
      labs(x="Adverse events", y="Study_Day") + coord_flip() +
     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
           panel.background = element_blank(), axis.line = element_line(colour = "black"),
           panel.border = element_blank(), 
           legend.position="top", legend.box = "horizontal", plot.caption = element_text(hjust = 0))+
     geom_hline(yintercept = 1, linetype="dashed")+
      theme(axis.text.x=element_text(angle=45, hjust=1))
   print (xx)
    }
  })
  
  output$Conmeds <- renderPlot({
    
    tasks <- read.csv(input$file1$datapath)
   
    tasks.long <- tasks %>%
      filter(Category == "CM") %>%
      gather(date.type, task.date, -c(Subject, Project, Task, Category)) %>%
      arrange(date.type, task.date) %>%
      mutate(Task = factor(Task, levels=rev(unique(Task)), ordered=TRUE))
    
    # Calculate where to put the dotted lines that show up every three entries
    x.breaks <- seq(length(tasks$Task) + 0.5 - 3, 0, by=-3)
    
    tasks.long$Subject <- as.numeric(tasks.long$Subject)
    tasks.long$Project <- as.character(tasks.long$Project)
    tasks.long$Task <- as.character(tasks.long$Task)
    tasks.long$Category <- as.character(tasks.long$Category)
    tasks.long$task.date <- as.numeric(tasks.long$task.date)
    
    for (i in unique (tasks.long$Subject)){
      xx <-  ggplot(tasks.long[tasks.long$Subject == input$dist, ], aes(x=Task, y=task.date)) +
        geom_line(size=1) +
        #geom_vline(xintercept=x.breaks, colour="grey80", linetype="dotted") +
        guides(colour=guide_legend(title=NULL)) +
        labs(x="Concomitant Medications", y="Study_Day") + coord_flip() +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"),
              panel.border = element_blank(), 
              legend.position="top", legend.box = "horizontal", plot.caption = element_text(hjust = 0))+
        geom_hline(yintercept = 1, linetype="dashed")+
        theme(axis.text.x=element_text(angle=45, hjust=1))
      print (xx)
    }
  })
  
  output$Comments <- renderText({
    input$Comments
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)