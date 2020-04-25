library(tidyverse)
library(ggplot2)
library(openxlsx)
library(shiny)
library(DT)


df <- data.frame (Subject = c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,4,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,4,1,1,1,2,2,3,3,4,4,1,1,2,2,3,3,4,4),
                  Start = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,"Headache","Nausea","Vomiting",
                            "Rashes","Diarrhoea","Bleeding","Dizziness","Diarrhoea","Toothache","ABC","ABC",'ABC',"ABC",'ABC','ABC','ABC','ABC'),
                  End = c(30,31,32,33,34,35,36,37,38,39,40,41,42,43,'40',45,46,'47',48,49,50,30,31,32,33,34,35,36,37,'38',39,40,41,42,43,44,45,46,47,48,49,50,
                          'Headache','Nausea','Vomiting','Rashes','Diarrhoea','Bleeding','Dizziness','Diarrhoea','Toothache',1,21,1,31,1,25,1,42),
                  Project = c('AE','SAE','AED','AESI','AE','SAE','AED','AESI','AE','SAE','AED','AESI','AE','SAE','AED','AESI','SAE','AE','SAE','AED','AESI',NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                              NA,NA,NA,NA,NA,NA,NA,NA,NA,"4/25/1995","4/26/1995","4/27/1995","4/28/1995","4/29/1995","4/30/1995","5/1/1995","5/2/1995","5/3/1995",20,32,30,36,25,40,41,49),
                  Task = c('Headache','Nausea','Vomiting','Pain','Bleeding','Dizziness','Itching','Rashes','Diarrhoea','Toothache','Headache','Nausea','Vomiting','Pain','Bleeding','Dizziness',
                           'Itching','Rashes','Diarrhoea','Toothache','Vomiting','Paracetamol','Acenocumarol','Aspirine','Bisoprolol','Ciprofloxacine','Clopidogrel','Furosemide','Losartan','Nitroglicerine',
                           'Rosuvastatine','Rosuvastatine','Spironolactone','Ciprofloxacine','Clonazepan','Pantoprazole','Aspirine','Atenolol','Clopidogrel','Enalapril','Enoxaparine','Losartan','Y','Y','N'
                           ,'Y','N','Y','N','Y','N',10,10,10,10,10,10,10,10),
                  Category = c( rep ("AE",each = 21), rep ("CM", each = 21), rep ("MH", each = 9), rep ("DAR", each = 8)
                  ))




# Define UI for data upload app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("e-Narratives"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(width=3,
      
      
      # Input: Select a file ----
      
      
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
                 DT::dataTableOutput("MH")
        ),tabPanel("DAR",
                   #downloadButton('downloadPlot', 'Download'),
                   DT::dataTableOutput("DAR")
        ),
        tabPanel("Death",
                 #downloadButton('downloadPlot', 'Download'),
                 DT::dataTableOutput("Death")
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
    
    
       
    df1 <- df
    
    paste("Study No: XYZ1235",
          "TREATMENT: ABC 10mg", 
          unique(paste("Subject No:", df1$Subject[df1$Subject==input$dist])) , "\n\n",
          unique(paste("SAE:", df1$Task[df1$Project == "SAE" & df1$Subject==input$dist])), 
          unique(paste("AED:", df1$Task[df1$Project == "AED" & df1$Subject==input$dist])),"\n\n",
          unique(paste("First Dose:", df1$End[ df1$Category == "DAR" & df1$Subject==input$dist][1])),
          unique(paste("Last Dose:", df1$Project[df1$Category == "DAR" &  df1$Subject==input$dist][2])), sep ="\n")[1]
    
  })
  
  output$MH <- DT::renderDataTable({
    
   
    df1 <- df
    
    names(df1) <- c("Subject","Reported Term","Preferred Term","Date of diagnosis", "Ongoing/ Active at study entry","Category")
    return(
      subset (df1[df1$Subject == input$dist,], Category %in% c("MH"), 
              select = c("Subject","Reported Term","Preferred Term","Date of diagnosis", "Ongoing/ Active at study entry")))
    
    
  })
  
  output$DAR <- DT::renderDataTable({
   
    df1 <- df
    
  
    
    names(df1) <- c("Subject","Study drug name","Start Day","End Day", "Dose (mg)","Category")
    return(
      subset (df1[df1$Subject == input$dist,], Category %in% c("DAR"), 
              select = c("Subject","Study drug name","Start Day","End Day", "Dose (mg)")))
    
    
  })
  
  
  
  output$AE <- renderPlot({ 
    
    
    tasks <- df
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
    
    tasks <- df
    
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