library(shiny)
#library(shinydashboard)
library(ggplot2)
library(dplyr)
library(ggrepel)
library(fontawesome)
#library(shinydashboardPlus)

#library(devtools)#librarie pour import library from github
#install_github("deepanshu88/summaryBox")
#library(summaryBox)

#Question
#Pq les bars ne concordonnent pas avec les ticks -> page aidant

#Variable
home_icon <- icon("house")
senior_icon <- div(icon("person-cane"), " Senior")
help_icon <- div(icon("handshake-angle"), " Carer")
data_icon <- div(icon("database"), " Data Quality")
title_text <- div("Dataviz CALYXIS ", icon("house"))

source_cap <- "Source: Enquete Capacite, Aides et REssources des seniors (CARE menages) - Volet seniors - 2015"

get
#data

#senior_data_old <- read.csv(file = "C:\\Users\\pc\\OneDrive\\Bureau\\IRIAF\\M2 SARADS\\Projet CALYXIS\\data\\SENIOR_DATAVIZ_R.csv")



df_subset1 <- subset(senior_data, senior_data$rev_alr_conj_i != 0)
df_subset2 <- subset(senior_data, senior_data$rev_alr_senior_i != 0)

count_lvl <- carer_data %>% group_by(SCORE_AIDE) %>% summarise(count = n()) 
#count for each assistance lvl the number of values


#interface
ui <- dashboardPage( 
  dashboardHeader(title = "Dataviz Calyxis", titleWidth = 200),
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem(home_icon, tabName = "home"),
      menuItem(senior_icon, tabName = "Senior"),
      menuItem(help_icon, tabName = "Aidant")
      #menuItem(data_icon, tabName = "DataQ")
    )
  ),
  dashboardBody(
    tabItems(
      #home page
      tabItem(tabName = "home",
              fluidRow(align = "justify",
                       h3("Welcome in to this dashboard. "),
                       h3("This dashboard shows some statistics about seniors and their carers from the 2015 CAIRE study. For any information on the data used : ",
                          a("2015 CAIRE Study", href = "https://data.progedo.fr/studies/doi/10.13144/lil-1236"), "."
                       ), tags$br(),
                       h3("Throughout this dashboard, remember to click on the information icon if you are lost. It might help you."),
                       tags$br(), tags$br(), tags$br(), 
                       h3("As a reminder, our problem is the following :"), tags$u(h3("Does being helped by the entourage minimize the probability of falling in the elderly ?"))
              )        
      ),
      #senior page
      tabItem(tabName = "Senior",
              fluidRow(
                box(title = "Senior Age by Gender",
                    plotOutput("plot1"),
                    width = 8,
                    collapsible = TRUE,
                    solidHeader = TRUE),
                
                infoBox("Average age", 
                        format(round(mean(senior_data$AGE), 0), nsmall = 0),
                        icon = icon("calculator"),
                ),
                
                infoBox("Total of women", 
                        nrow(senior_data[senior_data$SEXE == 2, ]),
                        icon = icon("venus")),
                
                infoBox("Total of men", 
                        nrow(senior_data[senior_data$SEXE ==1, ]),
                        icon = icon("mars"))
              ),
              
              fluidRow(
                box(title = "Income of the senior and his spouse",
                    plotOutput("scatter_1"), width = 8,
                    collapsible = TRUE,solidHeader = TRUE),
                actionButton("info_button", icon("info")),
                infoBox(title = tags$p("Number of seniors receiving alimony", style = "font-size: 80%;"), 
                        value = nrow(df_subset2)
                ),
                infoBox(title = tags$p("Number of spouses receiving alimony", style = "font-size: 80%;"), 
                        value = nrow(df_subset1)
                )
              )
              
      ),
      #aidant page
      tabItem(tabName = "Aidant",
              fluidRow(
                box(title = "Carer Age",
                    plotOutput("carer_age_plot"),
                    width = 6,
                    collapsible = TRUE,
                    solidHeader = TRUE
                ),
                box(title = "Carer Gender",
                    plotOutput("carer_gender_plot"),
                    width = 6,
                    collapsible = TRUE,
                    solidHeader = TRUE
                )
              ),
              
              fluidRow(
                box(title = "Level of the assistance provided",
                    plotOutput("lvl_plot"),
                    width = 8,
                    collapsible = TRUE,
                    solidHeader = TRUE
                ),
                actionButton("info_lvl", icon("info")),
                infoBox(title = tags$p("Caregivers living with their senior",
                                       style = "font-size: 80%;"), 
                        value = sum(carer_data$AHENTCOH),
                        icon = icon("house-chimney-user"))
              )
      )
    )
  )
)

#server
server <- function(input, output) {
  
  #home page
  setwd("C:/M2/dataVizPape")
  senior_data <- read.delim(file = "SENIOR_DATAVIZ_2.csv", sep = ",")
  carer_data <- read.delim(file = "CARER_DATAVIZ_R.csv", sep = ",")
  
  #Senior page
  output$plot1 <- renderPlot({
    ggplot(senior_data, aes(x = AGE_CAT, fill = factor(SEXE))) + 
      geom_bar(position = "dodge", alpha = .5, width = .5) + 
      labs(caption =source_cap ) + 
      scale_x_discrete(limits=c('60 to 70','71 to 80','81 to 90','91 to 110')) +
      scale_fill_discrete(labels=c('Masculine', 'Feminine')) +
      labs(y = "", x = "Age cateogry",fill = "Gender") + theme_classic() +
      theme(axis.ticks.y=element_blank(),
            plot.caption = element_text( face = "italic"))
    
  })
  
  output$scatter_1 <- renderPlot({
    ggplot(senior_data, aes(rev_ind_senior_i, rev_ind_conj_i)) +
      geom_point(color = "darkturquoise", size = 2) +
      scale_x_continuous("Senior's Income") +
      scale_y_continuous("Income of the senior's spouse") +
      theme_classic()
  })
  
  observeEvent(input$info_button, {
    showModal(modalDialog(
      title = "Info about the income",
      "In this graph, the income represents the annual income of the senior and his spouse."
    ))
  })
  
  #Carer Page
  output$carer_age_plot <- renderPlot({
    ggplot(carer_data, aes(AHENTAGE)) +
      geom_bar(fill = "#0099f9") +
      labs(y = "", x = "Age", caption = source_cap) +
      theme_classic()
    
  })
  
  output$carer_gender_plot <- renderPlot({
    ggplot(carer_data, aes(AHENTSEX)) +
      geom_bar(fill = "#0099f9", width = .5) +
      labs(caption = source_cap) +
      scale_x_discrete(limits=c('Masculine', 'Feminine')) +
      labs(y = "", x = "Gender") +
      theme_classic()
    
  })
  
  output$lvl_plot <- renderPlot({
    ggplot(count_lvl, aes(x = SCORE_AIDE, y = count)) +
      geom_bar(stat = "identity", fill = "lightpink1") + 
      geom_text(data = count_lvl, aes(x = SCORE_AIDE, y = count_lvl$count, label = count), vjust = -0.5) +
      labs(y = "", x = "Level of assistance", caption = source_cap) +
      
      theme_classic()
    
  })
  
  observeEvent(input$info_lvl, {
    showModal(modalDialog(
      title = "Information about the assistance score",
      "This score is based on the variables : AHEVQ_01, AHEVQ_02, ..., AHEVQ_09.", tags$br(),
      "These variables calculate whether or not caregivers assist seniors with certain actions. In order to facilitate reading, we decided to create a score summarizing these variables.",
      tags$br(),tags$br(),
      "Reading : 24 caregivers have a score of 6. They therefore help their senior for at least 6 activities."
    ))
  })
  
  
}

#app
shinyApp(ui, server)
