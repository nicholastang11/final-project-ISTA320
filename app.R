#
#Author: Nick Tang
#ISTA320
#Final Project
#This script is to create a shiny app that displays visualizations
#from a data source on kaggle
#

library(lubridate)
library(usmap)
library(shiny)
library(tidyverse)

mental_health_survey_data <- read_csv("data/survey.csv")

#change Gender col to lower for pre-processing and creating 
#collections of different gender inputs and categorizing
mental_health_survey_data$Gender <- tolower(mental_health_survey_data$Gender)

male <- c("male","m","male-ish", "cis male","mail","malr", "mal","make","msle","cis man")
female <- c("female","trans-female","cis female","f","female (trans)","female (cis)","woman")
other_gender <- c("queer/she/they","non-binary","nah","all","enby","fluid","genderqueer","androgyne",
                  "agender","male leaning androgynous","queer","a little about you","p")

#min and max ages are unrealistic, will filter out
min(mental_health_survey_data[,'Age']) 
max(mental_health_survey_data[,'Age'])

#tidying data to make sure the person works for tech company
#and to select the desired columns for viz
mental_health_survey_data_tidy <- mental_health_survey_data %>% 
    filter(tech_company=='Yes',!is.na(work_interfere),!is.na(Age), Age>11, Age<100,!is.na(state)) %>% 
    select(Age,Gender,state,family_history:tech_company)

#changing Gender col to male, female, or other
mental_health_survey_data_tidy$Gender <-  
    ifelse(mental_health_survey_data_tidy$Gender %in%
               male, 'male', mental_health_survey_data_tidy$Gender) 

mental_health_survey_data_tidy$Gender <-  
    ifelse(mental_health_survey_data_tidy$Gender %in%
               female, 'female', mental_health_survey_data_tidy$Gender)

mental_health_survey_data_tidy$Gender <-  
    ifelse(mental_health_survey_data_tidy$Gender %in%
               other_gender, 'other', mental_health_survey_data_tidy$Gender)

#make new col for age groups
mental_health_survey_data_tidy <- mental_health_survey_data_tidy %>% 
    mutate(age_group = case_when(Age >= 18  & Age < 25 ~ '18-24',
                                 Age >= 25  & Age < 31 ~ '25-30',
                                 Age >= 31  & Age <36  ~ '31-35',
                                 Age >= 36  & Age <41  ~ '36-40',
                                 Age >= 41  & Age <46  ~ '41-45',
                                 Age >= 46  & Age <51  ~ '46-50',
                                 Age >= 51  & Age <56  ~ '51-55',
                                 Age >= 56  & Age <61  ~ '56-60',
                                 Age >= 61  & Age <66  ~ '61-65',
    ))

#Reordering work_interfere column
mental_health_survey_data_tidy$work_interfere <- 
    factor(mental_health_survey_data_tidy$work_interfere,
    levels=c("Never", "Rarely", "Sometimes","Often"))

#Reordering no_employees column
mental_health_survey_data_tidy$no_employees <- 
    factor(mental_health_survey_data_tidy$no_employees,
           levels=c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000"))

#orders age_group col
age_options <- mental_health_survey_data_tidy %>%
    distinct(age_group) %>%
    arrange(age_group)

#orders Gender col
gender_options <- mental_health_survey_data_tidy %>%
    distinct(Gender) %>%
    arrange(Gender)

# Define UI for application that displays data viz
ui <- fluidPage(
    
    # Application title
    titlePanel("Nick Tang ISTA320 Final Project"),
    
    # Sidebar with info about the data
    sidebarLayout(
        fluid=TRUE,
        sidebarPanel(
            
            tags$div(class="header", checked=NA,
                     p("These visualizations analyze data from Kaggle about worker's mental health in the tech industry"),
                     a(href="https://www.kaggle.com/osmi/mental-health-in-tech-survey", "Data Source"),
                     hr(),
                     h3("Data Description:"),
                     p("This data was collected as a survey and needed a lot of\n
                        preprocessing to be able to make visualizations.\n
                        Some of the most used columns in this data set include:\n
                        ",br(),
                       strong("Age"),br(),
                       strong("Gender"),br(),
                       strong("State"),br(),
                       strong("family_history"),br(),
                       strong("no_employees"))
                     
            )
        ),
        
        # Show plots and tabs
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Ages vs Mental Health", 
                                 selectInput("age_to_highlight",
                                             "Select Age Group to Plot:",
                                             choices = age_options),
                                 plotOutput("age_plot")),
                        tabPanel("Genders vs Mental Health",
                                 selectInput("gender_to_highlight",
                                             "Select Gender to Plot:",
                                             choices = gender_options),
                                 plotOutput("gender_plot")),
                        tabPanel("Tech Age Density",
                                 plotOutput("usa_plot")),
                        tabPanel("Company Size",
                                 plotOutput("company_size_plot")),
                        tabPanel("Family History",
                                 plotOutput("family_history_plot")))
        )
    )
)


# Defines server logic required to draw the plots
server <- function(input, output) {
    
    output$age_plot <- renderPlot({
        mental_health_survey_data_tidy %>%
            filter(age_group == input$age_to_highlight) %>% 
            ggplot(aes(x=work_interfere,y=age_group,col=work_interfere)) +
            geom_point()+
            geom_jitter(width = .3, height = .4, size = 3,alpha = .8)+
            ggtitle("How Tech Jobs Affect Mental Health (Age)") +
            labs(x = "How Much Work Interferes With Mental Health",
                 y = "Age Group",
                 caption = "There are no siginificant differences between age groups
                 and stress levels at work")+
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14,face = "bold"),
                  plot.title = element_text(size = 16,face = "bold"),
                  plot.caption = element_text(size = 10, face = "italic")
            )+
            scale_color_manual(values = c("red", "seagreen", "dodgerblue2","orange"))
    })
    
    output$gender_plot <- renderPlot({
        mental_health_survey_data_tidy %>%
            filter(Gender == input$gender_to_highlight) %>% 
            ggplot(aes(x=work_interfere,y=Gender,col=work_interfere)) +
            geom_point()+
            geom_jitter(width = .3, height = .4, size = 3, alpha = .8)+
            ggtitle("How Tech Jobs Affect Mental Health (Gender)") +
            labs(x = "How Much Work Interferes With Mental Health",
                 y = "Gender",
                 caption = "It looks like no matter what gender, the majority will feel some stress at work") +
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14,face = "bold"),
                  plot.title = element_text(size = 16,face = "bold"),
                  plot.caption = element_text(size = 10, face = "italic")
                  )+
            scale_color_manual(values = c("red", "black", "dodgerblue2","orange"))
    })
    
    output$usa_plot <- renderPlot({
        mental_health_survey_data_tidy %>% 
            plot_usmap(data = ., 
                       values = "Age") +
            theme(legend.position = "right") +
            scale_fill_continuous(name = "Ages",
                                  low = "lightblue", 
                                  high = "darkorchid") +
            ggtitle("Ages of Tech Workers Across the United States") +
            theme(plot.title = element_text(size = 14, face = "bold")
                  )
    })
    
    output$company_size_plot <- renderPlot({
        mental_health_survey_data_tidy %>% 
            ggplot(aes(x=work_interfere,fill=no_employees))+
            geom_bar()+
            ggtitle("Number of Employees and Mental Helath") +
            labs(x = "How Much Work Interferes With Mental Health",
                 y = "Count") +
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14,face = "bold"),
                  plot.title = element_text(size = 16,face = "bold")
                  )
        
    })
    
    output$family_history_plot <- renderPlot({
        mental_health_survey_data_tidy %>% 
            filter(work_interfere == "Sometimes" | work_interfere == "Often") %>% 
            ggplot(aes(x=work_interfere,fill=family_history))+
            geom_bar() +
            ggtitle("Worker's mental health and Their Family History") +
            labs(x = "How Much Work Interferes With Mental Health",
                 y = "Count",
                 caption = "This plot shows that those with history of mental health problems
                 are more likely to suffer from issues in work") +
            theme(axis.text = element_text(size = 12),
                  axis.title = element_text(size = 14,face = "bold"),
                  plot.title = element_text(size = 16,face = "bold"),
                  plot.caption = element_text(size = 10, face = "italic")
                  )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
