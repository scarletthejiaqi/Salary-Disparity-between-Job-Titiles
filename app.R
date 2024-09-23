library(shiny)
library(tidyverse)
library(dplyr)
source("R/monthly.R")

#data cleaning
salary = as_tibble(read_csv('data/Salary.csv'))
df = salary %>% 
  rename(YOE = "Years of Experience", job_title = "Job Title", edu_level = "Education Level") %>% 
  mutate(salary_in_k = Salary/1000,YOE = round(YOE,digits = 1))


# Define UI for application that draws a histogram
ui = navbarPage(
  title = "Salary",
  tabPanel(title = "Input / Visualization",
           titlePanel(title = "Salary by Job Title and Country"),
           sidebarLayout(
             sidebarPanel(
               selectInput(inputId = "jobtype",
                           label = "Job Title: ", 
                           choices = sort(unique(df$job_title)),
                           selected = "Data Analyst"),
               selectInput(inputId = "country",
                           label = "Country: ", 
                           choices = sort(unique(df$Country)),
                           selected = "USA",
                           multiple = TRUE),
               selectInput(inputId = "edu",
                           label = "Educational Level: ", 
                           choices = sort(unique(df$edu_level)),
                           multiple = TRUE),
               checkboxInput(inputId = "filter",
                             label = "table filter by education level",
                             value = FALSE)
             ),
             mainPanel(
               plotOutput("distPlot")
             )
           )
  ),
  tabPanel(title = "Table",dataTableOutput("table")),
  tabPanel(title = "About",includeMarkdown("about.Rmd"))
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  salary_jobtype = reactive({
    df %>% 
      filter(job_title %in% input$jobtype)
  })
  observeEvent(eventExpr = input$jobtype,
               handlerExpr = {
                 updateSelectInput(inputId = "country", 
                                   choices = sort(unique(salary_jobtype()$Country)),
                                   selected = sort(unique(salary_jobtype()$Country)[1])
                                   )
               })
  
  salary_country = reactive({
    salary_jobtype() %>% 
      filter(Country %in% input$country)
  })
  observeEvent(eventExpr = input$country,
               handlerExpr = {
                 updateSelectInput(inputId = "edu",
                                   choices = sort(unique(salary_country()$edu_level)),
                                   selected = sort(unique(salary_country()$edu_level))[1])
               })

  output$distPlot <- renderPlot({
    #sometthing ggplot(data = , aes(color,x,y))
    filtered_data = df %>% 
      filter(job_title %in% input$jobtype) %>% 
      filter(Country %in% input$country) %>%
      filter(edu_level %in% input$edu)
    if (nrow(filtered_data) >= 10) {
      ggplot(data = filtered_data, aes(x = YOE, y = salary_in_k)) +
        labs(title = "Salary Differences based on Years of Experience",
             caption = "Data Source: Kaggle",
             color = "Country") +
        xlab("Year of Experience") +
        ylab("Salary in thousands (k) of dollars") +
        geom_point(aes(color = Country, shape = Country),size=3) +
        geom_smooth() + 
        theme(text = element_text(size = 14))
    } else {
      # If not enough data points, plot only the points
      ggplot(data = filtered_data, aes(x = YOE, y = salary_in_k)) +
        labs(title = "Salary Differences based on Years of Experience",
             caption = "Data Source: Kaggle",
             color = "Country") +
        xlab("Year of Experience") +
        ylab("Salary in thousands (k) of dollars") +
        geom_point(aes(color = Country, shape = Country),size=3) +
        theme(text = element_text(size = 14))
    }
  })
  
  output$table = renderDataTable({
    edutable = salary_country() %>%
      monthly_salary()
    if (input$filter){
      edutable = edutable %>%
        filter(edu_level %in% input$edu)
    }
    edutable
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)
