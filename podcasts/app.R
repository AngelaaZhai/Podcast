## ui.R ##
library(shiny)
library(shinydashboard)
library(tidyverse)
library(wordcloud)
library(dplyr)
library(stringr)
library(tidytext)
library(RColorBrewer)
library(benford.analysis)
library(lubridate)
library(data.table)
library(ggraph)
library(igraph)

## Read dataset into R and clean
podcasts1 <- read_csv("podcasts1.csv")
podcasts2 <- read_csv("podcasts2.csv")
podcasts <- rbind(podcasts1, podcasts2)

podcasts <- podcasts %>%
  mutate(categories_num = str_count(categories, pattern="\\|")+1)

## English podcasts
podcasts_en <- podcasts %>%
  filter(language=="English")
## get 99316 English podcasts

## seperate categories
podcasts_cat <- podcasts_en %>%
  mutate(categories = strsplit(categories, "\\|")) %>% 
  unnest(categories) %>%
  mutate(categories = trimws(categories)) 


## Some audio length less or equal to 0, and some are longer than A DAY
## 656298 English episodes

## Separate by categories

## get all categories follow alphabetic order
category_order <- podcasts_cat %>%
  group_by(categories) %>%
  summarise()



##--- SHINY
ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Podcasts"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("tachometer")),
      menuItem("Podacasts", icon = icon("podcast"),
        menuSubItem("Language", tabName = "language", icon = icon("language")),
        menuSubItem("Category", tabName = "category", icon = icon("sliders")),
        menuSubItem("Description", tabName = "description", icon = icon("comment"))
      )
    )
  ),
  dashboardBody(
    tabItems(
      ## intro part
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Introduction", status = "info", 
            solidHeader = TRUE, collapsible = TRUE, width = 12,
            "This Shiny App is used to show some findings from published podcasts and episodes in December, 2017."
          )
        ),
        fluidRow(
          valueBox("Podcasts", 121175, icon = icon("podcast"), color = "yellow"),
          valueBox("Episodes", 881046, icon = icon("headphones"), color = "olive"),
          valueBox("Categories", 67, icon = icon("sliders"), color = "orange")
        ),
        fluidRow(
            img(src="earphone.png", width = 600,
                style="display: block; margin-left: auto; margin-right: auto;")
        )
      ),
      
      ## language bar chart panel
      tabItem(
        tabName = "language", 
        fluidRow(
          ## plot box
          box(
            plotOutput("plot1"), title = "Bar Chart", status = "primary", 
            solidHeader = TRUE, collapsible = TRUE, width = 8
          ),
          
          ## slider box
          box(
            sliderInput("slider", "Number of Languages:", 1, 43, 10),
            title = "Inputs", status = "warning", solidHeader = TRUE, width = 4, collapsible = TRUE
          ),
          
          ## text box
          box(
            "Most of the podcasts are by English. Only a few used other languages.",
            width = 12
          )
        )
      ),
      
      ## podcasts categories
      tabItem(
        tabName = "category",
        fluidRow(
          box(
            plotOutput("plot2"), title = "Bar Chart", status = "primary", 
            solidHeader = TRUE, collapsible = TRUE, width = 8
          ),
          box(
            sliderInput("slider2", "Number of Categories:", 1, 67, 10),
            title = "Inputs", status = "warning", solidHeader = TRUE, width = 4, collapsible = TRUE
          ),
          box(
            "Each podcasts are collected under different categories, and some providers will assign a bunch of categories to their podcasts.",
            width = 12
          )
        )
      ),
      

      ## podcasts description
      tabItem(
        tabName = "description",
        fluidRow(
          box(
            plotOutput("plot5"), title = "Wordcloud", status = "primary", 
            solidHeader = TRUE, collapsible = TRUE, width = 8
          ),
          box(
            selectInput("category", "Category:", category_order$categories, "Music"),
            title = "Inputs", status = "warning", solidHeader = TRUE, width = 4, collapsible = TRUE
          )
        )
      )
    )
  )
)

## app.R ##

server <- function(input, output) {
  
  output$plot1 <- renderPlot({
    ## different language podcasts
    languages_top <- podcasts %>%
      group_by(language) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(input$slider)
    
    language_plot <- podcasts %>%
      inner_join(languages_top, by="language")
    
    ggplot(data=language_plot, mapping=aes(x=reorder(language, table(language)[language]))) +
      geom_bar(aes(fill=language)) +
      coord_flip() +
      ggtitle("Bar Chart for Podcasts Languages") +
      labs(x="Languages", y="Count") +
      theme(panel.background = element_rect(fill = "white", colour = "grey50"),
            plot.title = element_text(size = 12), axis.title = element_text(size = 12),
            plot.subtitle = element_text(size = 10)) +
      guides(fill=FALSE)
  })
  
  output$plot2 <- renderPlot({
    categories_top <- podcasts_cat %>%
      group_by(categories) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(input$slider2)
    
    categories_plot <- podcasts_cat %>%
      inner_join(categories_top, by="categories")
    
    ggplot(data=categories_plot, mapping=aes(x=reorder(categories, table(categories)[categories]))) +
      geom_bar(aes(fill=categories)) +
      coord_flip() +
      ggtitle("Bar Chart for Categories") +
      labs(x="Categories", y="Count") +
      theme(panel.background = element_rect(fill = "white", colour = "grey50"),
            plot.title = element_text(size = 12), axis.title = element_text(size = 12),
            plot.subtitle = element_text(size = 10)) +
      guides(fill=FALSE)
  })
  


  output$plot5 <- renderPlot({
    ## how podcasts describe their content by categories
    tidy_description <- podcasts_cat %>%
      filter(categories==input$category) %>%
      mutate(description = str_replace_all(description, pattern = "<(.*?)>", "")) %>%
      unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word) %>%
      filter(!word2 %in% stop_words$word) %>% 
      filter(!is.na(word1)) %>%
      filter(!is.na(word2)) %>%
      count(word1, word2, sort = TRUE) %>%
      head(50) %>%
      mutate(word = paste(word1, word2, sep=" "))
    
    set.seed(2018)
    wordcloud(tidy_description$word, tidy_description$n, scale = c(2,0.5), colors = brewer.pal(6, "Set2"), rot.per = 0)
  })
  
}

shinyApp(ui, server)