library(httr)
library(tidyverse)
library(jsonlite)
library(rvest)

ii <- "https://s3-media3.fl.yelpcdn.com/bphoto/pghQ3BGbygXPcmOssuMEcA/o.jpg"


url_list = vector()
url_list <- c(url_list, 1:20)
id_list = vector()
id_list <- c(id_list, 1:20)
name_list = vector()
name_list <- c(name_list, 1:20)
rating_list = vector()
rating_list <- c(rating_list, 1:3)
rating_list = vector()
rating_list <- c(rating_list, 1:3)


readRenviron(".Renviron")
r1 <- GET(
  "https://api.yelp.com/v3/businesses/search",
  add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_KEY"))),
  query = list(
    location = "Davis",
    sort_by = "rating",
    limit = 20
  )
)
stop_for_status(r1)
json1 <- content(r1, as = "text", encoding = "UTF-8")
data1 <- fromJSON(json1, flatten = TRUE)
for (i in 1:20){
  url_list[i] <- data1$businesses[[6]][i]
  id_list[i] <- data1$businesses[[1]][i]
  name_list[i] <- data1$businesses[[3]][i]
  rating_list[i] <- data1$businesses[[9]][i]
}

index = c(1:20)
df=bind_cols(index, id_list) %>% 
  mutate(url_list) %>% 
  mutate(name_list) %>% 
  mutate(rating_list) %>% 
  rename(index = ...1) %>% 
  rename(id = ...2)

n=1
some_name = "S & S Deli"

library(shiny)


ui <- fluidPage(
  h1("Top 20 Restaurants in Davis"),
  selectInput("name", "Name", choices = c(df[4])),
  h2("Overall Rating"),
  fluidRow(
    column(2, verbatimTextOutput("Overall_Rating"))
  ),
  h2("Most Recent Reviews"),
  fluidRow(
    column(8, verbatimTextOutput("review1"))
  ),
  fluidRow(
    column(8, verbatimTextOutput("review2"))
  ),
  fluidRow(
    column(8, verbatimTextOutput("review3"))
  ),
  h2("Most Recent Ratings"),
  fluidRow(
    column(2, verbatimTextOutput("rating1"))
  ),
  fluidRow(
    column(2, verbatimTextOutput("rating2"))
  ),
  fluidRow(
    column(2, verbatimTextOutput("rating3"))
  )
  
)

index <- reactiveValues(data = NULL, title = "Title")

server <- function(input, output) {
  
  index <- reactive({
    name = input$name
    
    (while(n <= 20) {
      some_name = df[n,4]
      if (some_name == name) {
        break
      } else {
        n = n+1
      }
    })
    index = n
  })
  
  review1 <- reactive({
    
    name = input$name
    (while(n <= 20) {
      some_name = df[n,4]
      if (some_name == name) {
        break
      } else {
        n = n+1
      }
    })
    r2 <- GET(
      paste0("https://api.yelp.com/v3/businesses/", id_list[n],"/reviews"),
      add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_KEY")))
    )
    stop_for_status(r2)
    json2 <- content(r2, as = "text", encoding = "UTF-8")
    data2 <- fromJSON(json2, flatten = TRUE)
    review1 <- data2$reviews$text[1]
  })
  
  review2 <- reactive({
    
    name = input$name
    (while(n <= 20) {
      some_name = df[n,4]
      if (some_name == name) {
        break
      } else {
        n = n+1
      }
    })
    r2 <- GET(
      paste0("https://api.yelp.com/v3/businesses/", id_list[n],"/reviews"),
      add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_KEY")))
    )
    stop_for_status(r2)
    json2 <- content(r2, as = "text", encoding = "UTF-8")
    data2 <- fromJSON(json2, flatten = TRUE)
    review2 <- data2$reviews$text[2]
  })
  
  review3 <- reactive({
    
    name = input$name
    (while(n <= 20) {
      some_name = df[n,4]
      if (some_name == name) {
        break
      } else {
        n = n+1
      }
    })
    r2 <- GET(
      paste0("https://api.yelp.com/v3/businesses/", id_list[n],"/reviews"),
      add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_KEY")))
    )
    stop_for_status(r2)
    json2 <- content(r2, as = "text", encoding = "UTF-8")
    data2 <- fromJSON(json2, flatten = TRUE)
    review3 <- data2$reviews$text[3]
  })
  
  rating1 <- reactive({
    
    name = input$name
    (while(n <= 20) {
      some_name = df[n,4]
      if (some_name == name) {
        break
      } else {
        n = n+1
      }
    })
    r2 <- GET(
      paste0("https://api.yelp.com/v3/businesses/", id_list[n],"/reviews"),
      add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_KEY")))
    )
    stop_for_status(r2)
    json2 <- content(r2, as = "text", encoding = "UTF-8")
    data2 <- fromJSON(json2, flatten = TRUE)
    rating1 <- data2$reviews$rating[1]
  })
  
  rating2 <- reactive({
    
    name = input$name
    (while(n <= 20) {
      some_name = df[n,4]
      if (some_name == name) {
        break
      } else {
        n = n+1
      }
    })
    r2 <- GET(
      paste0("https://api.yelp.com/v3/businesses/", id_list[n],"/reviews"),
      add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_KEY")))
    )
    stop_for_status(r2)
    json2 <- content(r2, as = "text", encoding = "UTF-8")
    data2 <- fromJSON(json2, flatten = TRUE)
    rating2 <- data2$reviews$rating[2]
  })
  
  rating3 <- reactive({
    
    name = input$name
    (while(n <= 20) {
      some_name = df[n,4]
      if (some_name == name) {
        break
      } else {
        n = n+1
      }
    })
    r2 <- GET(
      paste0("https://api.yelp.com/v3/businesses/", id_list[n],"/reviews"),
      add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_KEY")))
    )
    stop_for_status(r2)
    json2 <- content(r2, as = "text", encoding = "UTF-8")
    data2 <- fromJSON(json2, flatten = TRUE)
    rating3 <- data2$reviews$rating[3]
  })
  
  output$Overall_Rating <- renderPrint({
    print(df[index(),5])
  })
  
  output$review1 <- renderPrint({
    print("Most Recent Review #1")
    print(review1())
  })
  
  output$review2 <- renderPrint({
    print("Most Recent Review #2")
    print(review2())
  })
  
  output$review3 <- renderPrint({
    print("Most Recent Review #3")
    print(review3())
  })
  
  output$rating1 <- renderPrint({
    print("Most Recent Rating #1")
    print(rating1())
  })
  
  output$rating2 <- renderPrint({
    print("Most Recent Rating #2")
    print(rating2())
  })
  
  output$rating3 <- renderPrint({
    print("Most Recent Rating #3")
    print(rating3())
  })
}


shinyApp(ui = ui, server = server)
