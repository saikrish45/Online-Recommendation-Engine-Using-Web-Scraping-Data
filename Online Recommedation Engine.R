#Loading the required libraries
library(rvest)
library(ggplot2)
library(rsconnect)
library(shiny)

#Specifying the URL for the website to be scraped and I have used IMDB Top 100 feature films in 2016
url <- 'http://www.imdb.com/search/title?count=100&release_date=2016,2016&title_type=feature'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Let's have a look at the rankings
head(rank_data)

#Checking the data type of rank_data
class(rank_data)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

#Let's have another look at the rankings
head(rank_data)

#Using CSS selectors to scrap the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

#Using CSS selectors to scrap the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

#Let's have a look at the description data
head(description_data)

#Data-Preprocessing: removing '\n'
description_data<-gsub("\n","",description_data)

#Let's have another look at the description data 
head(description_data)

#Using CSS selectors to scrap the Movie runtime section
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')

#Converting the runtime data to text
runtime_data <- html_text(runtime_data_html)

#Let's have a look at the runtime
head(runtime_data)

#Data-Preprocessing: removing mins and converting it to numerical

runtime_data<-gsub(" min","",runtime_data)
runtime_data<-as.numeric(runtime_data)

#Using CSS selectors to scrap the Movie genre section
genre_data_html <- html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data <- html_text(genre_data_html)

#Let's have a look at the runtime
head(genre_data)

#Data-Preprocessing: removing \n
genre_data<-gsub("\n","",genre_data)

#Data-Preprocessing: removing excess spaces
genre_data<-gsub(" ","",genre_data)

#taking only the first genre of each movie
genre_data<-gsub(",.*","",genre_data)

#Convering each genre from text to factor
genre_data<-as.factor(genre_data)

#Let's have another look at the genre data
head(genre_data)

#Using CSS selectors to scrap the IMDB rating section
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')

#Converting the ratings data to text
rating_data <- html_text(rating_data_html)

#Let's have a look at the ratings
head(rating_data)

#Data-Preprocessing: converting ratings to numerical
rating_data<-as.numeric(rating_data)

#Let's have another look at the ratings data
head(rating_data)

#Using CSS selectors to scrap the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- html_text(votes_data_html)

#Let's have a look at the votes data
head(votes_data)

#Data-Preprocessing: removing commas
votes_data<-gsub(",","",votes_data)

#Data-Preprocessing: converting votes to numerical
votes_data<-as.numeric(votes_data)

#Let's have another look at the votes data
head(votes_data)

#Using CSS selectors to scrap the directors section
directors_data_html <- html_nodes(webpage,'.text-muted+ p a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#Let's have a look at the directors data
head(directors_data)

#Data-Preprocessing: converting directors data into factors
directors_data<-as.factor(directors_data)

#Using CSS selectors to scrap the actors section
actors_data_html <- html_nodes(webpage,'.lister-item-content .ghost+ a')

#Converting the gross actors data to text
actors_data <- html_text(actors_data_html)

#Let's have a look at the actors data
head(actors_data)

#Data-Preprocessing: converting actors data into factors
actors_data<-as.factor(actors_data)

#Using CSS selectors to scrap the metascore section
metascore_data_html <- html_nodes(webpage,'.metascore')

#Converting the runtime data to text
metascore_data <- html_text(metascore_data_html)

#Let's have a look at the metascore 
head(metascore_data)

#Data-Preprocessing: removing extra space in metascore
metascore_data<-gsub(" ","",metascore_data)

#Lets check the length of metascore data
length(metascore_data)

#After visual inspection Metascore is missing for 3 movies with rankings 31,68,93
for (i in c(31,68,93)){
  
  a<-metascore_data[1:(i-1)]
  
  b<-metascore_data[i:length(metascore_data)]
  
  metascore_data<-append(a,list("NA"))
  
  metascore_data<-append(metascore_data,b)
  
}

#Let's have another look at length of the metascore data
length(metascore_data)

#Data-Preprocessing: converting metascore to numerical
metascore_data<-as.numeric(metascore_data)

#Let's look at summary statistics
summary(metascore_data)

#Using CSS selectors to scrap the gross revenue section
gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)

#Let's have a look at the votes data
head(gross_data)

#Data-Preprocessing: removing '$' and 'M' signs
gross_data<-gsub("M","",gross_data)

gross_data<-substring(gross_data,2,6)

#Let's check the length of gross data
length(gross_data)

#Filling missing entries of gross data with NA for below movie rankings
for (i in c(18,21,31,68,72,81,88,89,93)){
  
  a<-gross_data[1:(i-1)]
  
  b<-gross_data[i:length(gross_data)]
  
  gross_data<-append(a,list("NA"))
  
  gross_data<-append(gross_data,b)
  
}

#Data-Preprocessing: converting gross to numerical
gross_data<-as.numeric(gross_data)

#Let's have another look at the length of gross data
length(gross_data)

#Combining all the lists to form a data frame
movies_df<-data.frame(Rank = rank_data[1:100], Title = title_data[1:100],
                      
                      Description = description_data[1:100], Runtime = runtime_data[1:100],
                      
                      Genre = genre_data[1:100], Rating = rating_data[1:100],
                      
                      Metascore = metascore_data[1:100], Votes = votes_data[1:100],
                      
                      Gross_Earning_in_Mil = gross_data, Director = directors_data[1:100], Actor = actors_data[1:100])

#Structure of the data frame
str(movies_df)

#Data which got from web-scraping
head(movies_df)

#Analyzing which movie from which Genre has the highest and Lowest Run time
ggplot(data=movies_df,aes(Runtime))+geom_histogram(aes(fill=Genre),bins=30)


#Analyzing which Genre has the highest votes with different Run times
ggplot(data=movies_df,aes(Runtime,Rating))+geom_point(aes(size=Votes,color=Genre))

#Analyzing across all Genres which has highest average gross earnings in runtime 100 to 120
ggplot(data=movies_df,aes(Runtime,Gross_Earning_in_Mil))+geom_point(aes(size=Rating,color=Genre))

#ui.R
    ui <- fluidPage(
      titlePanel("Online Movie Recommendation Engine"),
        fluidRow(
          column(5,
              selectInput(inputId = "movie1", label = h3("Choose Three Movies You Like"),
                       choices = as.character(movies_df$Title)),
           
              selectInput(inputId = "movie2", label = NA,
                       choices = as.character(movies_df$Title)),
           
              selectInput(inputId = "movie3", label = NA,
                       choices = as.character(movies_df$Title)),
  
              submitButton("Submit")
       ),
    
    column(7,
           h3("You Might Like These Too!"),
           tableOutput("table"))
  )
)

#server.R
server<- function(input,output){
  output$table <- renderTable({ 
      #Getting the user inputted movie values from ui.R
      movie1 <- input$movie1 
      movie2 <- input$movie2 
      movie3 <- input$movie3
      
      #Reading the csv file created in ui.R
      #movies_dfs<-read.csv("IMDBmovies.csv")
      
      #Creating empty vector for recommended movies to the user
      recommendedmovies<-c()
      
      #Combining the input movies in to a vector
      mymovies<-c()
      mymovies<-c(mymovies,movie1,movie2,movie3)
      
      #Finding recommended movies using Genre of user inputted movies
      ################Context Based Filtering Approach##################
      
      for (eachmovie in mymovies)
      {
        eachgenre<-movies_df$Genre[movies_df$Title==eachmovie]  
        allmoviesgenre<-movies_df$Title[movies_df$Genre==as.character(eachgenre) & movies_df$Title!=eachmovie]
        recommendedmovies<-c(recommendedmovies,head(as.character(allmoviesgenre),3))
      }
      
      #Converting in to dataframe
      recommendedmovies_df<-as.data.frame(recommendedmovies)
      
      recommendedmovies_df
    })
}  
 
# Run the app
shinyApp(ui = ui, server = server)
