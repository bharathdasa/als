#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
#######Installing the required packages  #########
###Install Packages (One Time activity). It is expected to have these packages only if 
###error occurs go ahead and install else proceed to next step.
install.packages("tm")
install.packages("ggplot2")
install.packages("wordcloud")
install.packages("dplyr")
install.packages("plotly")
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#


#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
#######Load all the required libraries  #########
#TM: Used for text mining on the "Genre" columns in the data
library(tm)
#Ggplot2: Used to plot charts
library(ggplot2)
#Wordcloud: Used to chart wordcloud in the genre text analysis
library(wordcloud)
#Dplyr: Used for data manipulation
library(dplyr)
#Plotly: Used to plot interactive charts
library(plotly)
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#


#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
#######Importing the Data  #########
#We read the data from a csv file.
#We use the argument stringsAsFactors = FALSE to make sure the values are considered to
##be characters not as a factors.
###To know more about factors refer https://www.stat.berkeley.edu/classes/s133/factors.html
movie <- as_data_frame(read.csv("C:/AI_Related/DataSet/movie_metadata.csv", stringsAsFactors = FALSE))
#Let us Examine the structure of the dataset to check if we have the new column appened to the 
##dataset
View(movie)
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
#######Data Cleansing  #########
#Remove the 'Â',| from the data 
movie$movie_title <- (sapply(movie$movie_title,gsub,pattern="\\Â",replacement=""))
movie$genres_2 <- (sapply(movie$genres,gsub,pattern="\\|",replacement=" "))
movie$plot_keywords_2 <- (sapply(movie$plot_keywords,gsub,pattern="\\|",replacement=" "))

#Size of the Datset before removing duplicates.
dim(movie)

#Remove the Duplicate Data.
movie = movie[!duplicated(movie$movie_title),]

#Size of the Datset after removing duplicates.
dim(movie)

#Check for missing values
print(paste(sum(complete.cases(movie)),"Complete cases!"))

#We have capability to default the values
#Example -movies$Critic.Score[is.na(movies$Critic.Score)] <- 0
#but in this case we are removing the cases not to tamper the data. 

# Now let us remove the observations with missing values for any of the columns.
movie <- na.omit(movie)

#Size of the Datset after removing NA values.
dim(movie)


#Currency Standardization for two variables gross and budget.
movie <- transform(movie, budget = ifelse(country == "South Korea", budget/1173.49, budget))
movie <- transform(movie, budget = ifelse(country == "Japan", budget/115.33, budget))
movie <- transform(movie, budget = ifelse(country == "Turkey", budget/3.49, budget))
movie <- transform(movie, budget = ifelse(country == "Hungary", budget/298.17, budget))
movie <- transform(movie, budget = ifelse(country == "Thailand", budget/35.67, budget))

movie <- transform(movie, gross = ifelse(country == "South Korea", gross/1173.49, gross))
movie <- transform(movie, gross = ifelse(country == "Japan", gross/115.33, gross))
movie <- transform(movie, gross = ifelse(country == "Turkey", gross/3.49, gross))
movie <- transform(movie, gross = ifelse(country == "Hungary", gross/298.17, gross))
movie <- transform(movie, gross = ifelse(country == "Thailand", gross/35.67, gross))

#Deriving dependent variable
movie$profit_flag <- as.factor(ifelse((movie$gross > movie$budget),1,0))
#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#

#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
#######Descriptive Analysis #########

#Let us consider a variable and apply box plot for the same.
summary(movie$duration)
ggplot(movie, aes(x = "", y = imdb_score)) + geom_boxplot()

#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#


#/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\#
#######Explorartory Data Analysis  #########

#Movie Genre frequency graph
##Since we have multiple genre for same movie frequency of genre cannot be calculated directly.
genre <- Corpus(VectorSource(movie$genres_2))
genre_dtm <- DocumentTermMatrix(genre)
genre_freq <- colSums(as.matrix(genre_dtm))
freq <- sort(colSums(as.matrix(genre_dtm)), decreasing=TRUE) 
genre_wf <- data.frame(word=names(genre_freq), freq=genre_freq)

ggplot(genre_wf, aes(x=reorder(word,-freq), y=freq))+ 
  geom_bar(stat="identity")+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  ggtitle("Movie Genre frequency graph")+
  xlab("Genre")+
  ylab("Frequency")

#WordCloud: Movie Genres
set.seed(10)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(genre_wf$word,genre_wf$freq,random.order=FALSE,
          rot.per=.15, colors=pal2,scale=c(4,.9),
          title="WordCloud: Movie Genres")


#INTERACTIVE 3D Scatter plot: IMDB Score vs Revenue vs Budget
plot_ly(movie, x = ~imdb_score, y = ~budget/1000000, z = ~gross/1000000, 
        color = ~profit_flag, colors = c('#BF382A', '#0C4B8E'),size = I(3)) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'IMDB Score'),
                      yaxis = list(title = 'Budget (M$)'),
                      zaxis = list(title = 'Revenue (M$)')),
         title = "INTERACTIVE 3D Scatter plot: IMDB Score vs Revenue vs Budget",
         showlegend = FALSE)

#\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/#
