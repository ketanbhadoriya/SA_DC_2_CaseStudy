#Start of the Script

#Loading the required Packages

library(ggplot2)
library(tidytext)
library(dplyr)
library(tidyr)

load(file="shakespeare.rda")

#Exploring the Data

head(shakespeare)

str(shakespeare)

# get_sentiments("bing") %>%
#   count(sentiment)

shakespeare %>%
  count(title,type)

#Tidying the Data
tidy_shakespeare <- shakespeare %>%
  # Group by the titles of the plays
  group_by(title) %>%
  # Define a new column linenumber
  mutate(linenumber=row_number()) %>%
  # Transform the non-tidy text data to tidy text data
  unnest_tokens(word, text) %>%
  ungroup()

tidy_shakespeare

# Pipe the tidy Shakespeare data frame to the next line
tidy_shakespeare %>% 
  # Use count to find out how many times each word is used
  count(word, sort = TRUE)

#Sentiment Analysis

shakespeare_sentiment <- tidy_shakespeare %>%
  # Implement sentiment analysis with the "bing" lexicon
  inner_join(get_sentiments("bing")) 

shakespeare_sentiment

shakespeare_sentiment %>%
  # Find how many positive/negative words each play has
  count(title, sentiment)

# 
# #Question :Which plays have a higher percentage of negative words? 
# Do the tragedies have more negative words than the comedies?

sentiment_counts <- tidy_shakespeare %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count the number of words by title, type, and sentiment
  count(title, type, sentiment)

sentiment_counts %>%
  # Group by the titles of the plays
  group_by(title) %>%
  # Find the total number of words in each play
  mutate(total = sum(n),
         # Calculate the number of words divided by the total
         percent = n / total) %>%
  # Filter the results for only negative sentiment
  filter(sentiment == "negative") %>%
  arrange(percent)

#Comment : tragedies do in fact have a higher percentage of negative words!

#Finding Most common positive and negative words

word_counts <- tidy_shakespeare %>%
  # Implement sentiment analysis using the "bing" lexicon
  inner_join(get_sentiments("bing")) %>%
  # Count by word and sentiment
  count(word, sentiment)

top_words <- word_counts %>%
  # Group by sentiment
  group_by(sentiment) %>%
  # Take the top 10 for each sentiment
  top_n(10) %>%
  ungroup() %>%
  # Make word a factor in order of n
  mutate(word = reorder(word, n))

# Use aes() to put words on the x-axis and n on the y-axis
ggplot(top_words, aes(word, n, fill = sentiment)) +
  # Make a bar chart with geom_col()
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free") +  
  coord_flip()

# #Comments : Death is pretty negative and love is positive, 
# but are there words in that list that  
# the lexicon has misidentified.