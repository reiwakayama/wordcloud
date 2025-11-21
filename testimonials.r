# Generate word cloud of client testimonials

library(tidytext)
library(dplyr)
library(wordcloud)
library(RColorBrewer)
library(stringr)

testimonials <- readLines("~/Desktop/testimonials.txt", warn = FALSE)
testimonials_df <- data.frame(text = testimonials, stringsAsFactors = FALSE)

custom_stop <- c("rei","team","eventually")

group_phrases <- function(text) {
  text <- tolower(text)  
  text <- gsub("\\blead\\b(?! generation)", "leads", text, perl = TRUE)
  text <- gsub("\\bleading\\b", "leads", text)
  text <- gsub("\\btrack\\b", "tracking", text)
  text <- gsub("[[:punct:]]", " ", text)
  return(text)
}

testimonials_df$text <- group_phrases(testimonials_df$text)

word_data <- testimonials_df %>%
  mutate(text = str_replace_all(text, "[\r\n]", " ")) %>%  # replace newlines
  unnest_tokens(word, text) %>%
  filter(nchar(word) > 2) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% custom_stop) %>%
  count(word, sort = TRUE)

print(head(word_data, 20))

# Make the wordcloud

set.seed(123)
wordcloud(
  words = word_data$word,
  freq = word_data$n,
  min.freq = 1,
  max.words = 30,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2"),
  scale = c(1.8, 0.5)
)

