install.packages(c("tidyverse", "tidytext", "wordcloud", "tm", "SnowballC", "topicmodels"))
Yes

install.packages("slam")
Yes

install.packages("tm")
Yes

install.packages("stopwords")

install.packages("ggwordcloud")
Yes

# Load libraries
library(tidyverse)
library(tidytext)
library(wordcloud)
library(tm)
library(SnowballC)
library(topicmodels)
library(stopwords)
library(ggwordcloud)



############################ LOADING DATASET ###################################
Answer_1 <- read.csv("Answer_1.csv", stringsAsFactors = FALSE)

# Define custom stopwords 
custom_stopwords <- c("data")

# Tokenize and clean text
tidy_answers <- Answer_1 %>%
  unnest_tokens(word, Answer) %>%                         
  filter(!word %in% stopwords("en")) %>%                  
  filter(!word %in% custom_stopwords) %>%                 
  filter(str_detect(word, "^[a-z]+$")) %>%                
  filter(nchar(word) > 2)                                 

# Most common words overall
tidy_answers %>%
  count(word, sort = TRUE) %>%
  head(20)

# 5. Word cloud (excluding "data")
word_freq <- tidy_answers %>%
  count(word, sort = TRUE)

wordcloud(words = word_freq$word,
          freq = word_freq$n,
          max.words = 100,
          colors = brewer.pal(8, "Dark2"))

# 6. Word frequency by Sector
tidy_answers %>%
  group_by(Sector, word) %>%
  summarise(freq = n(), .groups = "drop") %>%
  arrange(desc(freq)) %>%
  slice_max(freq, n = 10, by = Sector) %>%                
  ggplot(aes(x = reorder(word, freq), y = freq, fill = Sector)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Sector, scales = "free") +
  coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Top words by Sector")

# 7. Word frequency by Country
tidy_answers %>%
  group_by(Country, word) %>%
  summarise(freq = n(), .groups = "drop") %>%
  arrange(desc(freq)) %>%
  slice_max(freq, n = 10, by = Country) %>%               
  ggplot(aes(x = reorder(word, freq), y = freq, fill = Country)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Country, scales = "free") +
  coord_flip() +
  labs(x = "Word", y = "Frequency", title = "Top words by Country")



################################# FOCUS IN 3 ANALYSIS ##########################

# 2. Define focus words
focus_words <- c("descriptive", "predictive", "prescriptive")

# 3. Tokenize and keep only focus words
tidy_answers <- Answer_1 %>%
  unnest_tokens(word, Answer) %>%
  filter(word %in% focus_words)

# 4. Count frequency of each word
word_freq <- tidy_answers %>%
  count(word, sort = TRUE)
word_freq

word_freq <- data.frame(
  word = c("descriptive", "predictive", "prescriptive"),
  freq = c(25, 22, 17)
)

# Add color column: red for the most frequent word
word_freq$color <- ifelse(word_freq$freq == max(word_freq$freq), "red", "lightblue")

# Word cloud
ggplot(word_freq, aes(label = word, size = freq, color = color)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 60) +
  scale_color_identity() +  
  theme_minimal()


################################# ANALYSIS PER SECTOR ##########################

tidy_answers <- data.frame(
  Sector = c("Public", "Public", "Public", "Private", "Private", "Private"),
  word = c("descriptive", "predictive", "prescriptive",
           "descriptive", "predictive", "prescriptive"),
  freq = c(12, 10, 8, 13, 12, 9)
)

# Create bar chart
ggplot(tidy_answers, aes(x = word, y = freq, fill = Sector)) +
  geom_col(position = "dodge") +
  labs(title = "How is data analyzed and used in your organization? by Sector",
       x = "Type of Analysis",
       y = "Count") +
  scale_fill_manual(values = c("Public" = "gray", "Private" = "darkgreen")) + # custom colors
  theme_minimal() +
  theme(text = element_text(size = 12))


############################## BY COUNTRY ######################################

Answer_1 <- read.csv("Answer_1.csv", stringsAsFactors = FALSE)

# Ensure Country is character and replace missing
Answer_1$Country <- as.character(Answer_1$Country)
Answer_1$Country[is.na(Answer_1$Country) | Answer_1$Country == ""] <- "Missing data"

# Define focus words
focus_words <- c("descriptive", "predictive", "prescriptive")

# Tokenize and filter only the focus words
tidy_answers <- Answer_1 %>%
  unnest_tokens(word, Answer) %>%
  filter(word %in% focus_words)

# Count frequency by Country
country_summary <- tidy_answers %>%
  group_by(Country, word) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(Country) %>%
  mutate(proportion = count / sum(count)) %>%   
  ungroup()

# Bar chart with values inside bars
ggplot(country_summary, aes(x = word, y = proportion, fill = Country)) +
  geom_col(position = position_dodge(width = 0.8)) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1)),
            position = position_dodge(width = 0.8),
            vjust = -0.5, size = 4) +  
  scale_y_continuous(labels = NULL, expand = expansion(mult = c(0, 0.15))) + # remove y-axis labels
  labs(title = "Proportion of Analysis Types by Country",
       x = NULL,
       y = NULL,
       fill = "Country") +
  theme_minimal() +
  theme(text = element_text(size = 14))