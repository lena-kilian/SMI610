## following https://www.tidytextmining.com/topicmodeling.html

library(tidyverse)
library(lubridate)
library(rjson)
library(ggridges)
library(tm)
library(proxy)
library(wordcloud) # take this out of final analysis
library(fpc)
library(cluster)
library(tidytext)
library(GGally)
library(reshape2)
library(plotly)
library(viridis)
library(ggfortify)
library(gridExtra)
library(ggrepel)

#specific to topic modelling
library(topicmodels)
library(gofastr)

set.seed(1234)
ted_data <- read.csv('Data/ted_main.csv', sep=',', header=TRUE, stringsAsFactors = FALSE) %>% 
  distinct() %>%
  mutate(tags = tolower(tags),
         title = tolower(title))
ted_transcripts <- read.csv('Data/transcripts.csv', sep=',', header=TRUE, stringsAsFactors = FALSE) %>% distinct()

# convert film_date from UNIX to datetime format
ted_data <- ted_data %>%
  mutate(film_date = as_datetime(film_date),
         published_date = as_datetime(published_date),
         published_year = as.character(substring(published_date, 1, 4)),
         filmed_year = as.character(substring(film_date, 1, 4)),
         talk_id = c(1:as.integer(count(ted_data))),
         title_length = nchar(title), # add string length of title
         description_length = nchar(description), # add string length of title
         comments_per_view = comments/views) # add comments per view

ted_transcripts <- ted_transcripts %>%
  left_join(select(ted_data, url, talk_id), by='url') %>%
  mutate(transcript = tolower(transcript))


# create dtm
temp_transcripts <- ted_transcripts %>%
  filter(talk_id != 4 & talk_id != 11 & talk_id <= 20)
transcript_corpus <- Corpus(VectorSource(ted_transcripts$transcript)) %>% # convert to corpus
  # clean up
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords("english"))
  

transcript_dtm <- DocumentTermMatrix(transcript_corpus) #%>%
  #remove_stopwords(min.char = 6, denumber = TRUE)

# k-topic LDA (Latent Dirichlet Allocation)
transcript_lda <- topicmodels::LDA(transcript_dtm, k=2, control=list(seed=123))

# interpret results
## word-topic probablities (by topic)
transcript_topics.beta <- tidy(transcript_lda, matrix="beta")

# plot top beta score per topic (beta score "probability of that term being generated from that topic")
transcript_top_terms.beta <- transcript_topics.beta %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

transcript_top_terms.beta %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# words with gretaest differences
beta_spread <- transcript_topics.beta %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1 > .00075 | topic2 > .00075) %>%
  mutate(log_ratio = log2(topic2 / topic1))

beta_spread %>%
  filter(log_ratio >= 12 | log_ratio <= -12) %>%
  ggplot(aes(y=log_ratio, x=reorder(term, log_ratio))) +
  geom_col() +
  coord_flip()

# Document-topic probabilities (gamma is the "per-document-per-topic probability")
transcript_topics.gamma <- tidy(transcript_lda, matrix="gamma")

check <- tidy(transcript_dtm) %>%
  filter(document == 634) %>%
  arrange(desc(count))

transcript_topics.topic <- transcript_topics.gamma %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(as.integer(document)) #%>%
  #mutate(talk_id=ted_transcripts$talk_id)

ggplot(transcript_topics.topic, aes(x=as.character(topic), y=gamma)) +
  geom_boxplot()

### do topic modelling by most important words

####
## TF-IDF analysis
# https://www.tidytextmining.com/tfidf.html
######
transcript_words <- ted_transcripts %>%
  select(-url) %>%
  unnest_tokens(word, transcript) %>%
  count(talk_id, word, sort = TRUE) %>%
  group_by(talk_id) %>%
  mutate(transcript_total = sum(n)) %>%
  ungroup()

# word appearance for talks 1-6
transcript_words %>%
  filter(talk_id<=6) %>%
  ggplot(aes(n/transcript_total, fill=talk_id)) +
  geom_histogram(show.legend = FALSE, bins=50) +
  xlim(NA, 0.02) +
  facet_wrap(~talk_id, ncol = 2, scales = "free_y")

# bind and get importance value
transcript_words.bound <- transcript_words %>%
  bind_tf_idf(word, talk_id, n) %>%
  select(-transcript_total) %>%
  arrange(desc(tf_idf))

# visualise for talks 1-6
transcript_words.bound %>%
  filter(talk_id<=6) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(talk_id) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = talk_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~talk_id, ncol = 2, scales = "free") +
  coord_flip()

# take out 300 most important words
top_words_transcript <- transcript_words.bound %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(talk_id) %>% 
  top_n(500, tf_idf) %>% 
  mutate(top_words = toString(word)) %>%
  ungroup() %>%
  select(talk_id, top_words) %>%
  distinct()

# create dtm
top_words_corpus <- Corpus(VectorSource(top_words_transcript$top_words)) %>% # convert to corpus
  # clean up
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, stopwords("english"))

top_words_dtm <- DocumentTermMatrix(top_words_corpus)

# 2-topic LDA (Latent Dirichlet Allocation)
top_words_lda <- topicmodels::LDA(top_words_dtm, k=2, control=list(seed=1234))

# interpret results
## word-topic probablities (by topic)
top_words_topics.beta <- tidy(top_words_lda, matrix="beta")

# plot top beta score per topic (beta score "probability of that term being generated from that topic")
top_words_top_terms.beta <- top_words_topics.beta %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_words_top_terms.beta %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

# words with gretaest differences
top_words_beta_spread <- top_words_topics.beta %>%
  mutate(topic = paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  mutate(log_ratio = log2(topic2 / topic1))

top_words_beta_spread %>%
  filter(log_ratio >= 11 | log_ratio <= -11) %>%
  ggplot(aes(y=log_ratio, x=reorder(term, log_ratio))) +
  geom_col() +
  coord_flip()

# Document-topic probabilities (gamma is the "per-document-per-topic probability")
transcript_topics.gamma <- tidy(transcript_lda, matrix="gamma")

check <- tidy(transcript_dtm) %>%
  filter(document == 6) %>%
  arrange(desc(count))



# 5-topic LDA (Latent Dirichlet Allocation)
top_words_lda <- topicmodels::LDA(top_words_dtm, k=5, control=list(seed=123))

# interpret results
## word-topic probablities (by topic)
top_words_topics.beta <- tidy(top_words_lda, matrix="beta")

# plot top beta score per topic (beta score "probability of that term being generated from that topic")
top_words_top_terms.beta <- top_words_topics.beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_words_top_terms.beta %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

top_words_topics.gamma <- tidy(top_words_lda, matrix="gamma") %>%
  group_by(document) %>%
  top_n(1, gamma) %>%
  ungroup() %>%
  arrange(as.integer(document)) %>%
  mutate(talk_id=ted_transcripts$talk_id)

ggplot(top_words_topics.gamma, aes(x=as.character(topic), y=gamma)) +
  geom_boxplot()
