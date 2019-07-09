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

ted_data <- read.csv('Data/ted_main.csv', sep=',', header=TRUE, stringsAsFactors = FALSE) %>% 
  distinct() %>%
  mutate(tags = tolower(tags),
         title = tolower(title))
ted_transcripts <- read.csv('Data/transcripts.csv', sep=',', header=TRUE, stringsAsFactors = FALSE) %>% distinct()

# explore ted_data variables
names(ted_data)

# convert film_date from UNIX to datetime format
ted_data <- ted_data %>%
  mutate(film_date = as_datetime(film_date),
         published_date = as_datetime(published_date),
         talk_id = c(1:as.integer(count(ted_data)))) %>%
  mutate(title_length = nchar(title), # add string length of title
         comments_per_view = comments/views) # add comments per view

# look at views over time
ggplot(ted_data, aes(x=film_date, y=log(views))) + # by film date
  geom_point()

ggplot(ted_data, aes(x=published_date, y=views)) + # by published date
  geom_point()


# explore other variables
ggplot(ted_data, aes(x=languages, y=log(views))) + # by number of languages
  geom_jitter() +
  geom_smooth()

ggplot(ted_data, aes(x=comments_per_view.log, y=log(views))) + # by comments per view
  geom_jitter()

ggplot(ted_data, aes(x=log(comments_per_view))) +
  geom_histogram()

ggplot(ted_data, aes(x=log(views))) +
  geom_histogram()

ggplot(ted_data, aes(x=log(comments))) +
  geom_histogram()

ggplot(ted_data, aes(x=title_length, y=log(views))) + # by title length
  geom_jitter() +
  geom_smooth()

ggplot(ted_data, aes(x=log(comments), y=log(views))) + # by number of comments
  geom_jitter() +
  geom_smooth()

ted_data %>% gather('date_type', 'date', c('film_date', 'published_date')) %>%
  ggplot(aes(x=date, y=languages, colour=date_type)) + # by film date
  geom_jitter(size=0.5)

ggplot(ted_data, aes(x=film_date, y=languages)) + # by film date
  geom_point()

ggplot(ted_data, aes(x=film_date)) + # by film date
  geom_histogram(bins=50)
ggplot(ted_data, aes(x=published_date)) + # by film date
  geom_histogram(bins=60)
ggplot(ted_data, aes(x=log(views))) + # by film date
  geom_histogram(bins=50)
ggplot(ted_data, aes(x=languages)) + # by film date
  geom_histogram(bins=50)

# analyse ratings
ted_ratings <- ted_data %>%
  select(talk_id, ratings) %>%
  mutate(ratings2 = toString(ratings),
         ratings2 = str_remove_all(ratings2, '\\[|\\]|\\{'),
         ratings2 = strsplit(ratings2, split='}', fixed=TRUE))

# function to clean up ratings
ted_talkid <- ted_data$talk_id
clean_ratings <- function(n){
  
  ted_ratings <- data.frame(ratings = ted_data$ratings[n]) %>%
    mutate(ratings2 = toString(ratings),
           ratings2 = str_remove_all(ratings2, '\\]|\\{|\\,|\\ '),
           ratings2 = strsplit(ratings2, split='}', fixed=TRUE))
  
  data <- data.frame(var1 = ted_ratings$ratings2[1]) %>%
    separate(1, into = c('empty', 'id', 'name', 'count'), sep=':', remove=TRUE) %>%
    select(-empty) %>%
    mutate(id = str_remove(id, ", 'name'"),
           name = str_remove(name, "''count'"),
           name = str_remove(name, "'"),
           count = as.integer(count)) %>%
    group_by(id) %>%
    mutate(total_count = sum(count)) %>%
    ungroup() %>%
    select(name, total_count) %>%
    distinct()
  row.names(data) <- data$name
  data <- data %>%
    select(total_count) %>%
    t()
  data <- as.data.frame(data) %>%
    mutate(talk_id = ted_talkid[n])
  return(data)
}


# clean ratings and make ratings df
ratings_data <- clean_ratings(1)

for (i in 2:as.integer(count(ted_data))) {
  temp_data <- clean_ratings(i)
  ratings_data <- rbind(ratings_data, temp_data)}

ratings_data <- ratings_data %>%
  mutate(total = Funny + Beautiful + Ingenious + Courageous + Longwinded + Confusing + Informative + Fascinating +
           Unconvincing + Persuasive + `Jaw-dropping` + OK + Obnoxious + Inspiring)

# convert to percentage
temp <- select(ratings_data, -talk_id, -total)
ratings_percent <- (temp / rowSums(temp) * 100) %>%
  mutate(talk_id = ted_talkid)

# visualising relationships 
names(ratings_data)

temp_data <- left_join(ted_data, ratings_percent, by='talk_id')
ggplot(temp_data, aes(x=Funny, y=views))+
  geom_point()

# look at percentage because more views are likely linked to more comments
ratings_temp <- gather(ratings_percent, 'characteristic', 'rating', c('Funny':'Inspiring'))
ratings_temp %>% right_join(select(ted_data, talk_id, views), by='talk_id') %>%
  ggplot(aes(x=rating, y=log(views))) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_wrap(~characteristic, scales = 'free')

# distributions of percentage ratings
ratings_temp %>% right_join(select(ted_data, talk_id, views), by='talk_id') %>%
  ggplot(aes(x=rating, y=reorder(characteristic, rating), fill = ..x..)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

ratings_temp %>% mutate(rating = log(rating)) %>% spread(characteristic, rating) %>% select(-talk_id) %>% 
  pairs(pch=19)

ratings_temp %>% mutate(rating = log(rating)) %>% spread(characteristic, rating) %>% select(-talk_id) %>% 
  ggpairs() #+ ggtitle("Anderson's Iris Data -- 3 species")

#######
### text analysis
######

# function for plot
wordfreq_by_cluster <- function(n_cluster, top_words) {
  data_temp <- filter(clusters, cuts == n_cluster)
  temp_corpus <- Corpus(VectorSource(data_temp$tags)) %>% # convert to corpus
    tm_map(removeWords, stopwords("english")) %>% # clean up
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>%
    tm_map(removeWords, c('tedx', 'fellow', 'ted'))
  
  temp_dtm <- DocumentTermMatrix(temp_corpus)
  temp_freq <- colSums(as.matrix(temp_dtm))
  
  temp_freq_df <- data.frame(word=names(temp_freq), freq=temp_freq) %>%
    arrange(freq) %>%
    top_n(top_words)

  ggplot(subset(temp_freq_df), aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity") + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
}

# prepare data
names(ted_data)

tag_corpus <- Corpus(VectorSource(ted_data$tags)) %>% # convert to corpus
  tm_map(removeWords, stopwords("english")) %>% # clean up
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, c('tedx', 'fellow', 'ted'))

tag_dtm <- DocumentTermMatrix(tag_corpus)

# look at most frequent terms
tag_freq <- colSums(as.matrix(tag_dtm))

tag_freq_df <- data.frame(word=names(tag_freq), freq=tag_freq)

ggplot(subset(tag_freq_df, freq>100), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

# cluster by keywords

# k-means --> probably won't use as there seems to be more overlap!!
sparse_tag_dtm <- removeSparseTerms(tag_dtm, sparse=0.95)
mat <- as.matrix(sparse_tag_dtm)
docsdissim <- dist(scale(mat))
set.seed(1234)
kfit <- kmeans(docsdissim, 5)   
#clusplot(as.matrix(docsdissim), kfit$cluster, color=T, shade=T, labels=2, lines=0)

clusters <- data.frame(cuts=kfit$cluster, talk_id=ted_data$talk_id) %>%
  left_join(ted_data, by='talk_id') %>%
  mutate(cluster = paste('group', cuts))

# look at views across clusters
clusters %>% 
  ggplot(aes(x=views, y=reorder(cluster, views))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
  ggplot(aes(x=comments, y=reorder(cluster, comments))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
  ggplot(aes(x=languages, y=reorder(cluster, languages))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
  ggplot(aes(x=duration, y=reorder(cluster, duration))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
  ggplot(aes(x=title_length, y=reorder(cluster, title_length))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>%
  ggplot(aes(x=published_date)) +
  geom_histogram(bins=25) +
  facet_wrap(~cluster)

clusters %>%
  ggplot(aes(x=film_date)) +
  geom_histogram(bins=25) +
  facet_wrap(~cluster)

# get summary
cluster_summary_k <- clusters %>% 
  group_by(cluster) %>% 
  mutate(mean = mean(views), std = sd(views), count = n()) %>%
  ungroup() %>% 
  select(cluster, mean, std, count) %>%
  distinct()

# plot top word frequencies
#wordfreq_by_cluster(1, 30)
#wordfreq_by_cluster(2, 30)
#wordfreq_by_cluster(3, 30)
#wordfreq_by_cluster(4, 30)
#wordfreq_by_cluster(5, 30)


# hierarchical clustering on tags
sparse_tag_dtm <- removeSparseTerms(tag_dtm, sparse=0.95)
mat <- as.matrix(sparse_tag_dtm)
docsdissim <- dist(scale(mat))

h <- hclust(docsdissim, method = "ward.D")
#plot(h, cex=0.5, hang=0.1)

cuts <- cutree(h, 5)
clusters <- data.frame(cuts) %>%
  mutate(talk_id = ted_data$talk_id) %>%
  left_join(ted_data, by='talk_id') %>%
  mutate(cluster = paste('group', as.character(cuts)),
         views = as.double(views))

clusters_final <- clusters

clusters %>% 
  ggplot(aes(x=views, y=reorder(cluster, views))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

# ggplot(clusters, aes(x=cluster, y=views)) + geom_boxplot()

# get summary
cluster_summary_h <- clusters %>% 
  group_by(cluster) %>% 
  mutate(mean = mean(views), std = sd(views), count = n()) %>%
  ungroup() %>% 
  select(cluster, mean, std, count) %>%
  distinct()

# plot word counts by cluster
wordfreq_by_cluster(1, 30)
wordfreq_by_cluster(2, 30)
wordfreq_by_cluster(3, 30)
wordfreq_by_cluster(4, 30)
wordfreq_by_cluster(5, 30)
#wordfreq_by_cluster(6, 30)
#wordfreq_by_cluster(7, 30)

# explore variables by h clusters
ggplot(clusters, aes(x=film_date, y=log(views), colour=cluster)) + # by film date
  geom_point()

ggplot(clusters, aes(x=languages, y=log(views), colour=cluster)) + # by film date
  geom_point()

ggplot(clusters, aes(x=published_date, y=log(views), colour=cluster)) + # by film date
  geom_point()

clusters %>% 
  ggplot(aes(x=views, y=reorder(cluster, views))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
  ggplot(aes(x=comments, y=reorder(cluster, comments))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
  ggplot(aes(x=languages, y=reorder(cluster, languages))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
  ggplot(aes(x=duration, y=reorder(cluster, duration))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
  ggplot(aes(x=title_length, y=reorder(cluster, title_length))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>%
  ggplot(aes(x=published_date)) +
  geom_histogram(bins=25) +
  facet_wrap(~cluster)

clusters %>%
  ggplot(aes(x=film_date)) +
  geom_histogram(bins=25) +
  facet_wrap(~cluster)

data_temp <- ratings_temp %>% left_join(select(clusters, views, cluster, languages, talk_id), by='talk_id') %>%
  mutate(rating_ln = log(rating + 0.001))

ggplot(data_temp, aes(x=rating, y=log(views))) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_grid(cluster~characteristic, scales = 'free')

ggplot(data_temp, aes(x=characteristic, y=rating, fill=cluster)) +
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  coord_flip()

ggplot(data_temp, aes(x=characteristic, y=rating)) +
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  coord_flip() +
  facet_grid(~cluster, scales = 'fixed') 

ggplot(data_temp, aes(y=reorder(characteristic, rating), x=rating)) +
  geom_density_ridges_gradient() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_grid(~cluster, scales = 'fixed') 

ggplot(data_temp, aes(y=reorder(characteristic, rating_ln), x=rating_ln)) +
  geom_density_ridges_gradient() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_grid(~cluster, scales = 'fixed') 

ggplot(data_temp, aes(y=cluster, x=rating_ln)) +
  geom_density_ridges_gradient() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  facet_grid(~characteristic, scales = 'fixed')

ggplot(data_temp, aes(y=reorder(characteristic, rating_ln), x=rating_ln, fill=cluster)) +
  geom_density_ridges_gradient()

ggplot(data_temp, aes(x=rating)) +
  geom_histogram(bins=30) + 
  facet_grid(characteristic~cluster, scales = 'free') 

ggplot(data_temp, aes(x=rating_ln)) +
  geom_histogram(bins=30) + 
  facet_grid(characteristic~cluster, scales = 'free') 

ggplot(data_temp, aes(x=rating, y=log(views), colour=cluster)) +
  geom_point(size=0.5) + 
  facet_wrap(~characteristic, scales = 'free') 

ggplot(data_temp, aes(x=rating, y=log(views), colour=cluster)) +
  geom_smooth(method='lm', se=FALSE) + 
  facet_wrap(~characteristic, scales = 'free') 


# most frequent words in titles
title_corpus <- Corpus(VectorSource(ted_data$title)) %>% # convert to corpus
  tm_map(removeWords, stopwords("english")) %>% # clean up
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

# convert to document term matrix
title_dtm <- DocumentTermMatrix(title_corpus)

title_freq <- colSums(as.matrix(title_dtm))

title_freq_df <- data.frame(word=names(title_freq), freq=title_freq)

ggplot(subset(title_freq_df, freq>=20), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

#########
# look at transcript data
#########

# word frequency
count(ted_transcripts)
ted_transcripts <- mutate(ted_transcripts, transcript = tolower(transcript)) %>% # change all to lower case
  left_join(select(ted_data, talk_id, url), by='url')
rownames(ted_transcripts) = ted_transcripts$talk_id

ted_corpus <- Corpus(VectorSource(ted_transcripts$transcript)) %>% # convert to corpus
  tm_map(removeWords, stopwords("english")) %>% # clean up
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

#summary(ted_corpus)
#ted_corpus[[6]]$content # check if everything is fine

# convert to document term matrix
ted_dtm <- DocumentTermMatrix(ted_corpus)

sparse_ted_dtm <- removeSparseTerms(ted_dtm, sparse=0.95)
mat <- as.matrix(sparse_ted_dtm)
docsdissim <- dist(scale(mat))

h <- hclust(docsdissim, method = "ward.D")
#plot(h, cex=0.8, hang=0.1)

# cluster into 5 groups (by making cut)
cuts <- cutree(h, 5)
clusters <- data.frame(cuts) %>%
  mutate(talk_id = ted_transcripts$talk_id) %>%
  left_join(ted_transcripts, by='talk_id') %>%
  left_join(ted_data, by='talk_id')

# view word clouds by clusters
wordcloud_by_cluster <- function(cluster, word_freq) {
  cx <- filter(clusters, cuts == cluster)
  cx_corpus <- Corpus(VectorSource(cx$transcript)) %>% # convert to corpus
    tm_map(removeWords, stopwords("english")) %>% # clean up
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace)
  cx_dtm <- DocumentTermMatrix(cx_corpus)
  ft <- colSums(as.matrix(cx_dtm))
  df <- data.frame(word=names(ft), freq=ft)
  set.seed(142)   
  wordcloud(df$word, df$freq, min.freq=word_freq)
}

#wordcloud_by_cluster(1, 500)
#wordcloud_by_cluster(2, 500)
#wordcloud_by_cluster(3, 500)
#wordcloud_by_cluster(4, 500)
#wordcloud_by_cluster(5, 150)

# most common tags in transcript hierarchical clusters
wordfreq_by_cluster(1, 30)
wordfreq_by_cluster(2, 30)
wordfreq_by_cluster(3, 30)
wordfreq_by_cluster(4, 30)
wordfreq_by_cluster(5, 30)

# k-means clustering
ted_dtm <- DocumentTermMatrix(ted_corpus)

sparse_ted_dtm <- removeSparseTerms(ted_dtm, sparse=0.95)
mat <- as.matrix(sparse_ted_dtm)
docsdissim <- dist(scale(mat))

kfit <- kmeans(docsdissim, 5)   
#clusplot(as.matrix(docsdissim), kfit$cluster, color=T, shade=T, labels=2, lines=0)   

clusters <- data.frame(cuts = kfit$cluster, talk_id = ted_transcripts$talk_id) %>%
  left_join(ted_transcripts, by='talk_id') %>%
  left_join(ted_data, by='talk_id')


#wordcloud_by_cluster(1, 500)
#wordcloud_by_cluster(2, 500)
#wordcloud_by_cluster(3, 500)
#wordcloud_by_cluster(4, 500)
#wordcloud_by_cluster(5, 150)

# most common tags in transcript k-means clusters
wordfreq_by_cluster(1, 30)
wordfreq_by_cluster(2, 30)
wordfreq_by_cluster(3, 30)
wordfreq_by_cluster(4, 30)
wordfreq_by_cluster(5, 30)

##########
# sentiment analysis of transcripts
##########

rownames(ted_transcripts) <- ted_transcripts$talk_id

ted_corpus <- VCorpus(VectorSource(ted_transcripts$transcript)) %>% # convert to corpus
  tm_map(removeWords, stopwords("english")) %>% # clean up
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

ted_txt <- tidytext::tidy(ted_corpus) %>%
  mutate(talk_id = ted_transcripts$talk_id)

# calculate occurence of positive/negative sentiment 
get_sentiment <- function(row_n){
  temp_txt <- ted_txt[row_n,]
  tidy_temp_text <- temp_txt %>%
    select(text, id) %>%
    group_by(id) %>% 
    unnest_tokens(word, text) %>%
    ungroup() %>% # you can experiment with not including this when you compute count()
    anti_join(stop_words, by='word') 
  
  temp_result <- tidy_temp_text %>%
    inner_join(get_sentiments("bing"), by='word') %>%
    count(id, sentiment, word) %>%
    ungroup() %>%
    group_by(sentiment) %>%
    summarize(words = sum(n)) %>%
    mutate(id = row_n)
  return(temp_result)
}

ted_sentiment <- get_sentiment(1)

# run across all transcripts
for (i in c(2:as.integer(count(ted_txt)))){
  data_temp <- get_sentiment(i)
  ted_sentiment <- rbind(ted_sentiment, data_temp)
}

ted_sentiment <- ted_sentiment %>%
  spread(sentiment, words)

ted_sentiment[is.na(ted_sentiment)] <- 0

ted_data_sentiment <- ted_sentiment %>%
  left_join(mutate(ted_txt, id=as.integer(id)), by='id') %>%
  select(talk_id, negative, positive, language) %>%
  mutate(perc.pos = positive/(positive+negative)*100,
         perc.neg = negative/(positive+negative)*100) %>%
  left_join(ted_data, by='talk_id')

# plot sentiment and views
ted_data_sentiment %>% gather('sentiment', 'count', c(positive, negative)) %>%
  ggplot(aes(x=count, y=log(views), colour=sentiment)) +
  geom_jitter(size=0.8)

ted_data_sentiment %>% 
  ggplot(aes(x=perc.pos, y=log(views))) +
  geom_point()

ted_data_sentiment %>% 
  ggplot(aes(x=positive, y=log(comments))) +
  geom_point()

# plot sentiment and ratings
ratings_temp %>% right_join(ted_data_sentiment, by='talk_id') %>%
  ggplot(aes(x=rating, y=perc.pos)) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_wrap(~characteristic, scales = 'free')

# look at sentiment by clusters (tags, hierarchical)
ted_data_sentiment %>% left_join(select(clusters_final, cluster, talk_id), by='talk_id') %>%
  group_by(cluster) %>%
  mutate(cluster.pos = sum(positive),
         cluster.neg = sum(negative),
         mean.perc.pos = mean(perc.pos),
         mean.perc.neg = mean(perc.neg)) %>%
  ungroup() %>%
  select(cluster, cluster.pos, cluster.neg, mean.perc.pos, mean.perc.neg) %>%
  distinct()

clusters_final %>% group_by(cluster) %>% 
  mutate(views.mean = mean(views),
         views.sd = sd(views),
         comments.mean = mean(comments),
         comments.sd = sd(comments),
         languages.mean = mean(languages),
         languages.sd = sd(languages)) %>%
  ungroup() %>%
  select(cluster, views.mean, views.sd, comments.mean, comments.sd, languages.mean, languages.sd) %>%
  distinct()

### look at relationships

# percentage ratings
clusters_final1 <- left_join(clusters_final, ratings_percent, by='talk_id') %>% 
  spread(cluster, cuts) %>%
  mutate(`group 1` = ifelse(is.na(`group 1`), 0, 1),
         `group 2` = ifelse(is.na(`group 2`), 0, 1), 
         `group 3` = ifelse(is.na(`group 3`), 0, 1), 
         `group 4` = ifelse(is.na(`group 4`), 0, 1), 
         `group 5` = ifelse(is.na(`group 5`), 0, 1))
nums <- unlist(lapply(clusters_final1, is.numeric))  
data_temp <- clusters_final1[ , nums]

cor_data <- data.frame(cor(data_temp, method = c("pearson")))


left_join(ted_data_sentiment, select(clusters_final, talk_id, cluster), by='talk_id') %>%
  ggplot(aes(x=cluster, y=perc.pos)) +
  geom_boxplot()

ggplot(ted_data_sentiment, aes(x=perc.pos)) +
  geom_histogram()

left_join(ted_data_sentiment, select(clusters_final, talk_id, cluster), by='talk_id') %>%
  ggplot(aes(x=cluster, y=positive)) +
  geom_boxplot()



  
wordfreq_by_cluster.transcript <- function(n_cluster, top_words) {
  data_temp <-  left_join(ted_transcripts, select(clusters_final, talk_id, cuts), by='talk_id')%>% 
    filter(cuts == n_cluster)
  temp_corpus <- Corpus(VectorSource(data_temp$transcript)) %>% # convert to corpus
    tm_map(removeWords, stopwords("english")) %>% # clean up
    tm_map(removePunctuation) %>%
    tm_map(stripWhitespace) %>% tm_map(removeWords, c('<U+266A>', '<U+266B>'))
  
  temp_dtm <- DocumentTermMatrix(temp_corpus)
  temp_freq <- colSums(as.matrix(temp_dtm))
  
  temp_freq_df <- data.frame(word=names(temp_freq), freq=temp_freq) %>%
    arrange(-freq) %>%
    top_n(top_words, freq) %>%
    mutate(cuts = n_cluster)
  return(temp_freq_df)
}

wordfreq_by_cluster.transcript.plot <- function(temp_freq_df, top_words) {
  temp_freq_df %>% top_n(top_words, freq) %>%
  ggplot(aes(x = reorder(word, -freq), y = freq)) +
    geom_bar(stat = "identity") + 
    theme(axis.text.x=element_text(angle=45, hjust=1))
}




c1.transcriptfreq <- wordfreq_by_cluster.transcript(1, 150)
wordfreq_by_cluster.transcript.plot(c1.transcriptfreq, 30)

c2.transcriptfreq <- wordfreq_by_cluster.transcript(2, 150)
wordfreq_by_cluster.transcript.plot(c2.transcriptfreq, 30)

c3.transcriptfreq <- wordfreq_by_cluster.transcript(3, 150)
wordfreq_by_cluster.transcript.plot(c3.transcriptfreq, 30)

c4.transcriptfreq <- wordfreq_by_cluster.transcript(4, 150)
wordfreq_by_cluster.transcript.plot(c4.transcriptfreq, 30)

c5.transcriptfreq <- wordfreq_by_cluster.transcript(5, 150)
wordfreq_by_cluster.transcript.plot(c5.transcriptfreq, 30)

cX.transcriptfreq <- rbind(c1.transcriptfreq, c2.transcriptfreq, c3.transcriptfreq, 
                           c4.transcriptfreq, c5.transcriptfreq)


plot.ranks <- function(df1, df2){
  df1 <- df1 %>%
    mutate(rank = c(1:as.integer(count(df1))))
  df2 <- df2 %>%
    mutate(rank = c(1:as.integer(count(df2))))
  transcriptfreq <- full_join(select(df1, -freq, -cuts), select(df2, -freq, -cuts), by='word')
  #transcriptfreq[is.na(transcriptfreq)] <- 0
  ggplot(transcriptfreq, aes(x=transcriptfreq[[2]], y=transcriptfreq[[3]])) +
    geom_point() +
    geom_smooth(method='lm')
}


plot.ranks(c1.transcriptfreq, c2.transcriptfreq)
plot.ranks(c1.transcriptfreq, c3.transcriptfreq)
plot.ranks(c1.transcriptfreq, c4.transcriptfreq)
plot.ranks(c1.transcriptfreq, c5.transcriptfreq)

plot.ranks(c2.transcriptfreq, c3.transcriptfreq)
plot.ranks(c2.transcriptfreq, c4.transcriptfreq)
plot.ranks(c2.transcriptfreq, c5.transcriptfreq)

plot.ranks(c3.transcriptfreq, c4.transcriptfreq)
plot.ranks(c3.transcriptfreq, c5.transcriptfreq)

plot.ranks(c4.transcriptfreq, c5.transcriptfreq)


spearman_rho.transcriptfreq <- function(df1, df2){
  df1 <- spread(df1, cuts, freq)
  df2 <- spread(df2, cuts, freq)
  temp_df <- full_join(df1, df2, by='word')
  temp_df[is.na(temp_df)] <- 0
  spr.cor <- cor.test(temp_df[[2]], temp_df[[3]], method="spearman")
  return(spr.cor)
}

spearman_rho.transcriptfreq(c1.transcriptfreq, c2.transcriptfreq)
spearman_rho.transcriptfreq(c1.transcriptfreq, c3.transcriptfreq)
spearman_rho.transcriptfreq(c1.transcriptfreq, c4.transcriptfreq)
spearman_rho.transcriptfreq(c1.transcriptfreq, c5.transcriptfreq)

spearman_rho.transcriptfreq(c2.transcriptfreq, c3.transcriptfreq)
spearman_rho.transcriptfreq(c2.transcriptfreq, c4.transcriptfreq)
spearman_rho.transcriptfreq(c2.transcriptfreq, c5.transcriptfreq)

spearman_rho.transcriptfreq(c3.transcriptfreq, c4.transcriptfreq)
spearman_rho.transcriptfreq(c3.transcriptfreq, c5.transcriptfreq)

spearman_rho.transcriptfreq(c4.transcriptfreq, c5.transcriptfreq)



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
  ungroup() %>%
  left_join(select(clusters_final, cluster, talk_id), by='talk_id') %>%
  group_by(cluster) %>%
  mutate(cluster_total = sum(n)) %>%
  ungroup()

# word appearance by clusters
ggplot(transcript_words, aes(n/cluster_total, fill = cluster)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~cluster, ncol = 2, scales = "free_y")

# Zipf's law states that the frequency that a word appears is inversely proportional to its rank
freq_by_rank <- transcript_words %>% 
  group_by(talk_id) %>% 
  mutate(rank = row_number(), 
         term_frequency = n/transcript_total)

# Zipf's law for ted talks (by cluster)
freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, colour = cluster)) + 
  geom_point(size=0.8)+ 
  scale_x_log10() +
  scale_y_log10()

# Zipf's law for ted talks (by talk) --> DOES NOT RUN!! FREEZES EVERYTHING
#freq_by_rank %>% 
#  ggplot(aes(rank, term_frequency, colour=talk_id)) + 
#  geom_line(size=0.8, alpha=0.5, show.legend=FALSE) + 
#  scale_x_log10() +
#  scale_y_log10()

# We see that transcripts are similar to each other, and that the relationship between rank and frequency has a negative slope
# It is not quite constant, though; perhaps we could view this as a broken power law with, say, three sections. 
# Let's see what the exponent of the power law is for the middle section of the rank range.
rank_subset <- freq_by_rank %>% 
  filter(rank < 500)

# fit power law line
pl <- lm(log10(term_frequency) ~ log10(rank), data = rank_subset)

freq_by_rank %>% 
  ggplot(aes(rank, term_frequency, colour = cluster)) + 
  geom_abline(intercept=pl$coefficients[1], slope=pl$coefficients[2], color = "gray50", linetype = 2) + 
  geom_point(size=0.8)+ 
  scale_x_log10() +
  scale_y_log10()

## 3.3 The bind_tf_idf function