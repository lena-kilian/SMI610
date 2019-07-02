library(tidyverse)
library(lubridate)
library(rjson)
library(ggridges)
library(tm)
library(proxy)
library(wordcloud) # take this out of final analysis

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
         talk_id = c(1:as.integer(count(ted_data))))

# look at views over time
ggplot(ted_data, aes(x=film_date, y=log(views))) + # by film date
  geom_point()

ggplot(ted_data, aes(x=published_date, y=views)) + # by published date
  geom_point()


# explore other variables
ggplot(ted_data, aes(x=languages, y=log(views))) + # by film date
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

library(GGally)
ratings_temp %>% mutate(rating = log(rating)) %>% spread(characteristic, rating) %>% select(-talk_id) %>% 
  ggpairs() #+ ggtitle("Anderson's Iris Data -- 3 species")

#########
# look at transcript data
#########

count(ted_transcripts)
ted_transcripts <- mutate(ted_transcripts, transcript = tolower(transcript)) %>% # change all to lower case
  left_join(select(ted_data, talk_id, url), by='url')
rownames(ted_transcripts) = ted_transcripts$talk_id

ted_corpus <- Corpus(VectorSource(ted_transcripts$transcript)) %>% # convert to corpus
  tm_map(removeWords, stopwords("english")) %>% # clean up
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

summary(ted_corpus)
ted_corpus[[6]]$content # check if everything is fine

# convert to document term matrix
ted_dtm <- DocumentTermMatrix(ted_corpus)

sparse_ted_dtm <- removeSparseTerms(ted_dtm, sparse=0.95)
mat <- as.matrix(sparse_ted_dtm)
docsdissim <- dist(scale(mat))

h <- hclust(docsdissim, method = "ward.D")
plot(h, cex=0.8, hang=0.1)

# cluster into 5 groups (by making cut)
cuts <- cutree(h, 5)
clusters <- data.frame(cuts) %>%
  mutate(talk_id = ted_transcripts$talk_id) %>%
  left_join(ted_transcripts, by='talk_id')

# view word clouds by clusters
wordclud_by_cluster <- function(cluster, word_freq) {
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

wordclud_by_cluster(1, 500)
wordclud_by_cluster(5, 200)

# k-means clustering
library(fpc)   
library(cluster)

ted_dtm <- DocumentTermMatrix(ted_corpus)

sparse_ted_dtm <- removeSparseTerms(ted_dtm, sparse=0.95)
mat <- as.matrix(sparse_ted_dtm)
docsdissim <- dist(scale(mat))

kfit <- kmeans(docsdissim, 3)   
clusplot(as.matrix(docsdissim), kfit$cluster, color=T, shade=T, labels=2, lines=0)   


# cluster by keywords

names(ted_data)

tag_corpus <- Corpus(VectorSource(ted_data$tags)) %>% # convert to corpus
  tm_map(removeWords, stopwords("english")) %>% # clean up
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, c('tedx', 'fellow', 'ted'))

tag_dtm <- DocumentTermMatrix(tag_corpus)

sparse_tag_dtm <- removeSparseTerms(tag_dtm, sparse=0.95)
mat <- as.matrix(sparse_tag_dtm)
docsdissim <- dist(scale(mat))

set.seed(1298)
kfit <- kmeans(docsdissim, 10)   
clusplot(as.matrix(docsdissim), kfit$cluster, color=T, shade=T, labels=2, lines=0)

# look at most frequent terms
tag_freq <- colSums(as.matrix(tag_dtm))

tag_freq_df <- data.frame(word=names(tag_freq), freq=tag_freq)

ggplot(subset(tag_freq_df, freq>100), aes(x = reorder(word, -freq), y = freq)) +
  geom_bar(stat = "identity") + 
  theme(axis.text.x=element_text(angle=45, hjust=1))



# hierarchical clustering on tags
sparse_tag_dtm <- removeSparseTerms(tag_dtm, sparse=0.95)
mat <- as.matrix(sparse_tag_dtm)
docsdissim <- dist(scale(mat))

h <- hclust(docsdissim, method = "ward.D")
plot(h, cex=0.5, hang=0.1)

cuts <- cutree(h, 10)
clusters <- data.frame(cuts) %>%
  mutate(talk_id = ted_data$talk_id) %>%
  left_join(ted_data, by='talk_id') %>%
  mutate(cluster = paste('group', as.character(cuts)),
         views = as.double(views))

clusters %>% 
  ggplot(aes(x=views, y=reorder(cluster, views), fill = ..x..)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

# ggplot(clusters, aes(x=cluster, y=views)) + geom_boxplot()

cluster_summary <- clusters %>% 
  group_by(cluster) %>% 
  mutate(mean = mean(views), std = sd(views), count = n()) %>%
  ungroup() %>% 
  select(cluster, mean, std, count) %>%
  distinct()

# plot word counts by cluster
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

wordfreq_by_cluster(1, 30)
wordfreq_by_cluster(2, 30)
wordfreq_by_cluster(3, 30)
wordfreq_by_cluster(4, 30)
wordfreq_by_cluster(5, 30)
#wordfreq_by_cluster(6, 30)
#wordfreq_by_cluster(7, 30)


ggplot(clusters, aes(x=film_date, y=log(views), colour=cluster)) + # by film date
  geom_point()

ggplot(clusters, aes(x=languages, y=log(views), colour=cluster)) + # by film date
  geom_point()

ggplot(clusters, aes(x=published_date, y=log(views), colour=cluster)) + # by film date
  geom_point()

data_temp <- ratings_temp %>% left_join(select(clusters, views, cluster, languages, talk_id), by='talk_id') %>%
  mutate(rating_ln = log(rating + 0.001))

ggplot(data_temp, aes(x=rating, y=log(views))) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_grid(cluster~characteristic, scales = 'free')

ggplot(data_temp, aes(x=characteristic, y=rating, fill=cluster)) +
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=45, hjust=1))


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


# titles
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

