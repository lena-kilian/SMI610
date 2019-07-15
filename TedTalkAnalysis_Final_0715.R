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
library(plyr)

set.seed(1234)
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
         published_year = as.character(substring(published_date, 1, 4)),
         filmed_year = as.character(substring(film_date, 1, 4)),
         talk_id = c(1:as.integer(count(ted_data))),
         title_length = nchar(title), # add string length of title
         description_length = nchar(description), # add string length of title
         comments_per_view = comments/views) # add comments per view

# look at variable distributions
ggplot(ted_data, aes(x=log(views))) +
  geom_histogram()
ggplot(ted_data, aes(x=log(comments_per_view))) +
  geom_histogram()
ggplot(ted_data, aes(x=log(comments))) +
  geom_histogram()
ggplot(ted_data, aes(x=film_date)) + # by film date
  geom_histogram(bins=50)
ggplot(ted_data, aes(x=published_date)) + # by film date
  geom_histogram(bins=60)
ggplot(ted_data, aes(x=languages)) + # by film date
  geom_histogram(bins=50)

# look at trends over time
ggplot(ted_data, aes(x=film_date, y=log(views))) + # by film date
  geom_point() 
ggplot(ted_data, aes(x=published_date, y=log(views))) + # by published date
  geom_point()
ted_data %>% gather('date_type', 'date', c('film_date', 'published_date')) %>%
  ggplot(aes(x=date, y=languages, colour=date_type)) + # by film date
  geom_jitter(size=0.8)
ggplot(ted_data, aes(x=published_date, y=languages)) + # by publication date
  geom_jitter(size=0.8)
ggplot(ted_data, aes(x=film_date, y=languages)) + # by film date
  geom_jitter(size=0.8)
ggplot(ted_data, aes(x=filmed_year, y=languages)) +
  geom_boxplot()
ggplot(ted_data, aes(y=filmed_year, x=languages)) +
  geom_density_ridges_gradient()
ggplot(ted_data, aes(y=published_year, x=languages)) +
  geom_density_ridges_gradient()

# explore relationships between numerical variables and views
nums <- unlist(lapply(ted_data, is.numeric)) #extract all numerical variables
data_temp <- ted_data[ , nums] %>%
  mutate(comments_per_view.log = log(comments_per_view),
         duration.log = log(duration)) %>%
  gather('variable', 'value', c(1, 2, 3, 4, 6, 7, 8, 9, 10, 11))
ggplot(data_temp, aes(x=value, y=log(views))) +
  geom_jitter(size=0.5) +
  geom_smooth(method='lm') +
  facet_wrap(~variable, scales='free')

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

ratings_temp %>% right_join(select(ted_data, talk_id, views), by='talk_id') %>%
  filter(rating != 0) %>%
  ggplot(aes(x=log(rating), y=log(views))) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_wrap(~characteristic, scales = 'free')

ratings_temp %>% right_join(select(ted_data, talk_id, duration), by='talk_id') %>%
  ggplot(aes(x=rating, y=log(duration))) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_wrap(~characteristic, scales = 'free')

ratings_temp %>% right_join(select(ted_data, talk_id, comments_per_view), by='talk_id') %>%
  ggplot(aes(x=log(rating + 0.1), y=log(comments_per_view))) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_wrap(~characteristic, scales = 'free')

# distributions of percentage ratings
ratings_temp %>% right_join(select(ted_data, talk_id, views), by='talk_id') %>%
  ggplot(aes(x=rating, y=reorder(characteristic, rating))) +
  geom_density_ridges_gradient()

ratings_temp %>% mutate(rating = log(rating)) %>% spread(characteristic, rating) %>% select(-talk_id) %>% 
  pairs(pch=19)

# cluster by ratings
# PCA assumes that variables are uncorrelated
ratings_temp %>% mutate(rating = log(rating)) %>% spread(characteristic, rating) %>% select(-talk_id) %>% 
  ggpairs() #+ ggtitle("Anderson's Iris Data -- 3 species")
# --> look mostly non-correlated, let's check pearson

cor_matrix.ratings <- cor(select(ratings_percent, -talk_id), method = c("pearson"))

ggplot(data = melt(cor_matrix.ratings), aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  scale_fill_viridis() +
  geom_text(aes(label=round(value, 2)), colour='white')
# --> mostly not correlated, but some at +/-0.5 (Beautiful & Informative, and Fascinating & Courageous)



pca1 <- princomp(data.frame(cor_matrix.ratings))

pca1$loadings # not sure what this shows???

summary(pca1) # shows variance explained by components 

pca1$scores # shows scores on different components

pca_data <- data.frame(pca1$scores)[,c(1,2,3)] %>% # here using the first 3 components to explain 80% of variance
  mutate(characteristic = row.names(pca1$loadings))

plot_ly(pca_data, x =~Comp.1, y =~Comp.2, z =~Comp.3, mode='text', text=~characteristic) %>%
  add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Component 1'),
                      yaxis = list(title = 'Component 2'),
                      zaxis = list(title = 'Component 3')))


kmns_pca <- kmeans(select(pca_data, -characteristic), 3) # divide data (first 2 pca comps) into 3 clusters

# plot pca vs non-pca cluster results
kmns <- kmeans(data.frame(cor_matrix.ratings), 3)

cluster_data <- pca_data %>%
  mutate(cluster_pca = paste('Cluster_', kmns_pca$cluster),
         cluster = paste('Cluster_', kmns$cluster),
         characteristic = pca_data$characteristic)
  

p1 <- ggplot(cluster_data, aes(x=Comp.1, y=Comp.2, colour=cluster_pca, label=characteristic)) +
  geom_point()+
  ggtitle('PCA K-means')  + 
  geom_label_repel()

p2 <- ggplot(cluster_data, aes(x=Comp.1, y=Comp.2, colour=cluster, label=characteristic)) +
  geom_point() +
  ggtitle('Raw K-means')  + 
  geom_label_repel()

grid.arrange(p1, p2, nrow=2)

rating_cluster <- ratings_temp %>%
  #gsub(characteristic, "-", ".") %>%
  mutate(characteristic = ifelse(characteristic == 'Jaw-dropping', 'Jaw.dropping', characteristic)) %>%
  left_join(select(cluster_data, characteristic, cluster_pca), by='characteristic') 

rating_cluster %>%
  group_by(talk_id) %>%
  top_n(1, rating) %>%
  left_join(ted_data, by='talk_id') %>%
  ggplot(aes(x=cluster_pca, y=log(views))) +
  geom_boxplot()

rating_cluster %>%
  group_by(talk_id) %>%
  top_n(1, rating) %>%
  left_join(ted_data, by='talk_id') %>%
  ggplot(aes(x=characteristic, y=log(views))) +
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=45, hjust=1))

cluster_lookup <- rating_cluster %>%
  select(cluster_pca, characteristic) %>%
  distinct()

cluster_ratings <- select(ratings_data, -total) %>%
  gather(characteristic, rating, c(1:14)) %>%
  mutate(characteristic = ifelse(characteristic == 'Jaw-dropping', 'Jaw.dropping', characteristic)) %>%
  left_join(distinct(select(rating_cluster, cluster_pca, characteristic)), by='characteristic') %>%
  group_by(talk_id, cluster_pca) %>%
  mutate(total = sum(rating)) %>%
  select(-characteristic, -rating) %>%
  distinct() %>%
  group_by(talk_id) %>%
  mutate(percent = total/sum(total) *100) %>%
  ungroup() %>%
  select(talk_id, cluster_pca, percent)

cluster_ratings %>%
  left_join(ted_data, by='talk_id') %>%
  ggplot(aes(x=percent, y=log(views))) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_wrap(~cluster_pca)


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
# hierarchical clustering on tags
sparse_tag_dtm <- removeSparseTerms(tag_dtm, sparse=0.95)
mat <- as.matrix(sparse_tag_dtm)
docsdissim <- dist(scale(mat))

h <- hclust(docsdissim, method = "ward.D")

cuts <- cutree(h, 6)
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

clusters %>% 
  left_join(cluster_ratings, by='talk_id') %>%
  ggplot(aes(x=percent, y=cluster)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous() +
  facet_wrap(~cluster_pca, scales='free')

clusters %>% 
  left_join(ratings_temp, by='talk_id') %>%
  ggplot(aes(x=rating, y=cluster)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous() +
  facet_wrap(~characteristic, scales='free')

difference_characteristics <- clusters %>% 
  left_join(ratings_temp, by='talk_id') %>%
  filter(characteristic %in% c('Beautiful', 'Courageous', 'Fascinating', 'Informative', 'Persuasive', 'Jaw-dropping', 'Ingenious'))

ggplot(difference_characteristics, aes(x=rating, y=cluster)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous() +
  facet_wrap(~characteristic, scales='free')

# get summary
cluster_summary_h <- clusters %>% 
  group_by(cluster) %>% 
  mutate(mean = mean(views), std = sd(views), count = n()) %>%
  ungroup() %>% 
  select(cluster, mean, std, count) %>%
  distinct()

# plot word counts by cluster
p1 <- wordfreq_by_cluster(1, 15)
p2 <- wordfreq_by_cluster(2, 15)
p3 <- wordfreq_by_cluster(3, 15)
p4 <- wordfreq_by_cluster(4, 15)
p5 <- wordfreq_by_cluster(5, 15)
p6 <- wordfreq_by_cluster(6, 15)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2)


##### split up cluster one further:
temp_data <- clusters_final %>%
  filter(cuts==1) %>%
  select(-cuts, -cluster)

tag_corpus.cluster1 <- Corpus(VectorSource(temp_data$tags)) %>% # convert to corpus
  tm_map(removeWords, stopwords("english")) %>% # clean up
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, c('tedx', 'fellow', 'ted'))

tag_dtm.cluster1 <- DocumentTermMatrix(tag_corpus.cluster1)

# cluster by keywords
# hierarchical clustering on tags
sparse_tag_dtm.cluster1 <- removeSparseTerms(tag_dtm.cluster1, sparse=0.95)
mat.cluster1 <- as.matrix(sparse_tag_dtm.cluster1)
docsdissim.cluster1 <- dist(scale(mat.cluster1))

h.cluster1 <- hclust(docsdissim.cluster1, method = "ward.D")

cuts.cluster1 <- cutree(h.cluster1, 2)
group_no_other <- max(clusters_final$cuts)-1

clusters.cluster1 <- data.frame(cuts.cluster1) %>%
  mutate(talk_id = temp_data$talk_id) %>%
  left_join(temp_data, by='talk_id') %>%
  mutate(cuts.split1 = ifelse(cuts.cluster1==1, 1, cuts.cluster1+group_no_other),
         views = as.double(views))

clusters_final.cluster1 <- clusters.cluster1 %>%
  select(talk_id, cuts.split1)

clusters_final.all <- clusters_final %>%
  left_join(clusters_final.cluster1, by='talk_id') %>%
  mutate(cuts.split1 = ifelse(is.na(cuts.split1), cuts, cuts.split1),
         cluster.split1 = ifelse(cluster=='group 1', paste('group', as.character(cuts.split1)), cluster))

clusters_final.all %>% 
  left_join(cluster_ratings, by='talk_id') %>%
  ggplot(aes(x=percent, y=cluster.split1)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous() +
  facet_wrap(~cluster_pca, scales='free')


# get summary
cluster_summary_h.split1 <- clusters_final.all %>% 
  group_by(cluster.split1) %>% 
  mutate(mean = mean(views), std = sd(views), count = n()) %>%
  ungroup() %>% 
  select(cluster.split1, mean, std, count) %>%
  distinct()

clusters <- mutate(clusters_final.all, cuts = cuts.split1)

difference_characteristics <- clusters %>% 
  left_join(ratings_temp, by='talk_id') %>%
  filter(characteristic %in% c('Beautiful', 'Courageous', 'Fascinating', 'Informative', 'Persuasive', 'Jaw-dropping', 'Ingenious'))

# plot word counts by cluster
p1 <- wordfreq_by_cluster(1, 15)
p2 <- wordfreq_by_cluster(2, 15)
p3 <- wordfreq_by_cluster(3, 15)
p4 <- wordfreq_by_cluster(4, 15)
p5 <- wordfreq_by_cluster(5, 15)
p6 <- wordfreq_by_cluster(6, 15)
p7 <- wordfreq_by_cluster(7, 15)
#p8 <- wordfreq_by_cluster(8, 15)

grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol=3, nrow=3)


# explore variables by h clusters
ggplot(clusters, aes(x=film_date, y=log(views), colour=cluster.split1)) + # by film date
  geom_point(size=0.8) +
  facet_wrap(~cluster.split1)

ggplot(clusters, aes(x=languages, y=log(views), colour=cluster.split1)) + # by film date
  geom_point(size=0.8) +
  facet_wrap(~cluster.split1)

ggplot(clusters, aes(x=published_date, y=log(views), colour=cluster.split1)) + # by film date
  geom_point(size=0.8) +
  facet_wrap(~cluster.split1)

clusters %>% 
  ggplot(aes(x=log(views), y=cluster.split1)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
  ggplot(aes(x=log(comments), y=cluster.split1)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
  ggplot(aes(x=languages, y=cluster.split1)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
  ggplot(aes(x=log(duration), y=cluster.split1)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>% 
    ggplot(aes(x=title_length, y=cluster.split1)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

clusters %>%
  ggplot(aes(x=published_date)) +
  geom_histogram(bins=25) +
  facet_wrap(~cluster.split1)

clusters %>%
  ggplot(aes(x=film_date)) +
  geom_histogram(bins=25) +
  facet_wrap(~cluster.split1)

data_temp <- ratings_temp %>% left_join(select(clusters, views, cluster.split1, languages, talk_id), by='talk_id') %>%
  mutate(rating_ln = log(rating + 0.001))

ggplot(difference_characteristics, aes(x=log(rating), y=log(views))) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_grid(cluster.split1~characteristic, scales = 'free')


ggplot(data_temp, aes(x=characteristic, y=rating, fill=cluster.split1)) +
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  coord_flip()


ggplot(difference_characteristics, aes(x=characteristic, y=rating, fill=cluster.split1)) +
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  coord_flip()


ggplot(data_temp, aes(x=characteristic, y=rating)) +
  geom_boxplot() + 
  theme(axis.text.x=element_text(angle=45, hjust=1)) +
  coord_flip() +
  facet_grid(~cluster.split1, scales = 'fixed') 

ggplot(difference_characteristics, aes(x=rating, y=log(views), colour=characteristic)) +
  geom_point(size=0.5) + 
  facet_grid(characteristic~cluster.split1, scales = 'free') 

ggplot(difference_characteristics, aes(x=log(rating), y=log(views), colour=characteristic)) +
  geom_point(size=0.5) + 
  facet_grid(characteristic~cluster.split1, scales = 'free')

ggplot(difference_characteristics, aes(x=rating, y=log(views), colour=characteristic)) +
  geom_smooth(method='lm') + 
  facet_grid(characteristic~cluster.split1, scales = 'free') 

ggplot(difference_characteristics, aes(x=log(rating), y=log(views), colour=characteristic)) +
  geom_smooth() + 
  facet_grid(characteristic~cluster.split1, scales = 'free')


##########
# sentiment analysis of transcripts ----> also find out how many neutral words, to get better percentage
##########

#########
# look at transcript data
#########

ted_transcripts <- mutate(ted_transcripts, transcript = tolower(transcript)) %>% # change all to lower case
  left_join(select(ted_data, talk_id, url), by='url')
rownames(ted_transcripts) = ted_transcripts$talk_id

ted_corpus <- VCorpus(VectorSource(ted_transcripts$transcript)) %>% # convert to corpus
  tm_map(removeWords, stopwords("english")) %>% # clean up
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

ted_txt <- tidytext::tidy(ted_corpus) %>%
  mutate(talk_id = ted_transcripts$talk_id) %>%
  mutate(total_words = str_count(text, " ") + 1)

##### BING
# calculate occurence of positive/negative sentiment --> bing
get_sentiment_bing <- function(row_n){
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

ted_sentiment <- get_sentiment_bing(1)

# run across all transcripts
for (i in c(2:as.integer(count(ted_txt)))){
  data_temp <- get_sentiment_bing(i)
  ted_sentiment <- rbind(ted_sentiment, data_temp)
}

ted_sentiment <- ted_sentiment %>%
  spread(sentiment, words)

ted_sentiment[is.na(ted_sentiment)] <- 0

ted_sentiment.bing <- ted_sentiment %>%
  left_join(mutate(ted_txt, id=as.integer(id)), by='id') %>%
  select(talk_id, negative, positive, language, total_words) %>%
  mutate(perc.pos = positive/total_words*100,
         perc.neg = negative/total_words*100) %>%
  left_join(ted_data, by='talk_id')

##### AFINN
# calculate occurence of positive/negative sentiment --> afinn scale 
get_sentiment_afinn <- function(row_n){
  temp_txt <- ted_txt[row_n,]
  tidy_temp_text <- temp_txt %>%
    select(text, id) %>%
    group_by(id) %>% 
    unnest_tokens(word, text) %>%
    ungroup() %>% # you can experiment with not including this when you compute count()
    anti_join(stop_words, by='word') 
  
  temp_result <- tidy_temp_text %>%
    inner_join(get_sentiments("afinn"), by='word')
  return(temp_result)
}

ted_sentiment <- get_sentiment_afinn(1)

# run across all transcripts
for (i in c(2:as.integer(count(ted_txt)))){
  data_temp <- get_sentiment_afinn(i)
  ted_sentiment <- rbind(ted_sentiment, data_temp)
}

ted_sentiment.afinn <- ted_sentiment %>%
  mutate(talk_id=as.integer(id)) %>%
  group_by(talk_id) %>%
  mutate(value.median = median(value),
         value.mean = mean(value),
         value.sd = sd(value),
         value.count = n()) %>%
  ungroup() %>%
  select(-id, -word, -value) %>%
  distinct()


##### LOUGHRAN
# calculate occurence of positive/negative sentiment --> loughran
get_sentiment_loughran <- function(row_n){
  temp_txt <- ted_txt[row_n,]
  tidy_temp_text <- temp_txt %>%
    select(text, id) %>%
    group_by(id) %>% 
    unnest_tokens(word, text) %>%
    ungroup() %>% # you can experiment with not including this when you compute count()
    anti_join(stop_words, by='word') 
  
  temp_result <- tidy_temp_text %>%
    inner_join(get_sentiments("loughran"), by='word') %>%
    count(id, sentiment, word) %>%
    ungroup() %>%
    group_by(sentiment) %>%
    summarize(words = sum(n)) %>%
    mutate(id = row_n)
  return(temp_result)
}

ted_sentiment <- get_sentiment_loughran(1)

# run across all transcripts
for (i in c(2:as.integer(count(ted_txt)))){
  data_temp <- get_sentiment_loughran(i)
  ted_sentiment <- rbind(ted_sentiment, data_temp)
}

ted_sentiment <- ted_sentiment %>%
  spread(sentiment, words)

ted_sentiment[is.na(ted_sentiment)] <- 0

ted_sentiment.loughran <- (ted_sentiment / rowSums(ted_sentiment) * 100) %>%
  mutate(talk_id = ted_sentiment$id) %>%
  select(-id)



####~~~

# plot sentiment and views
ted_sentiment.afinn %>% 
  left_join(ted_data, by='talk_id') %>%
  ggplot(aes(x=value.mean, y=log(views))) +
  geom_point() +
  geom_smooth()

ted_sentiment.afinn %>% 
  left_join(ted_data, by='talk_id') %>%
  ggplot(aes(x=value.median, y=log(views))) +
  geom_point() +
  geom_smooth()

ted_sentiment.loughran %>% gather('sentiment', 'count', c(positive, negative)) %>%
  left_join(ted_data, by='talk_id') %>%
  ggplot(aes(x=log(count), y=log(views), colour=sentiment)) +
  geom_jitter(size=0.8)

ted_sentiment.bing %>% gather('sentiment', 'count', c(perc.pos, perc.neg)) %>%
  ggplot(aes(x=count, y=log(views), colour=sentiment)) +
  geom_jitter(size=0.8) +
  geom_smooth(method='lm')

ted_sentiment.bing %>% 
  ggplot(aes(x=perc.pos, y=log(views))) +
  geom_point()

ted_sentiment.bing %>% 
  ggplot(aes(x=perc.neg, y=log(views))) +
  geom_point()

ted_sentiment.bing %>% 
  ggplot(aes(x=positive, y=log(comments))) +
  geom_point()

# plot sentiment and ratings
ted_sentiment.afinn %>% 
  left_join(clusters_final.all, by='talk_id') %>%
  ggplot(aes(x=value.mean, y=log(views))) +
  geom_jitter(size=0.5) +
  facet_wrap(~cluster.split1)

ratings_temp %>% right_join(ted_sentiment.bing, by='talk_id') %>%
  ggplot(aes(x=rating, y=perc.pos)) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_wrap(~characteristic, scales = 'free')

ratings_temp %>% right_join(ted_sentiment.bing, by='talk_id') %>%
  ggplot(aes(x=rating, y=perc.neg)) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_wrap(~characteristic, scales = 'free')

ratings_temp %>% right_join(ted_sentiment.bing, by='talk_id') %>%
  gather(sentiment, percentage, c(perc.pos, perc.neg)) %>%
  ggplot(aes(x=rating, y=percentage, colour = sentiment)) +
  geom_jitter(size=0.8, colour='black') +
  geom_smooth(method='lm') +
  facet_wrap(~characteristic, scales = 'free')

ratings_temp %>% right_join(ted_sentiment.loughran, by='talk_id') %>%
  gather(sentiment, percentage, c(positive, negative)) %>%
  ggplot(aes(x=rating, y=log(percentage+0.1), colour = sentiment)) +
  geom_jitter(size=0.8, colour='black') +
  geom_smooth(method='lm') +
  facet_wrap(~characteristic, scales = 'free')

ratings_temp %>% right_join(ted_sentiment.bing, by='talk_id') %>%
  mutate(perc.sent = perc.pos + perc.neg) %>%
  ggplot(aes(x=rating, y=perc.sent)) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_wrap(~characteristic, scales = 'free')

ratings_temp %>% right_join(ted_sentiment.bing, by='talk_id') %>%
  mutate(perc.sent = perc.pos + perc.neg) %>%
  ggplot(aes(y=log(views), x=perc.sent)) +
  geom_point(size=0.8) +
  geom_smooth()

ratings_temp %>% right_join(ted_sentiment.bing, by='talk_id') %>%
  gather(sentiment, percentage, c(perc.pos, perc.neg)) %>%
  ggplot(aes(y=log(views), x=percentage, colour=sentiment)) +
  geom_point(size=0.8) +
  geom_smooth(method='lm')

# look at sentiment by clusters (tags, hierarchical)
ted_sentiment.bing %>% 
  left_join(select(clusters_final, cluster, talk_id), by='talk_id') %>%
  mutate(perc.sent = perc.pos + perc.neg) %>%
  group_by(cluster) %>%
  mutate(cluster.pos = sum(positive),
         cluster.neg = sum(negative),
         mean.perc.pos = mean(perc.pos),
         mean.perc.neg = mean(perc.neg),
         mean.perc.sent = mean(perc.sent)) %>%
  ungroup() %>%
  select(cluster, cluster.pos, cluster.neg, mean.perc.pos, mean.perc.neg, mean.perc.sent) %>%
  distinct() %>%
  arrange(mean.perc.sent)

ted_sentiment.loughran %>% 
  left_join(select(clusters_final, cluster, talk_id), by='talk_id') %>%
  group_by(cluster) %>%
  mutate(cluster.pos = sum(positive),
         cluster.neg = sum(negative),
         mean.perc.pos = mean(positive),
         mean.perc.neg = mean(negative)) %>%
  ungroup() %>%
  select(cluster, cluster.pos, cluster.neg, mean.perc.pos, mean.perc.neg) %>%
  distinct()


ted_sentiment.afinn %>% 
  left_join(select(clusters_final, cluster, talk_id), by='talk_id') %>%
  group_by(cluster) %>%
  mutate(mean = mean(value.mean)) %>%
  ungroup() %>%
  select(cluster, mean) %>%
  distinct()

ted_sentiment.afinn %>% 
  left_join(select(clusters_final.all, cluster.split1, talk_id), by='talk_id') %>%
  ggplot(aes(x=cluster.split1, y=value.mean)) +
  geom_boxplot()

ted_sentiment.bing %>% 
  left_join(select(clusters_final.all, cluster.split1, talk_id), by='talk_id') %>%
  mutate(perc.sent = perc.pos + perc.neg) %>%
  ggplot(aes(x=cluster.split1, y=perc.sent)) +
  geom_boxplot()

ted_sentiment.loughran %>% 
  gather('sentiment', 'percent', c(1:6)) %>%
  left_join(select(clusters_final.all, cluster.split1, talk_id), by='talk_id') %>%
  ggplot(aes(x=sentiment, y=log(percent+0.1), fill=cluster.split1)) +
  geom_boxplot()

clusters_final %>% group_by(cluster) %>% 
  mutate(views.mean = mean(views),
         views.sd = sd(views),
         comments.mean = mean(comments),
         comments.sd = sd(comments),
         languages.mean = mean(languages),
         languages.sd = sd(languages)) %>%
  ungroup() %>%
  select(cluster, views.mean, views.sd, comments.mean, comments.sd, languages.mean, languages.sd) %>%
  distinct() %>%
  arrange(-views.mean)

clusters_final %>%
  ggplot(aes(x=cluster, y=log(views))) +
  geom_boxplot()

clusters_final %>%
  ggplot(aes(x=cluster, y=views)) +
  geom_boxplot()

### look at relationships

# percentage ratings
clusters_final1 <- left_join(clusters_final.all, ratings_percent, by='talk_id') %>% 
  spread(cluster.split1, cuts.split1) %>%
  mutate(`group 1` = ifelse(is.na(`group 1`), 0, 1),
         `group 2` = ifelse(is.na(`group 2`), 0, 1), 
         `group 3` = ifelse(is.na(`group 3`), 0, 1), 
         `group 4` = ifelse(is.na(`group 4`), 0, 1), 
         `group 5` = ifelse(is.na(`group 5`), 0, 1), 
         `group 6` = ifelse(is.na(`group 6`), 0, 1), 
         `group 7` = ifelse(is.na(`group 7`), 0, 1),
         views.log = log(views))
nums <- unlist(lapply(clusters_final1, is.numeric))  
data_temp <- clusters_final1[ , nums]

cor_data <- data.frame(cor(data_temp, method = c("pearson")))

left_join(ted_sentiment.bing, select(clusters_final, talk_id, cluster), by='talk_id') %>%
  ggplot(aes(x=cluster, y=perc.pos)) +
  geom_boxplot()

ggplot(ted_data_sentiment, aes(x=perc.pos)) +
  geom_histogram()

left_join(ted_data_sentiment, select(clusters_final, talk_id, cluster), by='talk_id') %>%
  ggplot(aes(x=cluster, y=positive)) +
  geom_boxplot()




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

# The bind_tf_idf function in the tidytext package takes a tidy text dataset as input with one row per token (term), per document.
# the higher the td_idf the more important the word; takes into account words that appear frequently in one but not across talks
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


### cluster talks on most important words
transcript_clusters <- transcript_words.bound %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(talk_id) %>% 
  top_n(100, tf_idf) %>% 
  select(talk_id, word) %>%
  ungroup() %>%
  ddply(.(talk_id), summarise, top_words=list(paste(word)))

# hierarchical clustering on most important words
transcript_corpus <- Corpus(VectorSource(transcript_clusters$top_words)) %>% # convert to corpus
  tm_map(removeWords, stopwords("english")) %>% # clean up
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace) %>%
  tm_map(removeWords, c('tedx', 'fellow', 'ted'))

transcript_dtm <- DocumentTermMatrix(transcript_corpus)

sparse_transcript_dtm <- removeSparseTerms(transcript_dtm, sparse=0.95)
mat <- as.matrix(sparse_transcript_dtm)
docsdissim <- dist(scale(mat))

h <- hclust(docsdissim, method = "ward.D")

cuts <- cutree(h, 6)
clusters <- data.frame(cuts) %>%
  mutate(talk_id = transcript_clusters$talk_id) %>%
  left_join(ted_data, by='talk_id') %>%
  mutate(cluster = paste('group', as.character(cuts)),
         views = as.double(views))

clusters %>% 
  ggplot(aes(x=views, y=reorder(cluster, views))) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()

p1 <- wordfreq_by_cluster(1, 15)
p2 <- wordfreq_by_cluster(2, 15)
p3 <- wordfreq_by_cluster(3, 15)
p4 <- wordfreq_by_cluster(4, 15)
p5 <- wordfreq_by_cluster(5, 15)
p6 <- wordfreq_by_cluster(6, 15)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol=3, nrow=2)


# visualise most important words by cluster
transcript_words.bound %>%
  left_join(select(clusters, cluster, talk_id), by='talk_id') %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(cluster) %>% 
  top_n(15, tf_idf) %>% 
  ungroup() %>%
  ggplot(aes(reorder(word, tf_idf), tf_idf, fill = cluster)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~cluster, ncol = 2, scales = "free") +
  coord_flip()



########### 
###### LOOK AT RELATED TALKS
##############


ted_data[1,]$related_talks

# analyse ratings
ted_related_talks <- ted_data %>%
  select(talk_id, related_talks) %>%
  mutate(ratings2 = toString(related_talks),
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

