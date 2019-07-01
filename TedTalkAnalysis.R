library(tidyverse)
library(lubridate)
library(rjson)
library(ggridges)

ted_data <- read.csv('Data/ted_main.csv', sep=',', header=TRUE, stringsAsFactors = FALSE)
ted_transcripts <- read.csv('Data/transcripts.csv', sep=',', header=TRUE, stringsAsFactors = FALSE)

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

ratings_temp <- gather(ratings_percent, 'characteristic', 'rating', c('Funny':'Inspiring'))
ratings_temp %>% right_join(select(ted_data, talk_id, views), by='talk_id') %>%
  ggplot(aes(x=rating, y=log(views))) +
  geom_point(size=0.8) +
  geom_smooth(method='lm') +
  facet_wrap(~characteristic, scales = 'free')


ratings_temp %>% right_join(select(ted_data, talk_id, views), by='talk_id') %>%
  ggplot(aes(x=rating, y=characteristic, fill = ..x..)) +
  geom_density_ridges_gradient() +
  scale_fill_continuous()
