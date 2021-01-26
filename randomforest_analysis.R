library(tidyverse)
library(tree)
library(randomForest)

clusters <- read_csv('cluster_lookup.csv')

rf_data <- read_csv('rf_data.csv')

data <- rf_data %>%
  gather(characteristic, value, c(28:41)) %>%
  left_join(clusters, by='characteristic') %>%
  select(-characteristic) %>%
  filter(!is.na(cluster_pca)) %>%
  group_by(talk_id, cluster_pca) %>%
  mutate(value=sum(value)) %>%
  distinct() %>%
  spread(cluster_pca, value) %>%
  ungroup()

my_vars.views <- c('views', 'cuts', 'comments_per_view', 'published_year', 'languages', 
             'Cluster_1', 'Cluster_2', 'Cluster_3', 'Cluster_4', 'description_length', 
             'title_length')

my_data <- select(data, my_vars.views) %>%
  mutate(cuts = paste('Group', cuts, sep='_'),
         dummy = 1) %>%
  spread(cuts, dummy)
my_data[is.na(my_data)] <- 0

set.seed(2)
tree.views <- tree::tree(views~., my_data)

summary(tree.views)

tree.views

plot(tree.views)
text(tree.views, pretty=0)

cv.views <- cv.tree(tree.views)
plot(cv.views$size, cv.views$dev, type='b')

train <- sample(1:nrow(my_data), nrow(my_data)/1.25)
tree.views <- tree(views~., data=my_data, subset=train)
summary(tree.views)

# pruning
prune.views <- prune.tree(tree.views, best=5)
plot(prune.views)
text(prune.views, pretty=0)

yhat <- predict(tree.views, newdata=my_data[-train,])
views.test <- my_data[-train, 'views']
plot(yhat, views.test$views)
abline(0,1)
mean((yhat-views.test$views)^2)^(1/2)
mean((abs(yhat-views.test$views)/yhat)*100)

# bagging
set.seed(1)
bag.views <- randomForest(views~., data=my_data, subset=train,  mtry=13, importance=TRUE)
bag.views

yhat.bag <- predict(bag.views, newdata=my_data[-train,])
mean((yhat.bag-views.test$views)^2)^(1/2)
mean((abs(yhat.bag-views.test$views)/yhat.bag)*100)

# random forest
rf.views <- randomForest(views~., data=my_data, subset=train,  mtry=5, importance=TRUE)
yhat.rf <- predict(rf.views, newdata=my_data[-train,])
mean((yhat.rf-views.test$views)^2)^(1/2)
mean((abs(yhat.rf-views.test$views)/yhat.rf)*100)


importance(rf.views)
importance(bag.views)
