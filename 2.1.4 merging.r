train.features <- read.csv("../data/processed/train_features.csv")
train.likes <- read.csv("../data/processed/train_likes_count.csv")
train.data <- merge(train.likes, train.features, by = "post_id", all = TRUE)
train.data$likes[is.na(train.data$likes)] <- 0
write.csv(train.data, file = "../data/processed/train_features2.csv", row.names = FALSE)
