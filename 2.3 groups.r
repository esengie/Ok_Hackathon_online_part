###Finds groups that have 1% of posts in them

basic.group.statistics <- function(group){ 
  #Counter for max group id
  maxG = max(group)
  allPosts = length(group)
  group_posts = sapply(seq(maxG), function (v) sum(group == v)/allPosts)
  big_groups = which(group_posts>0.004)
  in_bigs = as.numeric(sapply(group, function(v) v %in% big_groups))
  df = data.frame(is_big = in_bigs)
}

tr <- read.csv("../data/processed/train_features2.csv")
dfs = basic.group.statistics(tr$group_id)
training.data.features <- cbind(tr, dfs)
write.csv(training.data.features, "../data/processed/train_features3.csv", row.names = FALSE)  

test <- read.csv("../data/processed/test_features2.csv")
dfs2 = basic.group.statistics(test$group_id)
test.data.features <- cbind(test, dfs2)
write.csv(test.data.features, "../data/processed/test_features3.csv", row.names = FALSE)  