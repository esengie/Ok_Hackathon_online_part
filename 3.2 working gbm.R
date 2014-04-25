library("gbm")
library(caret)

normalise <- function(v, ms, sds){
	(v-ms)/sds}
renormalise <- function(v, ms, sds){
	sds * v + ms}

starte <- Sys.time ()

set.seed(255)
train.data <- read.csv("../data/processed/train_features4.csv")
test.features <- read.csv("../data/processed/test_features4.csv")

year = train.data$year
train.data = train.data[year > 20,]

library(data.table)
testg = test.features$group_id
traing = train.data$group_id
not_in =setdiff(testg, traing)
test = data.table(test.features)
setkey(test,group_id)
nn =test[, group_id %in% not_in] #ids of them

train.data$is_big = as.numeric(train.data$is_big)
test.features$is_big = as.numeric(test.features$is_big)

dddtr = train.data$is_big > 0.1
nnntr = train.data$is_big < 0.1

#fitControl <- trainControl(method = "repeatedcv", number = 10,repeats = 10)

means = rep(0, length(names(train.data)))
for (i in 1:length(names(train.data))) {means[i] = mean(train.data[,i])}
sds = rep(0, length(names(train.data)))
for (i in 1:length(names(train.data))) {sds[i] = sd(train.data[,i])}
for (i in 2:length(names(train.data))) {train.data[,i] = normalise(train.data[,i], means[i], sds[i])}

#xnam1 <- paste0(c("n_chars", "n_words", "avg_word","n_punct_excl", "n_punct_cl_parenth", "n_punct_Images", "n_punct_smiles", "n_punct_dots",
 #"n_punct_http", "hour" , "month" , "year", "minute","second","day",               
#"avg_gr_month", "avg_log_gr" , "avg_log_gr_month","avg_gr_day", "avg_gr_hour","avg_gr_minute","avg_gr_second")) 
xnam1 <- paste0(c("avg_gr_month", "avg_gr_day", "avg_gr_hour","avg_gr_minute","avg_gr_second")) 

fmla1 <- as.formula(paste("likes ~ ", paste(xnam1, collapse= "+")))


xnam2 <- paste0(c("avg_gr_month", "avg_gr_day", "avg_gr_hour","avg_gr_minute","avg_gr_second")) 
fmla2 <- as.formula(paste("likes ~ ", paste(xnam2, collapse= "+")))

#pfit1 <- train(likes~. - post_id, data=train.data[dddtr,], method = "gbm",trControl = fitControl, verbose = FALSE, metric =  "Rsquared")
#pfit2 <- train(likes~. - post_id, data=train.data[nnntr,], method = "gbm",trControl = fitControl, verbose = FALSE, metric =  "Rsquared")
pfit1 = gbm(fmla1,data=train.data[dddtr,],distribution="gaussian",n.trees=3000,shrinkage=0.01,
		   interaction.depth=4,bag.fraction = 0.5,train.fraction = 1,n.minobsinnode = 10,
		   cv.folds = 10,keep.data=TRUE,verbose=FALSE, n.cores=4)								 

pfit2 = gbm(fmla2,data=train.data[nnntr,],distribution="gaussian",n.trees=1000,shrinkage=0.01,
		   interaction.depth=4,bag.fraction = 0.5,train.fraction = 1,n.minobsinnode = 10,
		   cv.folds = 10,keep.data=TRUE,verbose=FALSE, n.cores=4)								 

		   
best.iter1 <- gbm.perf(pfit1,method="cv")
best.iter2 <- gbm.perf(pfit2,method="cv")

gbm.predict = rep(0, dim(test.features)[1])

dddte = test.features$is_big > 0.1
nnnte = test.features$is_big < 0.1

test.features[,1] = normalise(test.features[,1], means[3], sds[3])
for (i in 3:length(names(test.features))) {test.features[,i] = normalise(test.features[,i], means[i+1], sds[i+1])}

gbm.predict[dddte] <- predict(pfit1, test.features[dddte,], best.iter1)
gbm.predict[nnnte] <- predict(pfit2, test.features[nnnte,], best.iter2)

test.features[,1] = renormalise(test.features[,1], means[3], sds[3])
for (i in 3:length(names(test.features))) {test.features[,i] = renormalise(test.features[,i], means[i+1], sds[i+1])}
gbm.predict = renormalise(gbm.predict, means[2], sds[2])

gbm.predict[gbm.predict < 0] <- 0
gbm.predict[test.features$year < 21] <- 0
gbm.predict[nn] <- 0

submission <- data.frame(post_id = test.features$post_id, predict = gbm.predict)
write.csv(submission, "../data/baseline.csv", row.names = FALSE)

timeTots = Sys.time () - starte