normalise <- function(v, ms, sds){
	(v-ms)/sds
	}
renormalise <- function(v, ms, sds){
	sds * v + ms
	}

library(rpart)
library(caret)
library(gbm)

set.seed(255)
train.data <- read.csv("../data/processed/train_features4.csv")
test.features <- read.csv("../data/processed/test_features4.csv")
#train.data <- read.csv("../data/processed/train_featuresTrain2.csv")
#test.features <- read.csv("../data/processed/test_featuresTest2.csv")

#miniM = rbinom(dim(test.features)[1], size = 1, prob = 0.5)
#cvs = test.features[miniM == 1,]
#test.features = test.features[miniM == 0,]

month = train.data$month
train.data = train.data[month > 504,]

library(data.table)
testg = round(test.features$group_id)
traing = round(train.data$group_id)
not_in =setdiff(testg, traing)
test = data.table(test.features)
setkey(test,group_id)
#nn =test[J(not_in)]   #elems
nn =test[, group_id %in% not_in] #ids of them


train.data$is_big = as.numeric(train.data$is_big)
test.features$is_big = as.numeric(test.features$is_big)

#linear.model <- lm(likes ~. - post_id, train.data)
dddtr = train.data$is_big > 0.1
nnntr = train.data$is_big < 0.1

means = rep(0, length(names(train.data)))
for (i in 1:length(names(train.data))) {means[i] = mean(train.data[,i])}
sds = rep(0, length(names(train.data)))
for (i in 1:length(names(train.data))) {sds[i] = sd(train.data[,i])}



#pfit1 = lm(likes ~. - post_id, train.data[dddtr,])
#pfit2 = lm(likes ~. - post_id, train.data[nnntr,])

#ctrl <- trainControl(method='cv')
#rp1 <-  expand.grid(cp = c(0.003, 0.002, 0.001, 0.0008))
#rp2 <-  expand.grid(cp = c(0.00001, 0.00003))

xnam <- paste0(c("group_id","n_chars", "n_words", "avg_word","n_punct_excl", "n_punct_cl_parenth", "n_punct_Images", "n_punct_smiles", "n_punct_dots",
 "n_punct_op_parenth", "n_punct_ques", "n_punct_http","n_punct_apos", "hour" , "month" , "year", "minute","second","day",               
"is_big", "avg_gr", "avg_gr_month", "avg_log_gr" , "avg_log_gr_month","avg_gr_day", "avg_gr_hour","avg_gr_minute","avg_gr_second","avg_log_gr_day")) 
#xnam <- paste0(names(train.data)[4:length(names(train.data))]) 

xnam1 <- paste0(c("n_chars", "n_words", "avg_word","n_punct_excl", "n_punct_cl_parenth", "n_punct_Images", "n_punct_smiles", "n_punct_dots",
 "n_punct_op_parenth", "n_punct_ques", "n_punct_http", "hour" , "minute","second","day",               
  "med_gr", "mode_gr","mode_gr_month", "med_gr_month", "avg_log_gr_month", "avg_log_gr", 
  "med_gr_day", "mode_gr_day", "avg_log_gr_day", "med_gr_second", "mode_gr_second")) 
fmla1 <- as.formula(paste("likes ~ ", paste(xnam1, collapse= "+")))


xnam2 <- paste0(c("n_chars", "n_words", "avg_word","n_punct_excl", "n_punct_cl_parenth", "n_punct_Images", "n_punct_smiles", "n_punct_dots",
 "n_punct_op_parenth", "n_punct_ques", "n_punct_http","n_punct_apos","hour" , "month" , "year", "minute","second","day",               
"med_gr", "mode_gr","mode_gr_month", "med_gr_month", "avg_log_gr_month", "avg_log_gr")) 
fmla2 <- as.formula(paste("likes ~ ", paste(xnam2, collapse= "+")))

#for (i in 2:length(names(train.data))) {train.data[,i] = normalise(train.data[,i], means[i], sds[i])}

pfit1 = gbm(fmla1, data = train.data[dddtr, ], distribution ="gaussian", n.trees = 1000, shrinkage=0.001, interaction.depth = 3, bag.fraction = 0.5,
			train.fraction = 1, n.minobsinnode = 10, cv.folds = 8, keep.data=TRUE, verbose=FALSE, n.cores=4)								 
best.iter1 <- gbm.perf(pfit1,method="cv")

#rpartFit1 <- rpart(fmla1, data =train.data[dddtr,],method="anova", control=rpart.control(cp=0.001))#metric="Rsquared", trControl = ctrl, tuneGrid=rp2)
#pfit1<- prune(rpartFit1, cp = 0.005)#$finalModel

rpartFit2 <- rpart(fmla2, data =train.data[nnntr,],method="anova", control=rpart.control(cp=0.001))#metric="Rsquared", trControl = ctrl, tuneGrid=rp2)
pfit2<- prune(rpartFit2, cp = 0.005)#$finalModel

gbm.predict = rep(0, dim(test.features)[1])

dddte = test.features$is_big > 0.1
nnnte = test.features$is_big < 0.1

gbm.predict[nnnte] <- predict(pfit2, test.features[nnnte,])

#test.features[,1] = normalise(test.features[,1], means[3], sds[3])
#for (i in 3:length(names(test.features))) {test.features[,i] = normalise(test.features[,i], means[i+1], sds[i+1])}

gbm.predict[dddte] <- predict(pfit1, test.features[dddte,], best.iter1)

###################       Normalisation

#test.features[,1] = renormalise(test.features[,1], means[3], sds[3])
#for (i in 3:length(names(test.features))) {test.features[,i] = renormalise(test.features[,i], means[i+1], sds[i+1])}

#gbm.predict[dddte] = renormalise(gbm.predict[dddte], means[2], sds[2])
##############################

gbm.predict[gbm.predict < 0] <- 0
gbm.predict[test.features$month < 505] <- 0

gbm.predict[nn] <- 0

r2 = function(x, y) var(x-y)/var(x)
#mm = (1 - r2(test.features$likes, gbm.predict))*1000					#424			421
#mm1 = (1 - r2(test.features$likes[dddte], gbm.predict[dddte]))*1000		#167			166
#mm2 = (1 - r2(test.features$likes[nnnte], gbm.predict[nnnte]))*1000		#673  -- 676 теперь 754
submission <- data.frame(post_id = test.features$post_id, predict = gbm.predict)
write.csv(submission, "../data/baseline.csv", row.names = FALSE)
