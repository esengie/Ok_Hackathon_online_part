library("gbm")
#setwd("C:/Users/esengie/Downloads/Odnoklassniki Hackathon/R")
load("../data/processed/miniMTr.rda")
load("../data/processed/miniMTe.rda")

year = train$year
train = train[year > 20,]
#todo выкинуть year
#linear.model <- lm(likes ~. - post_id, train)

#test.features$likes[is.na(test.features$likes)] <- 0
train$is_big = as.numeric(train$is_big)
test$is_big = as.numeric(test$is_big)
#linear.predict.test <- predict(linear.model, test.features)
set.seed(255)


gbm1 = gbm(likes~ . - post_id,         # formula  best.iter is 10000  crazy gbm 
    data=train,                   # dataset
    distribution="gaussian",     # see the help for other choices
    n.trees=2500,                # number of trees
    shrinkage=0.01,              # shrinkage or learning rate,
                                 # 0.001 to 0.1 usually work
	interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
    bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
    train.fraction = 1,        # fraction of data for training,
                                 # first train.fraction*N used for training
    n.minobsinnode = 10,         # minimum total weight needed in each node
    cv.folds = 3,              # do 3-fold cross-validation
    keep.data=TRUE,              # keep a copy of the dataset with the object
    verbose=FALSE,               # don't print out progress
    n.cores=1)                   # use only a single core (detecting #cores is
                                 # error-prone, so avoided here)
best.iter <- gbm.perf(gbm1,method="cv")
gbm.predict = predict(gbm1, test, best.iter)
gbm.predict[gbm.predict < 0] <- 0
gbm.predict[test$year < 21] <- 0
#linear.predict.test[linear.predict.test < 0] <- 0
r2 = function(x, y) var(x-y)/var(x)
mm = (1 - r2(test$likes, gbm.predict))*1000
#submission <- data.frame(post_id = test$post_id, predict = gbm.predict)
#write.csv(submission, "../data/baseline.csv", row.names = FALSE)
 
