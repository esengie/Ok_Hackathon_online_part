set.seed(255)

train.data <- read.csv("../data/processed/train_features3.csv")

probs = c(0.8, .0, 0.2)

#miniM = rbinom(dim(train.data)[1], size = 1, prob = 0.1)
x = train.data#[miniM == 1,]
trainIndicator = rmultinom(dim(x)[1], size = 1, prob = probs)
train = x[trainIndicator[1,] == 1, ]
cv = x[trainIndicator[2,] == 1, ]
test = x[trainIndicator[3,] == 1, ]


write.csv(train, "../data/processed/train_featuresTrain.csv", row.names = FALSE)  
write.csv(test, "../data/processed/test_featuresTest.csv", row.names = FALSE)  
