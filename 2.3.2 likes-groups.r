library(data.table)


library(modeest)

tr <- read.csv("../data/processed/train_features3.csv")
#tr <- read.csv("../data/processed/train_featuresTrain.csv")
tr = data.table(tr)

tr[, med_gr:=as.numeric(median(likes)), by=group_id]
tr[, mode_gr:=as.numeric(mfv(likes)[1]), by=group_id]

tr[, mode_gr_month:=as.numeric(mfv(likes)[1]), by=list(group_id, month)]
tr[, med_gr_month:=as.numeric(median(likes)), by=list(group_id, month)]

tr[, avg_log_gr:=mean(log(likes+1)), by=group_id]
tr[, avg_log_gr_month:=mean(log(likes+1)), by=list(group_id, month)]

tr[, med_gr_day:=as.numeric(median(likes)), by = list(group_id,day)]   #fix this
tr[, mode_gr_day:=as.numeric(mfv(likes)[1]), by = list(group_id,day)]   #fix this
tr[, avg_log_gr_day:=mean(log(likes+1)), by=list(group_id,day)]

tr[, med_gr_second:=as.numeric(median(likes)), by = list(group_id,second)]
tr[, mode_gr_second:=as.numeric(mfv(likes)[1]), by = list(group_id,second)]

tr[, med_gr_hour:=as.numeric(median(likes)), by = list(group_id,hour)]
tr[, mode_gr_hour:=as.numeric(mfv(likes)[1]), by = list(group_id,hour)]


setkey(tr,group_id)

write.csv(tr, "../data/processed/train_features4.csv", row.names = FALSE)  
#write.csv(tr, "../data/processed/train_featuresTrain2.csv", row.names = FALSE)  

matcher = tr[,c(3,16, 17, 19,20,21, 23,24,25,26,27,28,29,30,31,32,33,34,35),with=FALSE]

names(matcher)[1] = "group_ida"
names(matcher)[2] = "houra"
names(matcher)[3] = "montha"
names(matcher)[4] = "minuta"
names(matcher)[5] = "seconda"
names(matcher)[6] = "daya"

test <- read.csv("../data/processed/test_features3.csv")
#test <- read.csv("../data/processed/test_featuresTest.csv")
test = data.table(test)
setkey(test,group_id, month)

setkey(matcher,group_ida,montha)

test[, med_gr:=matcher[.(group_id), mult="first"]$med_gr, by=group_id]
test$med_gr[is.na(test$med_gr)] = 0
test[, mode_gr:=matcher[.(group_id), mult="first"]$mode_gr, by=group_id]
test$mode_gr[is.na(test$mode_gr)] = 0

test[, med_gr_month:=matcher[.(group_id, month), mult="first"]$med_gr_month, by=group_id]
test$med_gr_month[is.na(test$med_gr_month)] = 0
test[, mode_gr_month:=matcher[.(group_id, month), mult="first"]$mode_gr_month, by=group_id]
test$mode_gr_month[is.na(test$mode_gr_month)] = 0

test[, avg_log_gr:=matcher[.(group_id), mult="first"]$avg_log_gr, by=group_id]
test$avg_log_gr[is.na(test$avg_log_gr)] = 0
test[, avg_log_gr_month:=matcher[.(group_id, month), mult="first"]$avg_log_gr_month, by=group_id]
test$avg_log_gr_month[is.na(test$avg_log_gr_month)] = 0


setkey(matcher,group_ida,daya)
test[, med_gr_day:=matcher[.(group_id, day), mult="first"]$med_gr_day, by=group_id]
test$med_gr_day[is.na(test$med_gr_day)] = 0
test[, mode_gr_day:=matcher[.(group_id, day), mult="first"]$mode_gr_day, by=group_id]
test$mode_gr_day[is.na(test$mode_gr_day)] = 0

test[, avg_log_gr_day:=matcher[.(group_id, day), mult="first"]$avg_log_gr_day, by=group_id]
test$avg_log_gr_day[is.na(test$avg_log_gr_day)] = 0

setkey(matcher,group_ida, seconda)
test[, med_gr_second:=matcher[.(group_id, second), mult="first"]$med_gr_second, by=group_id]
test$med_gr_second[is.na(test$med_gr_second)] = 0
test[, mode_gr_second:=matcher[.(group_id, second), mult="first"]$mode_gr_second, by=group_id]
test$mode_gr_second[is.na(test$mode_gr_second)] = 0


setkey(matcher,group_ida, houra)
test[, med_gr_hour:=matcher[.(group_id, hour), mult="first"]$med_gr_hour, by=group_id]
test$med_gr_hour[is.na(test$med_gr_hour)] = 0
test[, mode_gr_hour:=matcher[.(group_id, hour), mult="first"]$mode_gr_hour, by=group_id]
test$mode_gr_hour[is.na(test$mode_gr_hour)] = 0



write.csv(test, "../data/processed/test_features4.csv", row.names = FALSE)  
#write.csv(test, "../data/processed/test_featuresTest2.csv", row.names = FALSE)  