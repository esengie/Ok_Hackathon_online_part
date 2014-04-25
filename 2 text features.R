#Вытаскивает текстовые фичи, выкидывает текст, на этапе 3 НЕТУ! текста
library("NLP")
library("stringr")
library(tm)

## A very trivial sentence tokenizer.
sent_tokenizer <- function(s) {
    s <- as.String(s)
    m <- gregexpr("[^[:space:]][^.]*\\.", s)[[1L]]
    Span(m, m + attr(m, "match.length") - 1L)
  }
# количество предложений
gg= getwd()
setwd("C:/Users/esengie/Downloads/Odnoklassniki Hackathon/R")
basic.text.statistics <- function(text.data){ 
  n_chars <- nchar(text.data)                                #chars in text -0.018
  n_words <- sapply(gregexpr("[\\W ]+", text.data), length)  #words			-0.017
  avg_word <- n_chars/n_words								 #avg words		-0.004
  n_sentences <- unname(sapply(text.data,function(dd) length(sent_tokenizer(dd)))) #sentences -0.015
  avg_sent <- n_chars/n_sentences							 #avg sentences	0.002
  n_punct_dots <- unname(sapply(text.data,function(dd) str_count(dd, "\\.")))# 	0.01
  n_punct_excl <- unname(sapply(text.data,function(dd) str_count(dd, "!")))# 	-0.004
  n_punct_smiles <- unname(sapply(text.data,function(dd) str_count(dd, ":)")))#  0.011																			
  n_punct_cl_parenth = unname(sapply(text.data,function(dd) str_count(dd, "))"))) 	# 	0.048
  n_punct_op_parenth = unname(sapply(text.data,function(dd) str_count(dd, "\\("))) 	# 	0.02
  n_punct_ques <- unname(sapply(text.data,function(dd) str_count(dd, "?")))# 	-0.018
  n_punct_Images <- unname(sapply(text.data,function(dd) str_count(dd, "Image")))# 	0.108
  n_punct_http <- unname(sapply(text.data,function(dd) str_count(dd, "http")))# 	-0.024
  n_punct_apos <- unname(sapply(text.data,function(dd) str_count(dd, "\"")))#     -0.011
  
  data.frame(n_chars=n_chars, n_words=n_words, avg_word=avg_word, n_sentences=n_sentences, 
  avg_sent=avg_sent, n_punct_dots=n_punct_dots, n_punct_excl=n_punct_excl, n_punct_smiles=n_punct_smiles, 
  n_punct_cl_parenth= n_punct_cl_parenth, n_punct_op_parenth = n_punct_op_parenth, n_punct_ques=n_punct_ques,
  n_punct_Images = n_punct_Images, n_punct_http = n_punct_http, n_punct_apos=n_punct_apos)
}
#Для моего формата с лайками
extract.features1 <- function(input, output) {
  txt= load(input)
  data1 <- get(txt)
  rm(list = txt)
  features <- basic.text.statistics(data1$text)
  training.data.features <- cbind(data1[,-5], features) #Выкидывает текст, на этапе 3 НЕТУ! текста
  write.csv(training.data.features, output, row.names = FALSE)  
}
##Для Лешиного формата без лайков
extract.features2 <- function(input, output) {
  data1 <- read.csv(input, stringsAsFactors = FALSE, encoding = "UTF-8", strip.white = TRUE)
  features <- basic.text.statistics(data1$text)
  training.data.features <- cbind(data1[,-4], features)
  write.csv(training.data.features, output, row.names = FALSE)  
}

#Пока что минисет
extract.features1("../data/processed/miniMTr.rda", "../data/processed/train_features.csv")
#extract.features1("../data/processed/miniMCv.rda", "../data/processed/cv_features.csv")
#extract.features1("../data/processed/miniMTe.rda", "../data/processed/test_features.csv")
extract.features2("../data/processed/test_content.csv", "../data/processed/test_features.csv")
setwd(gg)