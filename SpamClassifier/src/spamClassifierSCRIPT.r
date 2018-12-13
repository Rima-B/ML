#-------------------emailCleanerStemmer-----------------
emailCleanerStemmer<- function(email){
  #email<-file_email
  #Lower case
  email$text<-stri_trans_tolower(email$text)        
  #Strip all HTML
  email$text<-stri_replace_all_regex(email$text, '<[^<>]+>', '')   
  #all number become "number"
  email$text<-stri_replace_all_regex(email$text, '[0-9]+[,\\.]{0,1}[0-9]*', 'number')
  #stri_replace_all_regex("1 undye 45.6 skjdkd 456 kkaji 45,76", '[0-9]+[,\\.]{0,1}[0-9]*', 'number')
  #Handle URLS
  email$text<-stri_replace_all_regex(email$text,'(http|https)://[^\\s]*', 'httpaddr') 
  #handle email$text@ddress
  email$text<-stri_replace_all_regex(email$text, '[^\\s]+@[^\\s]+', 'emailaddr') 
  #$price, think to handle dinar, euro, pound
  email$text<-stri_replace_all_regex(email$text, '[$]+', 'dollar')  
  #Remove any non alphanumeric characters
  email$text <- stri_replace_all_regex(email$text,'[^a-zA-Z0-9 ]', ' ')
  #trim multiple spaces
  email$text <- stri_replace_all_regex(email$text,'( )+', ' ') 
  #Stemming
  corpus <- Corpus(DataframeSource(data.frame(email)))
  corpus <- tm_map(corpus, stemDocument, "english") 
  return (corpus[[1]]$content)
}
#-------------------getVocabList-----------------
getVocabList <- function(){
  vocabList <- read.delim('vocab.txt', sep='\t', header = FALSE)[2]
  return(vocabList)
}
#-------------------emailToFeature-----------------
emailToFeature <- function(emailCS){
  vocabList <- getVocabList() 
  #emailCS.copy=emailCS
  emailCS<-strsplit(emailCS,' ')
  emailCS<-unlist(lapply(emailCS[[1]], function(x) {which(vocabList==x)}))
  # octv_ind <- read.delim('word_indices.txt', sep="", header = FALSE)
  # c(setdiff(octv_ind[[1]],emailCS2),setdiff(emailCS,octv_ind[[1]]))
  # vocabList[[1]][1676]
  emailTF <-  rep(0, length(vocabList$V2))
  emailTF <- unlist(lapply(1:length(emailTF), function(i){
    if (i %in% emailCS) emailTF[i]=1 else emailTF[i]=0
  }))
  # make sure cells at 1 are good
  #c(setdiff(octv_ind[[1]],which(emailTF==1)),setdiff(which(emailTF==1),octv_ind[[1]]))
  return (emailTF)
}
#-------------------prepareEmailsFiles-----------------
prepareEmailsFiles<- function(){
  unlink(paste0(getwd(),"/spamAssassinCorpus/emailCorpus"))
  list_dirs <- list.dirs(paste0(getwd(),"/spamAssassinCorpus"), full.names = TRUE, recursive = FALSE)
  dir.create(paste0(getwd(),"/spamAssassinCorpus/emailCorpus"))
  countFold<-1
  for (dir in list_dirs){
    f_list <- list.files(paste0(dir,"/"))
    folder<-sub(".*[/]", "", dir)    #(.*[/]) all strings finishing with /
    for (f in f_list){
      newf<-paste0(length(grep("spam",folder)),"_",countFold,sub("\\..*", "", f),".txt")
      file.copy(paste0(dir,"/",f),paste0(getwd(),"/spamAssassinCorpus/emailCorpus/",newf))
    }
    countFold<-countFold+1
  }
  return(paste0(getwd(),"/spamAssassinCorpus/emailCorpus"))
}
#-------------------buildCorpus-----------------
buildCorpus <- function(path){
  X<-y<-NULL
  f_list <- list.files(path)
  for (f in f_list){
    file_email<-readtext(paste0(path,"/",f))
    #Clean and Stemm email
    emailCS <- emailCleanerStemmer(file_email)
    emailFeature = emailToFeature(emailCS)
    X<-rbind(X,t(as.matrix(emailFeature)))
    #View(X[,1:100])
    y<-rbind(y,as.vector(length(grep("1_",f))))
  }
  #str(list("X"=X,"y"=y))
  return (list("X"=X,"y"=y))
}
#-------------------prepareSetsTVT-----------------
prepareSetsTVT <- function(spamDataSet){
  df<-cbind(spamDataSet$X,spamDataSet$y)
  spec = c(train = .6, test = .2, validate = .2)
  g = sample(cut(seq(nrow(df)), nrow(df)*cumsum(c(0,spec)), labels = names(spec)))
  res = split(data.frame(df), g)
  #str(res)
  spamTrain<-list(X=as.matrix(res$train[,1:ncol(res$train)-1]), y=as.matrix(res$train[,ncol(res$train)]))
  # str(spamTrain); str(res$train[1])
  spamValidate<-list(X=as.matrix(res$validate[,1:ncol(res$validate)-1]), y=as.matrix(res$validate[,ncol(res$validate)]))
  spamTest<-list(X=as.matrix(res$test[,1:ncol(res$test)-1]), y=as.matrix(res$test[,ncol(res$test)]))
  sum(spamTrain$y)/length(spamTrain$y)
  sum(spamValidate$y)/length(spamValidate$y)
  sum(spamTest$y)/length(spamTest$y)
}
#-----------------MAIN----------------------
# install.packages("e1071")
# install.packages("readtext")
# install.packages("rstudioapi")
# install.packages("tm")
# install.packages("SnowballC")
# install.packages("R.matlab")
library("data.table")
library("tm")
library("e1071")
library("readtext")
library("rstudioapi")
library('stringi')
library('SnowballC')
library("R.matlab")
setwd(dirname(getActiveDocumentContext()$path))

corpusPath<-prepareEmailsFiles()
start_time <- Sys.time()
spamDataSet<-buildCorpus(paste0(corpusPath,""))
print(Sys.time() - start_time)
writeMat("spamAssassinDataSetMatrix.mat", X=spamDataSet$X, y=spamDataSet$y)
# data <- readMat("spamAssassinDataSetMatrix.mat")
# str(data)
#--------------split a data frame into training, validation, and test sets-----------
prepareSetsTVT(spamDataSet)
df<-cbind(spamDataSet$X,spamDataSet$y)
spec = c(train = .6, test = .2, validate = .2)
g = sample(cut(seq(nrow(df)), nrow(df)*cumsum(c(0,spec)), labels = names(spec)))
res = split(data.frame(df), g)
#str(res)
spamTrain<-list(X=as.matrix(res$train[,1:ncol(res$train)-1]), y=as.matrix(res$train[,ncol(res$train)]))
# str(spamTrain); str(res$train[1])
spamValidate<-list(X=as.matrix(res$validate[,1:ncol(res$validate)-1]), y=as.matrix(res$validate[,ncol(res$validate)]))
spamTest<-list(X=as.matrix(res$test[,1:ncol(res$test)-1]), y=as.matrix(res$test[,ncol(res$test)]))
sum(spamTrain$y)/length(spamTrain$y)
sum(spamValidate$y)/length(spamValidate$y)
sum(spamTest$y)/length(spamTest$y)
which(spamTrain$y==1)
#-------------Train SVM-------------
# #load matrice
# path <- system.file("mat-files", package = "R.matlab")
# spamTrain <- readMat("spamTrain.mat")
#train the svm, classification
start_time <- Sys.time()
spamModel <- svm(spamTrain$X, spamTrain$y, type="C-classification") 
print(Sys.time() - start_time)
summary(spamModel)
# test with train data
pred <- predict(spamModel, spamTrain$X)
# (same as:) pred <- fitted(spamModel)
# Check accuracy:
d <- table(pred, spamTrain$y)
sum(diag(d))/sum(rowSums(d))
#-------------Test SVM-------------
#spamTest <- readMat("spamTest.mat")
predT <- predict(spamModel, spamTest$X)
dT <- table(predT, spamTest$y)
sum(diag(dT))/sum(rowSums(dT))
#-------------Test 1 email-------------
file_email<-readtext(paste0(corpusPath,'/0_400177.txt'))
emailFeature <- emailToFeature(emailCleanerStemmer(file_email))
predict(spamModel, t(data.frame(emailFeature)))
#-------------Test hard ham emails (real emails close to spams)-------------
f_list <- list.files(corpusPath, pattern = ".*_4.*")
XHam<-yHam<-NULL
for (f in f_list){
  file_email<-readtext(paste0(corpusPath,"/",f))
  #Clean and Stemm email
  emailCS <- emailCleanerStemmer(file_email)
  emailFeature = emailToFeature(emailCS)
  XHam<-rbind(XHam,t(as.matrix(emailFeature)))
  #View(X[,1:100])
  yHam<-rbind(yHam,as.vector(length(grep("1_",f))))
  #print(length(yHam))
  #str(XHam)
}
predTHam<-predict(spamModel, XHam)
dTHam <- table(predTHam, t(yHam))
sum(diag(dTHam))/sum(rowSums(dTHam))
which(predTHam==1)
