##############################################################
# Author: Scott Malec
# Email: scott.malec [at] gmail [dot] com
# Date: 5/22/2014
#
# DERIVED and ADAPTED FROM Timothy D'Auria / Boston Decision ::
# http://bostondecision.com/2012/05/14/the-text-classifier-determine-the-author-of-a-document-with-machine-learning-in-r/
#
# TITLE: AUTOPROPP-KNN, v.2.0
#
# Purpose: R tools to predict the Proppian function class to which a particular content chunk belongs
# after training on a corpus annotated with PFTML 2.0.
# Uses a simple K-nearest Neighbor algorithm.
#
# Setup: Set pathname to a directory that contains 1 subdirectory
# for each Proppian function/"content chunk".
# Each content chunk subdirectory contains 1 or more content chunks saved as .txt files.
#
# Set content chunks to the names of the content chunk subdirectories
#
# Predicts content chunk class membership to a mean ~80% precision over 1000 iterations.
#
# NOTE: Some proppian content chunks are better defined than others.
# Mileage varies depending on Proppian function/content chunk
# and parameters that have been used in creating language model.
#########################################################################
#
#
#
#
# Load libraries
library(tm)
library(wordcloud)
library(kernlab)
library(plyr)
library(class)
library(SnowballC)
#library(RWeka)
library(Rgraphviz)
# Set options
options(stringsAsFactors = FALSE)
#BiGramTokenize
#myTokenizer <- function(x) RWeka::NGramTokenizer(x, Weka_control(min = 3, max = 3))
# Set parameters
pathname <- "/amanuensis/data/proppian_function_language_models/"
#proppianContentChunks <- c("AlienForms", "Villainy", "Lack", "Preparation", "Departure", "DonorFunctions", "Epilogue", "Villainy", "Return")
proppianContentChunks <- c("Villainy", "Preparation", "DonorFunctions", "Wedding")
knn_performance <- vector()
for (counter in 1:10) {
pathname <- "/amanuensis/data/proppian_function_language_models/"
# Function to generate corpus where each content chunck is
# set as a document.
generateProppianFunctionDocCorpus <- function(proppianFunctionCandidate, path) {
# Set directory and list files
s.dir <- sprintf("%s/%s", path, proppianFunctionCandidate)
filelist <- list.files(s.dir, full.names = TRUE)
# Read each content chunk and append to vector
folktale.v <- unlist(sapply(filelist, function(x) {
folktale.temp <- readLines(x)
folktale.tmp <- folktale.temp[folktale.temp != ""]
return(folktale.temp)
}))
# Instantiate Corpus
s.cor <- Corpus(VectorSource(folktale.v, encoding = 'UTF-8'))
return(s.cor)
}
# Function to clean Corpus text
cleanCorpus <- function(corpus) {
# Apply corpus clean-up functions
corpus.temp <- tm_map(corpus, removeWords, stopwords("SMART"), mc.cores=4)
corpus.temp <- tm_map(corpus.temp, removePunctuation, mc.cores=4)
corpus.temp <- tm_map(corpus.temp, removeNumbers, mc.cores=4)
corpus.temp <- tm_map(corpus.temp, content_transformer(tolower))
return(corpus.temp)
}
# Function to generate corpus from a single file
generatefolktaleDocCorpus <- function(filepath) {
# Read data from file
vector <- scan(filepath, what = "", quiet = TRUE)
# Collapse word vector
vector <- paste(vec, collapse = " ")
# Instantiate Corpus
folktale.corpus <- Corpus(VectorSource(vector, encoding = 'UTF-8'))
return(folktale.corpus)
}
# Function to generate term document matrices
generateTDM <- function(proppianFunctionCandidate, path) {
# Set directory
s.dir <- sprintf("%s/%s", path, proppianFunctionCandidate)
# Instantiate Corpus
s.cor <- Corpus(DirSource(directory = s.dir, encoding = 'UTF-8'))
#s.cor <- generateProppianFunctionDocCorpus(proppianFunctionCandidate, path)
# Clean corpus
s.cor.cl <- cleanCorpus(s.cor)
#ngrams <- RWeka::NGramTokenizer(s.cor, Weka_control(min=2, max=2))
# Create term document matrix
#s.tdm <- TermDocumentMatrix(s.cor.cl, control=list(tokenize = ngrams, weighting = weightTfIdf))
#s.tdm <- TermDocumentMatrix(s.cor.cl, control=list(weighting = weightTfIdf))
s.tdm <-TermDocumentMatrix(s.cor,
control = list(removePunctuation = TRUE,
stopwords = TRUE,
weighting = function(x)
weightSMART(x, spec = "ntc")))
# Remove sparse terms
s.tdm <- removeSparseTerms(s.tdm, 0.7) # 0.7
# Construct return object
result <- list(name = proppianFunctionCandidate, tdm = s.tdm)
return(result)
}
# Run term document matrix function on all proppianContentChunks
tdm <- lapply(proppianContentChunks, generateTDM, path = pathname)
# Bind Proppian Function/Content Chunk Name to Term Document Matrices
bindProppianFunctionToTDM <- function(tdm) {
s.mat <- t(data.matrix(tdm[["tdm"]]))
s.df <- as.data.frame(s.mat, stringsAsfactors = FALSE)
s.df <- cbind(s.df, rep(tdm[["name"]], nrow(s.df)))
colnames(s.df)[ncol(s.df)] <- "targetProppianFunction"
return(s.df)
}
# Append Proppian function candidate field to TDM
pfTDM <- lapply(tdm, bindProppianFunctionToTDM)
# Rbind Proppian Function TDMs
tdm.stack <- do.call(rbind.fill, pfTDM)
tdm.stack[is.na(tdm.stack)] <- 0
# Take 70% sample for training of language model; retain remainder for verifying accuracy
train.index <- na.omit(sample(nrow(tdm.stack), ceiling(nrow(tdm.stack) * .7)))
test.index <- na.omit((1:nrow(tdm.stack))[- train.index])
# Extract Proppian function name
tdm.proppianFunctionCandidate <- na.omit(tdm.stack[, "targetProppianFunction"])
tdm.stack.nl <- tdm.stack[,!colnames(tdm.stack) %in% "targetProppianFunction"]
#tdm.stack.nl
# K-nearest Neighbor
knn.pred <- knn(na.omit(tdm.stack.nl[train.index, ]), tdm.stack.nl[test.index, ], tdm.proppianFunctionCandidate[train.index])
knn.pred <- knn(tdm.stack.nl[train.index, ], tdm.stack.nl[test.index, ], tdm.proppianFunctionCandidate[train.index])
knn.train.data <- tdm.stack[train.index, ]
# Confusion Matrix
conf.mat <- table("Predictions" = knn.pred, Actual = tdm.proppianFunctionCandidate[test.index])
conf.mat
# Calculate precision = tp/(tp+tf)*100
(precision <- sum(diag(conf.mat))/length(test.index) * 100)
knn_performance <- c(knn_performance, precision)
}
mean(knn_performance)
