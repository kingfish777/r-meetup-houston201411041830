
library(tm)
  # it is best to use v5.9.1 of tm. v6.0.0 seems to be riddled with bugs
  # at least when I run it on Linux Mint/Ubuntu/CentOS
library(stringi)
library(proxy)
wiki <- "http://www.wikipedia.com/wiki/"
titles <- c("Support_vector_machine", "Hidden_Markov_model",
"Riemann-Stieltjes_integral", #"Baum-Welch_algorithm",
"Britney_Spears", "Iggy_Azalea",
"Vladimir_Propp",
"Lev_Tolstoj", "Franz_Kafka",
"Lindsey_Stirling", "Milli_Vanilli",
"Boney_M", "Leonard_Cohen",
"Lindsay_Lohan", "Kim_Kardashian",
"Stan_Getz", "Thelonius_Monk",
"Paris_Hilton", "Frank_Herbert"
)
articles <- character(length(titles))
for (i in 1:length(titles)) {
articles[i] <- stri_flatten(readLines(stri_paste(wiki, titles[i])), col = " ")
}
docs <- Corpus(VectorSource(articles))
docs[[1]]
docs2 <- tm_map(docs, function(x) stri_replace_all_regex(x, "<.+?>", " "), mc.cores = 1)
docs3 <- tm_map(docs2, function(x) stri_replace_all_fixed(x, "\t", " "), mc.cores = 1)
docs4 <- tm_map(docs3, PlainTextDocument)
docs5 <- tm_map(docs4, stripWhitespace)
docs6 <- tm_map(docs5, removeWords, stopwords("english"))
docs7 <- tm_map(docs6, removePunctuation)
docs8 <- tm_map(docs7, tolower)
docs9 <- tm_map(docs8, removeNumbers)
docs8[[1]]
docsTDM <- TermDocumentMatrix(docs9)
docsTDM <- weightTfIdf(docsTDM)
docsTDM <- removeSparseTerms(docsTDM, .9)
docsdissim <- dissimilarity(docsTDM, method = "cosine")
docsdissim2 <- as.matrix(docsdissim)
rownames(docsdissim2) <- titles
colnames(docsdissim2) <- titles
docsdissim2
h <- hclust(docsdissim, method = "ward.D2")
plot(h, labels = titles, sub = "")
#plot(docsTDM, corThreshold = .1, terms=c("music", "love", "alcohol", "money", "fame", "crisis", "literature", "folk"))
