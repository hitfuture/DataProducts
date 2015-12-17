library(tm)
library(wordcloud)
library(RColorBrewer)
contentList <- list(content = "Web.Description", author = "Name.of.Covered.Entity", heading = "Name.of.Covered.Entity",topic="Type.of.Breach")
reader <- readTabular(mapping = contentList)
dataSource <- DataframeSource(breaches)
breachCorpus <- Corpus(dataSource, readerControl = c(list(reader = reader),"the"))
inspect(breachCorpus[1:2])
skipWords <- function(x) removeWords(x, stopwords("english"))
as.lower <- function(x) content_transformer(tolower)
list.of.functions <- list(stripWhitespace,
                                  skipWords,
                                  removePunctuation,
                                  content_transformer(tolower))
bcMap <- tm_map(breachCorpus, FUN = tm_reduce, tmFuns = list.of.functions)
doc.term.matrix  <- DocumentTermMatrix(bcMap   ,control = list(minWordLength = 1))
dt.mat <- as.matrix(doc.term.matrix)
freq <- colSums(dt.mat)
hiFreq <- sort(freq,decreasing = TRUE)
pal <- brewer.pal(9,"BuGn")
pal <- pal[-(1:4)]
words <- names(hiFreq)
message(words)
wordcloud(words[2:101], hiFreq[1:100],colors = pal)