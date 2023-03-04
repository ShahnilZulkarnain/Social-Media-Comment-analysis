 #install all required packages
install.packages(c("ggplot2","e1071","caret","quanteda","irlba","randomForest"))
install.packages("quanteda.textplots")
install.packages("quanteda.textstats")
library(quanteda.textplots)
library(quanteda.textstats)
library(ggplot2)
library(e1071)
library(caret)
library(quanteda)

bbc.raw <- read.csv(file.choose(), stringsAsFactors = FALSE)
View(bbc.raw)

#modify the data set
names(bbc.raw) <- "Text"
View(bbc.raw)

#check data for missing values
length(which(!complete.cases(bbc.raw)))

#length of each comment
bbc.raw$TextLength <- nchar(bbc.raw$Text)
summary(bbc.raw$TextLength)

#graphical distribution of text length
ggplot(bbc.raw, aes(x = TextLength)) + theme_bw()+ 
  geom_histogram(bins = 200) +
  labs(y = "Text Count", x= "Length of the text", title = "Distribution of text length")

#looking up some data
bbc.raw$Text[1256]

#Tokenize the comments
comment.token <- tokens(bbc.raw$Text, what = "word",
                        remove_numbers = TRUE, remove_punct = TRUE,
                        remove_symbols = TRUE)
#take a look inti som3e data
comment.token [[9]]

#cleanup token lowering the cases
comment.token <- tokens_tolower(comment.token)
comment.token [[9]]

#stemming some commonly used words

comment.token.stem <- tokens_replace(comment.token,"পোষাক","পোশাক",valuetype = "fixed")
comment.token.stem <- tokens_replace(comment.token.stem,"পোশাকের","পোশাক",valuetype = "fixed")
comment.token.stem <- tokens_replace(comment.token.stem,"দেশের","দেশ",valuetype = "fixed")
comment.token.stem <- tokens_replace(comment.token.stem,"দেশে","দেশ",valuetype = "fixed")
comment.token.stem <- tokens_replace(comment.token.stem,"islam","ইসলাম",valuetype = "fixed")
comment.token.stem <- tokens_replace(comment.token.stem,"অশ্লীল","অশালীন",valuetype = "fixed")
comment.token.stem <- tokens_replace(comment.token.stem,"ল্যাংটা","লেংটা",valuetype = "fixed")
comment.token.stem <- tokens_replace(comment.token.stem,"শালিন","শালীন",valuetype = "fixed")


#modifying the stopword list


#Removing stopwords
lib.stopwords <- stopwords("bn", source = "stopwords-iso")
mystopwords <- lib.stopwords[! lib.stopwords %in% c("না","ঠিক")]
comment.token.stopwords <- tokens_select(comment.token.stem, mystopwords,
                                         selection = "remove")

comment.token.stopwords[[23]]

#making document term matrics
comment.token.dtm <- dfm(comment.token.stopwords)
comment.token.matrix <- as.matrix(comment.token.dtm)
View(comment.token.matrix[1:20,1:100])

#dimension of matrix
dim(comment.token.matrix)

#finding individual term occurance
#comment.token.dtm.transpose <- t(comment.token.dtm)
comment.token.dtm.transpose <- comment.token.dtm
top.words <- topfeatures(comment.token.dtm.transpose, n = 30)
topDf <- data.frame(
  list(
    term = names(top.words),
    frequency = unname(top.words)
  )
)
#sort by reverse frequency order

topDf$term <- with(topDf, reorder(term, frequency))
ggplot(topDf) + geom_bar(stat = "identity",aes(x= frequency, y= term))+
  theme(axis.text.x = element_text(angle =90, hjust = 1))

#another method

comment.dtm.occurance <- colSums(comment.token.matrix)
#comment.dtm.occurance <- as.data.frame(sort(comment.dtm.occurance, decreasing = TRUE),row.names = NULL)
View(as.matrix(comment.dtm.occurance))



#look up some words arround some tokens
head(kwic(comment.token.stem, "পোশাক", window = 3),n =50)
#dispersion plot
textplot_xray(head(kwic(comment.token.stem, "পোশাক", window = 3),n =50))

#creating a wprd cloud
textplot_wordcloud(comment.token.dtm, max_words =50, max_size = 4,
                   min_size = 1)

#term frequency stats
textstat_frequency(comment.token.dtm, n= 30)




#building up TF-IDF

#function for calculating relative term frequency (TF)
term.frequency <- function(row){
  row/sum(row)
}

#function for calculation IDF
inverse.document.frequency <- function(col){
  corpus.size <- length (col)
  doc.count <- length(which(col > 0))
  
  log10(corpus.size/ doc.count)
}

#TF-IDF function
tf.idf <- function(tf, idf) {
  tf*  idf}

#normalize document via TF
comment.token.df <- apply(comment.token.matrix,1,term.frequency)
dim(comment.token.df)
View(comment.token.df[1:20,1:100])

#calculate the IDF
comment.token.idf <- apply(comment.token.matrix,2,inverse.document.frequency)
str(comment.token.idf)

#calculate the TF-IDF of the function
comment.token.tfidf <- apply(comment.token.df,2,tf.idf, idf = comment.token.idf)
dim(comment.token.tfidf)
View(comment.token.tfidf[1:20,1:100])

#transpose the final result
comment.token.tfidf <- t(comment.token.tfidf)
dim(comment.token.tfidf)
View(comment.token.tfidf[1:20,1:100])

#testing cumulative tfidf
comment.token.tfidf.transpose <- t(comment.token.tfidf)
View(comment.token.tfidf.transpose[1:20,1:100])
comment.tfidf.occurance <- colSums(comment.token.tfidf, na.rm = TRUE)
#comment.tfidf.occurance <- sort(comment.tfidf.occurance, decreasing = TRUE)
View(as.matrix(comment.tfidf.occurance))

#graphing TF-IDF values


#check incomplete cases
incomplete.cases <- which(!complete.cases(comment.token.tfidf))
bbc.raw$Text[incomplete.cases]

#fix Incompete cases
comment.token.tfidf[incomplete.cases,] <- rep(0.0, ncol(comment.token.tfidf))
dim(comment.token.tfidf)
sum(which(!complete.cases(comment.token.tfidf)))

