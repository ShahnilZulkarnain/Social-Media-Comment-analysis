library(tm)
library(pdftools)
library(wordcloud)
library(extrafont)
library(showtext)
library(jsonlite)
library(readtext)
library(stopwords)
install.packages("quanteda")

#stopwords("bn", source = "stopwords-iso")


#google font
font_add_google("Noto Sans Bengali","Noto Sans Bengali")
showtext_auto()

# pdf location

#file_loction = "D:/Research Project/analysis/text.pdf"

#load pdf file
#txt = pdf_text(file_loction)
#cat (txt[2])

#using word file
stpw = readtext(file.choose())
txtw = readtext(file.choose())
txtw$text
stpw$text
cat (txtw[2])

#extra 
stpw.string = readline(stpw)


#create corpus
#txt_corpus = Corpus(VectorSource(txt))
txtw_corpus = Corpus(VectorSource(txtw))
stpw_corpus = Corpus(VectorSource(stpw))

#cleaning corpus
txtw_corpus.whitespace <- tm_map(txtw_corpus, stripWhitespace)
txtw_corpus.punctuations <- tm_map(txtw_corpus.whitespace, removePunctuation)
#txtw_corpus.dari <- tm_map(txtw_corpus.punctuations, removePunctuation)

#remove stopwords
head(stopwords("bn", source = "stopwords-iso"))
#txtw_corpus.stopwords <- tm_map(txtw_corpus.punctuations, removeWords, stopwords("bn", source = "stopwords-iso"))
txtw_corpus.stopwords <- tm_map(txtw_corpus.punctuations, removeWords, stpw)

#view content of corpus
#txt_corpus$content
txtw_corpus$content
txtw_corpus.whitespace$content
txtw_corpus.punctuations$content
txtw_corpus.stopwords$content

#create document term matrix
dtm = DocumentTermMatrix(txtw_corpus)
dtm = as.matrix(dtm)
dtm = t(dtm)


# sum of occurance of each words
number_occurances = as.matrix(rowSums(dtm))
number_occurances = sort(number_occurances, decreasing = TRUE)
View(number_occurances)

#using wordclouds
wordcloud(head(names(number_occurances),30),head(number_occurances,30), scale =c(2,1))


#length or number of different words
length(number_occurances)

#frequently occuring terms
dtm_original = DocumentTermMatrix(txtw_corpus)
findFreqTerms(dtm_original,lowfreq = 80)

#correlations
findAssocs(dtm_original,"না",0.1)
findAssocs(dtm_original,"অশালীন",0.3)
findAssocs(dtm_original,"পোশাক",0.3)
