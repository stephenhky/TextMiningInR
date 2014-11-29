library(RTextTools)
library(tm)
library(e1071)
library(maxent)

wrap.sentence<-function(sentence, originalMatrix=NULL) {
  create_matrix(textColumns = sentence, language = 'english', 
                removeNumbers = TRUE, toLower = TRUE, removeStopwords = 'TRUE', stemWords = TRUE,
                weighting=tm::weightTf, originalMatrix=originalMatrix)
}

#book.files<-list.files('AV1611text/')
#testaments<-c('OT', 'NT', 'NT', 'OT',
#              'NT', 'OT', 'NT', 'NT',
#              'OT', 'NT', 'NT', 'OT',
#              'NT', 'OT', 'NT', 'NT',
#              'NT', 'NT', 'OT', 'NT',
#              'OT', 'NA', 'OT', 'OT',
#              'NT', 'OT', 'OT', 'OT',
#              'OT', 'NT', 'OT', 'OT',
#              'OT', 'NT', 'OT', 'OT',
#              'NT', 'OT', 'OT', 'OT',
#              'NT', 'OT', 'OT', 'NT',
#              'OT', 'OT', 'OT', 'NT',
#              'OT', 'NT', 'NT', 'OT',
#              'OT', 'OT', 'OT', 'OT',
#              'NT', 'NT', 'NA', 'NA',
#              'OT', 'OT', 'NT', 'NT',
#              'OT', 'OT', 'NT', 'OT',
#              'OT')
#testaments<-factor(testaments)
bible.content.page<-read.csv('BibleContentPage.csv')
book.files<-bible.content.page$BookFile[ !is.na(bible.content.page$Testament)]
testaments<-bible.content.page$Testament[ !is.na(bible.content.page$Testament)]

documents<-mapply(function(book.file) readChar(file.path('AV1611text', book.file), 
                                               file.info(file.path('AV1611text', book.file))$size), 
                                               book.files)
dtmatrix<-wrap.sentence(documents)

nb.classifier<-naiveBayes(as.matrix(dtmatrix), testaments)
predict(nb.classifier, as.matrix(wrap.sentence(examples, originalMatrix = dtmatrix)))

svm.classifier<-svm(as.compressed.matrix(dtmatrix), testaments, cost=100, gamma=1, kernel='linear')
as.vector(predict(svm.classifier, as.matrix(wrap.sentence(examples, originalMatrix = dtmatrix))))

maxent.classifier<-maxent(as.compressed.matrix(dtmatrix), testaments)
predict(maxent.classifier, as.matrix(wrap.sentence(examples, originalMatrix = dtmatrix)))


