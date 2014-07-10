library(RTextTools)
library(e1071)
library(tm)

wrap.sentence<-function(sentence) {
  create_matrix(textColumns = sentence, language = 'english', 
                removeNumbers = TRUE, toLower = TRUE, removeStopwords = 'TRUE', stemWords = TRUE,
                weighting=tm::weightTf)
}

# train a Naive Bayes classifier
sentences<-c('devise mathematical models',
             'manufacture calculator',
             'counselling',
             'weld metals',
             'write Java codes',
             'keel caster',
             'produce textile')
classes<-c('intellectual', 'industry', 'intellectual', 'blue-collar', 'intellectual', 'blue-collar', 'industry')
tfmatrix<-wrap.sentence(sentences)
nb.classifier<-naiveBayes(as.matrix(tfmatrix), as.factor(classes))

predict(nb.classifier, as.matrix(wrap.sentence('weld calculator')))
