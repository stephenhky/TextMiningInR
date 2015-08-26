library(RTextTools)
library(e1071)
library(tm)
library(maxent)

wrap.sentence<-function(sentence, originalMatrix=NULL) {
  create_matrix(textColumns = sentence, language = 'english', 
                removeNumbers = TRUE, toLower = TRUE, removeStopwords = 'TRUE', stemWords = TRUE,
                weighting=tm::weightTf, originalMatrix=originalMatrix)
}

svm.model<-function(sentence, label, cost=100, gamma=1, kernel='linear') {
  tfmatrix<-wrap.sentence(sentence)
  classifier<-svm(as.compressed.matrix(tfmatrix), as.factor(label),
                  cost=cost, gamma=gamma, kernel=kernel)
  list(origMatrix=tfmatrix, classifier=classifier)
}

maxent.model<-function(sentence, label) {
  tfmatrix<-wrap.sentence(sentence)
  classifier<-maxent(as.matrix(tfmatrix), as.factor(label))
  list(origMatrix=tfmatrix, classifier=classifier)
}

svm.predict<-function(sentence, svm.obj, decision.values=TRUE, ...) {
  predict(svm.obj$classifier, 
          wrap.sentence(sentence, originalMatrix=svm.obj$origMatrix), 
          decision.values=decision.values, ...)
}

maxent.predict<-function(sentence, maxent.obj) {
  predict(maxent.obj$classifier,
          wrap.sentence(sentence, originalMatrix=maxent.obj$origMatrix),
          ...)
}
