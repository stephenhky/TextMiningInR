library(RTextTools)
library(e1071)
library(tm)
library(maxent)

getMinor<-function(soc) {
  getMinorSingle<-function(soc) {
    if (substr(soc, 1, 5) %in% c('15-11', '51-51')) {
      paste(substr(soc, 1, 5), '00', sep='')
    } else {
      paste(substr(soc, 1, 4), '000', sep='')
    }
  }
  as.vector(mapply(getMinorSingle, soc))
}

wrap.sentence<-function(sentence, originalMatrix=NULL) {
  create_matrix(textColumns = sentence, language = 'english', 
                removeNumbers = TRUE, toLower = TRUE, removeStopwords = 'TRUE', stemWords = TRUE,
                weighting=tm::weightTf, originalMatrix=originalMatrix)
}

#tasks.data<-read.csv('onet_tasks_rows.csv', header=TRUE, stringsAsFactors=FALSE)
#tfmatrix<-wrap.sentence(tasks.data$Task)
tasks.data<-read.csv('onet_tasks_preprocessed.csv', header=TRUE, stringsAsFactors=FALSE)
tfmatrix<-wrap.sentence(tasks.data$preprocessed.task)
nb.classifier<-naiveBayes(as.matrix(tfmatrix), as.factor(tasks.data$SOC3))
me.classifier<-maxent(as.matrix(tfmatrix), as.factor(tasks.data$SOC3))
svm.classifier<-svm(as.compressed.matrix(tfmatrix), as.factor(tasks.data$SOC3), cost=100, gamma=1, kernel='linear')

PersonID.Tasks<-data.frame(PersonID=usrenal.tasks.features$PersonID,
                           Task=paste(usrenal.tasks.features$Task1, usrenal.tasks.features$Task2, 
                                      usrenal.tasks.features$Task3, usrenal.tasks.features$Task4, 
                                      usrenal.tasks.features$Task5, sep=' '))
PersonID.Tasks<-unique(PersonID.Tasks)

# MaxEnt
int.res<-predict(me.classifier, wrap.sentence(PersonID.Tasks$Task, originalMatrix=tfmatrix))
lscore<-mapply(function(id, soc3) {
  intresid<-which(PersonID.Tasks$PersonID==id)
  ifelse(soc3 %in% attr(int.res[intresid,], 'names'), as.numeric(int.res[intresid, soc3]), 0)
  }, usrenal.tasks.maxent$PersonID, getMinor(usrenal.tasks.maxent$SOC))
maxent.selected<-mapply(function(id, soc3) {
  intresid<-which(PersonID.Tasks$PersonID==id)
  ifelse(soc3==as.character(int.res[[intresid]]), 1, 0)
}, usrenal.tasks.features$PersonID, getMinor(usrenal.tasks.features$SOC))

# SVM
# assign 1 to the identified class, 0 to the unselected classes
int.res<-predict(svm.classifier, wrap.sentence(PersonID.Tasks$Task, originalMatrix=tfmatrix), decision.values=TRUE)
svm.selected<-mapply(function(id, soc3) {
  intresid<-which(PersonID.Tasks$PersonID==id)
  ifelse(soc3==as.character(int.res[[intresid]]), 1, 0)
}, usrenal.tasks.features$PersonID, getMinor(usrenal.tasks.features$SOC))
