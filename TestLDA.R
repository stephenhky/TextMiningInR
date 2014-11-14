library(RTextTools)
library(topicmodels)
library(tm)

documents<-c('China Chinese Beijing computer',
             'topology cosmology immununology physics materials science',
             'topology biophysics condensation fermion electrons cosmology',
             'Calvinism trinity eschatology science apostles',
             'China solid accelerator electrons particles',
             'electrons Calvinism China apostles trinity',
             'topology condensation electrons immunology physics')
documents<-c('China Chinese Beijing computer',
             'physics science condensation cosmology',
             'China China Beijing China',
             'physics biophysics condensation China',
             'topology physics condensation cosmology Beijing',
             'Einstein condensation teaching')
documents<-c('apple orange apple',
             'computer threading multiprocessing modeling',
             'apple pineapple apple orange',
             'calculator computer threading',
             'computer compute computation')


wrap.sentence<-function(sentence, originalMatrix=NULL) {
  create_matrix(textColumns = sentence, language = 'english', 
                removeNumbers = TRUE, toLower = TRUE, removeStopwords = 'TRUE', stemWords = TRUE,
                weighting=tm::weightTf, originalMatrix=originalMatrix)
}

doctfmat<-wrap.sentence(documents)

ldamodel<-LDA(doctfmat, 3)
posterior(ldamodel)$topics

