
get_toppiccy_words <- function(doc_vector, model, top_n){
  
  pred <- predict(model, doc_vector, method = "dot")
  
  top3classes = order(pred,decreasing=TRUE)[0:top_n]
  
  top3classes_dis = model$phi[top3classes,]
  
  #R doesnt broadcast word vector into a matrix when you multply vector
  # by a matrix??. So here i stack the word vector 3 times (once per class)
  word_vectors = rbind(test_word_vector, test_word_vector,test_word_vector)
  
  #Elementwise multiplication of both matrices (theyre now the same shape)
  highest_prob_words = top3classes_dis * word_vectors 
  
  sort_tokens <- function(word_dis){
    colnames(highest_prob_words)[order(word_dis, decreasing=TRUE)[0:5]]
  }
  
  apply(highest_prob_words, 1, sort_tokens)
}