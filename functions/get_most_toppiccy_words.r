
get_topiccy_words <- function(doc_vector, model, top_n){
  
  #step 1: get topic predictions for that word vector
  pred <- predict(model, doc_vector, method = "dot")
  
  #step 2: get the indexes of the top n classes
  topnclasses = order(pred,decreasing=TRUE)[0:top_n]
  
  #step 3: get the word probabilities for each of those classes
  topnclasses_dis = model$phi[topnclasses,]
  
  #R doesn't broadcast word vector into a matrix when you multiply vector
  # by a matrix??. So here i stack the word vector top_n times
  word_vectors = t(replicate(top_n,doc_vector))
  
  #Elementwise multiplication of both matrices (they're now the same shape)
  highest_prob_words = topnclasses_dis * word_vectors 
  
  #step 4: multiply word vector (ie word counts) by probability vectors
  sort_tokens <- function(word_dis){
    colnames(highest_prob_words)[order(word_dis, decreasing=TRUE)[0:5]]
  }
  
  apply(highest_prob_words, 1, sort_tokens)
}