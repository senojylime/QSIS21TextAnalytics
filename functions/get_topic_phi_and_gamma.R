get_topic_phi_and_gamma <- function(model) {
  
  phi <- as.data.frame(model[["phi"]]) %>%
  add_rownames(var = "topic") %>%
  pivot_longer(!topic,names_to = "term", values_to = "probability") %>%
  group_by(topic) %>%
  slice_max(order_by = probability, n = 10) %>%
  ungroup() %>%
  mutate(term = str_replace(term,"_"," "),
         type = "phi",
         size = 1)

  gamma <- as.data.frame(model[["gamma"]]) %>%
    add_rownames(var = "topic") %>%
    pivot_longer(!topic,names_to = "term", values_to = "probability") %>%
    group_by(topic) %>%
    slice_max(order_by = probability, n = 5) %>%
    ungroup() %>%
    mutate(term = str_replace(term,"_"," "),
           type = "gamma",
           size = 1)
  
  topic_terms <- rbind(phi,gamma)
}