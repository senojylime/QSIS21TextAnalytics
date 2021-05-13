#Can I use the risk comments to predict team compliance and therefore identify teams who say they are compliant when they are not?

library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(topicmodels)
library(textmineR)
library(lubridate)
library(ngram)
library(rsample)

#read in sd data
qsis_self_declarations <- read.csv("data/qsis_self_declarations.csv")

#create dataset with distinct risk comments, compliance field and document id
risk_docs <- qsis_self_declarations %>%
  distinct(service_name,team_id,publish_on,pr_risk_comments,score) %>%
  na.omit() %>%
  mutate(document = row_number(),
         compliance = ifelse(score == 100, "fully compliant","not fully compliant"))

library(rsample)

#create training and testing datasets
risk_split <- risk_docs %>%
  select(document) %>%
  initial_split()
train_data <- training(risk_split)
test_data <- testing(risk_split)

#tidy and tokenise
tidy_risk <- risk_docs %>%
  unnest_tokens(word, pr_risk_comments) %>%
  group_by(word) %>%
  filter(n() > 6) %>% #keep comments which have word count higher than 6
  ungroup()

#transform from tidy to sparse
sparse_words <- tidy_risk %>%
  count(document, word) %>%
  inner_join(train_data) %>%
  cast_sparse(document, word, n)
dim(sparse_words)

#We also need to build a dataframe with a response variable to associate each of the rownames() 
#of the sparse matrix with a compliance, to use as the quantity we will predict in the model.
word_rownames <- as.integer(rownames(sparse_words))

risk_joined <- data_frame(document = word_rownames) %>%
  left_join(risk_docs %>%
              select(document, compliance))

#Now it's time to train our classification model! Let's use the glmnet package to fit a logistic 
#regression model with LASSO regularization. It's a great fit for text classification because the variable 
#selection that LASSO regularization performs can tell you which words are important for your prediction problem. 
#The glmnet package also supports parallel processing with very little hassle, so we can train on multiple cores 
#with cross-validation on the training set using cv.glmnet().

library(glmnet)
#library(doMC)
#registerDoMC(cores = 8)

is_compliant <- risk_joined$compliance == "fully compliant"
model <- cv.glmnet(sparse_words, is_compliant,
                   family = "binomial",
                   parallel = TRUE, keep = TRUE
)
plot(model)
plot(model$glmnet.fit)

#Which coefficents are the largest in size, in each direction?
library(broom)

coefs <- model$glmnet.fit %>%
  tidy() %>%
  filter(lambda == model$lambda.1se)

  coefs %>%
  group_by(estimate > 0) %>%
  top_n(10, abs(estimate)) %>%
  ungroup() %>%
  ggplot(aes(fct_reorder(term, estimate), estimate, fill = estimate > 0)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  labs(
    x = NULL,
    title = "Coefficients that increase/decrease probability the most",
  )

  #What about a confusion matrix? Let's use probability of 0.5 as our cutoff point, for example.
  library(yardstick)
  
  intercept <- coefs %>%
    filter(term == "(Intercept)") %>%
    pull(estimate)
  
  classifications <- tidy_risk %>%
    inner_join(test_data) %>%
    inner_join(coefs, by = c("word" = "term")) %>%
    group_by(document) %>%
    summarize(score = sum(estimate)) %>%
    mutate(probability = plogis(intercept + score))
  
  comment_classes <- classifications %>%
    left_join(risk_docs %>%
                select(compliance, document), by = "document") %>%
    mutate(compliance = as.factor(compliance),
           prediction = case_when(
             probability > 0.5 ~ "fully compliant",
             TRUE ~ "not fully compliant"))
  
  comment_classes %>%
    mutate(
      prediction = case_when(
        probability > 0.5 ~ "fully compliant",
        TRUE ~ "not fully compliant"
      ),
      prediction = as.factor(prediction)
    ) %>%
    conf_mat(compliance, prediction)

#I want to look more closely at the ones who say they are fully compliant but the model predicts they won't be
check <- comment_classes %>%
  filter(compliance == "fully compliant" & prediction == "not fully compliant") %>%
  left_join(risk_docs %>%
              distinct(service_name,pr_risk_comments,document),
            by = "document")

qsis_self_declarations %>%
  filter(str_detect(service_name,"Adult Critical Care") == TRUE) %>%
  distinct(service_name, indicator_name,indicator_theme)
