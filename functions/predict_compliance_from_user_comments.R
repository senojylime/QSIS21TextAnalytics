#Can I use the user and risk comments  to predict team compliance with indicators
#and therefore identify teams who say they are compliant when they are not?

# adapted from Julia Silge blog https://juliasilge.com/blog/tidy-text-classification/


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
qsis_self_declarations <- read.csv("C:/Users/EJones1/Data Science Accelerator/qsis_self_declarations.csv")

#create dataset with distinct risk comments, compliance field and document id
usercomments <- qsis_self_declarations %>%
  distinct(service_name,team_id,publish_on,indicator_name,user_comments,value) %>%
  na.omit() %>%
  mutate(document = row_number(),
         compliance = ifelse(value == 1, "compliant","not compliant"))

library(rsample)

#create training and testing datasets
usercomments_split <- usercomments %>%
  select(document) %>%
  initial_split()
train_data <- training(usercomments_split)
test_data <- testing(usercomments_split)

#tidy and tokenise
tidy_usercomments <- usercomments %>%
  unnest_tokens(word, user_comments) %>%
  anti_join(stop_words, by = "word") %>%
  group_by(word) %>%
  filter(n() > 3) %>% #keep comments which have word count higher than 6
  ungroup()

#transform from tidy to sparse
sparse_words <- tidy_usercomments %>%
  count(document, word) %>%
  inner_join(train_data) %>%
  cast_sparse(document, word, n)
dim(sparse_words)

#We also need to build a dataframe with a response variable to associate each of the rownames() 
#of the sparse matrix with a compliance, to use as the quantity we will predict in the model.
word_rownames <- as.integer(rownames(sparse_words))

usercomments_joined <- tibble(document = word_rownames) %>%
  left_join(usercomments %>%
              select(document, compliance))

#Now it's time to train our classification model! Let's use the glmnet package to fit a logistic 
#regression model with LASSO regularization. It's a great fit for text classification because the variable 
#selection that LASSO regularization performs can tell you which words are important for your prediction problem. 
#The glmnet package also supports parallel processing with very little hassle, so we can train on multiple cores 
#with cross-validation on the training set using cv.glmnet().

library(glmnet)
#library(doMC) #won't work on my version of R, dunno what it's for anyway
#registerDoMC(cores = 8)

is_compliant <- usercomments_joined$compliance == "compliant"
model <- cv.glmnet(sparse_words, is_compliant,
                   family = "binomial",
                   parallel = TRUE, keep = TRUE
)
plot(model)
plot(model$glmnet.fit)

#TESTS TO DO ON MODEL
#Are the deviance residuals more or less centred on 0 and evenly balanced?

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

#create a confusion matrix
library(yardstick)

intercept <- coefs %>%
  filter(term == "(Intercept)") %>%
  pull(estimate)

classifications <- tidy_usercomments %>%
  inner_join(test_data) %>%
  inner_join(coefs, by = c("word" = "term")) %>%
  group_by(document) %>%
  summarize(score = sum(estimate)) %>%
  mutate(probability = plogis(intercept + score))

comment_classes <- classifications %>%
  left_join(usercomments %>%
              select(compliance, document), by = "document") %>%
  mutate(compliance = as.factor(compliance),
         prediction = case_when(
           probability > 0.6 ~ "compliant",
           TRUE ~ "not compliant"))

comment_classes %>%
  mutate(
    prediction = case_when(
      probability > 0.6 ~ "compliant",
      TRUE ~ "not compliant"
    ),
    prediction = as.factor(prediction)
  ) %>%
  conf_mat(compliance, prediction)

#I want to look more closely at the ones who say they are fully compliant but the model predicts they won't be
potential_non_compliance <- comment_classes %>%
  filter(compliance == "compliant" & prediction == "not compliant") %>%
  left_join(usercomments %>%
              distinct(team_id,publish_on,service_name,indicator_name,user_comments,document),
            by = "document")

