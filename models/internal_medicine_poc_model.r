library(tidyverse)
library(stringr)
library(tidytext)
library(quanteda)
library(topicmodels)
library(textmineR)
library(lubridate)
library(ngram)
library(maps)
library(mgcv)


source("functions/get_most_toppiccy_words.R")
source("functions/tidy_the_data.R")


#Pipeline for creating a nice scatter plot with topics per year

# Zach: I have to set the encoding to latin to avoid weird characters!
risk_df <- read.csv("data/qsis_self_declarations.csv", encoding="latin1")

#Fix the dates
risk_df$publish_on = dmy(risk_df$publish_on)

poc = 'Internal Medicine'

# Let's just look at cancer
risk_df = risk_df %>% filter(poc==poc)

# Create doc ids
risk_df <- risk_df %>%
  distinct(service_name,team_id,publish_on,pr_risk_comments) %>%
  na.omit()  %>%
  mutate(doc_id = paste(team_id,publish_on))


# Lots of these comments that arent useful rightnow
risk_df =
  risk_df %>% 
  filter(str_detect(pr_risk_comments, 'nil|Nil|none|None|no risks|No risks')==FALSE)


# Remove duplicates
for_dtm <- risk_df %>%
  distinct(team_id,publish_on,pr_risk_comments,doc_id)

#Token counts
tokens = risk_df %>% unnest_tokens(token,pr_risk_comments) %>% 
  group_by(token) %>%
  summarise(n=n())

#rare words i probably dont want
rarewords = tokens %>% filter(n<11) %>% pull(token)

#too common words i also dont want
commonwords = tokens %>% filter(n>1500) %>% pull(token)

#create document term matrix
dtm <- CreateDtm(doc_vec = for_dtm$pr_risk_comments, # character vector of documents
                 doc_names = for_dtm$doc, # document names
                 ngram_window = c(1,2), # minimum and maximum n-gram length
                 stopword_vec = c(stopwords::stopwords("en"), # stopwords from tm
                                  stopwords::stopwords(source = "smart"),
                                  rarewords,
                                  commonwords,
                                  c("patient","patients","trusts","trust",
                                    "risk","risks","register","service","services","nhs","nhse","nhsei",
                                    "oncology","unit","north","south","east","west","cumbria","bristol","identified"),
                                  world.cities$name),
                 lower = TRUE, # lowercase - this is the default value
                 remove_punctuation = TRUE, # punctuation - this is the default
                 remove_numbers = TRUE, # numbers - this is the default
                 verbose = TRUE, # Turn off status bar for this demo
                 cpus = 4) # default is all available cpus on the system

# (Zach: This is filtering out bigrams/tokens with 10 occurrences or less)
dtm <- dtm[,colSums(dtm) > 10] 


# We want to search around the parameter space to find a decent starting point
# Hence the loop where we train multiple models

#we cant really plot any more than ten categories without it looking too busy!
k=10
#lets try various alpha priors
#higher alpha = more topics per doc
#searching from 0.01 to 1 suggests lower (0.06) is a decent guess
alpha=0.06
#higher beta = more words per topic
#searching from 0.01 to 1 the coherence jumps around quite a lot
betas=seq(0.01, 1,0.04)

models = vector('list', length(betas))

for (i in seq(1:length(betas))){
  model <- FitLdaModel(dtm = dtm, 
                       k = k,
                       iterations = 600,
                       burnin = 300,
                       alpha = alpha,
                       beta = betas[[i]],
                       calc_likelihood = TRUE,
                       optimize_alpha = TRUE,
                       calc_coherence = TRUE,
                       calc_r2 = TRUE)
  models[[i]] = model
}

coherence = unlist(lapply(models,function(x) mean(x$coherence)))

model = models[[which.max(coherence)]]
saveRDS(model, paste0('models/trained_models/',today(),poc,k,'model.rds'))

# predict topics contained in each document
prediction <- predict(model, dtm, method = "gibbs",iterations = 200,burnin = 100)
prediction <- as.data.frame(prediction)
prediction$doc_id <- rownames(prediction)
prediction = prediction %>% pivot_longer(-c(doc_id))


df = qsis_self_declarations %>% left_join(prediction, by=c('doc_id'='doc_id')) 

colnames(df) = c('team_id','date_published','risk','service_name','doc_id','topic','p')

df$year = year(df$date_published)

topics_per_year = df %>% 
  select(year, topic,p) %>%
  group_by(year, topic) %>% 
  summarise(p = sum(p), n = n()) %>%
  mutate (p = p/n * 100) 

summary = SummarizeTopics(model)

topics_per_year = topics_per_year %>% left_join(summary,by=c('topic'='topic'))

topics_per_year$topic = paste0(topics_per_year$topic,topics_per_year$label_1)

fig <- plot_ly(
  topics_per_year,
  x = ~year, 
  y = ~p, 
  group_by = ~topic,
  color = ~topic,
  hoverinfo = 'text',
  hovertext = ~top_terms_phi,
  type = 'scatter',
  mode='line'
)

fig = fig %>%  layout(
  xaxis = list(title = 'Year'),
  yaxis = list (title = 'Topic prevalence')
)

fig





