tidy_the_data <- function(qsis_self_declarations) {

    #tokenise and filter data
    sd_tidy <- qsis_self_declarations %>%
      distinct(service_name,team_id,publish_on,pr_risk_comments) %>%
      na.omit()  %>%
      mutate(doc = paste(team_id,publish_on),
             word_count = sapply(strsplit(pr_risk_comments, " "), length),
             pr_risk_comments = paste(service_name,pr_risk_comments),
             doc = paste(team_id,publish_on)) %>%
      unnest_tokens(word, pr_risk_comments, "words", drop = FALSE) %>%
      anti_join(stop_words) %>%
      filter(word_count > 3 & !(word %in% c("quoracy","capacity","recruitment","theatre","palliative","locum","staff","transport","data","staffing",
                                            "attendance","itc","pathology","nurse","validation","lack"))) %>%
      filter(str_detect(pr_risk_comments, "No patient related risk|No risk*|no risk*|no known patient risk") == FALSE) %>%
      distinct()
      
    #remove domain specific stop words
    sd_tidy <- sd_tidy %>%
      anti_join(sd_tidy %>%
                  count(word) %>%
                  slice_max(order_by = n, n = 10) %>%
                  filter(word != "mdt"), by = "word")
}
