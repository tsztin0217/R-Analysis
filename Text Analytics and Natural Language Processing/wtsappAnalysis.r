library(textreadr)
library(tm)
library(dplyr)
library(magrittr)
library(stringi)
library(tidyverse)
library(tidytext)

setwd("/Users/tsztinviviansoo/Desktop/Text analytics/whatsapp")
nm <- list.files(path="/Users/tsztinviviansoo/Desktop/Text analytics/whatsapp")
my_data <- read_document(file=nm[1])
my_data_together <- paste(my_data, collapse = " ")

View(my_data_together)
my_txt_text <- do.call(rbind, lapply(nm, function(x) paste(read_document(file=x), collapse = " ")))
my_txt_df <- as.data.frame(my_txt_text)
colnames(my_txt_df)[1] <- "text"

#################
### txt_token ###
#################
txt_token <- my_txt_df %>%
  mutate(my_txt_df, text = gsub(x = text, pattern = "[0-9]", replacement = "")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% c(stop_words$word, "pm","time","free","attached",
                      "message","document","class","hult", "image",
                      "omitted", "deleted", "sticker", "age_standard",
                       "invite","joined","it’s","i’m","link","group's","https",
                       "library","tidytuesday","netflix","checking","data",
                       "binary","checking_norm","duration_norm","age_norm",
                       "germ_train","good_bad","my_logit","my_logit_norm",
                       "unnest_tokens","geom_jitter","duration",
                       "german_credit_card","anti_join","ggplot")) %>%
  #removing more noise in addition to stop words
  count(word, sort=TRUE)
#filter is the alternative to anti_join according to class
View(txt_token)

txt_token%>%
  filter(word=="happy")
# the frequency of word "happy" is 8
txt_token%>%
  filter(word=="love")

##############
### BIGRAM ###
##############

txt_bigrams <- my_txt_df %>%
  mutate(my_txt_df, text = gsub(x = text, pattern = "[0-9]", replacement = "")) %>%
  #ommitted numbers because random phone numbers popped up too frequently
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  count(bigram, sort = TRUE)
View(txt_bigrams)


library(tidyr)

bigrams_separated <- txt_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% c(stop_words$word, "pm","time","free","attached",
                       "message","document","class","hult", "image",
                       "omitted", "deleted", "sticker", "age_standard",
                       "invite","joined","it’s","i’m","link","group's","https",
                       "library","tidytuesday","netflix","checking","data",
                       "binary","checking_norm","duration_norm","age_norm",
                       "germ_train","good_bad","my_logit","my_logit_norm",
                       "unnest_tokens","geom_jitter","duration",
                       "german_credit_card","anti_join","ggplot")) %>% 
  filter(!word2 %in% c(stop_words$word, "pm","time","free","attached",
                       "message","document","class","hult", "image",
                       "omitted", "deleted", "sticker", "age_standard",
                       "invite","joined","it’s","i’m","link","group's","https",
                       "library","tidytuesday","netflix","checking","data",
                       "binary","checking_norm","duration_norm","age_norm",
                       "germ_train","good_bad","my_logit","my_logit_norm",
                       "unnest_tokens","geom_jitter","duration",
                       "german_credit_card","anti_join","ggplot"))



bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)

View(bigram_counts)


##################
### QUADROGRAM ###
##################
quadrogram <- my_txt_df %>%
  mutate(my_txt_df, text = gsub(x = text, pattern = "[0-9]", replacement = "")) %>%
  unnest_tokens(quadrogram, text, token = "ngrams", n=4) %>%
  separate(quadrogram, c("word1", "word2", "word3", "word4"), sep=" ") %>%
  filter(!word1 %in% c(stop_words$word, "pm","time","free","attached",
                      "message","document","class","hult", "image",
                      "omitted", "deleted", "sticker", "age_standard",
                      "invite","joined","it’s","i’m","link","group's","https",
                      "library","tidytuesday","netflix","checking","data",
                      "binary","checking_norm","duration_norm","age_norm",
                      "germ_train","good_bad","my_logit","my_logit_norm",
                      "unnest_tokens","geom_jitter","duration",
                      "german_credit_card","anti_join","ggplot")) %>%
  filter(!word2 %in% c(stop_words$word, "pm","time","free","attached",
                       "message","document","class","hult", "image",
                       "omitted", "deleted", "sticker", "age_standard",
                       "invite","joined","it’s","i’m","link","group's","https",
                       "library","tidytuesday","netflix","checking","data",
                       "binary","checking_norm","duration_norm","age_norm",
                       "germ_train","good_bad","my_logit","my_logit_norm",
                       "unnest_tokens","geom_jitter","duration",
                       "german_credit_card","anti_join","ggplot")) %>%
  filter(!word3 %in% c(stop_words$word, "pm","time","free","attached",
                       "message","document","class","hult", "image",
                       "omitted", "deleted", "sticker", "age_standard",
                       "invite","joined","it’s","i’m","link","group's","https",
                       "library","tidytuesday","netflix","checking","data",
                       "binary","checking_norm","duration_norm","age_norm",
                       "germ_train","good_bad","my_logit","my_logit_norm",
                       "unnest_tokens","geom_jitter","duration",
                       "german_credit_card","anti_join","ggplot")) %>%
  filter(!word4 %in% c(stop_words$word, "pm","time","free","attached",
                       "message","document","class","hult", "image",
                       "omitted", "deleted", "sticker", "age_standard",
                       "invite","joined","it’s","i’m","link","group's","https",
                       "library","tidytuesday","netflix","checking","data",
                       "binary","checking_norm","duration_norm","age_norm",
                       "germ_train","good_bad","my_logit","my_logit_norm",
                       "unnest_tokens","geom_jitter","duration",
                       "german_credit_card","anti_join","ggplot"))
quadrogram

quadrogram_united <- quadrogram %>%
  unite(quadrogram, word1, word2, word3, word4, sep=" ")
View(quadrogram_united)
quadrogram_tf_idf <- quadrogram_united %>%
  count(quadrogram) %>%
  bind_tf_idf(quadrogram, quadrogram, n) %>%
  arrange(desc(n))#here i changed desc(tf_idf) to desc(n) since for some reason,
#all the idf and tf_idf were the same....


quad_counts <- quadrogram %>%
  count(word1, word2, word3, word4, sort = TRUE)

View(quad_counts)
##################

word_cors %>%
  filter(item1 == "love")
##################
##################################
### BIGRAM AND QUADGRAM GRAPHS ###
##################################
library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n>3) %>%
  graph_from_data_frame()

bigram_graph


library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

quad_graph <- quad_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()


ggraph(quad_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

quad_graph2 <- quad_counts %>%
  filter(n==1) %>%
  graph_from_data_frame()

ggraph(quad_graph2, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)
###################
### CORRELATION ###
###################
word_cors <- txt_token %>%
  group_by(word) %>%
  filter(n>=30) %>%
  pairwise_cor(word,txt_token, sort=TRUE)
word_cors
View(word_cors)

###
my_txt_dtm <- my_txt_df %>%
  mutate(my_txt_df, text = gsub(x = text, pattern = "[0-9]", replacement = "")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% c(stop_words$word, "pm","time","free","attached",
                      "message","document","class","hult", "image",
                      "omitted", "deleted", "sticker", "age_standard",
                      "invite","joined","it’s","i’m","link","group's","https",
                      "library","tidytuesday","netflix","checking","data",
                      "binary","checking_norm","duration_norm","age_norm",
                      "germ_train","good_bad","my_logit","my_logit_norm",
                      "unnest_tokens","geom_jitter","duration",
                      "german_credit_card","anti_join","ggplot")) %>%
  count(word, word) %>%
  cast_dtm(word, word, n)

my_txt_dtm

###########
### LDA ###
###########
#now that we have a dtm, we can use LDA to find topics
library(topicmodels)
txt_lda <- LDA(my_txt_dtm, k=2, control=list(seed=123))


library(tidytext)
txt_topics <- tidy(txt_lda, matrix="beta")
txt_topics

top_terms <- txt_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
View(top_terms)

top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()
#topic on the left seems more casual whereas topic on the right more serious

beta_spread <- txt_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>0.001 | topic2 >0.001) %>%
  mutate(log_rate = log2(topic2/topic1))

View(beta_spread)
#########GAMMA
chapters_gamma <- tidy(txt_lda, matrix="gamma") #we created ap_lda in our LDA scripts 
View(chapters_gamma)
str(chapters_gamma)
gamma_df<- as.data.frame(chapters_gamma) %>%
  arrange(desc(gamma))
        
write.csv(gamma_df, file = '/Users/tsztinviviansoo/Desktop/Text analytics/gammadf.csv', row.names = FALSE)
#I exported this table to a csv file
########


mutate(my_txt_df, text = gsub(x = text, pattern = "[0-9]", replacement = "")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% c(stop_words$word, "pm","time","free","attached",
                      "message","document","class","hult", "image",
                      "omitted", "deleted", "sticker", "age_standard",
                      "invite","joined","it’s","i’m","link","group's","https",
                      "library","tidytuesday","netflix","checking","data",
                      "binary","checking_norm","duration_norm","age_norm",
                      "germ_train","good_bad","my_logit","my_logit_norm",
                      "unnest_tokens","geom_jitter","duration",
                      "german_credit_card","anti_join","ggplot")) %>% #removing pm in addition to stop words
  count(word, sort=TRUE)

############

my_txt_df %>%
  mutate(my_txt_df, text = gsub(x = text, pattern = "[0-9]", replacement = "")) %>%
  unnest_tokens(word, text) %>%
#################
### Frequency ###
#################
library(ggplot2)
freq_hist <- my_txt_df %>%
  mutate(my_txt_df, text = gsub(x = text, pattern = "[0-9]", replacement = "")) %>%
  unnest_tokens(word,text) %>%
  filter(!word %in% c(stop_words$word, "pm","time","free","attached",
                      "message","document","class","hult", "image",
                      "omitted", "deleted", "sticker", "age_standard",
                      "invite","joined","it’s","i’m","link","group's","https",
                      "library","tidytuesday","netflix","checking","data",
                      "binary","checking_norm","duration_norm","age_norm",
                      "germ_train","good_bad","my_logit","my_logit_norm",
                      "unnest_tokens","geom_jitter","duration",
                      "german_credit_card","anti_join","ggplot")) %>%
  count(word, sort=TRUE) %>%
  filter(n>15) %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()
print(freq_hist)

#total words
total_words <- txt_token %>%
  summarize(total=sum(n))

txt_words <- c(txt_token, total_words)
txt_words <- as.data.frame(txt_words)

freq_by_rank <- txt_words %>%
  mutate(rank = row_number(),
         `term frequency`=n/total) %>%

View(freq_by_rank)


freq_by_rank %>%
  ggplot(aes(rank, `term frequency`))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()


txt_tfidf_words <- txt_words %>%
  bind_tf_idf(word, word, n) %>%
  arrange(desc(tf_idf))


txt_tfidf_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~word, ncol=2, scales="free")+
  coord_flip()










word_cors <- my_txt_df %>%
  mutate(my_txt_df, text = gsub(x = text, pattern = "[0-9]", replacement = "")) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% c(stop_words$word, "pm")) %>%
  group_by(word) %>%
  filter(n() >= 8) %>%
  pairwise_cor(word, word)

word_cors %>%
  filter(item1 == "love")




word_cors %>%
  filter(correlation <1) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha=correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()


###sentiments
library(textdata)
library(tidytext)
afinn <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")
bing <- get_sentiments("bing")
View(sentiments)
sentiments <- bind_rows(mutate(afinn, lexicon="afinn"),
                        mutate(nrc, lexicon= "nrc"),
                        mutate(bing, lexicon="bing")
)



nrcsurprise <- get_sentiments("nrc") %>%
  filter(sentiment == "surprise")
nrcanger <- get_sentiments("nrc") %>%
  filter(sentiment == "anger")
nrcdisgust <- get_sentiments("nrc") %>%
  filter(sentiment =="disgust")
nrcfear <- get_sentiments("nrc") %>%
  filter(sentiment =="fear")
nrctrust <- get_sentiments("nrc") %>%
  filter(sentiment =="trust")


txt_token %>%
  inner_join(nrctrust) %>%
  count(word, sort=T)

txt_token %>%
  inner_join(nrcsurprise) %>%
  count(word, sort=T)

txt_token %>%
  inner_join(nrcanger) %>%
  count(word, sort=T)

txt_token %>%
  inner_join(nrcdisgust) %>%
  count(word, sort=T)

txt_token %>%
  inner_join(nrcfear) %>%
  count(word, sort=T)



afinn <- txt_token %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  txt_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  txt_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bing_and_nrc

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

bing_counts <- txt_token %>%
  inner_join(get_sentiments("bing")) %>%
  top_n(10) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts
summary(bing_counts)




bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()
