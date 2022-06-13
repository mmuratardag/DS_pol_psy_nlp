load("data/PolPsyArticles.RData")
load("data/PolPsyArticles_q_corp.RData")
library(quanteda)

df$Title %>% tokens(remove_punct = TRUE,
                    remove_symbols = TRUE,
                    remove_numbers = TRUE) %>%
  tokens_select(pattern = stopwords("en"), selection = 'remove') %>%
  dfm() %>% topfeatures(100)

exp_dfm <- PolPsyArt_q_corp %>% tokens(remove_punct = TRUE,
                                       remove_symbols = TRUE,
                                       remove_numbers = TRUE) %>%
  tokens_select(pattern = stopwords("en"), selection = 'remove') %>%
  dfm()
exp_dfm_tstat_dist <- as.dist(quanteda.textstats::textstat_dist(exp_dfm))

mds_fit <- cmdscale(exp_dfm_tstat_dist, eig = TRUE, k = 2)
mds_points <- data.frame(x = mds_fit$points[, 1], y = mds_fit$points[, 2])

library(tidyverse)
ggplot(mds_points, aes(x = x, y = y)) +
  geom_point(data = mds_points, aes(x = x, y = y, color = df$Source)) +
  geom_text(data = mds_points, aes(x = x, y = y - 0.2,
                               label = df$doc_id)) +
  labs(x = "Dimension 1", y = "Dimension 2",
       title = "MDS Plot",
       subtitle = "Possible # of topics judging by the titles of the articles",
       caption = "Heuristic expectation: 10 - 14 topics") +
  theme_bw() + theme(legend.position = "none")

gdata::keep(df, PolPsyArt_q_corp, sure = TRUE)

btm_corp <- df %>% select(doc_id, text = Title)

library(udpipe)
udp_anno <- udpipe(btm_corp, "english", trace = 10)
library(data.table)
bi_terms <- as.data.frame(udp_anno)

txt_stats <- txt_freq(udp_anno$upos)
txt_stats$key <- factor(txt_stats$key, levels = rev(txt_stats$key))
txt_stats %>% ggplot(aes(x = key, y = freq)) +
  geom_col() + 
  labs(x  = "", y = "Frequency",
       title = "Text Statistics",
       subtitle = "Universal Parts of Speech",
       caption = "In many languages nouns, adjectives & verbs are most useful
       \nfor understanding semantic relationships") +
  coord_flip() + theme_bw()

library(BTM)
traindata <- subset(udp_anno, upos %in% c("NOUN", "PROPN", "ADJ", "VERB") &
                      !lemma %in% stopwords("en") & nchar(lemma) > 2)

bi_terms_dt <- as.data.table(bi_terms)
bi_terms_btm <- bi_terms_dt[, cooccurrence(x = lemma,
                                    relevant = upos %in% c("NOUN", "PROPN",
                                                          "ADJ", "VERB") & 
                                    nchar(lemma) > 2 & !lemma %in% stopwords("en"),
                                  skipgram = 3),
                   by = list(doc_id)]
traindata <- traindata[, c("doc_id", "lemma")]

library(textplot)
library(ggraph)

fit_BTM_get_plot <- function(num_top, sub_title) {
  set.seed(666)
  btm_model <- BTM(traindata, biterms = bi_terms_btm, k = num_top,
                   iter = 2000, background = TRUE, trace = 100)
  set.seed(666)
  plot(btm_model, top_n = num_top,
       title = "BTM model", subtitle = sub_title)
}

fit_BTM_get_plot(10, "10 Topics")
fit_BTM_get_plot(11, "11 Topics")
fit_BTM_get_plot(12, "12 Topics")
fit_BTM_get_plot(13, "13 Topics")
fit_BTM_get_plot(14, "14 Topics")

