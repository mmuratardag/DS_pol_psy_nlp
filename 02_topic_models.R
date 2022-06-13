
load("data/PolPsyArticles.RData")

library(tidyverse)
library(data.table)
library(udpipe)

corp <- df %>% select(doc_id, text)
udp_anno <- udpipe(corp, "english", trace = 10) %>% as.data.table
tm_df <- subset(udp_anno, upos %in% c("NOUN", "PROPN", "ADJ", "VERB"))
tm_df$topic_level_id <- unique_identifier(tm_df,
                                          fields = c("doc_id",
                                                     "paragraph_id",
                                                     "sentence_id"))
dtf <- document_term_frequencies(tm_df,
                                 document = "topic_level_id",
                                 term = "lemma")
dtm <- document_term_matrix(x = dtf)
dtm <- dtm_remove_terms(dtm, terms = c(dtm@Dimnames[[2]][1:101]))

library(ldatuning)

tm_s_rsl <- FindTopicsNumber(dtm, topics = seq(from = 4, to = 25, by = 1),
                             metrics = c("Griffiths2004", "CaoJuan2009",
                                         "Arun2010", "Deveaud2014"),
                             method = "Gibbs",
                             control = list(seed = 666),
                             mc.cores = 7L, verbose = TRUE)
FindTopicsNumber_plot(tm_s_rsl)

library(topicmodels)
LDA_tm_op <- LDA(dtm, k = 13, method = "Gibbs", 
                 control = list(nstart = 5,
                                burnin = 2000,
                                best = TRUE, seed = 666:670))

library(tidytext)
LDA_tidy <- tidy(LDA_tm_op)
LDA_tm_terms <- LDA_tidy %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

LDA_tm_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() + theme_bw()

tm_df_mwe <- subset(udp_anno, upos %in% c("NOUN", "PROPN", "ADJ"))
tm_df_mwe$phrase_tag <- as_phrasemachine(tm_df_mwe$upos, type = "upos")
tm_df_mwe$topic_level_id <- unique_identifier(tm_df_mwe,
                                              fields = c("doc_id",
                                                         "paragraph_id",
                                                         "sentence_id"))

kw_rake <- keywords_rake(udp_anno, 
                         term = "token", group = c("doc_id", "paragraph_id", "sentence_id"), 
                         relevant = udp_anno$upos %in% c("NOUN", "PROPN", "ADJ"), 
                         ngram_max = 3, n_min = 3)

tm_df_mwe$term <- tm_df_mwe$token
tm_df_mwe$term <- txt_recode_ngram(tm_df_mwe$term, 
                                   compound = kw_rake$keyword,
                                   ngram = kw_rake$ngram)

tm_df_mwe$term <- ifelse(tm_df_mwe$upos %in% "NOUN", tm_df_mwe$term,
                         ifelse(tm_df_mwe$term %in% c(kw_rake$keyword,
                                                      kw_rake$keyword),
                               tm_df_mwe$term, NA))


dtm <- document_term_frequencies(tm_df_mwe, document = "topic_level_id",
                                 term = "term")
dtm <- document_term_matrix(x = dtm)
dtm <- dtm_remove_terms(dtm, terms = c(dtm@Dimnames[[2]][1:51]))

tm_mwe_rsl <- FindTopicsNumber(dtm, topics = seq(from = 4, to = 25, by = 1),
                               metrics = c("Griffiths2004", "CaoJuan2009",
                                           "Arun2010", "Deveaud2014"),
                               method = "Gibbs",
                               control = list(seed = 666),
                               mc.cores = 7L, verbose = TRUE)
FindTopicsNumber_plot(tm_mwe_rsl)

library(quanteda)
quanteda_dfm <- tidy(dtm) %>% cast_dfm(row, column, value)

library(keyATM)
keyATM_docs <- keyATM_read(texts = quanteda_dfm)
summary(keyATM_docs)

set.seed(666)
docs_withSplit <- keyATM_read(texts = quanteda_dfm,
                              split = 0.3)

out <- weightedLDA(docs = docs_withSplit$W_split,
                   number_of_topics = 10,
                   model = "base",
                   options = list(seed = 666))

top_words(out, n = 20, measure = "lift")

keywords <- list(genial = c("political", "social", "psychology", "issue",
                            "paper", "article", "Research", "Previous research",
                            "Reviewers", "analysis", "question", "Effects",
                            "empirical", "political psychology", "work",
                            "question*", "studies", "research", "model"),
                 personality = c("personality", "Personality",
                                 "individual differenc*", "individual*"),
                 leadership = c("operational codes", "integrative complexity",
                                "leadership", "political interviews",
                                "leader"), 
                 dp_sdo_rwa = c("SDO", "Social dominance orientation",
                                "Wing Authoritarianism", "authoritarianism",
                                "Social Dominance Orientation",
                                "social dominance orientation", "RWA"),
                 intergroup = c("Identity", "intergroup", "group*", "identit*",
                                "intergroup conflict", "prejudice",
                                "social identit*", "national identity",
                                "majority", "minority", 
                                "conflict"),
                 prejudice = c("race relations", "Islamophobia", "immigrants",
                               "refugees", "ethnic", "racial prejudice",
                               "Racism"),
                 pol_ide = c("political ideology", "right", "left",
                             "conservatives", "welfare",
                             "populist radical right", "Polarization"),
                 voting_beh = c("vot*", "candidate", "party", "partisanship",
                                "candidates", "political preferences"),
                 gender = c("gender", "female candidates"),
                 att_beh = c("political behavior", "political attitudes"),
                 political_soc = c("political socialization", "social capital"),
                 emo_sent = c("emotions", "Emotions", "Shame", "sadness", "emotional reactions",
                              "sentiments"), 
                 val_mor = c("cultural", "justice", "social", "Representations",
                             "moral", "foundations", "Social representations"), 
                 rational_cho = c("prospect theory"),
                 collective_act = c("collective action", "social change",
                                    "civic engagement", "Change",
                                    "social movements", "Activism"),
                 political_comm = c("social", "influence", "persuasion"),
                 pol_participation = c("participation", "democr*",
                                       "political participation"),
                 reconciliation = c("reconciliation", "victims"),
                 conflict_res = c("intractable conflict", "reconciliation"),
                 cnt_lct_cs = c("Barack Obama", "US", "American", "Europe",
                                "EU", "European Union", "Netherlands", "Turkey",
                                "Northern Ireland", "Chile", "Rwanda"),
                 conspiracy_the = c("belief*", "conspiracy theories",
                                    "conspiracy beliefs"),
                 sec_ter_ir = c("security", "terror", "Terror", "war", "threat",
                                "terrorist attacks", "foreign policy"),
                 covid = c("covid", "COVID", "pandemic"))

visualize_keywords(docs = keyATM_docs, keywords = keywords)

ss_out <- keyATM(docs = keyATM_docs, no_keyword_topics = 5,
                 keywords = keywords, model = "base",
                 options = list(seed = 666))

top_words(ss_out, n = 20)

tidy(ss_out)

plot_modelfit(ss_out)
plot_alpha(ss_out)
pp <- plot_pi(ss_out)
pp$values %>% ggplot(aes(x = Topic, y = Probability)) +
  geom_col() + 
  labs(title = "Probability of words drawn from keyword topic-word distribution") +
  coord_flip() + theme_bw()

pp$values %>% ggplot(aes(x = Topic, y = count)) +
  geom_col() + 
  labs(title = "Topic Count",
       caption = "3543 n-grams are extracted from 1382 truncated abstracts") +
  coord_flip() + theme_bw()
