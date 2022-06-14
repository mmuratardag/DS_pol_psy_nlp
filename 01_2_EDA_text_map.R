
ncores <- parallelly::availableCores()

load("data/PolPsyArticles.RData")

library(tidyverse)
library(data.table)
library(future)
library(udpipe)

corp <- df %>% select(doc_id, text)
udp_anno <- udpipe(corp, "english", trace = 10) %>% as.data.table

library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)

tokens_sel <- udp_anno[upos %in% c("NOUN","PROPN","ADJ","VERB"),
                       c("doc_id","lemma")][,.(text = paste(lemma, collapse=" ")),
                                            by = doc_id] %>% corpus %>%
  tokens(remove_punct = TRUE,
         remove_symbols = TRUE,
         remove_numbers = TRUE) %>%
  tokens_select(pattern = stopwords("en"), selection = 'remove')

dfm_sel <- tokens_sel %>% dfm(tolower = FALSE)
word_freqs <- dfm_sel %>% textstat_frequency %>% as.data.table %>% setorder(-frequency) 

flexionsf <- function(words,d){ 
  tokdist <- stringdist::stringdistmatrix(words,method="jw",nthread=ncores) %>% 
    as.matrix %>% Matrix::forceSymmetric(., uplo="U")
  rownames(tokdist) <- words
  colnames(tokdist) <- words
  tokdist <- tokdist[grepl("^[[:lower:]]", rownames(tokdist)),] 
  cnslc <- tolower(colnames(tokdist))
  nchars <- nchar(colnames(tokdist))
  spldf <- split(tokdist %>% as.matrix %>% as.data.frame, seq(1, nrow(tokdist),
                                                              by = floor(nrow(tokdist)/(ncores-1))))
  plan(multisession)
  temp <- future.apply::future_lapply(spldf, function(x) {
    lapply(rownames(x), function(w) { 
      selector <- unname(
        (x[w,] < d | w == cnslc | paste0(w,"s") == cnslc | paste0(w,"x") == cnslc) & 
          nchar(w) <= nchars
      )
      if ( length(selector[selector]) > 1 ) {
        return(colnames(x)[selector])
      } else {
        return(NULL)
      }
    })
  }) %>% unlist(recursive=FALSE)
  future:::ClusterRegistry("stop")
  temp <- temp[!sapply(temp, is.null)]
  names(temp) <- sapply(temp,function(i) i[1])
  temp <- temp[grepl("^[[:lower:]]", names(temp))] 
  lapply(temp, function(i) i[-1]) 
}

flexions_list <- flexionsf(word_freqs$feature, 0.04)
flexions_dict <- flexions_list %>% dictionary(tolower = FALSE)

tokens_sel_upd <- tokens_sel %>%
  tokens_lookup(flexions_dict, exclusive = FALSE,
                capkeys = FALSE,
                case_insensitive = FALSE)
dfm_sel_upd <- tokens_sel_upd %>% 
  dfm(tolower = FALSE)
word_freqs_upd <- dfm_sel_upd %>% 
  textstat_frequency %>% as.data.table %>% setorder(-frequency)

word_freqs_upd %>% head(75) %>% ggplot() + 
  geom_col(aes(y = frequency,
               x = reorder(feature, frequency),
               fill = docfreq),
           width = .3) + 
  coord_flip() + theme_bw()

fcm_sel <- fcm(tokens_sel_upd,
               context="window",
               count="weighted",
               window = 2) %>% 
  fcm_remove(word_freqs_upd[frequency < 5, feature], case_insensitive = FALSE)
fcm_sel %>% head(50)

fcm_sel_sym <- Matrix::forceSymmetric(fcm_sel, uplo = "U")
fcm_sel_wgh <- fcm_sel / rowSums(fcm_sel_sym)^0.9
fcm_sel_sym_wgh <- Matrix::forceSymmetric(fcm_sel_wgh, uplo = "U")
fcm_sel_sym_upd <- fcm_sel_sym_wgh
gc()

fcm_select(fcm_sel_wgh,
           pattern = topfeatures(fcm_sel_wgh, 50) %>%
             names, selection = "keep") %>% textplot_network

fcm_select(fcm_sel,
           pattern = topfeatures(fcm_sel, 50) %>%
             names, selection = "keep") %>% textplot_network

library(igraph)
library(tidygraph)
library(ggraph)
library(particles)

g <- graph_from_adjacency_matrix(fcm_sel_sym_upd,
                                 weighted = TRUE,
                                 diag = FALSE, mode="undirected") %>% 
  as_tbl_graph

setkey(word_freqs_upd,"feature")
V(g)$freq <- word_freqs_upd[V(g)$name,frequency]
V(g)$group <- cluster_leiden(g) %>% membership %>% as.character

g_top <- g %>%  
  activate(nodes) %>% 
  arrange(-freq) %>% 
  slice(1:500)

g_laidout <- g_top %>% 
  create_layout(layout = "igraph", algorithm = "drl")

mr <- max(g_laidout$x, g_laidout$y) / 10
mw <- min(E(g)$weight) + (max(E(g)$weight) - min(E(g)$weight)) * 0.08

set.seed(666)
ggraph(g_laidout %>% slice(1:100)) +
  geom_edge_link(aes(width = ifelse(weight < mw, 0, weight)),
                 alpha = 0.2, color="grey", show.legend = FALSE) +
  scale_edge_width('Value', range = c(0.1, 1)) + 
  geom_node_voronoi(aes(fill = group), alpha=0.1, max.radius = mr,
                    colour = "white",show.legend = FALSE) + 
  geom_node_text(aes(label = name, size = log(freq), color = group),
                 fontface = "bold", show.legend = FALSE) +
  theme_void()

g_laidout_2 <- g_top %>% 
  simulate() %>% 
  wield(link_force, strength = weight, distance = 1/weight) %>% 
  wield(manybody_force) %>% 
  wield(collision_force, radius=freq^1) %>%
  evolve() %>% 
  as_tbl_graph() %>% 
  create_layout(layout = "manual", x = x, y = y) %>%
  slice(1:200)

mr <- max(g_laidout_2$x, g_laidout_2$y) / 6
mw <- min(E(g)$weight) + (max(E(g)$weight) - min(E(g)$weight)) * 0.08

set.seed(666)
ggraph(g_laidout_2) +
  geom_edge_link(aes(width = ifelse(weight < mw, 0, weight)),
                 alpha = 0.2, color = "grey", circular = FALSE,
                 show.legend = FALSE) +
  scale_edge_width('Value', range = c(0, 5)) + 
  geom_node_voronoi(aes(fill = group), alpha = 0.1, colour = "white",
                    show.legend = FALSE,
                    max.radius = mr) +
  geom_node_text(aes(label = name, size = log(freq), color = group),
                 fontface = "bold", show.legend = FALSE) +
  theme_void() +
  labs(title = "Text map of sampled 1382 political psychology article abstracts",
      subtitle = "Time interval is 1979 - 2022",
      caption = "NOTE THAT abstracts are truncated
      \nOnly 2 journals are included: Political Psychology
      \nand Journal of Social & Political Psychology")

library(stringdist)
library(text2vec)
library(fastcluster)

all_tokens <- udp_anno[, c("doc_id","lemma")][,.(text = paste(lemma, collapse=" ")),
                                              by = doc_id] %>% corpus %>% tokens

it <- itoken(all_tokens %>% as.list)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)

tcm <- create_tcm(it, vectorizer, skip_grams_window = 5)
glove <- GlobalVectors$new(rank = 50, x_max = 10)
wv_main <- glove$fit_transform(tcm, n_iter = 50,
                               convergence_tol = 0.01,
                               n_threads = ncores)
wv_context <- glove$components
word_vectors <- wv_main + t(wv_context) 
word_vectors.dt <- word_vectors %>% as.data.table
word_vectors.dt[,lemma:=vocab$term]
word_vectors.dt[,freq:=vocab$term_count]
setkey(word_vectors.dt,lemma)

text_lemmas_simplified <- udp_anno[,c("lemma","upos")] %>% 
  as.data.table %>% unique %>% setkey(lemma)

word_vectors_dt <- text_lemmas_simplified[word_vectors.dt][
  upos %in% c("NOUN","PROPN","ADJ","VERB") & 
    !lemma %chin% stopwords("en")
  ,-c("upos")
] %>% unique

word_vectors_simplified_df <- word_vectors_dt[,-c("lemma","freq")] %>%
  as.data.frame
rownames(word_vectors_simplified_df) <- word_vectors_dt$lemma

distance <- dist(word_vectors_simplified_df)
clusters <- hclust(distance)

pc <- prcomp(word_vectors_simplified_df)
screeplot(pc,
          type="lines",
          npcs = length(pc$sdev))

pcx <- pc$x %>% as.data.table
pcx[,groupe12:=cutree(clusters, 12) %>% as.character] 
pcx[,lemma:=word_vectors_dt$lemma]
pcx[,freq:=word_vectors_dt$freq]
setkey(pcx,"lemma")
pcx <- text_lemmas_simplified[pcx][
  upos %in% c("NOUN","PROPN","ADJ","VERB") & 
    !lemma %chin% stopwords("en")
  ,-c("upos")
] %>% unique

pcx.grouptops <- copy(pcx)
setorder(pcx.grouptops,-freq)
pcx.grouptops <- lapply(1:12,function(i){
  pcx.grouptops[groupe12==i] %>% head(50)
}) %>% rbindlist

ggplot(pcx.grouptops) +
  geom_text(
    aes(label = lemma, x = PC1, y = PC2, colour = groupe12, size=log(freq)),
    alpha = 0.7
  ) +
  facet_wrap("groupe12", scales = "free") +
  theme_bw()