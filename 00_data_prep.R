
load("data/PolPsyArticles.RData")
library(tidyverse)
df <- df %>% unite("text", c(Title, Abstract), sep = " ", remove = FALSE) %>%
  rowid_to_column("doc_id") %>%
  relocate(text, .after = last_col())
save(df, file = "data/PolPsyArticles.RData")

library(quanteda)
docu_vars <- df %>% select(doc_id:Abstract)
PolPsyArt_q_corp <- corpus(df$text, docvars = docu_vars)
save(PolPsyArt_q_corp, file = "data/PolPsyArticles_q_corp.RData")
