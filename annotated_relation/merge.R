library(dplyr)

v1 <- readr::read_csv('v1.csv')
v2 <- readr::read_csv('v2.csv')
v3 <- readr::read_csv('v3.csv')


merged <- rbind(v1[1:5], v2[1:5], v3[1:5]) %>%
  arrange(lemma)
duplic_idx <- duplicated(merged[-5])
duplic_lemmas <- merged$lemma[duplic_idx]


for (lemma in duplic_lemmas) {
  duplic_rows <- merged[merged$lemma == lemma, ]
  response <- sum(duplic_rows$response)/nrow(duplic_rows)
  
  response <- ifelse(dplyr::near(response, 0.5) || response >= 0.5,
                     1, 0)
  duplic_rows[1, 'response'] <- response
  
  # remove duplicated rows
  idx_to_remove <- which(merged$lemma == lemma)
  merged <- merged[-idx_to_remove,]
  merged <- rbind(merged, duplic_rows[1, ])
}

merged <- arrange(merged, lemma)

readr::write_csv(merged, 'merged.csv')