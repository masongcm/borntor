# Additional lyric cleaning ideas
# - clean repetitions (e.g. choruses) using n-grams
# - 

library(tidytext)

# stopwords
data(stop_words)
extrasw <- c("li", "la", "whoa", "whoah", "yeah", "ooh", "na", 
             "hey", "doo", "ah", "sha", "hoo", "em")
custom_stop_words <- bind_rows(tibble(word = extrasw, 
                                      lexicon = "custom"), 
                               stop_words)

# only studio albums
springsteen_studio <- readRDS(here("data/springsteen_lyrics.rds")) %>%
  filter(type == "Studio")

# get data into lines
springsteen_lines <- springsteen_studio %>% 
  unnest(lyrics) %>%
  unnest(lyrics) %>%
  group_by(album, title) %>%
  mutate(line = 1:n()) %>%
  ungroup() %>%
  rename(text = lyrics)


# get into word-level (tidy)
springsteen_tidy <- springsteen_lines %>%
  unnest_tokens(word, text) %>%
  anti_join(custom_stop_words)

springsteen_tidy %>%
  count(word, sort = TRUE)



