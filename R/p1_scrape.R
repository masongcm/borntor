
# http://bradleyboehmke.github.io/2015/12/scraping-html-text.html
# https://kvsingh.github.io/lyrics-sentiment-analysis.html
# https://michaeljohns.github.io/lyrics-lab/

library(tidyverse)
library(stringr)
library(magrittr)
library(rvest)

# read main page URL
url_root <- "https://springsteenlyrics.com/lyrics.php?cmd=songslistedbyrelease"
mainpage <- read_html(url_root)

# each panel group is an album tracklist
album_panel <- mainpage %>% html_nodes('.panel-group')
album_ids <- album_panel %>% html_attr('id')
album_headings <- album_panel %>% html_nodes('.panel-heading')
album_body <- album_panel %>% html_nodes('.panel-body')


# function to extract tracks
get_tracks <- function(panel) {
  track_row <- panel %>% 
    html_nodes('.panel-body') %>% 
    html_nodes('li') %>% html_nodes('a')
  # extract title
  track_title <- track_row %>%
    html_text() %>% 
    str_trim() 
  # extract URL
  track_url <- track_row %>%
    html_attr('href') %>% str_trim()
  # assemble into data frame
  data.frame(title = track_title, 
             url = track_url, 
             stringsAsFactors = FALSE) %>%
    mutate(url = paste0("https://springsteenlyrics.com/",url))
  
}
tracks <- map(album_panel, get_tracks)

track_url <- "https://springsteenlyrics.com/lyrics.php?song=blindedbythelight"


# get name and year of album
getnameyear <- function(panel) {
  text <- panel %>% html_nodes('.panel-heading') %>%
    html_text()
}
nameslist <- map(album_panel, getnameyear)

# CLEAN UP ALBUM DATA
albums <- data.frame(unlist(nameslist)) %>%
  set_colnames("titleyear") %>%
  mutate(titleyear = str_trim(str_remove_all(as.character(titleyear), "\n| track list"))) %>%
  # separate year and title
  separate(titleyear, c("year", "album"), " - ") %>% 
  mutate(year = as.numeric(year)) %>%
  # encode album type
  mutate(type = fct_infreq(as.factor(
    ifelse(str_detect(album, "Live|Concert|Tour Highlights|Hammersmith|Chimes"), "Live", 
           ifelse(str_detect(album, "Greatest|Essential|Collection: 1973-2012|Tracks|Chapter"), "Compilation",
                  ifelse(str_detect(album, "Blood Brothers|American Beauty"), "EP",
                         ifelse(str_detect(album, "Ties That Bind|Seeger"), "Other",
                                "Studio")
                  )))
    ))
  )

# append tracks as lists
albums$tracks <- lapply(tracks, as.tibble)


# (vectorised) function to get lyrics
get_lyrics <- function(url) {
  tryCatch(
    {
      read_html(url) %>%
        html_nodes('.project-detail') %>%
        html_nodes('p') %>%
        html_text() %>%
        extract(2) %>% # second cell of text is lyrics?
        gsub('\"', "", ., fixed = TRUE) %>% # remove escaped quotes
        gsub('\n', " ", ., fixed = TRUE) # remove escaped newlines
    },
    error=function(cond) {
      message(paste("URL error:", url))
      message(cond)
      return("") # return nothing in case of error
    }
  )
}
get_lyrics <- Vectorize(get_lyrics) 

# unnest data and get lyrics
springsteen_lyrics <- albums %>% 
  unnest() %>%
  mutate(lyrics = get_lyrics(url))

library(here)
saveRDS(springsteen_lyrics, file = here("data/springsteen_lyrics.rds"))

