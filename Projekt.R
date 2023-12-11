rm(list=ls())

library(gutenbergr)
library(tidytext)
library(dplyr)
library(magrittr)
library(ggplot2)
Shak_id <- c(2245, 2239, 1777, 2243, 2242, 1785, 1781, 2265, 2267, 1791, 1794, 1795, 2268, 1797)
Shak <- gutenberg_download(Shak_id)

Aust <- gutenberg_download(c(1212, 946, 42671, 141, 158))

Dick <- gutenberg_download(c(675, 19337, 653, 678, 676, 821, 644, 42232, 699, 786, 1422, 1415, 809, 1394))


g <- gutenberg_works()

Shak_books <- g[g$gutenberg_id %in% Shak_id, c("gutenberg_id","title")] 

Shak %<>% left_join(Shak_books) %>%
  mutate(gutenberg_id = NULL)

Shakkk_bookkks <- Shak %>%
  group_by(title) %>%
  mutate(linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, text)

Shak_book1 <- Shak %>%
  filter(title == "The Taming of the Shrew") %>%
  unnest_tokens(word, text) %>%
  left_join(parts_of_speech) %>%
  count(pos, sort = T) %>%
  mutate(nn = n / sum(n))

Shak_book1

ggplot(Shak_book1) + 
  geom_bar(aes(x = reorder(pos, -nn), nn, fill = reorder(pos, nn)), stat="identity") + 
  coord_flip() + theme(legend.position = "none") + 
  labs(y = "fraction", x = "POS")


#Przyszłość
#Zapisać tytuły w wektorze, chronologicznie
#Zrobić funkcję która będzie wypluwała danej części mowy (argument: lista książek, zwraca: df)
#Napisać funkcję, która będzie robiła przebiegi dla konkretnej części mowy dla kilku autorów