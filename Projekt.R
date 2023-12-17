rm(list=ls())

library(gutenbergr)
library(tidytext)
library(dplyr)
library(magrittr)
library(ggplot2)



pos <- parts_of_speech %>%
  count(pos, sort = T) %>%
  mutate(nn = n / sum(n))




pos_freq<-function(book)
{
    f<-book%>%left_join(pos) %>%
    count(pos, sort = T) %>%
    mutate(nn = n / sum(n))
  
  return(f)
}





Shak_id <- c(2245, 2239, 1777, 2243, 2242, 1785, 1781, 2265, 2267, 1791, 1794, 1795, 2268, 1797)
Shak <- gutenberg_download(Shak_id)

Shak_tit<-c("The Comedie of Errors", "Romeo and Juliet", "The Merchant of Venice", "A Midsummer Night's Dream", "The Trgaedy of julius Caesar", "The Merry Wives of Windsor", "The Tragedie of Hamlet", "Othello", "Alls Well That Ends Well", "The Tragedy of King Lear", "The Tragedy of Macbeth", "The Tragedie of Anthonie and Cleopathra", "The Tragedy of Coriolanius")

Aust <- gutenberg_download(c(1212, 946, 42671, 141, 158))

Aust_tit<-c("Love and Freindship", "Lady Susan", "Pride and Prejudice", "Mansfield Park", "Emma")

Dick <- gutenberg_download(c(675, 19337, 653, 678, 676, 821, 644, 42232, 699, 786, 1422, 1415, 809, 1394))

Dick_tit<-c("American Notes for General Circulation", "A Christmas Carol", "The Chimes", "The Cricket on The Harth A Fairy Tale of Home", "The Battle of Life", "Dombey and Son", "The Haunted Man and the Ghost's Bargain", "A Child's Dream of a Star", "A Child's History of England", "Hard Times", "Going into Society", "Doctor Marigold", "Holiday Romance in Four Parts", "The Holly-Tree")

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



Shak









#Przyszłość
#Zapisać tytuły w wektorze, chronologicznie - zrobione
#Zrobić funkcję która będzie wypluwała częstość danej części mowy (argument: lista książek, zwraca: df)
#Napisać funkcję, która będzie robiła przebiegi dla konkretnej części mowy dla kilku autorów
















