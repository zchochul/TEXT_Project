rm(list=ls())

library(gutenbergr)
library(tidytext)
library(dplyr)
library(magrittr)
library(ggplot2)



pos <- parts_of_speech %>%
  count(pos, sort = T) %>%
  mutate(nn = n / sum(n))




Shak_id <- c(2245, 2239, 1777, 2243, 2242, 1785, 1781, 2265, 2267, 1791, 1794, 1795, 2268, 1797)
Shak <- gutenberg_download(Shak_id)

Shak_tit<-c("The Comedie of Errors", "Romeo and Juliet", "The Merchant of Venice", "A Midsummer Night's Dream", "The Trgaedy of julius Caesar", "The Merry Wives of Windsor", "The Tragedie of Hamlet", "Othello", "Alls Well That Ends Well", "The Tragedy of King Lear", "The Tragedy of Macbeth", "The Tragedie of Anthonie and Cleopathra", "The Tragedy of Coriolanius")

Aust <- gutenberg_download(c(1212, 946, 42671, 141, 158))

Aust_tit<-c("Love and Freindship", "Lady Susan", "Pride and Prejudice", "Mansfield Park", "Emma")


Dick_id <- c(19337, 676, 821, 644, 42232, 699,1422, 1415, 1394)
Dick <- gutenberg_download(Dick_id)

Dick_tit<-c( "A Christmas Carol", "The Battle of Life", "Dombey and Son", "The Haunted Man and the Ghost's Bargain", "A Child's Dream of a Star", "A Child's History of England", "Going into Society", "Doctor Marigold", "The Holly-Tree")

g <- gutenberg_works()

Shak_books <- g[g$gutenberg_id %in% Shak_id, c("gutenberg_id","title")] 

Dick_books <- g[g$gutenberg_id %in% Dick_id, c("gutenberg_id","title")] 


Shak %<>% left_join(Shak_books) %>%
  mutate(gutenberg_id = NULL)

Dick %<>% left_join(Dick_books) %>%
  mutate(gutenberg_id = NULL)


pos_freq<-function(titles, pos="noun")
{
  books<- Dick %>% filter(title == titles) %>%
    unnest_tokens(word, text) %>%
    left_join(parts_of_speech, relationship = "many-to-many") %>%
    count(pos, sort = T) %>%
    mutate(nn = n / sum(n))
  
  if(pos=="noun")
    return(books$nn[1])
  if(pos=="adjective")
    return(books$nn[2])
  if(pos=="Verb")
    return(books$nn[3]+books$nn[5]+books$nn[7])#Verb występuje w 3 różnych formach, wrzucam w jedno
  if(pos=="Adverb")
    return(books$nn[4])
  
}




df.noun <- data.frame(a = sapply(Dick_tit, pos_freq, pos="noun"), b = Dick_tit)
df.adj <- data.frame(a = sapply(Dick_tit, pos_freq, pos="adjective"), b = Dick_tit)
df.verb <- data.frame(a = sapply(Dick_tit, pos_freq, pos="Verb"), b = Dick_tit)
df.adv <- data.frame(a = sapply(Dick_tit, pos_freq, pos="Adverb"), b = Dick_tit)


df.noun <- df.noun %>% mutate(type = "noun")
df.verb <- df.verb %>% mutate(type = "verb")
df.adj <- df.adj %>% mutate(type = "adjective")
df.adv <- df.adv %>% mutate(type = "adverb")

df.all <- rbind(df.noun, df.verb, df.adj, df.adv)


df.all


g<-ggplot()+
  geom_point(data=df.all, aes(x=b, y=a, color=type))+
  theme(text = element_text(size=11), axis.text.x = element_text(angle=90, hjust=1))+
  ggtitle("Frequency of POS, Dickens") +
  xlab("title") + ylab("frequency")



df.all
g



#Przyszłość
#Zapisać tytuły w wektorze, chronologicznie - zrobione
#Zrobić funkcję która będzie wypluwała częstość danej części mowy (argument: lista książek, zwraca: df)
#Napisać funkcję, która będzie robiła przebiegi dla konkretnej części mowy dla kilku autorów
















