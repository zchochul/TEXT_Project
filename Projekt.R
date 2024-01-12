rm(list=ls())

library(gutenbergr)
library(tidytext)
library(dplyr)
library(magrittr)
library(ggplot2)
library(corrplot)



pos <- parts_of_speech %>%
  count(pos, sort = T) %>%
  mutate(nn = n / sum(n))


Dick_id <- c(19337, 676, 821, 644, 42232, 699, 1422, 1415, 1394)
Dick <- gutenberg_download(Dick_id)

Dick_tit<-c( "A Christmas Carol", "The Battle of Life", "Dombey and Son", "The Haunted Man and the Ghost's Bargain", "A Child's Dream of a Star", "A Child's History of England", "Going into Society", "Doctor Marigold", "The Holly-Tree")


Doyle_id <- c(244, 2097, 903, 2852, 139, 3289, 2350)

Doyle <-gutenberg_download(Doyle_id)

Doyle_tit <- c("A Study in Scarlet", "The Sign of the Four", "The White Company", "The Hound of the Baskervilles", "The Lost World", "The Valley of Fear", "His Last Bow: An Epilogue of Sherlock Holmes")





g <- gutenberg_works()

Dick_books <- g[g$gutenberg_id %in% Dick_id, c("gutenberg_id","title")] 


Doyle_books <- g[g$gutenberg_id %in% Doyle_id, c("gutenberg_id","title")] 




Shak %<>% left_join(Shak_books) %>%
  mutate(gutenberg_id = NULL)

Dick %<>% left_join(Dick_books) %>%
  mutate(gutenberg_id = NULL)


Aust %<>% left_join(Aust_books) %>%
  mutate(gutenberg_id = NULL)


Doyle %<>% left_join(Doyle_books) %>%
  mutate(gutenberg_id = NULL)


Dumas %<>% left_join(Dumas_books) %>%
  mutate(gutenberg_id = NULL)





pos_freq<-function(titles, pos="noun")
{
  books<- Dick %>% filter(title == titles) %>% #zmienić na osobę którą się chce analizować
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

tit<-Dick_tit#lista tytułów analizowanej osoby


df.noun <- data.frame(a = sapply(tit, pos_freq, pos="noun"), b = tit)
df.adj <- data.frame(a = sapply(tit, pos_freq, pos="adjective"), b = tit)
df.verb <- data.frame(a = sapply(tit, pos_freq, pos="Verb"), b = tit)
df.adv <- data.frame(a = sapply(tit, pos_freq, pos="Adverb"), b = tit)


df.noun <- df.noun %>% mutate(type = "noun") %>% mutate(b =ordered(b, levels = unique(b)))
df.verb <- df.verb %>% mutate(type = "verb") %>% mutate(b =ordered(b, levels = unique(b)))
df.adj <- df.adj %>% mutate(type = "adjective") %>% mutate(b =ordered(b, levels = unique(b)))
df.adv <- df.adv %>% mutate(type = "adverb") %>% mutate(b =ordered(b, levels = unique(b)))

df.all <- rbind(df.noun, df.verb, df.adj, df.adv)


df.all


g<-ggplot()+
  geom_point(data=df.all, aes(x=b, y=a, color=type))+
  theme(text = element_text(size=11), axis.text.x = element_text(angle=90, hjust=1))+
  ggtitle("Frequency of POS, Dickens") + #zmienić nazwisko
  xlab("title") + ylab("frequency")


g


kor <- cor(df.all)

df.all


do_korelacji<-data.frame(
  noun = df.noun$a,
  verb = df.verb$a,
  adj = df.adj$a,
  adv = df.adv$a
  )

M <- cor(do_korelacji)


corrplot(M, method = 'number') # colorful number


#Przyszłość
#Zapisać tytuły w wektorze, chronologicznie - zrobione
#Zrobić funkcję która będzie wypluwała częstość danej części mowy (argument: lista książek, zwraca: df)
#Napisać funkcję, która będzie robiła przebiegi dla konkretnej części mowy dla kilku autorów









