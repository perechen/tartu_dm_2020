# check for mandatory libraries and download if needed
lapply(c("stylo", "tidytext","tidyverse","ggdendro", "e1071", "tidymodels", "textrecipes", "kernlab"), 
       function(x) if(!is.element(x, installed.packages())) install.packages(x, dependencies = T))



#load libraries
library(tidyverse)
library(tidytext)
library(ggdendro)


############ INPUT DATA

### list all files in the folder with your corpus
files = list.files("corpus", full.names = T)
files

### combine titles and texts as LONG strings
########## 

corpus = tibble(title = files, 
                text = sapply(files, read_file)) %>%
  mutate(title = str_replace(title, "corpus/(.*?).txt", "\\1")) 

# NB if you're on Windows, use double // in "corpus//"

### count word freqs and sort by most frequent (it will be handy to know the order of MFW) 

rank = corpus %>% 
  #tokenize by word -> new column "word" out of column "text"
  unnest_tokens(input = text, output = word, token = "words") %>%
  #count & sort
  count(word, sort = T)


### tokenize corpus, count frequencies in each document, compute relative frequencies, spread to matrix-like format

freqs = corpus %>%
  unnest_tokens(input = text, output = word, token = "words") %>%
  count(title, word) %>% # count words within each text
  group_by(title) %>%
  mutate(n = n/sum(n)) %>% # because of group_by() will sum word counts only within titles -> we get relative frequencies
  ungroup() %>% 
  rename(text_title = title) %>% 
  mutate(word = factor(word, levels = rank$word)) %>% #reorder words by their rank so in the long format they would appear in desired order
  spread(key="word", value="n", fill = 0)

freqs[1:5,1:10]

#normalize with scale()
z_freqs = freqs %>%
  select(-text_title) %>% # remove title column
  as.matrix() %>% # transform to matrix
  scale() # z-scores

# supply document titles (they go in alphabetical order so we can take them from "corpus" variable)
rownames(z_freqs) = corpus$title 

z_freqs[1:5,1:10]

### now when we have z-scored Document-Term Matrix, we can proceed to calculate distances

mfw=400 # set desired number of MFWs

delta = dist(z_freqs[,1:mfw],method = "manhattan", diag=T,upper=T)/mfw

as.matrix(delta)[1:3,1:3]

### cluster by Ward's method (minimize divergence between clusters)

clusters = delta %>%
  hclust(method = "ward.D2") %>%
  as.dendrogram()

### quick dendrogram with "ggdendro" package

ggdendrogram(clusters,rotate=T)

############ larger code to customize plot

## first prepare a ggdendro object
ggclust = dendro_data(clusters)

#### then modify the table of labels within "ggclust$labels"
ggclust$labels # looks tidy!
ggclust$labels = ggclust$labels %>%
  mutate(author = str_replace(label, "^(.*?)_.*", "\\1")) #extract all characters before the first underscore (= author)

ggclust$labels
# plot: segments & labels as separate geometry
### Don't worry, its less intimidating than it looks!
ggplot() + 
  geom_segment(data=ggclust$segments,aes(x,y,xend=xend, yend=yend),size=1) +
  geom_text(data=ggclust$labels, aes(x,y,label=label, color=author), hjust=1, angle=0, size=4) +
  coord_flip() + 
  scale_y_continuous(expand=c(1, 0)) + 
  theme_dendro() +
  guides(color=F)
