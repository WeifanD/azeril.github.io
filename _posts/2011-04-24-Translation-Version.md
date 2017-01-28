---
layout: post
title: 对实习医生格蕾的初步探索"
description: "An analysis and visualization of a holiday classic."
output: html_document
date: 2017-1-28 3:30:00 -0400
category: r
tags: [r]
comments: true
---

```{r cache = FALSE, echo = FALSE}
knitr::opts_chunk$set(cache = TRUE, message = FALSE, warning = FALSE)

library(ggplot2)
theme_set(theme_bw())
```

今天是大年初一，是个好日子，耳边传来的是小岳岳的歌声，手下正敲打着键盘，想说一定要把这个小坑tm给填了！！
数据挖掘中文本挖掘一直觉得是挺有意思的主题，今天就模仿着David Robinson的blog[love actually](http://varianceexplained.org/r/love-actually-network/)

### 数据源

首先当然就是获得数据文本，就在百度上搜了一部我最喜欢的医学美剧，'Gary's Anatomy'，实习医生格蕾的光碟还在大白熊那，估计是要尘封一段时间了，不行，要找时间讨回来的说！！

```{r}
raw <- readLines("gary's anatomy.txt")
df <- data_frame(raw = raw) %>% 
  filter(raw != "", !is.na(raw)) %>% 
  separate(raw, c("form","content"),sep = ": ",fill = "left") %>% 
  mutate(form = ifelse(is.na(form), "Others", form)) %>% 
  mutate(is_scene = str_detect(form, "2x"),
         scene = cumsum(is_scene))
head(df)
```
好了现在我们算是对raw data进行了initial manipulation,一个tidy data的过程。当然这里算是很简单了，正常数据挖掘中data cleaning通常要花费一名data analyst60%的时间，这我深有体会啊。叹一声先。。

我在网上找到的这个剧本是格雷第二季，包括了本集名字／演员的台词以及场景转换，当然还有我觉得最有意思的旁白。
先看看究竟第二季有哪些主题，都是谁写的，剧本中用到的高频单词是什么，以及台词频率最高的是不是就是我们印象中的那些主角？

```{r cache = FALSE, echo = FALSE}
series_name <- df %>% 
  filter(str_detect(form, "2x")) %>% 
  rename(name = content) %>% 
  select(form, name) %>% 
  mutate(form = str_replace(form, "2x", ""))
series_name
```

我们来看一下，第二季总共27集，有没有你印象最深的一集呢？我想第一集的writer可能没想到多年之后，有一个胖胖的女生唱了一首足够震撼动人心魄的歌曲，她的第一句音起就是“When the rain is blowing in your eyes and the whole world is on your case”～

```{r cache=FALSE echo=FALSE}
writer <- df %>% 
  filter(form == "Written by") %>% 
  separate(content, c("writer1","writer2"),sep = " & ",fill = "right")

writer1 <- writer %>% 
  select(writer1)

writer2 <- writer %>% 
  select(writer2)

library(ggplot2) 
writer1 %>%
  merge(writer2) %>% 
  count(writer1, sort = TRUE) %>% 
  mutate(content = reorder(writer1,n)) %>% 
  ggplot(aes(content, n))+
  geom_bar(stat = "identity")+
    coord_flip()+
  ggtitle("Main writers")

```
![center](http://p1.bqimg.com/567571/1efbc299a455094c.png)

```{r}
library(tidytext)
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
dialogue_words <- dialogue %>%
  select(content) %>% 
  unnest_tokens(word, content, token = "regex", pattern = reg) %>% 
  filter(!word %in% stop_words$word)

library(wordcloud2)
dialogue_words %>% 
  count(word, sort = T) %>% 
  as.data.frame(.) %>% 
  wordcloud2(size = 1.5, shape = 'star')
```
![center](http://i1.piimg.com/567571/3ea3e83d94f10490.png)

作为一部典型的美剧，格雷的台词除却医学专用此外，日常常用词还是占多数的，像什么god／ah／guy之类的口头语，chief／Dr的title，当然也有医学题材不可避免的surgery，接下来看看这部剧积极的情绪词频。

```{r}
senti_stat <- dialogue_words %>% 
  inner_join(sentiments)
  
senti_stat %>% 
  filter(sentiment == "positive") %>% 
  count(word, sort = T) %>% 
  as.data.frame(.) %>% 
  wordcloud2(size = 1)
```
跟想象中差不多，lucky／love／happy占首位

接下来我们挖一下人物关系吧

```{r}
character_df <- df %>% 
  filter(!is.na(form), 
         !str_detect(form, "by"),
         !str_detect(form, "2x"),
            !str_detect(form, "Original"),
         !str_detect(form, "Others"),
                     !str_detect(form, "MVO"))
  
character_df %>% 
  count(form, sort = TRUE) %>% 
  slice(1:5)
```


```{r lines}
lines <- character_df %>%
    filter(!is_scene) %>%
    rename(speaker = form, dialogue = content) %>% 
    group_by(scene, line = cumsum(!is.na(speaker))) %>%
    summarize(speaker = speaker[1], dialogue = str_c(dialogue, collapse = " ")) %>% 
  mutate(speaker = str_replace(speaker,"\\(.+", "")) %>% 
  filter(!is.na(speaker))
```

现在每集每个人每句台词形成一行，也就是one scene one line one observation, 将其转变成“speaker-by-scene matrix”，为之后的聚类做准备。

```{r speaker_scene_matrix, dependson = "cast"}
by_speaker_scene <- lines %>%
    count(scene, speaker) %>% 
  filter(n > 15)

library(reshape2)
speaker_scene_matrix <- by_speaker_scene %>%
    acast(speaker ~ scene, fun.aggregate = length)
norm <- speaker_scene_matrix / rowSums(speaker_scene_matrix)

h <- hclust(dist(norm, method = "manhattan"))
plot(h)
```
![center](http://i1.piimg.com/567571/9dc9a338573c4157.png)

啦啦啦我们五大主角果然聚在一起了，谁叫他们工作在一起住还住一起～

```{r ordering, dependson = "h"}
ordering <- h$labels[h$order]
```

```{r scenes, dependson = "speaker_scene_matrix"}
scenes <- by_speaker_scene %>%
    filter(n() > 1) %>%        # scenes with > 1 character
    ungroup() %>%
    mutate(scene = as.numeric(factor(scene)),
           character = factor(speaker, levels = ordering))

ggplot(scenes, aes(scene, speaker)) +
    geom_point() +
    geom_path(aes(group = scene))
```
![center](http://p1.bpimg.com/567571/8ab454ce2effc2ca.png)

我们可以看到每一集出现人物的关系线，Alex居然第九集没有出场，Mark第18集才出场，看来我得去回顾一下第二季了，先到这里等我review剧之后再把埋一下。

