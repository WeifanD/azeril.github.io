---
layout: post
title: R与Database
categories: [blog ]
tags: [SQL]
description: 在R中创建数据库
---
## 数据准备

加载R包，负责链接数据库工作和data manipulation的dplyr、绘图工作的ggplot2，以及我平常不太用的data.table，其中的数据读取与整合函数(fread/rbindlist)非常高效。

从 <http://www.nber.org/fda/faers> 上用download.file下载数据并解压。这里用到的数据是来自某博客的统计数据，包括13年至15年这段期间人口、药物、反应、结果、应对措施数据集。

## 👇连接季度数据文件并对每个类目创建单个数据集

### 人口统计数据

```
filenames <- list.files(pattern="^demo.*.csv", full.names=TRUE)
demography = rbindlist(lapply(filenames, fread,
        select=c("primaryid","caseid","age","age_cod","event_dt",
         "sex","wt","wt_cod","occr_country"),data.table=FALSE))

str(demography)
```


```
Classes ‘data.table’ and 'data.frame':  3037542 obs. of  9 variables:
 $ primaryid   : int  30375293 30936912 32481334 35865322 37005182 37108102 37820163 38283002 38346784 40096383 ...
 $ caseid      : int  3037529 3093691 3248133 3586532 3700518 3710810 3782016 3828300 3834678 4009638 ...
 $ age         : num  44 38 28 45 NA ...
 $ age_cod     : chr  "YR" "YR" "YR" "YR" ...
 $ event_dt    : int  199706 199610 1996 20000627 200101 20010810 20120409 NA 20020615 20030619 ...
 $ sex         : chr  "F" "F" "F" "M" ...
 $ wt          : num  56 56 54 NA NA 80 102 NA NA 87.3 ...
 $ wt_cod      : chr  "KG" "KG" "KG" "" ...
 $ occr_country: chr  "US" "US" "US" "AR" ...
```

可以看到人口统计数据有超过300万行观测，变量则包括年龄，年龄代码，事件发生日期，性别，体重，体重代码和事件发生国家。

### 药物数据

```
 Classes ‘data.table’ and 'data.frame':  10026718 obs. of  4 variables:
 $ primaryid: int  30375293 30375293 30375293 30375293 30375293 30375293 30375293 30375293 30936912 30936912 ...
 $ drug_seq : int  1 2 3 4 5 6 7 8 1 2 ...
 $ drugname : chr  "AVONEX" "AVONEX" "ZANAFLEX" "STEROID (NOS)" ...
 $ route    : chr  "INTRAMUSCULAR" "INTRAMUSCULAR" "" "" ...
 - attr(*, ".internal.selfref")=<externalptr> 
```

药物数据集有大概1000万的观测，变量包括药物名称和路径等。

### 诊断结果/反应特征

```
Classes ‘data.table’ and 'data.frame':  5675759 obs. of  3 variables:
 $ primaryid    : int  30375293 30375293 30375293 30375293 30375293 30375293 30375293 30375293 30936912 30936912 ...
 $ indi_drug_seq: int  1 2 3 4 5 6 7 8 1 2 ...
 $ indi_pt      : chr  "Multiple sclerosis" "Multiple sclerosis" "Muscle spasticity" "Arthritis" ...
 - attr(*, ".internal.selfref")=<externalptr> 
 ```

该数据集有600多万个观测，变量有身份证ID，药物序列和反应特征。

### 事件结果

```
Classes ‘data.table’ and 'data.frame':  1933641 obs. of  2 variables:
 $ primaryid: int  30375293 30936912 32481334 35865322 35865322 37005182 37108102 37820163 38283002 38346784 ...
 $ outc_cod : chr  "HO" "HO" "HO" "DE" ...
```

该数据集有2000多万观测，变量有省份证ID和最终结果。

### 针对事件的措施

```
fClasses ‘data.table’ and 'data.frame':  8045719 obs. of  2 variables:
 $ primaryid: int  30375293 30375293 30375293 30375293 30375293 30375293 30375293 30375293 30375293 30375293 ...
 $ pt       : chr  "Amenorrhoea" "Asthenia" "Bladder disorder" "Blood pressure increased" ...
```
这是一个有约1000万观测，变量为身份证ID和事件应对措施的数据集。

### 创建数据库

要在R中创建一个SQLite数据库，我们只需要设定路径，使用src_sqlite()函数来连接R和现有的sqlite数据库，再用tbl()函数把数据表和该库连接在一起就大功告成了。我们也可以用src_sqlite()函数在特定路径下创建新的SQLite数据库，如果不额外指定路径，数据库将被创建于当前工作目录下。

```
my_database<- src_sqlite("adverse_events", create = TRUE)
 # create =TRUE 该参数设定为创建新的数据库
```
### 将数据写入数据库

我们使用dplyr包中的copy_to()函数把数据上传到数据库。根据文档，新写入的对象可能只是一个临时文件，我们需要把temporary参数设定为false来使得新对象是永久文件。

## 上传各个类目的数据至SQLite数据库

```
copy_to(my_database,demography,temporary = FALSE)
copy_to(my_database,drug,temporary = FALSE)
copy_to(my_database,indication,temporary = FALSE) 
copy_to(my_database,reaction,temporary = FALSE)  
copy_to(my_database,outcome,temporary = FALSE)     
```
我已经把所有数据上传到了“不良事件”数据库中了，我现在可以访问这个库并做一些数据分析了。

连接到数据库

我们可以直接使用dplyr中的函数来操作数据，dplyr包会将我们的R代码转化为SQL代码。利用tbl()函数可以连接到数据库中的表格。

```
demography = tbl(my_database,"demography" )

class(demography)
```
```
head(demography,3)

US = filter(demography, occr_country=='US') %>% 
  arrange(age)# 过滤出发生在美国的不良事件数据
```

我们也能看到数据库如何执行这个查询指令

```
explain(US)
```

<figure>
    <img src='http://i1.piimg.com/1949/622af48a7d1de409.png'>
</figure>

利用相似方法，连接到其他数据集.有意思的是dplyr包会延迟这些查询操作，只在我们需要数据的时候才把相应的对象加载到R中。即当我们使用诸如collect()， head()， count()等函数时，先前的查询指令才被执行。（译者注：也就是遵循惰性求值原则）

当我们对数据库中提取的数据进行tail()操作，程序会报错。因为只有当整个查询指令被执行完毕，我们才能找到数据表中的最后几行观测。

```
head(indication,3)

tail(indication,3)
Error: tail is not supported by sql sources
```

对数据库中的表使用dplyr中的指令 (select, arrange, filter, mutate, summarize, rename)

我们可以利用magrittr包中的管道操作符%>%将不同指令连接起来。%>%符号会把左边的输出传递到右边的函数，作为右侧函数的第一个参数。

寻找不良事件发生最多的10个国家

```
demography %>% group_by(Country= occr_country) %>% 
       summarize(Total=n())%>%      
       arrange(desc(Total))%>%       
       filter(Country!='')%>% head(10)
```

我们也可以在操作链中加入ggplot函数来对数据进行可视化

<figure>
    <img src='http://i1.piimg.com/1949/1105d1291b5c38b3.png'>
</figure>


同样我们可以寻找最常见药物、最常见的5大事件结果、相应的应对措施。

## Joins(连接)

让我们把人口统计数据，结果数据和应对数据利用身份证ID做主键连接起来：

```
inner_joined = demography%>%inner_join(outcome, by='primaryid',copy = TRUE)%>%
           inner_join(reaction, by='primaryid',copy = TRUE)

head(inner_joined)
```

我们也可以设定在连接时设定主键和第二主键。让我们把药物和反应特征数据利用两个键连接起来。

```
drug_indication= indication%>%rename(drug_seq=indi_drug_seq)%>%
   inner_join(drug, by=c("primaryid","drug_seq"))
head(drug_indication)
```
