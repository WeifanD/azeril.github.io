---
layout: post
title: Blog with RStudio, R, RMarkdown, Jekyll and Github.
author: Weifan Dai
published: true
status: publish
draft: false
tags: R Jekyll RStudio
---
 
一直以来都想构建一个比较完整又自动化的个人博客站。这里有两个关键字，即**完整**和**自动**。怎么说呢，如果我只是一个单纯的文字工作者，写博文用[简书](http://www.jianshu.com/)足够，简洁的排版，你只需负责创作。但是如果又需要代码，可以考虑[CSDN](http://www.csdn.net/)，专注技术博文。看回我的需求，希望代码和文字一体，且代码运行结果能自动显示，写完.Rmd自动转换成.md，图片自动变为Markdown可接受的Link，之后同步上传到github进行安全托管。
 
**注:** .md是[Github Pages](https://pages.github.com/)上唯一接受的file extension.
 
这篇博文是[GitHub 博客简明使用指南]与[技术札记]的衍生，或许可以综合成一个系列，让小白少走些弯路。
 
用过Rstudio的RUsers都知道，左上角的R Notebook已经可以完成代码与结果一体化的需求，但是要将运行完成的.Rmd文件转化为.md格式，并自动显示图片或结果，就需要用到额外的函数。
 
以下是我根据其他作者编写进行修改的rmd2md.R函数，运行函数前在博客官网目录下创建三个文件夹，包括：_posts, _rmd, pictures。
 
 
1. 写函数前，确定必要的文件路径
 
```
rmd2md <- function( path_site = getwd(), # 当前博客地址
                    dir_rmd = "_rmd", # _rmd文件夹地址
                    dir_md = "_posts", # _posts文件夹地址                              
                    #dir_images = "figures",
                    url_images = "figures/",
                    out_ext='.md', 
                    in_ext='.Rmd', 
                    recursive=FALSE) 
```
 
2. 加载包
 
```
  # Loading the necessary packages
  require(knitr, quietly=TRUE, warn.conflicts=FALSE)
```
 
3. 找到需要转化的.Rmd文件
 
```
  #andy change to avoid path problems when running without sh on windows 
  # list out each rmd path in _rmd file using 'list.files'
  files <- list.files(path=file.path(path_site,dir_rmd), pattern=in_ext, ignore.case=TRUE, recursive=recursive)
  
  # for each file in files
  for(f in files) {
    # pop up the processing message
    message(paste("Processing ", f, sep=''))
```
 
4. 对每个文件进行文本搜寻，
  - 判断此文件是否已转换: 定位‘---‘，确定两个的位置，第一条在首行，第二条在两行之后，若找不到弹出warning
  - 判断Status的位置：确定在‘---’中间后取得状态内容，调整大小写，判断是否处理过，若处理过弹出warning；若没有则转化成处理了，并进行knit转化（使用try-else; Python: try-except）
  
```
    # determine the file and publish status
    content <- readLines(file.path(path_site,dir_rmd,f))
    frontMatter <- which(substr(content, 1, 3) == '---')
    if(length(frontMatter) >= 2 & 1 %in% frontMatter) {
      statusLine <- which(substr(content, 1, 7) == 'status:')
      publishedLine <- which(substr(content, 1, 10) == 'published:')
      if(statusLine > frontMatter[1] & statusLine < frontMatter[2]) {
        status <- unlist(strsplit(content[statusLine], ':'))[2]
        status <- sub('[[:space:]]+$', '', status)
        status <- sub('^[[:space:]]+', '', status)
        if(tolower(status) == 'process') {
          #This is a bit of a hack but if a line has zero length (i.e. a
          #black line), it will be removed in the resulting markdown file.
          #This will ensure that all line returns are retained.
          content[nchar(content) == 0] <- ' '
          message(paste('Processing ', f, sep=''))
          content[statusLine] <- 'status: publish'
          content[publishedLine] <- 'published: true'
          
          #andy change to path
          outFile <- file.path(path_site, dir_md, paste0(substr(f, 1, (nchar(f)-(nchar(in_ext)))), out_ext))
          
          #render_markdown(strict=TRUE)
          #render_markdown(strict=FALSE) #code didn't render properly on blog
          
          #andy change to render for jekyll
          render_jekyll(highlight = "pygments")
          #render_jekyll(highlight = "prettify") #for javascript
          
          opts_knit$set(out.format='markdown') 
          
          # andy BEWARE don't set base.dir!! it caused me problems
          # "base.dir is never used when composing the URL of the figures; it is 
          # only used to save the figures to a different directory. 
          # The URL of an image is always base.url + fig.path"
          # https://groups.google.com/forum/#!topic/knitr/18aXpOmsumQ
          
          opts_knit$set(base.url = "/")
          opts_chunk$set(fig.path = url_images)                     
          
          #andy I could try to make figures bigger
          #but that might make not work so well on mobile
          #opts_chunk$set(fig.width  = 8.5,
          #               fig.height = 5.25)
          
          try(knit(text=content, output=outFile), silent=FALSE)
          
        } else {
          warning(paste("Not processing ", f, ", status is '", status, 
                        "'. Set status to 'process' to convert.", sep=''))
        }
      } else {
        warning("Status not found in front matter.")
      }
    } else {
      warning("No front matter found. Will not process this file.")
    }
  }
  invisible()
}
```
 
 
 
