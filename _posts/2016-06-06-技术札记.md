---
layout: post
title: 技术札记
categories: [blog ]
tags: [Mac, ]
description: 写个创站小结吧～
---

创站绝对是一个大坑 我当初真有勇气。。 嗯 这个站主要就是 Github+Jekyll+markdown 基本上还是现在能用的比较习惯的模式 

### 基本流程

域名 》修改DNS 》github建个仓库github.io 》local上新建一个主站文件夹 》fock一个template 》clone到local上 》安装ruby和Gem 》安装jekyll 》修改仓库中的域名配置CNAME之类 》在sublime中对local的template进行更替 》jekyll预览 》git三部曲

### 一些tips
cd/mkdir/touch
vim index.html

git add －A > git status > git commit -m "" > git push modern master
jekyll serve -w (localhost:4000)固定地址

@ 192.30.252.153 A

@ 192.30.252.154 A

www .WeifanD.github.io CNAME

### GitHub + Jekyll 工作机制(by on_1y)

* 机制一 
  
简单地说，你在 GitHub 上有一个账号，名为username(任意)， 有一个项目，名为 username.github.io(固定格式，username与账号名一致)， 项目分支名为 master(固定)，这个分支有着类似下面的 目录结构:

{% highlight ruby %}
.
├── index.html
├── _config.yml
├── assets
│   ├── blog-images
│   ├── css
│   ├── fonts
│   ├── images
│   └── javascripts
├── _includes
├── _layouts
├── _plugins
├── _posts
└── _site
{% endhighlight %}

这样，当你访问 http://username.github.io/时，GitHub 会使用 Jekyll 解析 用户 username名下的username.github.io项目中，分支为master 的源代码，为你构建一个静态网站，并将生成的 index.html 展示给你。

关于这个目录更多的内容，我们还不需要关心，如果你好奇心比较重，可以先看 后文源代码一节。

看完上面的解释，你可能会有一些疑问，因为按照上面的说法，一个用户只能有一个 网站，那我有很多项目，每个项目都需要一个项目网站，该怎么办呢？另外，在阮一峰 同学的文章中，特别提到，分支名应该为 gh-pages，这又是怎么回事呢？

原来，GitHub认为，一个GitHub账号对应一个用户或者一个组织，GitHub会 给这个用户分配一个域名：username.github.io，当用户访问这个域名时， GitHub会去解析username用户下，username.github.io项目的master分支， 这与我们之前的描述一致。

另外，GitHub还为每个项目提供了域名，例如，你有一个项目名为blog， GitHub为这个项目提供的域名为username.github.io/blog， 当你访问这个域名时，GitHub会去解析username用户下，blog项目的gh-pages 分支。

所以，要搭建自己的博客，你可以选择建立名为 username.github.io的项目， 在master分支下存放网站源代码，也可以选择建立名为 blog 的项目，在 gh-pages分支下存放网站源代码。

GitHub 的 Help 文档中的 User, Organization and Project Pages对此有 详细的描述。

* 机制二

Jekyll 提供了插件功能，在网站源代码目录下，有一个名为 _plugins的目录， 你可以将一些插件放进去，这样，Jekyll在解析网站源代码时，就会运行你的插件， 这样插件是 Ruby 写成的。可以为Jekyll添加功能，例如，Jekyll默认是不提供分类 页面的，你可以写一个插件，根据文章内容生成分类页面。如果没有插件，你只能每 次写文章，添加分类时，为每个分类手动写 HTML 页面。

在本地运行 Jekyll 时，这些插件会自动被调用，但是GitHub在解析网站源代码时， 出于安全考虑，会开启安全模式，禁用这些插件。我们既想用这些插件，又想用 GitHub，怎么办呢怎么办呢？

GitHub还为我们提供了更一种解析网站的方式，那就是直接上传最终的静态网页， 这样，我们可以在本地使用 Jeklly 把网站解析出来，然后再上传到 GitHub上， 这就使得我们既使用了插件，又使用了 GitHub。在上文的目录结构中，有一个 名为 _site 的目录，这个就是Jeklly在本地解析时最终生成的静态网站，我们 把其中的内容上传到 GitHub 的项目中就可以了。例如，我在GitHub上的网站， 既解析后的 _site 目录

{% highlight ruby %}
.

├── index.html
├── 2013
├── 2014
├── assets
├── categories
├── page2
├── page3
├── page4
├── 工具
├── 思想
├── 技术
└── 源代码阅读
{% endhighlight %}

其中的 categories，2013, 2014 目录就是分类插件和归档插件帮助我生成的， 我只要把这个目录下的内容上传到 GitHub 相应的项目分支中就可以了。这样，你 访问 username.github.io时，GitHub就不解析了，直接把index.html返回给你了。

* 源代码

_config.yml
这是针对 Jekyll 的配置文件， 决定了 Jekyll 如何解析网站的源代码,下面是一个示例：

{% highlight ruby %}
baseurl: /StrayBirds
markdown: redcarpet
safe: false
pygments: true
excerpt_separator: "\n\n\n"
paginate: 5
{% endhighlight %}

我的网站建立在 StrayBirds 项目中，所以 baseurl 设置成 StrayBirds， 我的文章采用 Markdown 格式写成，可以指定 Markdown 的解析器 redcarpet。 另外，安全模式需要关闭，以便 Jekyll 解析时会运行插件。 pygments 可以使得Jekyll解析文章中源代码时加入特殊标记，例如指定代码类型， 这可以被很多 javascript 代码高度库使用。 excerpt_separator 指定了一个摘要分割符号，这样 Jekyll 可以在解析文章时， 将文章的提要提取出来。 paginate 指定了一页有几篇文章，页数太多时，我们可以将文章列表分页，我们在 后文还会提到。

_layouts
这个目录存放着一些网页模板文件，为网站所有网页提供一个基本模板，这样 每个网页只需要关心自己的内容就好，其它的都由模板决定。

可以看出，这个文件就是所有页面共有的东西，每个页面的具体内容会被填充在 {{ content }} 中，注意这个 content 两边的标记，这是一种叫 liquid 的标记语言。 另外，还有那个 {{ page.title }} ，其中 page 表示引用 default.html的 那个页面，这个页面的 title 值会在 page 相应页面中被设置，例如 下面的 index.html 文件，开头部分就设置了 title值。

index.html
这是网站的首页，访问 http://username.github.io 时，会指向 http://username.github.io/index.html.

注意，文件开头的描述，我们称之为 front-matter， 是对当前文件的一种描述，这里 设置的变量可以在解析时被引用，例如这里的 layout就会告诉 Jekyll, 生成 index.html 文件时，去 _layouts 目录下找 default.html 文件，然后把当前文件解析后，添加到 default.html 的 content 部分，组成最终的 index.html 文件。还有title 设置好的 值，会在 default.html 中通过 page.title 被引用。

文件的主体部分遍历了站点的所有文章，并将他们显示出来，这些语法都是 liquid 语法， 其中的变量，例如 site, 由 Jekyll 设置我们只需要引用就可以了。而 post 中的变量， 如 post.title, post.category 是由 post 文件中的 front-matter 决定，后面马上就会看到。

_posts
这个目录存放我们的所有博客文章，他们的名字有统一的格式：

YEAR-MONTH-DAY-title.MARKUP
例如，2014-02-15-github-jeklly.md，这个文件名会被解析，前面的 index.html 中， post.date 的值就由这里文件名中的日期而来。

可以看出，文章的 front-matter 部分设置了多项值，以后可以通过类似 post.title, post.category 的方式引用这些些，另外，layout部分的值和之前解释的一样， 文件的内容会被填充到 _layouts/default.html 文件的 content 变量中。

另外，文章中 为什么不试试呢之后的有三个不可见的 \n，它决定了这三个 \n 之前的内容会被放在 post.excerpt 变量中，供其它文件使用。

_plugins
这个文件中存放一些Ruby插件, 例如 gen_categories.rb，这些文件会在 Jekyll 解析网站源代码时被执行。下一节讲述的就是插件。

_site
Jekyll 解析整个网站源代码后，会将最终的静态网站源代码放在这里

* 插件

插件使用 Ruby 写成，放在 _plugins 目录下，有些 Jekyll 没有的功能，又不能 手动添加，因为页面的内容会随着文章日期类别的不同而不同，例如分类功能和归档功能， 这时，就需要使用插件自动生成一些页面和目录。

* 分类 

我的分类插件使用的是 jekyll-category-archive-plugin, 它会根据网站文章的分类信息，为每个类别生成一个页面。
使用方法是，把 plugins/categoryarchive_plugin.rb 放在 plugins 目录下， 把 _layouts/categoryarchive.html 放在 layouts 目录下， 这样，这个插件会在Jekyll解析网站时，生成相应categories目录，目录下是各个分类， 每个分类下都有一个 index.html 文件，这个文件是根据模板文件 categoryarchive.h

然后，你就可以通过 http://username.github.io/blog/categories/工具/ 访问 工具类下的 index.html 文件。

* 归档 
我的归档插件使用的是 jekyll-monthly-archive-plugin,它会根据网站 _posts目录下的文章日期，为每个月生成一个页面。
使用方法同上。注意，这个插件在 jekyll-1.4.2 中可能会出错，在 jekyll-1.2.0 中没有错误。

* 组件

分页
当文章很多时，就需要使用分页功能，在 Jekyll 官网上提供了一种 实现，把相应代码放在 主页上，然后在 _config.yml 中设置 paginate 值就行了。

评论
评论功能需要使用外挂，我使用的是 DISQUS, 注册 之后，将评论区的一段代码放在你需要使用评论功能的页面上, 然后，通过在页面的 front-matter 部分使用

comments: true
启用评论。此外，如果你 fork 了我的项目，需要修改 `_inclusds/comments.ext`，把里面的 `disqus_shortname ` 修改成你的博客短名，这个在注册的时候会设置。
