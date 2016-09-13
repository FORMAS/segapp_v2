GraphsExample
================
George C. G. Barbosa
13 de setembro de 2016

How to plot graphs from relations
=================================

### Importing Library's

``` r
library(openNLP)
library(NLP)
library(plyr)
```

### Sourcing Files + Loading Sentences

``` r
setwd('/Users/ohack/segapp_v2/')
source(file = 'R/NLPTasks.R')
source(file = 'R/Patterns.R')
source(file = 'R/Extractions.R')
source(file = 'R/Util.R')
source(file = 'R/Graphs.R')
source(file = 'R/Features.R')

df_training <- readTrainingDF("data/nyt-extractions-all-labeled.txt", 200)
```

### Extract NLP Tags for each sentence

``` r
tags <- lapply(df_training, function(l){
  tag(l['rel'])
})

nlp_tags <- list()
for (i in 1:length(df_training)){
  tags_word <- tags[[i]][tags[[i]]$type=='word']
  nlp_tags[[i]] <- ext_nlp_tags(tags_word$features)
  rel <- as.String(df_training[[i]]['rel'])
  nlp_tags[[i]]$words <- rel[tags_word]
}
```

### Construct graphs

``` r
graphs <- lapply(nlp_tags, function(l){
  createGraph(l$pos)
})
```

Plotting some graphs
--------------------

    ## Grafo 45

![](GraphsExample_files/figure-markdown_github/plot%20graphs-1.png)

    ## Grafo 46

![](GraphsExample_files/figure-markdown_github/plot%20graphs-2.png)

    ## Grafo 47

![](GraphsExample_files/figure-markdown_github/plot%20graphs-3.png)

    ## Grafo 48

![](GraphsExample_files/figure-markdown_github/plot%20graphs-4.png)

    ## Grafo 49

![](GraphsExample_files/figure-markdown_github/plot%20graphs-5.png)

    ## Grafo 50

![](GraphsExample_files/figure-markdown_github/plot%20graphs-6.png)
