---
title: "Visualizing Text and Distributions"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

# Data Visualization Project 03


In this exercise you will explore methods to visualize text data and practice how to recreate charts that show the distributions of a continuous variable. 


## Part 1: Density Plots

Using the dataset obtained from FSU's [Florida Climate Center](https://climatecenter.fsu.edu/climate-data-access-tools/downloadable-data), for a station at Tampa International Airport (TPA) from 2016 to 2017, attempt to recreate the charts shown below


```r
library(tidyverse)
weather_tpa <- read_csv("https://github.com/reisanar/datasets/raw/master/tpa_weather_16_17.csv")
# random sample 
sample_n(weather_tpa, 10)
```

```
## # A tibble: 10 x 6
##     year month   day precipitation max_temp min_temp
##    <dbl> <dbl> <dbl>         <dbl>    <dbl>    <dbl>
##  1  2016     3    31         0           83       69
##  2  2016     3    18         0           80       66
##  3  2016     5    20         0.570       90       71
##  4  2016     7    14         0.07        92       78
##  5  2016     7    25         0           90       75
##  6  2016    11    16         0           79       56
##  7  2016     5    28         0           89       73
##  8  2016     9    27         0           90       74
##  9  2016     8    25         0           94       78
## 10  2016     9    19         0           91       80
```

See https://www.reisanar.com/slides/relationships-models#10 for a reminder on how to use this dataset with the `lubridate` package for dates and times.


(a) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_facet.png" width="80%" style="display: block; margin: auto;" />

Hint: the option `binwidth = 3` was used with the `geom_histogram()` function.



```r
# Ordering month
weather_tpa$month<-factor(weather_tpa$month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
weather_tpa<-weather_tpa[order(weather_tpa$month),]

mymonths <- c("January","February","March", "April",
              "May","Jun", "July","August","September", 
              "October","November","December")
weather_tpa$MonthAbb <- mymonths[ weather_tpa$month ]

weather_tpa$MonthAbb<-factor(weather_tpa$MonthAbb,
                             levels = c("January","February","March", "April",
              "May","Jun", "July","August","September", 
              "October","November","December"))

weather_tpa<-weather_tpa[order(weather_tpa$MonthAbb),]

weather_tpa
```

```
## # A tibble: 367 x 7
##     year month   day precipitation max_temp min_temp MonthAbb
##    <dbl> <fct> <dbl>         <dbl>    <dbl>    <dbl> <fct>   
##  1  2016 1         1          0          81       70 January 
##  2  2016 1         2          0          73       59 January 
##  3  2016 1         3          0.18       61       50 January 
##  4  2016 1         4          0          66       49 January 
##  5  2016 1         5          0          68       49 January 
##  6  2016 1         6          0          67       54 January 
##  7  2016 1         7          0          72       56 January 
##  8  2016 1         8          0.54       76       63 January 
##  9  2016 1         9          0.65       78       62 January 
## 10  2016 1        10          0          72       56 January 
## # … with 357 more rows
```




```r
# Cleaning the data
weather_tpa<-weather_tpa%>%
  mutate(max_temp=as.double(max_temp),
         min_temp=as.double(min_temp), 
         precipitation=as.double(precipitation))%>%
           arrange(month)
weather_tpa
```

```
## # A tibble: 367 x 7
##     year month   day precipitation max_temp min_temp MonthAbb
##    <dbl> <fct> <dbl>         <dbl>    <dbl>    <dbl> <fct>   
##  1  2016 1         1          0          81       70 January 
##  2  2016 1         2          0          73       59 January 
##  3  2016 1         3          0.18       61       50 January 
##  4  2016 1         4          0          66       49 January 
##  5  2016 1         5          0          68       49 January 
##  6  2016 1         6          0          67       54 January 
##  7  2016 1         7          0          72       56 January 
##  8  2016 1         8          0.54       76       63 January 
##  9  2016 1         9          0.65       78       62 January 
## 10  2016 1        10          0          72       56 January 
## # … with 357 more rows
```






```r
# Plotting the data
library(viridis)
```

```
## Loading required package: viridisLite
```

```r
partA<- ggplot(data= weather_tpa, 
               mapping= aes(x = max_temp, fill = MonthAbb)) +
  geom_histogram( binwidth = 3) + 
  facet_wrap(~MonthAbb) + 
  theme_bw() +
  theme(legend.position = "None") + scale_fill_manual(values = viridis::viridis(n = 12))+
  labs(x = "Maximum temperatures", y = "Number of Days")

# Change the color, the size and the face of x and y axis labels
partA + theme(
axis.title.x = element_text(color="black", size=20),
axis.title.y = element_text(color="black", size=20)
)
```

![](gonzalez_project_03_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


(b) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_density.png" width="80%" style="display: block; margin: auto;" />

Hint: check the `kernel` parameter of the `geom_density()` function, and use `bw = 0.5`.



```r
partB<- ggplot(data= weather_tpa, 
               mapping=aes( x= max_temp), 
               fill = "gray") + 
  geom_density(fill = 'gray', 
               kernel = "epanechnikov", 
               bw = 0.5) + 
  theme_minimal() + 
  labs(x = "Maximum temperature")

partB + theme(
axis.title.x = element_text(color="black", size=20),
axis.title.y = element_text(color="black", size=20)
)
```

![](gonzalez_project_03_files/figure-html/unnamed-chunk-6-1.png)<!-- -->



(c) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_density_facet.png" width="80%" style="display: block; margin: auto;" />

Hint: default options for `geom_density()` were used. 


```r
partC<- ggplot(data=weather_tpa, 
               mapping=aes(x = max_temp, fill = MonthAbb)) +
  geom_density(kernel = "triangular") + 
  facet_wrap(~MonthAbb) + 
  theme_bw() +
  scale_fill_manual(values = viridis::viridis(n = 12)) +
  theme(legend.position = "None", 
        plot.title = element_text(size = 22, color="black")) + 
  labs(x = "Maximum temperature", y = "",
       title = "Density plots for each month in 2016")

partC + theme(
plot.title = element_text(color="black",size=22),
axis.title.x = element_text(color="black", size=18))
```

![](gonzalez_project_03_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

(d) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges.png" width="80%" style="display: block; margin: auto;" />

Hint: default options for `geom_density()` were used. 


```r
library(ggridges)
partD<- ggplot(data=weather_tpa, 
               mapping = aes(x = max_temp, 
                y = MonthAbb, fill = MonthAbb)) + 
  geom_density_ridges() +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5) + 
  scale_fill_manual(values = viridis::viridis(n = 12)) +
  theme_minimal() +
  theme(legend.position = "None") + 
  labs(x = "Maximum temperature", y = "")

partD + theme(
axis.title.x = element_text(color="black", size=18))
```

```
## Picking joint bandwidth of 1.49
## Picking joint bandwidth of 1.49
```

![](gonzalez_project_03_files/figure-html/unnamed-chunk-10-1.png)<!-- -->



(e) Recreate the plot below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges.png" width="80%" style="display: block; margin: auto;" />

Hint: use the`ggridges` package, and the `geom_density_ridges()` function paying close attention to the `quantile_lines` and `quantiles` parameters.


   


```r
partE<- ggplot(data=weather_tpa, 
               mapping = aes(x = max_temp, 
                y = MonthAbb, fill = MonthAbb)) + 
  geom_density_ridges() +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5) + 
  scale_fill_manual(values = viridis::viridis(n = 12)) +
  theme_minimal() +
  theme(legend.position = "None") + 
  labs(x = "Maximum temperature", y = "")

partE + theme(
axis.title.x = element_text(color="black", size=18))
```

```
## Picking joint bandwidth of 1.49
## Picking joint bandwidth of 1.49
```

![](gonzalez_project_03_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


 I think the plot let us visualize which months are the coldest in Tampa. It looks like January was the coldest month and June the hottest month in 2016.


(f) Recreate the chart below:

<img src="https://github.com/reisanar/figs/raw/master/tpa_max_temps_ridges_plasma.png" width="80%" style="display: block; margin: auto;" />

Hint: this uses the `plasma` option (color scale) for the _viridis_ palette.


```r
partF<- ggplot(data=weather_tpa, 
               mapping = aes(x = max_temp, 
                             y = MonthAbb, fill = stat(x))) + 
  stat_density_ridges(geom = "density_ridges_gradient",
                      quantile_lines = TRUE, quantiles = 0.5) +
  scale_fill_viridis_c(name = "", option = "C") +
  theme_minimal() +
  labs(x = "Maximum Temperature (in Fahrenheit degrees)", y = "")

partF + theme(
axis.title.x = element_text(color="black", size=18))
```

```
## Picking joint bandwidth of 1.49
```

![](gonzalez_project_03_files/figure-html/unnamed-chunk-14-1.png)<!-- -->



## Part 2: Visualizing Text Data

Review the set of slides (and additional resources linked in it) for visualizing text data: https://www.reisanar.com/slides/text-viz#1

Choose any dataset with text data, and create at least one visualization with it. For example, you can create a frequency count of most used bigrams, a sentiment analysis of the text data, a network visualization of terms commonly used together, and/or a visualization of a topic modeling approach to the problem of identifying words/documents associated to different topics in the text data you decide to use. 

Make sure to include a copy of the dataset in the `data/` folder, and reference your sources if different from the ones listed below:

- [Billboard Top 100 Lyrics](https://github.com/reisanar/datasets/blob/master/BB_top100_2015.csv)

- [RateMyProfessors comments](https://github.com/reisanar/datasets/blob/master/rmp_wit_comments.csv)

- [FL Poly News 2020](https://github.com/reisanar/datasets/blob/master/poly_news_FL20.csv)

- [FL Poly News 2019](https://github.com/reisanar/datasets/blob/master/poly_news_FL19.csv)

(to get the "raw" data from any of the links listed above, simply click on the `raw` button of the GitHub page and copy the URL to be able to read it in your computer using the `read_csv()` function)

## Getting the dataset


```r
rate_cloud<-read_csv("https://raw.githubusercontent.com/reisanar/datasets/master/rmp_wit_comments.csv")
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   course = col_character(),
##   comments = col_character()
## )
```

```r
rate_cloud
```

```
## # A tibble: 18 x 2
##    course   comments                                                            
##    <chr>    <chr>                                                               
##  1 MATH1900 "He is very enthusiastic to help students. His course content is or…
##  2 MATH250  "Great professor, really wants his students to pass. Puts all his n…
##  3 MATH2860 "Lectures are clear and pretty easy to follow. He is always open to…
##  4 MATH2860 "He is a great professor. He mixes humor into all of his lectures, …
##  5 MATH2025 "i found him to be a good professor he keeps the class entertained …
##  6 MATH2025 "He is a great professor. I would take him again in a heartbeat. Hi…
##  7 MATH2860 "Great professor, occasional fun games to help learning, lectures a…
##  8 MATH2025 "He is a great professor. Calculus has always been sort of scary to…
##  9 MATH430  "He is an awesome professor! I'm not one for math at all and frankl…
## 10 MATH430  "Best math teacher you will ever have. He is the man, plain and sim…
## 11 MATH430  "Great Professor,  He is a really nice guy. Wants students to achie…
## 12 MATH890  "Great guy. I loved the class. He has a great way of communicating …
## 13 MATH890  "He is a talented teacher.  Can convey material well and often touc…
## 14 MATH310  "One of my favorite professors, very smart and funny guy! The lectu…
## 15 MATH890  "Easily the best mathematics professor I had at Wentworth. I was re…
## 16 MATH890  "Great class. Relates well to students. Funny. Explains concepts we…
## 17 MATH250  "Great classes, great semester!"                                    
## 18 MATH250  "Had him for pre-calc. I know you guys are thinking pre Calc must b…
```







```r
library(tm)
```

```
## Loading required package: NLP
```

```
## 
## Attaching package: 'NLP'
```

```
## The following object is masked from 'package:ggplot2':
## 
##     annotate
```

```r
rate_corpus<-Corpus(VectorSource(rate_cloud$comments))

rate_corpus[[1]][1]
```

```
## $content
## [1] "He is very enthusiastic to help students. His course content is organized and the lecture is clear and  concise. I really enjoyed his lecture. He is very on-time to grade course materials and responsive on email-communication. He is not only helpful in the classroom but also in his office hour.  Overall he is a wonderful and awesome teacher."
```

## Cleaning the data


```r
#Cleaning 
rate_clean_corpus<- tm_map(rate_corpus, tolower)
```

```
## Warning in tm_map.SimpleCorpus(rate_corpus, tolower): transformation drops
## documents
```

```r
rate_clean_corpus<- tm_map(rate_clean_corpus, removeNumbers)
```

```
## Warning in tm_map.SimpleCorpus(rate_clean_corpus, removeNumbers): transformation
## drops documents
```

```r
rate_clean_corpus<- tm_map(rate_clean_corpus, removePunctuation)
```

```
## Warning in tm_map.SimpleCorpus(rate_clean_corpus, removePunctuation):
## transformation drops documents
```

```r
rate_clean_corpus<- tm_map(rate_clean_corpus, stripWhitespace)
```

```
## Warning in tm_map.SimpleCorpus(rate_clean_corpus, stripWhitespace):
## transformation drops documents
```

```r
rate_clean_corpus<- tm_map(rate_clean_corpus, removeWords, stopwords("english"))
```

```
## Warning in tm_map.SimpleCorpus(rate_clean_corpus, removeWords,
## stopwords("english")): transformation drops documents
```

```r
rate_clean_corpus<- tm_map(rate_clean_corpus, stemDocument)
```

```
## Warning in tm_map.SimpleCorpus(rate_clean_corpus, stemDocument): transformation
## drops documents
```

```r
inspect(rate_clean_corpus[1:18])
```

```
## <<SimpleCorpus>>
## Metadata:  corpus specific: 1, document level (indexed): 0
## Content:  documents: 18
## 
##  [1] enthusiast help student cours content organ lectur clear concis realli enjoy lectur ontim grade cours materi respons emailcommun help classroom also offic hour overal wonder awesom teacher                                     
##  [2] great professor realli want student pass put note onlin                                                                                                                                                                          
##  [3] lectur clear pretti easi follow alway open extra help need ask great professor challeng cours youll learn lot                                                                                                                    
##  [4] great professor mix humor lectur make class interest easi pay attent though concept difficult understand linear algebra tri make painless possibl exam arent difficult pay attent class definit take                             
##  [5] found good professor keep class entertain class never bore found hard meet person respons email respons quick                                                                                                                    
##  [6] great professor take heartbeat lectur realli good like professor test hes realli good explain thing theyr difficult even though materi isnt easiest stuff grasp                                                                  
##  [7] great professor occasion fun game help learn lectur thorough understand quiz test suggest problem can expect good grade make class enjoy                                                                                         
##  [8] great professor calculus alway sort scari materi class difficult good teach realli becom big deal feel way confid calc skill now also hes great email back ever troubl hw respons help                                           
##  [9] awesom professor im one math frank struggl lectur abl understand concept taught us didnt understand someth abl clarifi easili strong knowledg materi must take math cours                                                        
## [10] best math teacher will ever man plain simpl easi short onlin homework help understand materi never took attend math class better show els get left behind great class great teacher                                              
## [11] great professor realli nice guy want student achiev take learn real world cours requir due major take class doesnt grade hard will use current event mathemat exampl                                                             
## [12] great guy love class great way communic materi suggest show class sinc give popquizz end hit grade hard end main reason got bump b head though love make joke                                                                    
## [13] talent teacher can convey materi well often touch back point realli emb brain easi approach will work someth miss hw quiz powerpoint great write exampl send pretest test rock great professor                                   
## [14] one favorit professor smart funni guy lectur given powerpoint alway clear interest homework week never tough say grade easi side definit take anoth class                                                                        
## [15] easili best mathemat professor wentworth realli anxious take linear algebra diff eq multivari calc realli struggl understand happen lectur realli cool dude great communic powerpoint lectur make clear cut extrem help test prep
## [16] great class relat well student funni explain concept well take time everyon understand homework often must help look student                                                                                                     
## [17] great class great semest                                                                                                                                                                                                         
## [18] precalc know guy think pre calc must easi im realli good math withdrew pre calc last semest due crappi professor professor help understand much hes help just want succeed well colleg
```

## Removing extra stopword


```r
rate_clean_corpus<- tm_map(rate_clean_corpus, removeWords, c("know", "think", "also", "make", "take"))
```

```
## Warning in tm_map.SimpleCorpus(rate_clean_corpus, removeWords, c("know", :
## transformation drops documents
```

```r
rate_clean_corpus[[1]][1]
```

```
## $content
## [1] "enthusiast help student cours content organ lectur clear concis realli enjoy lectur ontim grade cours materi respons emailcommun help classroom  offic hour overal wonder awesom teacher"
```



## Creating TDM



```r
tdm_rate<- TermDocumentMatrix(rate_clean_corpus)
m<- as.matrix(tdm_rate)
v<- sort(rowSums(m), decreasing = TRUE)
d<- data.frame(word=names(v), freq=v)
```



## Creating a wordcloud



```r
library(wordcloud)
```

```
## Loading required package: RColorBrewer
```

```r
wordcloud(d$word, d$freq, random.order = F, rot.per = .025, scale = c(4,.5), max.words = 500, colors = brewer.pal(8,"Dark2"))
```

![](gonzalez_project_03_files/figure-html/unnamed-chunk-20-1.png)<!-- -->



