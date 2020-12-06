---
title: "Mini-Project 01"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

# Data Visualization Project 01

I have selected the *U.S births for the years 2000 to 2014* dataset for doing this project. This dataset has 5,479 observations and the data is presented by six variables that are year, month, date of month, day of week of week and the number of births. I will analyze the data by the observation the month and day of week that it is trending mothers to give births and the years that has the most births in the U.S between years 2000 and 2014.

```{r}
library(tidyverse)
birth <- read_csv("https://raw.githubusercontent.com/reisanar/datasets/master/us_births_00_14.csv")
```

```{r}
birth
```


# Total of births 

In the United States the total of births for the years 2000 to 2014 was 62,187,024. For analyzing the data by day of week, I will follow the steps below:

1. Summarize the data group by the weekday of birth. 
2. Ordered the categorical data, in this case, the day of week. 
3. Create a bar plot for showing the results. 


```{r}
number_births<-birth %>%
    summarise(number_births=sum(births))

number_births

```

-------------------------------------------------------------------

## Number of births by weekday

After getting the data by weekday, we have to organize that data because day of week is a categorical variable, and they must be ordered for representing then in a bar plot. 

```{r}
number_births_weekday<-birth%>%
  group_by(day_of_week)%>%
  summarise(number_births_weekday=sum(births))%>%
  mutate(total=number_births_weekday)
number_births_weekday
  


```



## Getting the weekdays ordered

```{r}
number_births_weekday$day_of_week<-factor(number_births_weekday$day_of_week, 
                                          levels = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"))
number_births_weekday<-number_births_weekday[order(
  number_births_weekday$day_of_week),]

```



After having ordered the data, I proceeded to represent the data in a bar plot using geom_col() and see which day of the week is more common for mothers to give birth and which one is not.  


## Barplot of number of births by weekday
 

```{r}
ggplot(data=number_births_weekday, mapping = aes(x=day_of_week,y=total, fill=day_of_week)) +
  geom_col(position= "dodge") + theme_minimal() +
  scale_fill_brewer(palette = "Set1") + 
  guides(fill=FALSE) +  labs(x = "",y = "Births", title = "U.S birth data for the years 2000 to 2014", 
                             subtitle = "Trends of Births by Weekday") + scale_color_brewer(palette = "Set2") +
  scale_y_continuous(labels = scales::comma) 
```


As is shown in the bar plot, the day of the week that is trending for births is Tuesday, it has a total of births of  10,274,874. However, Sunday looks like it is not common to give birth that day, it has a total of births of 5,886,889. I think most of births occurs in a unintentional day because their delivery method is natural, as it is explain in the paper ["Annual Summary of Vital Statistics:2013–2014"](https://pediatrics.aappublications.org/content/pediatrics/139/6/e20163239.full.pdf). However,  if mothers have to deliver a baby by induction or c-section, for doctors and coming parents will not select a weekend day. Maybe, this is why Tuesday has the most number of births in the week. 


---------------------------------------------------------------------------------------------------


## Total of births by months

According to our analysis, the day of the week that occurs more births is Tuesday. Now, I will proceed to check which month has the maximum number of births. For that we will do the following steps:

1. Summarize the data.
2. Organize our categorical variable.
3. Graphic representation of data. 


## Summarize the data 

```{r}
number_births_months<-birth%>%
  group_by(month)%>%
  summarise(number_births_months=sum(births))%>%
  mutate(total=number_births_months)
number_births_months

```





## Converting months from numbers to month abbreviations 

```{r}
number_births_months$month<-factor(number_births_months$month, levels = c("1","2","3","4","5","6","7","8","9","10","11","12"))
number_births_months<-number_births_months[order(number_births_months$month),]

mymonths <- c("Jan","Feb","Mar", "Apr","May","Jun", "Jul","Aug","Sep", "Oct","Nov","Dec")
number_births_months$MonthAbb <- mymonths[ number_births_months$month ]

number_births_months

```


## Getting the months ordered. 

```{r}
number_births_months$MonthAbb<-factor(number_births_months$MonthAbb, levels = c("Jan","Feb","Mar", "Apr","May","Jun", "Jul","Aug","Sep", "Oct","Nov","Dec"))
number_births_months<-number_births_months[order(number_births_months$MonthAbb),]
```


 
## Scatterplot of births by months


```{r}
ggplot(data=number_births_months, 
       mapping = aes(x=MonthAbb, y=total, color=MonthAbb)) +
  geom_point() + theme_minimal()  + guides(color=FALSE) + 
  labs(x = "",y = "Births", title = "U.S birth data for the years 2000 to 2014",
       subtitle = "Trends of Births by Month") +

```

According to our graph, in this case I have used an Scatterplot for representing the data, the months with more births are August and September and the month with less number of births is February.  It leads to babies are conceived during cold months like November and December instead warmer months like June or July. 




## Number of births in U.S by years 2000 to 2014

For analyzing the number of births in the U.S for the years 2000 to 2014, I will summarize the data by year and make a graph for representing the results. 

```{r}
number_births_years<-birth%>%
  group_by(year)%>%
  summarise(total=sum(births))
  
number_births_years
        
```


## Trends of births by year

```{r} 
ggplot(data=number_births_years,
       mapping = aes(x=year, y=total, color=year)) +
  geom_point() +   geom_line() + guides(color=FALSE) +
  theme_minimal() +  labs(x = "",y = "Births", title = "U.S birth data for the years 2000 to 2014",
                          subtitle = "Trends of Births by Year")
```



This scatterplot shows that 2007 was the year with more number of births in the U.S but after that year the number of births tend to decrease.What was happening in the U.S after  year 2007 ? Why the number of births decreased in a notable way? Well, the economic recession is the answer because economic conditions tend to affect fertility.  The economic crisis that stared in U.S between 2007 and 2008 produce that number of births fell abruptly. 

---------------------------------------------------
