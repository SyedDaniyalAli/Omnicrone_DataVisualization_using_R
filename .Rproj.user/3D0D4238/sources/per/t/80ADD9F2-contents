#1. Load libraries
library(tidyverse) # metapackage of all tidyverse packages

#2. Set file path
p1<- 'R/covid-cases-omicron.csv'

#3. Read the dataset
omi<- read_csv(p1)
omi %>% head

#4. Check the dimensions of the dataset
dim(omi)

#5. Convert character columns to factor columns
omi <- omi %>% mutate_if(is.character,as.factor)
omi %>% head

summary(omi$Day)

#6. How many countries reported the omicron variant of Covid-19?
length(table(omi$Entity))


#7. Adjust the display size of plot
fig <- function(width,height){
  options(repr.plot.width=width,repr.plot.height=height)
}

#8. Plot Omicron percentage along the timeline by countries
fig(21,18)
omi %>% group_by(Entity,Day) %>% summarize(Omicron_pct=mean(Omicron_percentage)) %>%
  ggplot(aes(Day,Omicron_pct))+geom_col()+facet_wrap(.~Entity,ncol=10)+
  labs(title='Omicron Variant Impact along the time by Countries(2021-09-20~2022-01-05)',caption='Done by Syed Daniyal Ali')+
  theme(plot.title=element_text(size=30,color='green'),axis.title.x=element_text(size=20,color='blue'),
        axis.title.y=element_text(size=20,color='blue'))


#9. Take a close look at the countries with Omicron impact on the day beyond 50%
fig(18,9)
omi %>% group_by(Entity,Day) %>% summarize(Omicron_pct=mean(Omicron_percentage)) %>% filter(Omicron_pct >= 50) %>%
  ggplot(aes(Day,Omicron_pct))+geom_col()+facet_wrap(.~Entity,ncol=10)+
  theme(plot.title=element_text(size=30,color='green'),plot.subtitle=element_text(size=20,color='blue'),
        axis.title.x.bottom=element_text(size=20,color='blue'),axis.title.y.left=element_text(size=20,color='blue'))+
  labs(title='Omicron Impact On the day with above 50% (2021-09-20~2022-01-05)',
       subtitle="From the plot,the following countries have to be more concerned as the trending on rise:
Botswana,Ghana,South Africa,UK,US.",caption='Done by Syed Daniyal Ali')


