library(officer)
library(magrittr)
library(tidyverse)
library(wordcloud)
library(dplyr)
library(stringr)
library(tidytext)
library(RColorBrewer)
library(benford.analysis)
library(lubridate)
library(data.table)
library(ggraph)
library(igraph)

##--- DATA PREPARATION
## Read dataset into R and clean
episodes1 <- read_csv("episodes1.csv")
episodes2 <- read_csv("episodes2.csv")
episodes3 <- read_csv("episodes3.csv")
episodes <- rbind(episodes1, episodes2, episodes3)

podcasts1 <- read_csv("podcasts1.csv")
podcasts2 <- read_csv("podcasts2.csv")
podcasts <- rbind(podcasts1, podcasts2)

podcasts <- podcasts %>%
  mutate(categories_num = str_count(categories, pattern="\\|")+1)

## English podcasts
podcasts_en <- podcasts %>%
  filter(language=="English")
## get 99316 English podcasts

## seperate categories
podcasts_cat <- podcasts_en %>%
  mutate(categories = strsplit(categories, "\\|")) %>% 
  unnest(categories) %>%
  mutate(categories = trimws(categories)) 

episodes_en <- episodes %>%
  inner_join(select(podcasts_en, uuid, categories), by=c("podcast_uuid"="uuid")) 

## Some audio length less or equal to 0, and some are longer than A DAY
episodes_length <- episodes_en %>%
  filter(audio_length>0) %>%
  mutate(length_min = audio_length/60, length_h = audio_length/3600,
         pub_time = as.ITime(pub_date), pub_date = as.IDate(pub_date), 
         pub_h = substring(as.character(pub_time), 1, 2)) %>%
  filter(length_min<1440)
## 656298 English episodes

## Separate by categories
episodes_cat <- episodes %>%
  inner_join(select(podcasts_cat, uuid, language, categories), by=c("podcast_uuid"="uuid"))

##--- EDA
## different language podcasts
languages_top <- podcasts %>%
  group_by(language) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

language_plot <- podcasts %>%
  inner_join(languages_top, by="language")

ggplot(data=language_plot, mapping=aes(x=reorder(language, table(language)[language]))) +
  geom_bar() +
  coord_flip() +
  ggtitle("Fig 1. Bar Chart for Podcasts Languages") +
  labs(x="Languages", y="Count", subtitle="Only contains top 10 languages") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(size = 10), axis.title = element_text(size = 10),
        plot.subtitle = element_text(size = 9))
ggsave("p1.jpg")

## how many categories most podcasts have
ggplot(data=podcasts, mapping=aes(x=categories_num)) +
  geom_bar()  +
  ggtitle("Fig 2. Bar Chart for Number of Categories Assigned to Podcasts") +
  labs(x="Number of Categories", y="Count") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(size = 10), axis.title = element_text(size = 10),
        plot.subtitle = element_text(size = 9))
ggsave("p2.jpg")

categories_top <- podcasts_cat %>%
  group_by(categories) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(10)

categories_plot <- podcasts_cat %>%
  inner_join(categories_top, by="categories")

ggplot(data=categories_plot, mapping=aes(x=reorder(categories, table(categories)[categories]))) +
  geom_bar() +
  coord_flip() +
  ggtitle("Fig 3. Bar Chart for Top 10 Categories") +
  labs(x="Categories", y="Count", subtitle="Only contains top 10 categories") +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(size = 10), axis.title = element_text(size = 10),
        plot.subtitle = element_text(size = 9))
ggsave("p3.jpg")

ggplot(data=subset(episodes_length, length_min<120), mapping=aes(x=length_min, y=..count..)) +
  geom_histogram(color="black", fill="white", binwidth=5) +
  labs(x="Audio Length in Minutes", y="Frequency", subtitle="Only focused on episodes which shorter than two hours") +
  ggtitle("Fig 4. Histogram of Audio Length for Episodes")  +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(size = 10), axis.title = element_text(size = 10),
        plot.subtitle = element_text(size = 9))
ggsave("p4.jpg")

## publish date
episodes_length$weekday <- weekdays(as.Date(episodes_length$pub_date))

ggplot(data=episodes_length, mapping=aes(x=as.character(pub_date), 
                                         fill=factor(weekday, 
                                                     levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), 
                                                     labels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))) +
  geom_bar() +
  coord_flip() +
  labs(fill="Weekday", x="Publish Date", y="Count") +
  scale_fill_brewer(palette="Paired") +
  ggtitle("Fig 5. Bar Chart of Publish Date")  +
  theme(panel.background = element_rect(fill = "white", colour = "grey50"),
        plot.title = element_text(size = 10), axis.title = element_text(size = 10),
        plot.subtitle = element_text(size = 9))
ggsave("p5.jpg")

## how podcasts describe their content by categories
tidy_description <- podcasts_cat %>%
  filter(categories=="Music") %>%
  mutate(description = str_replace_all(description, pattern = "<(.*?)>", "")) %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!is.na(word1)) %>%
  filter(!is.na(word2)) %>%
  count(word1, word2, sort = TRUE) %>%
  head(60) %>%
  mutate(word = paste(word1, word2, sep=" "))

png("p6.jpg")
set.seed(2018)
wordcloud(tidy_description$word, tidy_description$n, scale = c(3,0.5), colors = brewer.pal(6, "Set2"), rot.per = 0)
dev.off()


##--- SLIDES
my_pres <- read_pptx("template.pptx")
knitr::kable(layout_summary(my_pres))
my_pres<-
  read_pptx("template.pptx") %>%
  add_slide(layout = "Title Slide", master = "Wood Type") %>% #Slide 1
  ph_with_text(type = "ctrTitle", str = "Podcasts") %>%
  ph_with_text(type = "subTitle", str = "Angela Zhai \n15 December, 2018") %>%
  
  add_slide(layout = "Blank", master = "Wood Type") %>%
  ph_with_img_at(src = "podcasts header.jpg", height = 4, width = 8, left = 2.5, top = 1.5) %>%
  
  add_slide(layout = "Section Header", master = "Wood Type") %>%
  ph_with_text(type = "title", str = "Exploratory Data Analysis") %>%
  
  add_slide(layout = "Picture with Caption", master = "Wood Type") %>%
  ph_with_img(type = "pic", src = "p1.jpg") %>%
  ph_with_text(type = "body", str="Most of the podcasts are by English. Only a few used other languages.") %>%
  
  add_slide(layout = "Picture with Caption", master = "Wood Type") %>%
  ph_with_img(type = "pic", src = "p2.jpg") %>%
  ph_with_text(type = "body", str="Each podcasts are collected under different categories, and some providers will assign a bunch of categories to their podcasts.") %>%
  
  add_slide(layout = "Picture with Caption", master = "Wood Type") %>%
  ph_with_img(type = "pic", src = "p3.jpg") %>%
  ph_with_text(type = "body", str="Religion and Christianity are on the top. Society & Culture is a broad concept for category.") %>%
  
  add_slide(layout = "Picture with Caption", master = "Wood Type") %>%
  ph_with_img(type = "pic", src = "p4.jpg") %>%
  ph_with_text(type = "body", str="Most episodes are shorter than an hour.") %>%
  
  add_slide(layout = "Picture with Caption", master = "Wood Type") %>%
  ph_with_img(type = "pic", src = "p5.jpg") %>%
  ph_with_text(type = "body", str="Only a few episodes published on Saturday, and a lot episodes published on Monday. However, holiday is another factor which will influence whether the providers willing to publish their episodes.") %>%
  
  add_slide(layout = "Section Header", master = "Wood Type") %>%
  ph_with_text(type = "title", str = "Text Mining") %>%
  
  add_slide(layout = "Picture with Caption", master = "Wood Type") %>%
  ph_with_img(type = "pic", src = "p6.jpg") %>%
  ph_with_text(type = "body", str="Wordcloud for music category podcasts description") %>%
  
  add_slide(layout = "Title Slide", master = "Wood Type") %>% #Slide 1
  ph_with_text(type = "ctrTitle", str = "THANK YOU")

print(my_pres, target = "podcasts.pptx") 

