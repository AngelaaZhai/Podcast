# Podcast

## 1. Data Resource

[Kaggle] https://www.kaggle.com/listennotes/all-podcast-episodes-published-in-december-2017

Scraped from Listen Notes (https://www.listennotes.com/), which is a podcast search engine. Datasets include the meta data of (almost) all podcast episodes that were published in December 2017.

Dataset here is a little bit different from the one on Kaggle since Github storage limit. Split one into couple datasets, and remove some columns which I didn't use.



## 2. Data Manipulation

Rmd file: Report.Rmd


    
## 3. Podcasts EDA

* __What language does most podcasts used?__

<img width="747" alt="fig_1" src="https://user-images.githubusercontent.com/42655633/50501838-30318780-0a29-11e9-9dfc-f1ffacd2bcd7.png">

More than third quarter podcasts are made by __English__! IMPRESSIVE

Other than languages, publishers assign various categories for their podcast chanel.

* __How many categories usually be tagged to the podcast?__

<img width="739" alt="fig_2" src="https://user-images.githubusercontent.com/42655633/50501906-ae8e2980-0a29-11e9-8d92-548094700697.png">

Most podcasts just tagged one or two categories. However, there are some podcasts have __more than 15 categories__!

* __Soooo which category appears most?__

<img width="718" alt="fig_3" src="https://user-images.githubusercontent.com/42655633/50501944-d41b3300-0a29-11e9-8c52-c451139c4db7.png">

Seems like podcast is a popular way for religious purpose.


## 4. Episodes EDA

Do you have patience to catch full episode which lasts more than two hours? Probably not.

* __Then usually how long will an episode last?__

<img width="723" alt="fig_4" src="https://user-images.githubusercontent.com/42655633/50502234-a2a36700-0a2b-11e9-8b77-29f79f2decf4.png">

Only a few episodes will last more than an hour. Most episodes contains ten minutes content, or between 25 to 45 minutes.

* __What about publish date? Any trend?__

<img width="710" alt="fig_5" src="https://user-images.githubusercontent.com/42655633/50502238-ac2ccf00-0a2b-11e9-88a4-7cea9525f9b1.png">

Mondays are always the peak, and Saturdays...the opposite.

## 5. Text Mining

* __How publishers describe their podcasts?__

Here is a bigram wordcloud for podcasts under music category.

<img width="475" alt="fig_6" src="https://user-images.githubusercontent.com/42655633/50502244-b4850a00-0a2b-11e9-9d96-4b014c0ef9c3.png">

__Hip Hop__ shows a lot! __Of Course!__

Here is a Shiny App which could play with different category's wordcloud. Link below:

[shinyio] https://angelaz.shinyapps.io/podcasts/
