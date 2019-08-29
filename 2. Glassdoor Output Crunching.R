########## 2. Glassdoor Output Crunching ##########

# TO BE RUN AFTER SCRIPT 1. Glassdoor Web Scraper

# How to use --------------------------------------------------------------
# 1. If this is your first time running this script, install any packages that aren't installed (click install at top of the screen)
# 2. Update Inputs below within the " ": minimum date of review (in min_date) and words you want filtered out (in stop_words_custom)
# 3. If you wish to translate reviews to English using google translate, set the translate_with_google to "yes"
# and set the path to the login details
# 4. Then (after running script 1 first) run with ctrl+shift+enter, or by clicking "Source" in the top right corner


# Setting up google translate API -----------------------------------------
# If you wish your reviews to be translated into English, you will need to follow these setup steps

# 1. Sign in to google and go to https://console.developers.google.com/cloud-resource-manager?authuser=0
# 2. Click "Create project" and name it whatever you want. Hit 'Create'
# 3. Click on the logo Google APIs to open the API services, and then click ENABLE APIS AND SERVICES
# 4. In the library, search for Cloud Translation API. Click on it then click enable
# 5. You will then need to enable the billing, if your billing details aren't set then set them now. It will give you a very 
#    decent free trial amount, and won't charge you anything after that without asking. However, the free trial should be more
#    than sufficient
# 6. Click on the menu in the top left (3 lines in the corner) > APIs & Services > Credentials, then click on
#    Create Credentials > Service Account Key
# 7. Click 'New service account' in the drop down, name it whatever you want. Then on "Select a role" select Project > Owner
#    and click 'Create'(with JSON selected)
# 8. Save this file in the same folder as this script, and name it "google_api_key.json"
# 9. This file will give access to your google translate account, so IT IS RECOMMENDED NOT TO SHARE IT WITH ANYONE ELSE


# Inputs ------------------------------------------------------------------

# Change these dates to change the cutoff
min_date = "01/01/2017" # In the format dd/mm/yyyy
max_date = "01/06/2021" # In the format dd/mm/yyyy
translate_with_google = "yes"   # This must be "yes" or "no"

# You can add words to this list, in quotation marks, to be removed
words_to_remove <- c("low","lack","poor","en","whats","isnt","con","dont","don't","lot","lots","nicht","sehr","und","cons","pros","past","worst","terrible","bad","doesnt","left","due","top","und","bit","limited","real","didnt","india","cro","heavy","die","free","based","und","cab","von","huge","amazing","pretty","postitive","fantastic","kollegen","ist","der","ive","gute","zu","mit","cros","youre")







# Code --------------------------------------------------------------------

# Packages
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(tidytext)
library(SnowballC)
library(tm)
library(textcat)


folder <- output_folder


min_date = as.Date(min_date,"%d/%m/%Y")
max_date = as.Date(max_date,"%d/%m/%Y")

clean_tibble <- output_tibble

clean_tibble$id <- as.numeric(rownames(clean_tibble))
clean_tibble$year <- as.numeric(substr(clean_tibble$date,nchar(clean_tibble$date)-4,nchar(clean_tibble$date)))

# Removing reviews out of date range
clean_tibble <- clean_tibble %>% rowwise() %>% mutate(date2 = as.Date(date,"%B %d,%Y"))
clean_tibble <- filter(clean_tibble,date2 > min_date & date2 <= max_date)

# Removing duplicate reviews (for some reason it duplicates some old reviews, this removes those with duplicates in pros and cons)
clean_tibble <- subset(clean_tibble,!(duplicated(clean_tibble[,5]) & duplicated(clean_tibble[,6])))


# Translating foreign language reviews ------------------------------------

if(tolower(translate_with_google) == "yes"){
  library(googleLanguageR)
  library(googleAuthR)
  googletranslate_login <- "google_api_key.json"
  gl_auth(googletranslate_login)
  
  clean_tibble <- clean_tibble %>%
    mutate(pros = gl_translate(pros,target = "en")$translatedText) %>% # Translating to english (pros)
    mutate(cons = gl_translate(cons,target = "en")$translatedText) # Translating to english (cons)
}


# Counting words and phrases ----------------------------------------------

stop_words_custom <- tibble(word = c("company",tolower(name),words_to_remove))

pros_concat <- paste(clean_tibble$pros,collapse=" ")
cons_concat <- paste(clean_tibble$cons,collapse=" ")

pros_concat_clean <- removePunctuation(tolower(pros_concat))
cons_concat_clean <- removePunctuation(tolower(cons_concat))

pros_concat_table <- table(strsplit(pros_concat_clean," ")) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(., c("Var1", "n")) %>%
  arrange(desc(n)) %>%
  anti_join(stop_words,by=c("Var1" = "word")) %>% # Removing common words
  anti_join(stop_words_custom,by=c("Var1" = "word")) # Removing common words

cons_concat_table <- table(strsplit(cons_concat_clean," ")) %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(., c("Var1", "n")) %>%
  arrange(desc(n)) %>%
  anti_join(stop_words,by=c("Var1" = "word")) %>% # Removing common words
  anti_join(stop_words_custom,by=c("Var1" = "word")) # Removing common words

# Counting two-word phrases
pros_concat_table_bi <- tibble(text=pros_concat_clean) %>%
  unnest_tokens(bigram,text,token="ngrams",n=2) %>%
  separate(bigram,c("word1","word2"),sep=" ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1,word2,sort=TRUE) %>%
  mutate(Var1=paste(word1,word2,sep="-")) %>%
  select(Var1,n)

cons_concat_table_bi <- tibble(text=cons_concat_clean) %>%
  unnest_tokens(bigram,text,token="ngrams",n=2) %>%
  separate(bigram,c("word1","word2"),sep=" ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  count(word1,word2,sort=TRUE) %>%
  mutate(Var1=paste(word1,word2,sep="-")) %>%
  select(Var1,n)

# Counting three-word phrases
pros_concat_table_tri <- tibble(text=pros_concat_clean) %>%
  unnest_tokens(trigram,text,token="ngrams",n=3) %>%
  separate(trigram,c("word1","word2","word3"),sep=" ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1,word2,word3,sort=TRUE) %>%
  mutate(Var1=paste(word1,word2,word3,sep="-")) %>%
  select(Var1,n)

cons_concat_table_tri <- tibble(text=cons_concat_clean) %>%
  unnest_tokens(trigram,text,token="ngrams",n=3) %>%
  separate(trigram,c("word1","word2","word3"),sep=" ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1,word2,word3,sort=TRUE) %>%
  mutate(Var1=paste(word1,word2,word3,sep="-")) %>%
  select(Var1,n)

# Joining single, double and triple word phrases together
pros_table <- full_join(pros_concat_table,pros_concat_table_bi)
pros_table <- full_join(pros_table,pros_concat_table_tri) %>%
  arrange(desc(n)) %>%
  filter(n>1) %>%
  filter(Var1!="") %>%
  filter(is.na(as.numeric(Var1)) == TRUE)

cons_table <- full_join(cons_concat_table,cons_concat_table_bi)
cons_table <- full_join(cons_table,cons_concat_table_tri) %>%
  arrange(desc(n)) %>%
  filter(n>1) %>%
  filter(Var1!="") %>%
  filter(is.na(as.numeric(Var1)) == TRUE)


# Producing wordclouds ----------------------------------------------------

set.seed(142)
png(paste0(folder, name," Pros wordcloud.png"), width=1280,height=800,res=144)
pro_wordcloud <- wordcloud(words=pros_table$Var1,freq=pros_table$n,min.freq=1,max.words=200,random.order=FALSE,rot.per=0,colors=brewer.pal(8, "Dark2"))
dev.off()

png(paste0(folder, name," Cons wordcloud.png"), width=1280,height=800,res=144)
con_wordcloud <- wordcloud(words=cons_table$Var1,freq=cons_table$n,min.freq=1,max.words=200,random.order=FALSE,rot.per=0,colors=brewer.pal(8, "Dark2"))
dev.off()

write_csv(pros_table,paste0(folder, name," Pros Table.csv"))
write_csv(cons_table,paste0(folder, name," Cons Table.csv"))

write_csv(clean_tibble,paste0(folder, name," Full site scrape.csv"))