########## 1. Scraping Glassdoor for Reviews ##########

# TO BE RUN FIRST

# How to use --------------------------------------------------------------
# 1. If this is your first time running this script, install any packages that aren't installed (click install at top of the screen)
# 2. Update Inputs below within the " ": the company identifier within the Glassdoor Review page URL, company_name and
# number of pages
# 3. Run script with ctrl+shift+enter, or by clicking "Source" in the top right corner
# 4. Run next output script (file 2. Glassdoor Output Crunching)

# Inputs --------------------------------------------------------

# Overall settings
name <- "facebook"
single_or_bulk <- "single" # 'single' to scrape one company, 'bulk' to scrape a list of companies from a csv
no_pages <- 3 # 10 reviews per page
output_folder <- paste0("outputs/", "facebook")

# Single mode settings - ignore if using bulk mode
company_link <- "Facebook-Reviews-E40772" # This is the part of the url after glassdoor.co/uk/Reviews/ and before .htm

# Bulk mode settings - ignore if using single mode
companies_csv_path <- "inputs/companies_list_1.csv"



# Code --------------------------------------------------------------------

# Packages
library(tidyverse)
library(rvest)
library(purrr)
library(httr)
library(XML)
library(RCurl)

# Setup section -----------------------------------------------------------

baseurl <- "https://www.glassdoor.com/Reviews/"
company <- company_link
sort <- ".htm?sort.sortType=RD&sort.ascending=false&filter.defaultEmploymentStatuses=false&filter.defaultLocation=false" # Custom URL end to prevent defaulting to certain geography

totalreviews <- as.integer(gsub(",", "", html_text(html_nodes(read_html(paste(baseurl, company, sort, sep="")),".my-0 strong"))[1]))

# Maximum pages and minimum pages. Note reviews are oldest - newest
maxresults <- as.integer(ceiling(totalreviews/10))    #10 reviews per page
minresults <- maxresults - no_pages    

# Function section --------------------------------------------------------
failed_list <- c()

# Scraping function to create dataframe of: Date, Rating, Pros, Cons

f_output_review_tibble <- function(company_name,company_link,pagenos){
  
  # print(paste0("failed list: ",failed_list))
  input_list <- pagenos
  failed_list <<- c()
  
  # print(paste0("failed list wiped: ",failed_list))
  # print(paste(baseurl, company_link, "_P", "1", sort, sep=""))
  map_df(input_list, function(i) {
    
    Sys.sleep(sample(seq(1, 3, by=0.01), 1))    # Be a polite bot.
    
    # Working out what percentage through you are
    number <- grep(paste0("\\b",i,"\\b"),pagenos)
    
    #percentage <- (i - minresults) / no_pages
    percentage <- number / length(pagenos)
    
    cat(paste0(round(percentage*100,0),"%     "))   #progress indicator
    
    pg <- tryCatch(read_html(paste(baseurl, company_link, "_P", i, sort, sep="")),error=function(e) { #pagination
      print("Error opening page - waiting and retrying")
      Sys.sleep(sample(seq(1, 4, by=0.01), 1))
      read_html(paste(baseurl, company_link, "_P", i, sort, sep=""))
    })  
    
    f_dataframe <- function(){
      date = html_text(html_nodes(pg, ".authorJobTitle"))
      rating = html_attr(html_nodes(pg, ".gdStars.gdRatings.sm .rating .value-title"), "title")
      
      # Alternate method of getting pros and cons
      proscons <- html_text(html_nodes(pg, "p")) %>%
        as_tibble() %>%
        mutate(offset = lag(value,1)) %>%
        filter(offset == "Pros" | offset == "Cons") %>%
        mutate(reviewnum = ave(offset=="Pros","Pros",FUN=cumsum)) %>%
        spread(offset,value)
      
      pros <- proscons$Pros
      cons <- proscons$Cons
      
      # Uncomment this to see what attempts fail on
      if(attempt==1){
        if(length(date)==0){cat("Date|")}
        if(length(rating)==0){cat("Rating|")}
        if(length(pros)==0){cat("Pros|")}
        if(length(cons)==0){cat("Cons|")}
      }
      
      data.frame(company=company_name,
                 date=date,
                 rating=rating,
                 pros=pros,
                 cons=cons,
                 stringsAsFactors=F)
    }
    
    r <- NULL
    attempt <- 0
    while(is.null(r) && attempt < 3){
      if(attempt >= 1){
        Sys.sleep(sample(seq(1, 4+attempt*2, by=0.01), 1))
      }
      attempt <- attempt + 1
      if(attempt==2){cat(paste0(" Attempt failed pg"),i)} else {cat("")}
      if(attempt>1){cat(paste0("(",attempt,")" ))} else {cat("")}
      if(attempt==2){
        failed_list <<- unique(append(failed_list,i))
      }
      try(
        r <- tryCatch(f_dataframe(),error=function(e){})
      )
    }
    
    r
  })
}


f_glassdoor_scrape <- function(name,link){
  company_name <- name
  company_link <- link
  
  totalreviews <- as.integer(html_text(html_nodes(read_html(paste(baseurl, company_link, sort, sep="")),".my-0 strong")))
  if(totalreviews>1000){no_pages = 100} else {no_pages = ceiling(totalreviews/10)}
  
  print("")
  print(paste0(company_name,"---------- Total Reviews: ",totalreviews," --- Selected Pages: ",no_pages," ------"))
  
  # Maximum pages and minimum pages. Note reviews are oldest - newest
  maxresults <- as.integer(ceiling(totalreviews/10))    #10 reviews per page
  minresults <- maxresults - no_pages    
  
  output <- f_output_review_tibble(company_name,company_link,minresults:maxresults)
  write_csv(output,paste0("Client Outputs/Wreford Project/Company csvs/",company_name,".csv"))
}



# Running the code ---------------------------------------------------------

start_time <- Sys.time()

if(!dir.exists(output_folder)){
  dir.create(output_folder)
}

if(single_or_bulk == "single"){
  
  output_tibble <- f_output_review_tibble(name, company_link, minresults:maxresults) %>%
    unique() %>% 
    as_tibble()
  
  write_csv(output_tibble, paste0(output_folder, "_", name, "_single_output.csv"))

} else {
  
  companies_list <- read_csv(companies_csv_path)
  
  output_tibble <- pmap_df(companies_list, f_glassdoor_scrape) %>% 
    unique() %>%
    as_tibble()
  
  write_csv(bulk_output, paste0(output_folder, "_", name, "_bulk_output.csv"))
    
  }

end_time <- Sys.time()
time_taken <- end_time - start_time
print(paste0("Time taken: ",time_taken))



# Code running completed, now run script 2