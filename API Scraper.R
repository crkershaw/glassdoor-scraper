########## API Scraper ##########

# This script uses the Glassdoor API to pull ratings across the different categories for a company and its peers
# It requires you to have your own Glassdoor API key, saved in the file glassdoor_api_code.txt

library(jsonlite)
library(tidyverse)
library(ggrepel)

# Setup parameters --------------------------------------------------------
company_name <- "facebook"
chosen_id <- 40772
input_file_location <- "inputs/companies_list_1.csv"

folder <- paste0("outputs/",company_name)
if(!dir.exists(folder)){
  dir.create(folder)
}

companies <- read_csv(input_file_location)

api_base <- read_lines("glassdoor_api_code.txt")

# Setup -------------------------------------------------------------------

CustomTheme <- theme_bw()+
  theme(
    legend.text = element_text(colour="grey47"),
    legend.title = element_text(colour="grey47"),
    panel.background = element_rect("white"),
    plot.background = element_rect("white"),
    panel.border = element_rect("grey",fill=NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title = element_text(hjust=0.5,colour="grey47"),
    axis.title = element_text(colour="grey47"),
    axis.text = element_text(colour="grey47"),
    strip.background = element_rect(colour="grey70",fill="grey98"),
    strip.text = element_text(colour="grey47")
  )

output_table <- tibble(category=NA,ratings=NA)[-1,]


# Function Creation -------------------------------------------------------

for(x in 1:nrow(companies)){
  name = strsplit(gsub(" ","%20",unlist(companies[x,1])),"%20")[[1]][1]
  print(name)
  cat(paste0(round(x/nrow(companies),2)*100,"% "))
  code = unlist(companies[x,2])
  api_code = paste0(api_base,name)
  table = as.tibble(fromJSON(api_code,flatten=TRUE)$response$employers) %>%
    filter(id==code) %>%
    select(id,name,cultureAndValuesRating,seniorLeadershipRating,compensationAndBenefitsRating,careerOpportunitiesRating,workLifeBalanceRating) %>%
    gather("category","ratings",-id,-name)
  
  output_table = rbind(output_table,table)
}

f_pulldata <- function(company,code,group){
  name = strsplit(company," ")[[1]][1]
  print(company)
  api_code = paste0(api_base,name)
  table = as.tibble(fromJSON(api_code,flatten=TRUE)$response$employers) %>%
    filter(id==code) %>%
    select(id,name,cultureAndValuesRating,seniorLeadershipRating,compensationAndBenefitsRating,careerOpportunitiesRating,workLifeBalanceRating) %>%
    gather("category","ratings",-id,-name)
  
  output_table <<- rbind(output_table,table)
}

pmap(list(companies$Company,companies$Code,companies$Group),f_pulldata)

output_table <- left_join(output_table,companies,by=c("id"="Code"))

output_table$category <- gsub("cultureAndValuesRating","Culture and Values",output_table$category)
output_table$category <- gsub("seniorLeadershipRating","Senior Leadership",output_table$category)
output_table$category <- gsub("compensationAndBenefitsRating","Compensation and Benefits",output_table$category)
output_table$category <- gsub("careerOpportunitiesRating","Career Opportunities",output_table$category)
output_table$category <- gsub("workLifeBalanceRating","Work-Life Balance",output_table$category)

output_table$ratings <- output_table$ratings %>% as.numeric()
output_table <- unique(output_table) %>%
  group_by(category, Group) %>%
  mutate(extreme = ifelse(ratings <= min(ratings),1,ifelse(ratings >= max(ratings),1,0)))


# For one peer group
graph <- ggplot(output_table,aes(category,ratings,group=interaction(category,Group),colour=Group))+
  geom_boxplot(data=subset(output_table,id!=chosen_id),aes(fill=Group),position=position_dodge(width=1),alpha=0.05,width=0.5,coef=0,show.legend=FALSE) +
  geom_point(data=subset(output_table,id!=chosen_id),position=position_jitterdodge(jitter.width=0.05,jitter.height=0,dodge.width=1),size=2,alpha=1) +
  scale_shape_identity() +
  #geom_point(data=subset(output_table,id==chosen_id),aes(fill=Group),position=position_dodge(width=1),size=3,alpha=1,shape=23) +
  CustomTheme+
  xlab("Category")+
  ggtitle("Glassdoor Rankings of EVP Elements")+
  ylab("Average Rating (/5)")

# For multiple groups
# graph <- ggplot(output_table,aes(1,ratings,group=interaction(category,Group),colour=Group))+
#   geom_boxplot(data=subset(output_table,id!=chosen_id),aes(fill=Group),position=position_dodge(width=1),alpha=0.0,width=0.5,coef=0,show.legend=FALSE) +
#   geom_point(data=subset(output_table,id!=chosen_id),position=position_jitterdodge(jitter.width=0.15,jitter.height=0,dodge.width=1),size=2,alpha=0.6) +
#   scale_shape_identity() +
#   geom_point(data=subset(output_table,id==chosen_id),fill="yellow",colour="black",position=position_dodge(width=1),size=3,alpha=1,shape=23) +
#   CustomTheme+
#   facet_grid(.~category,switch="x")+
#   theme(axis.text.x=element_blank(),axis.ticks.x = element_blank(),axis.title.x = element_blank())+
#   ggtitle("Glassdoor Rankings of EVP Elements vs Comparators")+
#   ylab("Average Rating (/5)")

# Scatter only, no boxplots
# graph2 <- ggplot(output_table,aes(1,ratings,group=interaction(category,Group),colour=Group))+
#   #geom_boxplot(data=subset(output_table,id!=chosen_id),aes(fill=Group),alpha=0.0,width=0.5,coef=0,show.legend=FALSE) +
#   geom_point(data=output_table,size=2,alpha=0.9) +
#   scale_shape_identity() +
#   #geom_point(data=subset(output_table,id==chosen_id),fill="yellow",colour="black",position=position_dodge(width=1),size=3,alpha=1,shape=23) +
#   CustomTheme+
#   facet_grid(.~category,switch="x")+
#   theme(axis.text.x=element_blank(),axis.ticks.x = element_blank(),axis.title.x = element_blank())+
#   ggtitle("Glassdoor Rankings of EVP Elements vs Comparators")+
#   ylab("Average Rating (/5)")
# graph2_labelled <- graph2 + 
#   geom_text_repel(data=output_table,aes(label=Company),position=position_jitterdodge(jitter.width=0.15,jitter.height=0,dodge.width=1),show.legend=FALSE)

graph_labelled <- graph + 
  geom_label_repel(data=filter(output_table,extreme==1),aes(label=Company),position=position_jitterdodge(jitter.width=0.15,jitter.height=0,dodge.width=1),show.legend=FALSE,segment.color = NA)

# Writing Outputs ---------------------------------------------------------

write_csv(output_table,paste0(folder,"/api_output.csv"))

png(paste0(folder,"/", company_name," Graph.png"), width=1600,height=900,res=144)
graph
dev.off()

png(paste0(folder,"/", company_name," Graph Labelled.png"), width=1600,height=900,res=144)
graph_labelled
dev.off()



