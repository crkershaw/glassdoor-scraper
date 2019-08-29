# Glassdoor-Scraper

![pros wordlcoud](outputs/Single%20Example%20-%20Facebook/Facebook%20Pros%20wordcloud.png)

This repo is a series of scripts designed to understand and evaluate the value proposition companies provide to their employees, and their perception, on Glassdoor. It does this by going through reviews on the website with a web scraper, then pulling out words and phrases to produce wordclouds of 'pros' and 'cons' of working at a company.

This was written as a proof of concept in 2019 as a way to learn web scraping - it should only be used as a reference. In addition, website code changes frequently, and so the pieces of the code used to pull out particular elements from the web pages will likely go out of date - which will cause this not to work in the future.

The repo consists of 3 main scripts:

1. Glassdoor Web Scraper - Scrapes any number of pages of reviews and pulls the key information from each review into a table.
2. Glassdoor Output Crunching - Uses the tables of review data from script 1, optionally translates them to English through the Google Translate API, then counts frequency of words and two- and three-word phrases (such as 'work-life-balance'). It then uses these to produce wordclouds - a striking visualisation of a company's employee value proposition.
3. API Scraper - Requires an API key, and uses it to produce graphs of company ratings across key categories against large numbers of peers.

![comparison of peers](outputs/Single%20Example%20-%20Facebook/Facebook%20Graph.png)
