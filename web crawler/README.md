This is a R program to crawl, parse and extract all articles published in a specific journal.  
The sample journal is [Mobile DNA](https://mobilednajournal.biomedcentral.com/).  
There is three R scripts described below:   
1. saveHtml.R  
   * Creates directory for project1 in current working directory.
   * Counts the number of pages in Mobile DNA, then creates a vector containing the link for each page.
   * Extracts the DOI of each article for each page.
   * Accesses the Journal’s website , reads the content and saves it as an html file.  
   
Output: 186 articles saved as html files.  

2. extractHtml.R  
   * Creates global variables for the 10 fields.
   * Identifies the desired fields and stores the extracted data within a data frame.
   * Writes final data frame to a .txt file  
   
3. readTxt.R  
   * Reads data from .txt into R console.
   
