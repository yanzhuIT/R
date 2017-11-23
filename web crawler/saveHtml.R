library(xml2)
library(rvest)
library(magrittr)
library(stringr)
# folder name for project1
project1_folder = paste0(getwd(), "/project1/")
# create new folder under working directory
dir.create(project1_folder)
# count number of pages
url_pages = "https://mobilednajournal.biomedcentral.com/articles"
pages = url_pages %>% read_html() %>% html_node("span.Control_name") %>% html_text()
pages = gsub(".*of ", "", pages) %>% as.numeric()
# prefix for every article page
url = "https://mobilednajournal.biomedcentral.com/articles?searchType=journalSearch&sort=PubDate&page="
# vector for saving DOI for every article
DOI = c()
for(i in 1:pages) {
  # address for page 1 ~ 8
  # DOIs per page (normal number is 25)
  url_page = paste0(url,i)
  DOI_per_page = url_page %>% read_html() %>% html_nodes("a.fulltexttitle") %>% html_attr("data-event-label")
  DOI = c(DOI, DOI_per_page)
}
# prefix for single article
url_articles = "https://mobilednajournal.biomedcentral.com/articles/"
# get rid of the "/" because it cannot exist in filename
doi = c()
for(i in 1:length(DOI)){
  # completed address for single article
  url_article = paste0(url_articles,DOI[i])
  doi[i] <- gsub("/","",DOI[i])
  # read article webpage content and save it as a html
  url_article %>% read_html() %>% write_html(paste0(project1_folder, doi[i],".html"))
  # display the progress for saving
  cat("\r", paste0(round(i/length(DOI)*100), "% saved"))
}
