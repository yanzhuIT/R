library(xml2)
library(rvest)
library(magrittr)
library(stringr)

# all the global variables are for all articles
# save all DOIs
article_doi_all = c()
# save all titles
Title_all = c()
# save all authors
Authors_all = c()
# save all affiliations
Author_Affiliations_all = c()
# save all corresponding authors
Corresponding_Authors_all = c()
# save all corresponding email
Corresponding_Author_Email_all = c()
# save all publication date
Publication_Date_all = c()
# save all abstract
Abstract_all = c()
# save all keywords
Keywords_all = c()
# save all full text
Full_Text_all = c()
for (i in 1:length(doi)) {
  # each row in data.frame, refer to one article
  file_name = paste0(project1_folder, doi[i], ".html")
  # original data 
  data_original  = file_name %>% read_html()
  
  # doi for each article
  article_doi = data_original %>% html_node("p.ArticleDOI") %>% html_text()
  article_doi = gsub("https://doi.org/", "", article_doi)
  article_doi = gsub("\n", "", article_doi)
  article_doi_all = c(article_doi_all, article_doi)
  
  # Title for each article
  Title =  data_original %>% html_node("h1.ArticleTitle") %>% html_text()
  Title = gsub("\n *", "", Title)
  Title_all = c(Title_all, Title)
  
  # Authors for each article
  Authors = data_original %>% html_nodes("span.AuthorName") %>% html_text()
  Authors = gsub("\n", "", Authors)
  # convert vector to character
  Authors_collapse = paste(Authors,collapse  = ",")
  Authors_all = c(Authors_all, Authors_collapse)
  
  # Author Affiliations Text for each article
  Author_Affiliations = data_original %>% html_nodes("div.Affiliation") %>% html_text()
  Author_Affiliations = gsub("\n", "", Author_Affiliations)
  Author_Affiliations_collapse = paste(Author_Affiliations, collapse = ",")
  Author_Affiliations_all = c(Author_Affiliations_all, Author_Affiliations_collapse)
  
  # Corresponding Author for each article
  Corresponding_Author = data_original %>% html_nodes("a.EmailAuthor") %>% xml_parent() %>% html_node("span.AuthorName") %>% html_text()
  Corresponding_Author = gsub("\n", "", Corresponding_Author)
  Corresponding_Author_collapse = paste(Corresponding_Author, collapse = ",")
  Corresponding_Authors_all = c(Corresponding_Authors_all, Corresponding_Author_collapse)
  
  # Corresponding Author's Email
  Corresponding_Author_Email = data_original %>% html_nodes("a.EmailAuthor") %>% html_attr("href")
  Corresponding_Author_Email = gsub("mailto:", "", Corresponding_Author_Email)
  Corresponding_Author_Email = gsub("\n", "", Corresponding_Author_Email)
  Corresponding_Author_Email_collapse = paste(Corresponding_Author_Email, collapse = ",")
  Corresponding_Author_Email_all = c(Corresponding_Author_Email_all, Corresponding_Author_Email_collapse)
  
  # Publication Date for each article
  Publication_Date = data_original %>% html_node("p.HistoryOnlineDate") %>% html_text()
  Publication_Date = gsub("Published: ", "", Publication_Date)
  Publication_Date = gsub("\n", "", Publication_Date)
  Publication_Date_all =  c(Publication_Date_all, Publication_Date)
  
  # Abstract for each article
  Abstract = data_original %>% html_nodes("div.AbstractSection") %>% html_text()
  Abstract = gsub("\n", "", Abstract)
  Abstract_collapse = paste(Abstract, collapse = ",")
  if(Abstract_collapse == ""){
    Abstract = data_original %>% html_nodes("#Abs1 .Para") %>% html_text()
    Abstract = gsub("\n", "", Abstract)
    Abstract_collapse = paste(Abstract, collapse = ",")
  }
  Abstract_all = c(Abstract_all, Abstract_collapse)
  # Keywords for each article
  Keywords = data_original %>% html_nodes("span.Keyword") %>% html_text()
  Keywords = gsub("\n", "", Keywords)
  Keywords = paste(Keywords, collapse = ",")
  Keywords_all = c(Keywords_all, Keywords)
  
  # Full Text for each article
  Full_Text = data_original  %>% html_node("#Test-ImgSrc") %>% html_text() 
  Full_Text = gsub("\n", "", Full_Text)
  Full_Text_all = c(Full_Text_all, Full_Text)
  
}
# save all data to a data.frame
data_extract = data.frame(article_doi_all, Title_all,Authors_all,Author_Affiliations_all,Corresponding_Authors_all,Corresponding_Author_Email_all,Publication_Date_all,Abstract_all,Keywords_all, Full_Text_all,stringsAsFactors = FALSE )
txt_name = paste0(project1_folder, "Mobile DNA.txt")
write.table(data_extract, txt_name, sep = "\t",row.names = FALSE, col.names = FALSE , fileEncoding="UTF-8")

