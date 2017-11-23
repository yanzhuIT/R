### Read text file ###

dna=read.table(file=txt_name, sep="\t", quote="\"", na.strings = "", header = FALSE, stringsAsFactors = FALSE, fileEncoding="UTF-8")

### add column name to data frame ###
colnames(dna) <- c("DOI", "Title", "Authors", "Author_Affiliations", "Corresponding_Authors", "Corresponding_Author_Email", "Publication_Date", "Abstract", "Keywords","Full_text")
