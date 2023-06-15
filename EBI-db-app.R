# Goal: Fetch info from Norwegian submissions in PRIDE
# Plot them in ggplot
# Shiny
# June 2023, Illimar Rekand (illimar.rekand@uib.no)

library(httr)
library(jsonlite)
library(ggplot2)
library(wesanderson) #colorpalette
library(shiny)


#size limits the search to the first n hits. Otherwise, only 15 first are included
# URL for the fields available in PRIDE: https://www.ebi.ac.uk/ebisearch/metadata.ebi?db=pride
query = "https://www.ebi.ac.uk/ebisearch/ws/rest/pride?query=submitter_country:Norway&size=1000&fields=submission_date,publication_date&format=json"

#SimplifyDataFrame = T enables data retrieved into a dataframe format
# Sometimes the command below crashes. giving a CacheKey error. If this happens, restart R session (ctrl+shift+F10)
datasets <- fromJSON(query, flatten = TRUE, simplifyDataFrame = TRUE)

datasets

typeof(datasets)
typeof(datasets[[2]])
dim(datasets[[2]])

datasets.df <- as.data.frame(datasets[[2]])
datasets.df[1,3]
#convert to character first, because conversion directly to date does not work....
datasets.df$fields.submission_date <- as.character(datasets.df$fields.submission_date)
datasets.df$fields.submission_date <- format(as.Date(datasets.df$fields.submission_date, format = "%Y%m%d"), "%Y")

datasets.df$fields.publication_date <- as.character(datasets.df$fields.publication_date)
datasets.df$fields.publication_date <- format(as.Date(datasets.df$fields.publication_date, format = "%Y%m%d"), "%Y")

#datasets.df$fields.publication_date <- DateViaChar(datasets.df$fields.publication_date)
#typeof(datasets.df$fields.publication_date)

pal <- wes_palette("Zissou1", 11, type = "continuous")

ggplot(datasets.df, aes(x= fields.submission_date, fill = as.factor(after_stat(count))))+ #as.factor harmonizes the color-palette with fill function of the graph
  geom_bar() + 
  theme_classic() + 
  scale_fill_manual(values = pal) +
  theme(legend.position = "none")
ggplot(datasets.df, aes(x= fields.publication_date)) + geom_bar() + theme_classic()


