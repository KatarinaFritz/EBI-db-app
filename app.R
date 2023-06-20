# Goal: Fetch info from Norwegian submissions in PRIDE
# Plot them in ggplot
# Shiny
# June 2023, Illimar Rekand (illimar.rekand@uib.no)
# ELIXIR Bergen, UiB
# To run the Shiny app, use runApp("EBI-db-app-local") in the console in dir where the EBI-db-app-local folder is located

library(httr)
library(jsonlite)
library(ggplot2)
library(wesanderson) #colorpalette
library(shiny)
library(tidyverse)
library(dplyr)


#size limits the search to the first n hits. Otherwise, only 15 first are included
# URL for the fields available in PRIDE: https://www.ebi.ac.uk/ebisearch/metadata.ebi?db=pride
query = "https://www.ebi.ac.uk/ebisearch/ws/rest/pride?query=submitter_country:Norway&size=1000&fields=submission_date,publication_date,labhead_affiliation,submitter_affiliation,labhead_mail,labhead,search_count,view_count,download_count,citation_count,omics_type,species&format=json"

#SimplifyDataFrame = T enables data retrieved into a dataframe format
# Sometimes the command below crashes. giving a CacheKey error. If this happens, restart R session (ctrl+shift+F10)
datasets <- fromJSON(query, flatten = TRUE, simplifyDataFrame = TRUE)

datasets

typeof(datasets)
typeof(datasets[[2]])
dim(datasets[[2]])

datasets.df <- as.data.frame(datasets[[2]])
#convert to character first, because conversion directly to date does not work....
datasets.df$fields.submission_date <- as.character(datasets.df$fields.submission_date)
datasets.df$fields.submission_date <- format(as.Date(datasets.df$fields.submission_date, format = "%Y%m%d"), "%Y")

datasets.df$fields.publication_date <- as.character(datasets.df$fields.publication_date)
datasets.df$fields.publication_date <- format(as.Date(datasets.df$fields.publication_date, format = "%Y%m%d"), "%Y")

datasets.df$fields.labhead_affiliation <- as.character(datasets.df$fields.labhead_affiliation)
datasets.df$fields.labhead_mail  <- as.character(datasets.df$fields.labhead_mail)


#datasets.df$fields.labhead_affiliation
#datasets.df %>% 
  #mutate(affiliation_shortened = if(grepl("Bergen", datasets.df$fields.labhead_affiliation)) "UiB") %>%
  #fill(affiliation_shortened) %>%
  #as.data.frame()

#lapply(datasets.df$fields.labhead_affiliation, function(x) if(identical(x, character(0))) "missing" else x)

datasets.df$fields.labhead_affiliation

datasets.df$affiliation_shortened <- ifelse(grepl("Science and Technology", datasets.df$fields.labhead_affiliation, ignore.case =  T), "NTNU", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("Bergen", datasets.df$fields.labhead_affiliation, ignore.case = T)|grepl("PROBE", datasets.df$fields.labhead_affiliation)|grepl("UiB", datasets.df$fields.labhead_affiliation), "UiB", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("uib", datasets.df$fields.labhead_mail, ignore.case =  T), "UiB", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("Oslo", datasets.df$fields.labhead_affiliation), "UiO", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("uio", datasets.df$fields.labhead_mail), "UiO", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("UiT", datasets.df$fields.labhead_affiliation, ignore.case = T)|grepl("Troms", datasets.df$fields.labhead_affiliation, ignore.case = T), "UiT", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("uit", datasets.df$fields.labhead_mail), "UiT", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("Life Sciences", datasets.df$fields.labhead_affiliation, ignore.case = T)|grepl("NMBU", datasets.df$fields.labhead_affiliation, ignore.case = T), "NMBU", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("Nord university", datasets.df$fields.labhead_affiliation, ignore.case =  T), "NORD", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("UiS", datasets.df$fields.labhead_affiliation, ignore.case =  T)|grepl("Stavanger", datasets.df$fields.labhead_affiliation, ignore.case =  T), "UiS", datasets.df$affiliation_shortened)


##dataframes below are for debugging

#debug.df <- datasets.df[, c("id",
                            ##"fields.labhead_mail",
                            #"affiliation_shortened", "fields.labhead_affiliation")]
#debug.df

#write.table(debug.df, file = "debug-df.txt", sep = "\t")


#datasets.df$affiliation_shortened <- lapply(datasets.df$affiliation_shortened, function(x) if(grepl("Bergen", datasets.df$fields.labhead_affiliation)) "UiB" else x)
datasets.df$fields.labhead_affiliation


pal <- wes_palette("GrandBudapest2", 11, type = "continuous")

## Below are some ggplots for debugging purposes
#test_plot <- ggplot(datasets.df, aes_string("fields.publication_date"))+ #as.factor harmonizes the color-palette with fill function of the graph
#  geom_bar(aes(fill = as.factor(after_stat(count)))) + 
#  theme_classic() + 
# scale_fill_manual(values = pal) +
#  theme(legend.position = "none")
#test_plot

reorder <- TRUE
test_plot_2 <- ggplot(datasets.df, aes(
  if(reorder == TRUE){ x= fct_rev(fct_infreq(affiliation_shortened)) }
  else { x= affiliation_shortened },
  fill = as.factor(after_stat(count))))+ #as.factor harmonizes the color-palette with fill function of the graph
  geom_bar() + 
  theme_classic() + 
  scale_fill_manual(values = pal) +
  theme(legend.position = "none")
test_plot_2

#ggplot(datasets.df, aes(x= fields.publication_date)) + geom_bar() + theme_classic()


# Define UI for EBI-db-app app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Number of datasets per year"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against count ----
      # The below variables will be output as character strings
      selectInput("variable", "Variable:",
                  c("Submission date" = "fields.submission_date", 
                    "Publication date" = "fields.publication_date",
                    "Affiliation" = "affiliation_shortened"
                    )),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against count ----
      plotOutput("ebi.plot")
      
    )
  )
)

# Define server logic to plot various variables against count ----
server <- function(input, output) {
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$ebi.plot functions
  formulaText <- reactive({
    paste("after_stat(count) ~", input$variable)
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against count ----
  output$ebi.plot <- renderPlot({
    order_bars <- FALSE #we want institutions to be ordered after frequency, but not the years of publications
    if(input$variable == "affiliation_shortened"){order_bars <- TRUE}
    ggplot(datasets.df, aes(
      if(order_bars == TRUE) {x = fct_rev(fct_infreq(!!sym(input$variable)))}
      else {x = !!sym(input$variable)}
      ))+ #input$variable is a string, !!sym() converts them into symbols
      geom_bar(aes(fill = as.factor(after_stat(count)))) + #fill-component in aes needs to be declared here, because it is not compatible with aes_string ##this is probably not necessary after all with the impl of !!sym(), but we will keep it to make it easier to read
      theme_classic() + #remove gridlines
      scale_fill_manual(values = pal) + #enables the custom palette from wes_palette
      theme(legend.position = "none") # No legend
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)