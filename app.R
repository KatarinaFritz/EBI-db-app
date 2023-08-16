# Goal: Fetch info from Norwegian submissions in PRIDE
# Plot them in ggplot
# Shiny
# June 2023, Illimar Rekand (illimar.rekand@uib.no)
# ELIXIR Bergen, UiB
# To run the Shiny app, use runApp("EBI-db-app-local") in the console in dir where the EBI-db-app-local folder is located

#library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(shiny)
library(tidyverse)

library(ggwordcloud)
library(wordcloud)

find.count <- function(df, column){ #this functions creates a ggplot object and extracts the counts which are normally plotte don the y-variable
  plot <- ggplot(df, aes(x = column)) + geom_bar()
  plot.layer <- layer_data(plot, i = 1L) #extract info from the ggplot object
  plot.count <- plot.layer$count #gives the count from the ggplot layer
  return(plot.count)
}

#counts <- find.count(datasets.df, "fields.disease")
#counts

#size limits the search to the first n hits. Otherwise, only 15 first are included
# URL for the fields available in PRIDE: https://www.ebi.ac.uk/ebisearch/metadata.ebi?db=pride
query = "https://www.ebi.ac.uk/ebisearch/ws/rest/pride?query=submitter_country:Norway&size=1000&fields=submission_date,publication_date,labhead_affiliation,submitter,submitter_country,submitter_affiliation,labhead_mail,labhead,submitter_keywords,search_count,view_count,download_count,citation_count,technology_type,instrument_platform,tissue,species,species_suggester,omics_type,disease,disease_suggester,modification&format=json"

#SimplifyDataFrame = T enables data retrieved into a dataframe format
# Sometimes the command below crashes. giving a CacheKey error. If this happens, restart R session (ctrl+shift+F10)
datasets.raw <- fromJSON(query, flatten = TRUE, simplifyDataFrame = TRUE)

#datasets.df <- do.call(rbind, datasets.raw) #convert to dataframe
datasets.df <- datasets.raw$entries
datasets.df


class(datasets.df)
dim(datasets.df)


#convert to character first, because conversion directly to date does not work....
datasets.df$fields.submission_date <- as.character(datasets.df$fields.submission_date)
datasets.df$fields.submission_date <- format(as.Date(datasets.df$fields.submission_date, format = "%Y%m%d"), "%Y")

datasets.df$fields.publication_date <- as.character(datasets.df$fields.publication_date)
datasets.df$fields.publication_date <- format(as.Date(datasets.df$fields.publication_date, format = "%Y%m%d"), "%Y")

#datasets.df$fields.labhead_affiliation <- as.character(datasets.df$fields.labhead_affiliation)
#datasets.df$fields.labhead_mail  <- as.character(datasets.df$fields.labhead_mail)
#datasets.df$fields.species  <- as.character(datasets.df$fields.species)
#datasets.df$fields.disease  <- as.character(datasets.df$fields.disease)

#datasets.df$fields.labhead_affiliation
#datasets.df %>% 
  #mutate(affiliation_shortened = if(grepl("Bergen", datasets.df$fields.labhead_affiliation)) "UiB") %>%
  #fill(affiliation_shortened) %>%
  #as.data.frame()

#lapply(datasets.df$fields.labhead_affiliation, function(x) if(identical(x, character(0))) "missing" else x)

datasets.df$affiliation_shortened <- "other" # All institutiions which do not below to the lines below will be binned under the same label
                                             # Institution names are written in free-form, so the below text mining is necessary to group the affiliations properly
                                             # Below we are mining both the affiliation and the e-mail fields


datasets.df$affiliation_shortened <- ifelse(grepl("Science and Technology", as.character(datasets.df$fields.labhead_affiliation), ignore.case =  T), "NTNU", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("Bergen", datasets.df$fields.labhead_affiliation, ignore.case = T)|grepl("PROBE", datasets.df$fields.labhead_affiliation)|grepl("UiB", datasets.df$fields.labhead_affiliation), "UiB", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("uib", datasets.df$fields.labhead_mail, ignore.case =  T), "UiB", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("Oslo", datasets.df$fields.labhead_affiliation), "UiO", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("uio", datasets.df$fields.labhead_mail), "UiO", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("Tuula", datasets.df$fields.labhead_mail, ignore.case = T), "UiO", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("UiT", datasets.df$fields.labhead_affiliation, ignore.case = T)|grepl("Troms", datasets.df$fields.labhead_affiliation, ignore.case = T), "UiT", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("uit", datasets.df$fields.labhead_mail), "UiT", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("Life Sciences", datasets.df$fields.labhead_affiliation, ignore.case = T)|grepl("NMBU", datasets.df$fields.labhead_affiliation, ignore.case = T), "NMBU", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("Nord university", datasets.df$fields.labhead_affiliation, ignore.case =  T), "NORD", datasets.df$affiliation_shortened)
datasets.df$affiliation_shortened <- ifelse(grepl("UiS", datasets.df$fields.labhead_affiliation, ignore.case =  T)|grepl("Stavanger", datasets.df$fields.labhead_affiliation, ignore.case =  T), "UiS", datasets.df$affiliation_shortened)



##dataframes below are for debugging


#filtered <- datasets.df %>% group_by(fields.species) %>% filter(n() > 5)
#x_value = "fields.publication_date"
#test_plot <- ggplot(datasets.df, aes(!!sym(x_value)))+ #as.factor harmonizes the color-palette with fill function of the graph
  #geom_bar(aes()) + 
  #theme_classic() + 
 #scale_fill_manual(values = pal) +
  ##coord_flip() +
  #theme(legend.position = "none")
#test_plot
### Below are some ggplots for debugging purposes

#x_value = "affiliation_shortened"
##df.unpackt <- datasets.df %>% separate_longer_delim(!!sym(x_value), delim = ",")
#df.unpackt <- bind_rows(datasets.df$entries) %>%
  #unnest(!!sym(x_value))
#df.unpackt[[x_value]]
#dim(df.unpackt)
#min_value = 0
#order_bars <- FALSE

#test_plot_2 <- ggplot(df.unpackt  %>% group_by(!!sym(x_value)) %>% filter(n() > min_value), aes( #!!sym() converts character to variable
  #if(order_bars == TRUE) {x = fct_rev(fct_infreq(!!sym(x_value)))} 
  #else {x = !!sym(x_value)}
#))+ #input$variable is a string, !!sym() converts them into symbols
  #geom_bar(aes(fill = as.factor(after_stat(count)))) + #fill-component in aes needs to be declared here, because it is not compatible with aes_string ##this is probably not necessary after all with the impl of !!sym(), but we will keep it to make it easier to read
  #theme_classic() + #remove gridline +
  #theme(axis.text.x = element_text(angle = -45)) +
  #scale_fill_manual(values = pal) + #enables the custom palette from wes_palette
  #theme(legend.position = "none") # No legend
#test_plot_2

##ggplot(datasets.df, aes(x= fields.publication_date)) + geom_bar() + theme_classic()

#x_value = "fields.tissue"
#y_value = "fields.species"
#heatmap_test <- ggplot(datasets.unpackt %>% group_by(!!sym(x_value)) %>% filter(n() > 0), 
                       #aes(y = !!sym(y_value), x = as.character(!!sym(x_value)), fill = as.factor(after_stat(count)))
                       #) + 
  #geom_bin2d() +
  #theme_classic() +
  #scale_fill_manual(values = wes_palette("Zissou1", n = 8, type = "continuous"))

#-------------------



wordcloud <- ggplot(datasets.unpackt, aes(label = unique(fields.submitter_keywords))) +
                    geom_text_wordcloud() +
                    theme_minimal()


#wordcloud
#--------------------------



## Barplots
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
                    "Affiliation" = "affiliation_shortened",
                    "Species" = "fields.species",
                    "Disease" = "fields.disease",
                    "Tissue" = "fields.tissue",
                    "Instrument" = "fields.instrument_platform",
                    "Modifications" = "fields.modification",
                    "Keywords" = "fields.submitter_keywords",
                    "Omics" = "fields.omics_type"
                    )),
      uiOutput("numeric")  #This renders the slider input for some plots
      
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

exceptions <- c("fields.publication_date", "fields.submission_date") #these bar_plots will be ordered in chronological order, not after count

# Define server logic to plot various variables against count ----
server <- function(input, output) { #shiny passes selectInput as a string. To use these variables for subsetting dataframes, use e.g. df$!!sym(input$variable) or df[[input$variable]]
  
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
  
  # Render slider input for certain plots
  
  output$numeric <- renderUI({
    if(!(input$variable %in% exceptions)) { #if the input is NOT among the exceptions, render the min+max input options
      #find.count(datasets.df)
      list( # for the inputs below to be succesfully rendered inside an if-statement, we need to wrap them inside a list
      numericInput(inputId = "min","Exclude entries with counts lower than", 1),
      numericInput(inputId = "max","Exclude entries with counts higher than", 1)
      )
    }
  }
    
  )

  # Generate a plot of the requested variable against count ----

  output$ebi.plot <- renderPlot({ #The fields below are sorted chronologically, not after count5
    #counts <- find.count(datasets.df, input$variable)
    #print(counts)
    if(input$variable %in% exceptions
    ){order_bars <- FALSE
      min_value <- 1
      }
    else{
      order_bars <- TRUE
      min_value <- input$min 
    }
    #datasets.tibble <- as.tibble(datasets.df)
    if(class(datasets.df[[input$variable]])=="list"){
      df.unpackt <- (unpack <- datasets.df %>%
                       rowwise() |>
                       mutate(
                         fields.species =
                           list(as.character(!!sym(input$variable)))) |>
                       unnest_longer(!!sym(input$variable)))
      plot.df <- df.unpackt
    }
    else{
      plot.df <- datasets.df
    }
    ggplot(plot.df  %>% group_by(!!sym(input$variable))
           %>% filter(n() > min_value),
           aes( fill = affiliation_shortened, #!!sym() converts character to variable
      if(order_bars == TRUE) {x = fct_rev(fct_infreq(!!sym(input$variable)))} #sort bars after count, lowest to highest
      else {x = !!sym(input$variable)} #sort chronologically
      ))+ #input$variable is a string, !!sym() converts them into symbols
      geom_bar(position = "stack") + #fill-component in aes needs to be declared here, because it is not compatible with aes_string ##this is probably not necessary after all with the impl of !!sym(), but we will keep it to make it easier to read
      theme_classic() + #remove gridline
      theme(axis.text.x = element_text(angle = -45)) +
      scale_fill_brewer(palette = "Paired")
      #theme(legend.position = "none") # No legend
  })
  
}
shinyApp(ui, server)


## heatmaps
ui <- fluidPage(
  
  # App title ----
  titlePanel("Number of datasets per year"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for variable to plot against count ----
      # The below variables will be output as character strings
      selectInput("xvar", "X Variable:",
                  c("Tissue" = "fields.tissue",
                    "Species" = "fields.species",
                    "Technology Type" = "fields.technology_type",
                    "Instrument" = "fields.instrument_platform",
                    "Disease" = "fields.disease",
                    "Keywords" = "fields.submitter_keywords"
                  )),
      selectInput("yvar", "Y Variable:",
                  c("Species" = "fields.species", 
                    "Tissue" = "fields.tissue",
                    "Technology Type" = "fields.technology_type",
                    "Instrument" = "fields.instrument_platform",
                    "Disease" = "fields.disease",
                    "Keywords" = "fields.submitter_keywords"
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
# Create Shiny app ----

shinyApp(ui, server)


server <- function(input, output) {

  
  # Generate a plot of the requested variable against count ----
  
  output$ebi.plot <- renderPlot({
      heatmap <- ggplot(datasets.unpackt %>% group_by(!!sym(input$xvar)) %>% filter(n() > 0), 
                             aes(y = !!sym(input$yvar), x = as.character(!!sym(input$xvar)), fill = as.factor(after_stat(count)))
                            ) + 
                            geom_bin2d() +
                            theme_classic()
      heatmap.layer <- layer_data(heatmap, i = 1L) #extract info from the ggplot object
      n_colors  <- length(unique(heatmap.layer$count)) #computes the number of colors which will be used, based on the count
      heatmap + scale_fill_manual(values = wes_palette("Zissou1", n = n_colors, type = "continuous")) 
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
