# Goal: Fetch info from Norwegian submissions in PRIDE
# Plot them in ggplot
# Create Shiny-applications
# June 2023, Illimar Rekand (illimar.rekand@uib.no, illimar.rekand@gmail.com)
# ELIXIR Bergen, Department of Informatics, University of Bergen

library(jsonlite)
library(dplyr)
library(ggplot2)
library(shiny)
library(tidyverse)
library(ggwordcloud)

################################################################################################################
################################################ Functions #####################################################
################################################################################################################

unpack.df <- function(df, df.col){ # This function unpacks the lists inside a given column, and creates a single new row for each unpacked element, maintaining the id-field
  unpackt <- (df %>% 
       rowwise() |>
       mutate(
         field =
           list(as.character(!!sym(df.col)))) |>
       unnest_longer(!!sym(df.col)))
        unpackt.w.count <- unpackt %>% group_by(!!sym(df.col)) %>% add_count() #adds counts for the relevant column #!!sym() converts character to variable
return(unpackt.w.count)
}

################################################################################################################
################################################ Extracting data ###############################################
################################################################################################################




#size limits the search to the first n hits. Otherwise, only 15 first are included
# URL for the fields available in PRIDE: https://www.ebi.ac.uk/ebisearch/metadata.ebi?db=pride
query = "https://www.ebi.ac.uk/ebisearch/ws/rest/pride?query=submitter_country:Norway&size=1000&fields=submission_date,publication_date,labhead_affiliation,submitter,submitter_country,submitter_affiliation,labhead_mail,labhead,submitter_keywords,search_count,view_count,download_count,citation_count,technology_type,instrument_platform,tissue,species,species_suggester,omics_type,disease,disease_suggester,modification&format=json"

#SimplifyDataFrame = T enables data retrieved into a dataframe format
# Sometimes the command below crashes. giving a CacheKey error. If this happens, restart R session (ctrl+shift+F10)
datasets.raw <- fromJSON(query, flatten = TRUE, simplifyDataFrame = TRUE)

datasets.df <- datasets.raw$entries
datasets.df

################################################################################################################
################################################ Cleanup #######################################################
################################################################################################################




#we extract only the years from the dates below, to make plotting later on easier.
#convert to character first, because conversion directly to date does not work....
datasets.df$fields.submission_date <- as.character(datasets.df$fields.submission_date)
datasets.df$fields.submission_date <- format(as.Date(datasets.df$fields.submission_date, format = "%Y%m%d"), "%Y")
excluded_years <- c(2013, 2014, 2015, 2016, 2017, 2018)
datasets.df <- datasets.df[!(datasets.df$fields.submission_date %in% excluded_years), ]

datasets.df$fields.publication_date <- as.character(datasets.df$fields.publication_date)
datasets.df$fields.publication_date <- format(as.Date(datasets.df$fields.publication_date, format = "%Y%m%d"), "%Y")


datasets.df$fields.affiliation <- "notNAPI" # All institutiions which do not below to the lines below will be binned under the same label
                                             # Institution names are written in free-form, so the below text mining is necessary to group the affiliations properly
                                             # Below we are mining both the affiliation and the e-mail fields

datasets.df$fields.affiliation <- ifelse(grepl("Science and Technology", as.character(datasets.df$fields.labhead_affiliation), ignore.case =  T), "NTNU", datasets.df$fields.affiliation)
datasets.df$fields.affiliation <- ifelse(grepl("Bergen", datasets.df$fields.labhead_affiliation, ignore.case = T)|grepl("PROBE", datasets.df$fields.labhead_affiliation)|grepl("UiB", datasets.df$fields.labhead_affiliation), "UiB", datasets.df$fields.affiliation)
datasets.df$fields.affiliation <- ifelse(grepl("uib", datasets.df$fields.labhead_mail, ignore.case =  T), "UiB", datasets.df$fields.affiliation)
datasets.df$fields.affiliation <- ifelse(grepl("Oslo", datasets.df$fields.labhead_affiliation), "UiO", datasets.df$fields.affiliation)
datasets.df$fields.affiliation <- ifelse(grepl("uio", datasets.df$fields.labhead_mail), "UiO", datasets.df$fields.affiliation)
datasets.df$fields.affiliation <- ifelse(grepl("Tuula", datasets.df$fields.labhead_mail, ignore.case = T), "UiO", datasets.df$fields.affiliation)
datasets.df$fields.affiliation <- ifelse(grepl("UiT", datasets.df$fields.labhead_affiliation, ignore.case = T)|grepl("Troms", datasets.df$fields.labhead_affiliation, ignore.case = T), "UiT", datasets.df$fields.affiliation)
datasets.df$fields.affiliation <- ifelse(grepl("uit", datasets.df$fields.labhead_mail), "UiT", datasets.df$fields.affiliation)
datasets.df$fields.affiliation <- ifelse(grepl("Life Sciences", datasets.df$fields.labhead_affiliation, ignore.case = T)|grepl("NMBU", datasets.df$fields.labhead_affiliation, ignore.case = T), "NMBU", datasets.df$fields.affiliation)



################################################################################################################
################################################ Barplots ######################################################
################################################################################################################

#below, most variables will be sorted after frequency. The exceptions (dates) are added to a list
exceptions <- c("fields.publication_date", "fields.submission_date") #these bar_plots will be ordered in chronological order, not after count


## Barplots
# Define UI for EBI-db-app app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Choose input below"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Input: Selector for variable to plot against count ----
      # The below variables will be output as character strings
      selectInput("variable", "Variable:",
                  c("Submission date" = "fields.submission_date", 
                    "Publication date" = "fields.publication_date",
                    "Affiliation" = "fields.affiliation",
                    "Species" = "fields.species",
                    "Disease" = "fields.disease",
                    "Tissue" = "fields.tissue",
                    "Instrument" = "fields.instrument_platform",
                    "Modifications" = "fields.modification",
                    "Keywords" = "fields.submitter_keywords",
                    )),
      uiOutput("numeric"),  #This renders the slider input for some plots,
      downloadButton('downloadPlot')
      
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
server <- function(input, output) { #shiny passes selectInput as a string. To use these variables for subsetting dataframes, use e.g. df$!!sym(input$variable) or df[[input$variable]]
  
  # Compute the formula text ----
  # This is in a reactive expression since it is shared by the
  # output$caption and output$ebi.plot functions
  formulaText <- reactive({
    paste("Entries to the PRIDE database from NAPI partners since 2019")
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Render slider input for certain plots
  
  output$numeric <- renderUI({
    if(!(input$variable %in% exceptions)) { #if the input is NOT among the exceptions, render the min+max input options
      #find.count(datasets.df)
      df.unpackt <- unpack.df(datasets.df, input$variable)
      max.count <- max(df.unpackt$n)
      
      list( # for the inputs below to be succesfully rendered inside an if-statement, we need to wrap them inside a list
      numericInput(inputId = "min","Exclude entries with counts >=", 0),
      numericInput(inputId = "max","Exclude entries with counts <=", max.count)
      )
    }
  }
    
  )

  # Generate a plot of the requested variable against count ----
  bar_plot.reactive <- reactive({
    if(input$variable %in% exceptions
    ){order_bars <- FALSE
    }
    else{
      order_bars <- TRUE
      min_value <- input$min
      max_value <- input$max
    }
    if(!(input$variable %in% exceptions)){
      df.unpackt <- unpack.df(datasets.df, input$variable)
      plot.df <- df.unpackt %>% group_by(!!sym(input$variable)) %>% filter(n() >= min_value) %>% filter(n() <= max_value)
    }
    else{
      plot.df <- datasets.df
    }
    
    ggplot(plot.df,
                      aes( fill = fields.affiliation, 
                           if(order_bars == TRUE) {x = fct_rev(fct_infreq(!!sym(input$variable)))} #sort bars after count, lowest to highest
                           else {x = !!sym(input$variable)} #sort chronologically
                      ))+ #input$variable is a string, !!sym() converts them into symbols
      geom_bar(position = "stack") + #fill-component in aes needs to be declared here, because it is not compatible with aes_string ##this is probably not necessary after all with the impl of !!sym(), but we will keep it to make it easier to read
      xlab(paste(str_to_title( #Capitalize first words
        sub("_", " ", #replace underscores with spaces
            substring(input$variable, 8, nchar(input$variable)))))) +
      theme_classic() + #remove gridline
      theme(axis.text.x = element_text(angle = -45)) +
      scale_fill_brewer(palette = "Paired")
    #theme(legend.position = "none") # No legend
  })
  
  output$ebi.plot <- renderPlot(
    { #The fields below are sorted chronologically, not after count5
    bar_plot.reactive()
  }
  )
  output$downloadPlot <- downloadHandler(
    filename <- function()
    {paste0("PRIDE-plot-",input$variable,".png")},
    content <- function(file){
      png(file=file)
      plot(bar_plot.reactive())
      dev.off()
    }
  )
    
  
}
shinyApp(ui, server)


################################################################################################################
################################################ Heatmaps ######################################################
################################################################################################################


## heatmaps

field.list <- c("Submission date" = "fields.submission_date", 
                "Publication date" = "fields.publication_date",
                "Affiliation" = "fields.affiliation",
                "Species" = "fields.species",
                "Disease" = "fields.disease",
                "Tissue" = "fields.tissue",
                "Instrument" = "fields.instrument_platform",
                "Modifications" = "fields.modification",
                "Keywords" = "fields.submitter_keywords",
                "Omics" = "fields.omics_type"
)

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
                  field.list),
      selectInput("yvar", "Y Variable:",
                  field.list),
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against count ----
      plotOutput("ebi.plot"),
      
    )
  )
)
# Create Shiny app ----



server <- function(input, output) {

  
  # Generate a plot of the requested variable against count ----
  
  output$ebi.plot <- renderPlot({
    datasets.df
      heatmap <- ggplot(datasets.df, 
                             aes(y = as.character(!!sym(input$yvar)), x = as.character(!!sym(input$xvar)), fill = as.factor(after_stat(count)))
                            ) + 
                            geom_bin2d() +
                            theme_classic()
      heatmap
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)