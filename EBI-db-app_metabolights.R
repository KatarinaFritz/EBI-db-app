# Goal: Fetch info from Norwegian submissions in PRIDE
# Plot them in ggplot
# Create Shiny-applications
# November 2023, Illimar Rekand (illimar.rekand@uib.no, illimar.rekand@gmail.com)
# ELIXIR Bergen, Department of Informatics, University of Bergen

library(jsonlite)
library(dplyr)
library(ggplot2)
library(shiny)
library(tidyverse)
library(ggwordcloud)
library(wordcloud)
library(tm)

################################################################################################################
################################################ Functions #####################################################
################################################################################################################

unpack.df <- function(df, df.col){  # This function unpacks the lists inside a given column, and creates a single new row for each unpacked element, maintaining the id-field.
                                    # The count for the unpacked row is also given.
                                    # df is a dataframe, while df.col is a string containing the name of the column which is to be unpacked
                                    # returns a tibble
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
# URL for the fields available in PRIDE: https://www.ebi.ac.uk/ebisearch/metadata.ebi?db=metabolights
query = "https://www.ebi.ac.uk/ebisearch/ws/rest/metabolights?query=nmr&size=1000&fields=Author,metabolite_name,technology_type,name&format=json"
query = "https://www.ebi.ac.uk/ebisearch/ws/rest/metabolights?format=json"
query = "https://www.ebi.ac.uk/metabolights/ws/studies" #get all public studies from Metabolights

# Sometimes the command below crashes. giving a CacheKey error. If this happens, restart R session (ctrl+shift+F10)
study.list.raw <- fromJSON(query, flatten = TRUE, simplifyDataFrame = TRUE)

study.list <- study.list.raw$content
study.list.short <- c("MTBLS724", "MTBLS723", "MTBLS722", "MTBLS721", "MTBLS720") #for testing purposes

study.df <-as.data.frame(study.list)
study.inf.df <- apply(study.df, 1, function(study.id) {
  # Each study_id here is basically study.df[i,1]
  print(study.id)
  if(!file.exists(paste0("./JSONs/", study.id,".json"))) { #if the file does not exist, it will be downloaded
    print(paste0("File ", study.id, " does not exist, downloading this from Metabolights"))
    query = paste0("https://www.ebi.ac.uk:443/metabolights/ws/v1/study/", study.id)
    print(query)
    study.inf.raw <- fromJSON(query, flatten = TRUE, simplifyDataFrame = TRUE)
    write(toJSON(study.inf.raw), paste0("./JSONs/", study.id,".json"))
  } else{
    print("File exists!")
    print(study.id)
    study.inf.raw <- fromJSON(readLines(paste0("./JSONs/",study.id ,".json")))
  }
  country <- study.inf.raw$content$derivedData$country
  Users <- study.inf.raw$content$users
  Users.aff <- study.inf.raw$content$users$address
  SubmissionYear <- study.inf.raw$content$derivedData$submissionYear
  ReleaseYear <- study.inf.raw$content$derivedData$releaseYear
  Organism <- study.inf.raw$content$organism$organismName
  Keywords <- study.inf.raw$content$descriptors
  Technology <- study.inf.raw$content$assays$technology
  return.df <- data.frame(I(list(Users)), I(list(Users.aff)), study.id, country, SubmissionYear, ReleaseYear, I(list(Organism)), I(list(Technology)), I(list(Keywords))) 
  return.df
  return(return.df)
  
})

df.bound<- do.call(bind_rows, study.inf.df)
df.bound.NO <- subset(df.bound, country == "NO") #Find only Norwegian entries

df.bound.NO$SubmissionYear <- as.character(df.bound.NO$SubmissionYear)
df.bound.NO$ReleaseYear <- as.character(df.bound.NO$ReleaseYear)


df.bound.NO


################################################################################################################
################################################ Barplots ######################################################
################################################################################################################

exceptions <- c("SubmissionYear", "ReleaseYear")

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
                  c("Submission date" = "SubmissionYear",
                    "Publication date" = "ReleaseYear"
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
    paste("Entries to the Metabolights database from Norwegian institutions")
  })
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    formulaText()
  })
  
  # Render slider input for certain plots
  
  output$numeric <- renderUI({
    if(!(input$variable %in% exceptions)) { #if the input is NOT among the exceptions, render the min+max input options
      #find.count(df.bound.NO)
      df.unpackt <- unpack.df(df.bound.NO, input$variable)
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
      df.unpackt <- unpack.df(df.bound.NO, input$variable)
      plot.df <- df.unpackt %>% group_by(!!sym(input$variable)) %>% filter(n() >= min_value) %>% filter(n() <= max_value)
    }
    else{
      plot.df <- df.bound.NO
    }
    
    ggplot(df.bound.NO,
           aes( #fill = fields.affiliation, 
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
      ##theme(legend.position = "none") # No legend

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


ggplot(df.bound.NO,
       aes(x = SubmissionYear)) +
  geom_bar() +
  theme_classic()

