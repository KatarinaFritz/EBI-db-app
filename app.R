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

pal <- wes_palette("GrandBudapest2", 11, type = "continuous")

## Below are some ggplots for debugging purposes
#test_plot <- ggplot(datasets.df, aes_string("fields.publication_date"))+ #as.factor harmonizes the color-palette with fill function of the graph
#  geom_bar(aes(fill = as.factor(after_stat(count)))) + 
#  theme_classic() + 
# scale_fill_manual(values = pal) +
#  theme(legend.position = "none")
#test_plot

#test_plot_2 <- ggplot(datasets.df, aes(x= fields.publication_date, fill = as.factor(after_stat(count))))+ #as.factor harmonizes the color-palette with fill function of the graph
#  geom_bar() + 
#  theme_classic() + 
#  scale_fill_manual(values = pal) +
#  theme(legend.position = "none")
#test_plot_2

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
                    "Publication date" = "fields.publication_date")),
      
      
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
    ggplot(datasets.df, aes_string(x = input$variable))+ #aes_string is necessary because input$variable is a string
      geom_bar(aes(fill = as.factor(after_stat(count)))) + #fill-component in aes needs to be declared here, because it is not compatible with aes_string
      theme_classic() + #remove gridlines
      scale_fill_manual(values = pal) + #enables the custom palette from wes_palette
      theme(legend.position = "none") # No legend
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)