#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
load("../shiny/www/my_work_space.RData")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Gun Violence Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                           choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                           selected = 1),
        
        
  #      hr(),
   #     fluidRow(column(3, verbatimTextOutput("value")))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
       )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  #output$value <- renderPrint({ input$checkGroup })
  
   output$distPlot <- renderPlot({
   #    # generate bins based on input$bins from ui.R
   #    x    <- faithful[, 2] 
   #    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   #    
   #    # draw the histogram with the specified number of bins
   #    hist(x, breaks = bins, col = 'darkgray', border = 'white')
     fviz_cluster(list(data = df, cluster = grp),
                  palette = c("#2E9FDF", "#00AFBB", "#E7B800" ), ellipse.type = "convex", # Concentration ellipse...extra color code"#FC4E07","#DF2E9F"
                  repel = TRUE, # Avoid label overplotting (slow) 
                  show.clust.cent = FALSE, ggtheme = theme_minimal()
     )
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

