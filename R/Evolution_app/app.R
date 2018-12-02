#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Evolution simulation"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
         sliderInput("cells",
                     "Number of cells:",
                     min = 1,
                     max = 50,
                     value = 6),
         sliderInput("cell_size",
                     "Cell size:",
                     min = 1,
                     max = 40,
                     value = 40),
         actionButton("pop_generate","Generate population")
      ),

      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("cellPlot")
      )
   )
)

# Define server logic required to draw plots and interact
server <- function(input, output) {
   
  observeEvent(input$pop_generate, {
    rad <- 0.75
    cells <- input$cells
    mutant <- 2
    cell_size <- input$cell_size
    # cell_size <- 10
    
    population_structure_df <- data.frame(color_reps=sample(c(rep("wild type",cells-mutant),rep("mutant",mutant)),replace = F),
                                          rate=rep(1,cells))
    population_structure_df$angle <- seq(90,360+90,length.out = nrow(population_structure_df)+1)[1:nrow( population_structure_df)]
    
    population_structure_df$x_pos <- rad * cos(population_structure_df$angle*pi/180)  
    population_structure_df$y_pos <- rad * sin(population_structure_df$angle*pi/180)  
    # (ggplot(data = population_structure_df, aes(x=x_pos,y=y_pos)) + geom_point(aes(color=color_reps),size=cell_size) + theme_no_axes() + coord_cartesian(xlim = c(-1,1),ylim=c(-1,1)) + scale_color_discrete(name="Cell type"))
  })
  
   output$cellPlot <- renderPlot({

     (ggplot(data = population_structure_df, aes(x=x_pos,y=y_pos)) + geom_point(aes(color=color_reps),size=cell_size) + theme_no_axes() + coord_cartesian(xlim = c(-1,1),ylim=c(-1,1)) + scale_color_discrete(name="Cell type"))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

