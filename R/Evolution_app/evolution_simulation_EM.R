# 05-actionButton

library(shiny)

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
      actionButton(inputId = "gen_pop", 
                   label = "Generate population"),
      hr(),
      
      textInput(inputId = "HT",label = "Heads or tails?",value = NULL),
      numericInput(inputId = "cell_pick",value = NULL,label = "Cell pick"),
      actionButton(inputId = "update_pop",label = "Update population")
      
    ),
    
    mainPanel(
      plotOutput("testplot"),
      verbatimTextOutput("test_ht")
    )
  )
  
  
  # verbatimTextOutput("text")
)

server <- function(input, output) {
  
  
  
  # (re)generate plot
  observeEvent(input$gen_pop, {
    cells <- isolate({input$cells})
    cell_size <- isolate({input$cell_size})
    # output$text <- renderPrint({print(as.numeric(input$clicks))})
    rad <- 0.75
    # cells <- input$cells
    # cells <- 6
    mutant <- 2
    # cell_size <- input$cell_size
    # cell_size <- 10
    
    population_structure_df <- data.frame(color_reps=sample(c(rep("wild type",cells-mutant),rep("mutant",mutant)),replace = F),
                                          rate=rep(1,cells))
    population_structure_df$angle <- seq(90,360+90,length.out = nrow(population_structure_df)+1)[1:nrow( population_structure_df)]
    
    population_structure_df$x_pos <- rad * cos(population_structure_df$angle*pi/180)  
    population_structure_df$y_pos <- rad * sin(population_structure_df$angle*pi/180)  
    
    output$testplot <- renderPlot({ggplot(data = population_structure_df, aes(x=x_pos,y=y_pos)) + 
        geom_point(aes(color=color_reps),size=cell_size) + 
        theme_no_axes() + 
        coord_cartesian(xlim = c(-1,1),ylim=c(-1,1)) + 
        scale_color_discrete(name="Cell type")
      
      
    })
  })
  
  # update population
  observeEvent(input$update_pop, {
    cell_pick <- isolate({input$cell_pick})
    HT <- isolate({input$HT})
    output$test_ht <- renderText(HT)
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)