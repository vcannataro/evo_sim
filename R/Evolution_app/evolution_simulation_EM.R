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
      dataTableOutput("test_ht")
    )
  )
  
  
  # verbatimTextOutput("text")
)

server <- function(input, output) {
  
  population_structure_df <- reactiveValues(data=NULL)

  
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
    
    population_structure_df$data <- data.frame(color_reps=sample(c(rep("wild type",cells-mutant),rep("mutant",mutant)),replace = F),
                                          rate=rep(1,cells))
    population_structure_df$data[,"angle"] <- seq(90,360+90,length.out = nrow(population_structure_df$data)+1)[1:nrow( population_structure_df$data)]
    
    population_structure_df$data[,"x_pos"] <- rad * cos(population_structure_df$data[,"angle"]*pi/180)  
    population_structure_df$data[,"y_pos"] <- rad * sin(population_structure_df$data[,"angle"]*pi/180)  
    population_structure_df$data[,"cell_number"] <- 1:nrow(population_structure_df$data)
    
    output$testplot <- renderPlot({ggplot(data = population_structure_df$data, aes(x=x_pos,y=y_pos)) + 
        geom_point(aes(fill=color_reps),size=cell_size,shape=21,stroke=2) + 
        theme_no_axes() + 
        coord_cartesian(xlim = c(-1,1),ylim=c(-1,1)) + 
        scale_fill_discrete(name="Cell type") + 
        geom_text(aes(label=cell_number))
      
        # return(population_structure_df)
      
      
    })
  })
  
  
  # update population
  observeEvent(input$update_pop, {
    
    cells <- isolate({input$cells})
    cell_size <- isolate({input$cell_size})
    # static_df <- population_structure_df$data
    
    cell_pick <- isolate({input$cell_pick})
    HT <- isolate({input$HT})
    
    
    
    if(HT=="H") {
      cell_replace <- if(cell_pick>1){cell_pick - 1}else{cells} # if value == 1, pick largest value 
      population_structure_df$data[cell_replace,"color_reps"] <-  population_structure_df$data[cell_pick,"color_reps"] 
    }
    if(HT=="T") {
      
      cell_replace <- if(cell_pick<cells){cell_pick+1}else{1}
      
      population_structure_df$data[cell_replace,"color_reps"] <-  population_structure_df$data[cell_pick,"color_reps"] 
    }
    
    arrow_start_x <- population_structure_df$data[cell_pick,"x_pos"]
    arrow_end_x <- population_structure_df$data[cell_replace,"x_pos"]
    arrow_start_y <- population_structure_df$data[cell_pick,"y_pos"]
    arrow_end_y <- population_structure_df$data[cell_replace,"y_pos"]
    
    output$testplot <- renderPlot({ggplot(data = population_structure_df$data, aes(x=x_pos,y=y_pos)) + 
        geom_point(aes(fill=color_reps),size=cell_size,shape=21,stroke=2) + 
        geom_point(data=population_structure_df$data[c(cell_pick,cell_replace),],
                   aes(x=x_pos,y=y_pos,fill=color_reps),
                   color="yellow",shape=21,stroke=2,size=cell_size) + 
        geom_segment(aes(x=arrow_start_x,
                         xend=arrow_end_x,
                         y=arrow_start_y,
                         yend=arrow_end_y),
                     color="black",size=1,arrow = arrow(length = unit(0.3, "inches"))) + 
        theme_no_axes() + 
        coord_cartesian(xlim = c(-1,1),ylim=c(-1,1)) + 
        scale_fill_discrete(name="Cell type")})
    
    output$test_ht <- renderDataTable({population_structure_df$data})
    
    
    
  })
  
  
  
  
}

shinyApp(ui = ui, server = server)