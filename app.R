#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


pacman::p_load(tidyverse, shiny, shinydashboard, DT, palmerpenguins, lubridate)



# Define UI for application that draws a histogram
ui <- navbarPage("Microteach: Learn some data manipulation!",
                 
                 tabPanel(
                     "Quadrats",
                     
                     # Sidebar with a slider input for number of bins 
                     sidebarLayout(
                         sidebarPanel(
                             p("Choose new column names"),
                             actionButton("quadrat_go", "then Click here to pivot!"),
                             p(""),
                             p("Enter name for new column that will contain old column names"),
                             textInput("quadrat_names", "names_to = ", ""),
                             p("Enter name for new column that will contain values"),
                             textInput("quadrat_values", "values_to = ", "")
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                             tableOutput("quadrat_Table"),
                             verbatimTextOutput("quadrat_code"),
                             tableOutput("quadrat_pivoted"),
                             plotOutput("quadrat_plot", width = "60%", height = "200px")
                         )
                     )),
                 
                 
                 tabPanel(
                     "Finish Times",
                     
                     # Sidebar with a slider input for number of bins 
                     sidebarLayout(
                         sidebarPanel(
                             p("Choose new column names"),
                             actionButton("times_go", "then Click here to pivot!"),
                             p(""),
                             p("Enter name for new column that will contain old column names"),
                             textInput("times_names", "names_to = ", ""),
                             p("Enter name for new column that will contain values"),
                             textInput("times_values", "values_to = ", "")
                         ),
                         
                         # Show a plot of the generated distribution
                         mainPanel(
                             tableOutput("times_Table"),
                             verbatimTextOutput("times_code"),
                             tableOutput("times_pivoted")
                         )
                     )),
                 
                 
                 tabPanel(
                   "Penguins",
                   
                   # Sidebar with a slider input for number of bins 
                   sidebarLayout(
                       sidebarPanel(
                           actionButton("penguin_go","Fill in blanks, then Click here to pivot!"),
                           p(""),
                           p("Enter column names to combine"),
                           p("*case sensitive"),
                           textInput("penguin_col_start", "start column = ", ""),
                           textInput("penguin_col_end", "end column = ", ""),
                           p("Enter name for new column that will contain old column names"),
                           textInput("penguin_names", "names_to = ", ""),
                           p("Enter name for new column that will contain values"),
                           textInput("penguin_values", "values_to = ", "")
                       ),
                       
                       # Show a plot of the generated distribution
                       mainPanel(
                           tableOutput("penguin_Table"),
                           verbatimTextOutput("penguin_code"),
                           tableOutput("penguin_pivoted")
                       )
                   ))
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #### Quadrats #####
    quadrat_data <- tibble(
        Quadrat = c("Q_1", "Q_2"),
        SpeciesA = c(10, 4),
        SpeciesB = c(12, 3),
        SpeciesC = c(15, 4)
    )
    
    output$quadrat_Table <- renderTable({
        quadrat_data
    })
    
    output$quadrat_code <- renderPrint({
        cat("pivot_longer(data,", 
            "\n\tcols = SpeciesA:SpeciesC",
            "\n\tnames_to = ", input$quadrat_names,
            "\n\tvalues_to = ", input$quadrat_values, " )")
    })
    
    quadrat_data_pivot <- eventReactive(input$quadrat_go, {
        req(input$quadrat_names)
        quadrat_data %>%
            pivot_longer(.,
                         cols = SpeciesA:SpeciesC,
                         names_to = paste0(input$quadrat_names),
                         values_to = paste0(input$quadrat_values))
    }, ignoreNULL = FALSE)
    
    
    quadrat_pivot_plot <- eventReactive(input$quadrat_go, {
        req(input$quadrat_names)
        quadrat_data %>%
            pivot_longer(.,
                         cols = SpeciesA:SpeciesC,
                         names_to = paste0(input$quadrat_names),
                         values_to = paste0(input$quadrat_values)) %>%
            ggplot(., 
                   aes(x = get(paste0(input$quadrat_names)),
                       y = get(paste0(input$quadrat_values)),
                       fill = Quadrat))+
            geom_col(position = "dodge")+
            labs(x = paste0(input$quadrat_names), y = paste0(input$quadrat_values))+
            theme_light()
    }, ignoreNULL = FALSE)
    
    output$quadrat_pivoted <- renderTable({
        quadrat_data_pivot()
    })
    
    output$quadrat_plot <- renderPlot({
        quadrat_pivot_plot()
    })
    
    #### Times #####
    times_data <- tibble(
        Athlete = c("Alice", "Olli"),
        run1 = c("28:52", "22:39"),
        run2 = c("15:29", "22:25"),
        run3 = c("27:10", "20:56")
    )
    
    output$times_Table <- renderTable({
        times_data
    })
    
    output$times_code <- renderPrint({
        cat("pivot_longer(data,", 
            "\n\tcols = run1:run3",
            "\n\tnames_to = ", input$times_names,
            "\n\tvalues_to = ", input$times_values, " )")
    })
    
    times_data_pivot <- eventReactive(input$times_go, {
        req(input$times_names)
        times_data %>%
            pivot_longer(.,
                         cols = run1:run3,
                         names_to = paste0(input$times_names),
                         values_to = paste0(input$times_values))
    }, ignoreNULL = FALSE)
    
    output$times_pivoted <- renderTable({
        times_data_pivot()
    })
    
    
    
    #### Penguins #####
    output$penguin_Table <- renderTable({
        penguin_data <- penguins %>%
            select(species, bill_length_mm, flipper_length_mm, body_mass_g) %>%
            group_by(species) %>%
            slice(1)%>%
            ungroup()
        penguin_data
    })
    
    output$penguin_code <- renderPrint({
        cat("pivot_longer(data,", 
            "\n\tcols = ", input$penguin_col_start, ":", input$penguin_col_end,
            "\n\tnames_to = ", input$penguin_names,
            "\n\tvalues_to = ", input$penguin_values, " )")
    })
    
    penguin_data_pivot <- eventReactive(input$penguin_go, {
        req(input$penguin_names)
        penguins %>%
            select(species, bill_length_mm, flipper_length_mm, body_mass_g) %>%
            group_by(species) %>%
            slice(1)%>%
            ungroup() %>%
            pivot_longer(.,
                         cols = input$penguin_col_start : input$penguin_col_end,
                         names_to = paste0(input$penguin_names),
                         values_to = paste0(input$penguin_values))
    }, ignoreNULL = FALSE)
    
    output$penguin_pivoted <- renderTable({
        penguin_data_pivot()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
