library(shiny)
library(ggplot2)
library(RColorBrewer)


ui <- fluidPage(
  titlePanel("Dataset visualisation"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "dataset",
                  label = "Choose a dataset:",
                  choices = c("Iris flowers" = "iris", 
                              "Diamonds" = "diamonds",
                              "Orange trees" = "Orange")),
      selectInput(inputId = "plot",
                  label = "Select type of plot:",
                  choices = c("Dot plot" = "geom_point",
                              "Density plot" = "geom_density",
                              "Box plot" = "geom_boxplot",
                              "Bar plot" = "geom_bar"),
                  selected = "Dot plot"),
    uiOutput("conditionals"),
    conditionalPanel("input.plot == 'geom_bar'",
                     selectizeInput(inputId = "position_var",
                                    label = "Select type of position:",
                                    choices = c("Stack" = "position_stack",
                                                "Dodge" = "position_dodge",
                                                "Fill" = "position_fill"),
                                    options = list(
                                      placeholder = '',
                                      onInitialize = I('function() { this.setValue(""); }'))
                     )
    ),
    selectInput(inputId = "theme",
                label = "Choose a plot theme:",
                choices = c("Black and white" = "theme_bw",
                            "Grey" = "theme_grey",
                            "Light" = "theme_light",
                            "Dark" = "theme_dark")),
    selectInput(inputId = "palette",
                label = "Choose a color palette:",
                choices = c("Spectral" = "Spectral",
                            "Pastel colors" = "Pastel2",
                            "Bright colors" = "Set1",
                            "Dark colors" = "Dark2",
                            "Shades of blue" = "Blues"))
    ),
    mainPanel(
      plotOutput("myPlot")
    ),
  )
)



server <- function(input, output, session) {
  data <- reactive({ get(input$dataset) })
  user_columns <- reactive({
    switch(input$dataset,
           "iris" = c("Sepal length","Sepal width","Petal length","Petal width","Species"),
           "diamonds" = c("Carat","Cut","Color","Clarity","Depth percentage","Table","Price","Length","Width","Depth"),
           "Orange" = c("Tree","Age","Circumference"))
  })
  columns <- reactive({
    cols <- switch(input$dataset,
           "iris" = colnames(iris),
           "diamonds" = colnames(diamonds),
           "Orange" = colnames(Orange))
    names(cols) <- user_columns()
    cols
  })
  require_inputs <- reactive({
    switch(input$plot,
           "geom_point" = c("x","y","color","size","facet"),
           "geom_density" = c("x","color","facet"),
           "geom_boxplot" = c("x","y","color","facet"),
           "geom_bar" = c("x","color","facet"))
  })
  
  output$conditionals <- renderUI({
    numConditionals <- length(require_inputs())
    lapply(1:numConditionals, function(i) {
      selectizeInput(inputId = sprintf("%s_var", require_inputs()[i]),
                  label = sprintf("Select variable for %s:", require_inputs()[i]),
                  choices = columns(),
                  options = list(
                    placeholder = '',
                    onInitialize = I('function() { this.setValue(""); }')))
      })
    })
  output$myPlot <- renderPlot({
    if(input$plot %in% c("geom_point","geom_density","geom_bar"))req(input$x_var)
    if(input$plot %in% c("geom_point","geom_boxplot"))req(input$y_var)
    switch(input$plot,
           "geom_point" = {
             arg_x <- input$x_var
             arg_y <- input$y_var
             arg_dir <- "h"
             user_title <- sprintf("Scatter plot for %s dataset", input$dataset)
             user_x <- user_columns()[match(input$x_var, names(data()))]
             user_y <- user_columns()[match(input$y_var, names(data()))]
           },
           "geom_density" = {
             arg_x <- input$x_var
             user_x <- user_columns()[match(input$x_var, names(data()))]
             user_y <- "Density"
             if(input$facet_var != "")arg_dir <- "h"
             user_title <- sprintf("Density plot for %s dataset", input$dataset)
           },
           "geom_boxplot" = {
             arg_y <- input$y_var
             user_y <- user_columns()[match(input$y_var, names(data()))]
             user_x <- ""
             if(input$x_var != ""){
               arg_x <- input$x_var
               user_x <- user_columns()[match(input$x_var, names(data()))]
             }
             if(input$facet_var != "")arg_dir <- "v"
             user_title <- sprintf("Box plot for %s dataset", input$dataset)
           },
           "geom_bar" = {
             arg_x <- input$x_var
             user_x <- user_columns()[match(input$x_var, names(data()))]
             user_y <- "Value"
             arg_dir <- "h"
             user_title <- sprintf("Bar plot for %s dataset", input$dataset)
           }
    )
    user_color <- ""
    if(input$color_var != "") {
      user_color <- user_columns()[match(input$color_var, names(data()))]
    }
    ggplot(data()) +
      {if(input$x_var != "")aes(x = get(arg_x))} +
      {if(input$y_var != "")aes(y = get(arg_y))} +
      get(input$plot)() +
      {if(input$color_var != "")aes(color=get(input$color_var), fill=get(input$color_var), alpha=0.5)} +
      {if(input$size_var != "")aes(size=get(input$size_var))} +
      {if(input$position_var != "")geom_bar(position=get(input$position_var)())} +
      {if(input$facet_var != "")facet_wrap(input$facet_var, dir=arg_dir)} +
      labs(title = user_title, x = user_x, y = user_y, color = user_color, fill = user_color) +
      get(input$theme)() +
      scale_color_brewer(palette=input$palette) +
      scale_fill_brewer(palette=input$palette)
  })
}

shinyApp(ui = ui, server = server)