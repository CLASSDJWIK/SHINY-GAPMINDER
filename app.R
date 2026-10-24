#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# install.packages(c("shiny", "gapminder", "dplyr", "ggplot2", "plotly"))

library(shiny)
library(gapminder)
library(dplyr)
library(ggplot2)
library(plotly)

data <- as.data.frame(gapminder::gapminder)
years <- sort(unique(data$year))
continents <- sort(unique(as.character(data$continent)))

# Define UI for application that draws a histogram
#ui <- fluidPage(

    # Application title
 #   titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
  #  sidebarLayout(
   #     sidebarPanel(
    #        sliderInput("bins",
                   #     "Number of bins:",
      #                  min = 1,
       #                 max = 50,
        #                value = 30)
       # ),

        # Show a plot of the generated distribution
        #mainPanel(
         #  plotOutput("distPlot")
        #)
    #)
#)


ui <- fluidPage(
  tags$head(
    tags$title("Interactive Gapminder"),
    tags$style(HTML("
      body { background: #f5f7fa; color: #253448; }
      .app-header { padding: 18px 0 12px; }
      .app-header h2 { font-weight: 700; margin: 0 0 6px; }
      .app-header p { color: #63738a; margin: 0; }
      .well, .chart-card, .stat-card { background: white; border: 1px solid #e2e8f0;
        border-radius: 12px; box-shadow: none; }
      .well { padding: 20px; }
      .chart-card { padding: 15px; margin-bottom: 16px; }
      .stat-card { padding: 15px; margin-bottom: 15px; }
      .stat-card .value { font-size: 25px; font-weight: 700; }
      .stat-card .label { color: #63738a; font-size: 13px; }
      .help-note { font-size: 12px; color: #66758a; margin-top: 10px; }
      .control-label { font-weight: 600; }
    "))
  ),
  div(class = "app-header",
      h2("Interactive Gapminder"),
      p("Explore life expectancy, income and population across countries and years.")),
  sidebarLayout(
    sidebarPanel(width = 3,
                 sliderInput("year", "Year", min = min(years), max = max(years),
                             value = max(years), step = 5, sep = "", animate = animationOptions(interval = 1300, loop = FALSE)),
                 checkboxGroupInput("continents", "Continents", choices = continents,
                                    selected = continents),
                 selectInput("xvar", "Horizontal axis", choices = c(
                   "GDP per capita" = "gdpPercap", "Life expectancy" = "lifeExp",
                   "Population" = "pop"), selected = "gdpPercap"),
                 selectInput("yvar", "Vertical axis", choices = c(
                   "Life expectancy" = "lifeExp", "GDP per capita" = "gdpPercap",
                   "Population" = "pop"), selected = "lifeExp"),
                 checkboxInput("logx", "Log scale on horizontal axis", value = TRUE),
                 selectInput("country", "Highlight a country", choices = c("None", sort(unique(data$country))),
                             selected = "None"),
                 actionButton("reset", "Reset filters", icon = icon("refresh"), width = "100%"),
                 p(class = "help-note", "Move the year slider or press Play. Hover over a bubble for country details.")
    ),
    mainPanel(width = 9,
              fluidRow(
                column(4, div(class = "stat-card", div(class = "label", "Countries shown"),
                              div(class = "value", textOutput("n_countries", inline = TRUE)))),
                column(4, div(class = "stat-card", div(class = "label", "Total population"),
                              div(class = "value", textOutput("population", inline = TRUE)))),
                column(4, div(class = "stat-card", div(class = "label", "Mean life expectancy"),
                              div(class = "value", textOutput("life", inline = TRUE))))
              ),
              div(class = "chart-card",
                  plotlyOutput("bubble", height = "510px")),
              div(class = "chart-card",
                  h4("Data for the selected year"),
                  tableOutput("country_table"),
                  p(class = "help-note", "Table shows the 15 most populous countries among the selected continents."))
    )
  )
)


# Define server logic required to draw a histogram
#server <- function(input, output) {

 #   output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
  #      x    <- faithful[, 2]
   #     bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
    #    hist(x, breaks = bins, col = 'darkgray', border = 'white',
     #        xlab = 'Waiting time to next eruption (in mins)',
      #       main = 'Histogram of waiting times')
    #})
#}


server <- function(input, output, session) {
  observeEvent(input$reset, {
    updateSliderInput(session, "year", value = max(years))
    updateCheckboxGroupInput(session, "continents", selected = continents)
    updateSelectInput(session, "xvar", selected = "gdpPercap")
    updateSelectInput(session, "yvar", selected = "lifeExp")
    updateCheckboxInput(session, "logx", value = TRUE)
    updateSelectInput(session, "country", selected = "None")
  })
  
  filtered <- reactive({
    req(input$year)
    data %>% filter(year == input$year, as.character(continent) %in% input$continents)
  })
  
  output$n_countries <- renderText(nrow(filtered()))
  output$population <- renderText({
    n <- sum(filtered()$pop)
    if (nrow(filtered()) == 0) return("—")
    paste0(format(round(n / 1e9, 2), nsmall = 2), " B")
  })
  output$life <- renderText({
    if (nrow(filtered()) == 0) return("—")
    sprintf("%.1f years", mean(filtered()$lifeExp))
  })
  
  output$bubble <- renderPlotly({
    df <- filtered()
    validate(need(nrow(df) > 0, "Select at least one continent to display the chart."))
    xvar <- input$xvar
    yvar <- input$yvar
    label_map <- c(gdpPercap = "GDP per capita (USD)", lifeExp = "Life expectancy (years)",
                   pop = "Population")
    df$highlight <- if (input$country != "None") {
      ifelse(df$country == input$country, "Highlighted", "Other")
    } else "Other"
    df$details <- paste0("<b>", df$country, "</b><br>",
                         "Continent: ", df$continent, "<br>",
                         "Year: ", df$year, "<br>",
                         "Life expectancy: ", round(df$lifeExp, 1), " years<br>",
                         "GDP per capita: $", format(round(df$gdpPercap), big.mark = ","), "<br>",
                         "Population: ", format(df$pop, big.mark = ",", scientific = FALSE))
    p <- ggplot(df, aes(x = .data[[xvar]], y = .data[[yvar]],
                        size = pop, color = continent, text = details)) +
      geom_point(alpha = 0.75) +
      scale_size_area(max_size = 24, name = "Population") +
      labs(x = label_map[[xvar]], y = label_map[[yvar]], color = "Continent",
           title = paste("Gapminder", input$year)) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(face = "bold"), panel.grid.minor = element_blank())
    if (isTRUE(input$logx) && xvar %in% c("gdpPercap", "pop")) {
      p <- p + scale_x_log10()
    }
    if (input$country != "None" && input$country %in% df$country) {
      highlight_df <- df[df$country == input$country, , drop = FALSE]
      p <- p + geom_point(data = highlight_df, shape = 21, stroke = 1.7,
                          color = "black", fill = NA, size = 14,
                          inherit.aes = FALSE,
                          aes(x = .data[[xvar]], y = .data[[yvar]]))
    }
    ggplotly(p, tooltip = "text") %>% layout(legend = list(orientation = "h", y = -0.25),
                                             margin = list(b = 85))
  })
  
  output$country_table <- renderTable({
    df <- filtered()
    validate(need(nrow(df) > 0, "No countries to display."))
    df %>% arrange(desc(pop)) %>% head(15) %>%
      transmute(Country = country,
                Continent = as.character(continent),
                `Life expectancy (years)` = round(lifeExp, 1),
                `GDP per capita (USD)` = round(gdpPercap),
                Population = format(pop, big.mark = ",", scientific = FALSE))
  }, striped = TRUE, hover = TRUE, spacing = "s", rownames = FALSE)
}

#shinyApp(ui = ui, server = server)
# Run the application 
shinyApp(ui = ui, server = server)
