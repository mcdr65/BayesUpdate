# app.R

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Carnap → Bayes: Prior, Evidenz und Posterior"),

  sidebarLayout(
    sidebarPanel(
      sliderInput("priorA", "Prior Forscher A: P(H)", 
                  min = 0.01, max = 0.99, value = 0.20, step = 0.01),

      sliderInput("priorB", "Prior Forscher B: P(H)", 
                  min = 0.01, max = 0.99, value = 0.80, step = 0.01),

      hr(),

      sliderInput("sens", "P(Daten | H): Trefferwahrscheinlichkeit",
                  min = 0.01, max = 0.99, value = 0.80, step = 0.01),

      sliderInput("fpr", "P(Daten | nicht H): Falsch-positiv-Rate",
                  min = 0.01, max = 0.99, value = 0.20, step = 0.01),

      hr(),

      sliderInput("n", "Anzahl positiver Evidenzen",
                  min = 1, max = 50, value = 10, step = 1)
    ),

    mainPanel(
      h3("Bayes-Regel"),
      withMathJax("$$P(H \\mid D) =
                  \\frac{P(D \\mid H)P(H)}
                  {P(D \\mid H)P(H) + P(D \\mid \\neg H)P(\\neg H)}$$"),

      plotOutput("posteriorPlot", height = "400px"),

      h3("Posterior nach allen Evidenzen"),
      tableOutput("tab"),

      h3("Interpretation"),
      textOutput("interpretation")
    )
  )
)

server <- function(input, output) {

  update_one <- function(prior, sens, fpr) {
    (sens * prior) / (sens * prior + fpr * (1 - prior))
  }

  posterior_path <- reactive({
    n <- input$n

    postA <- numeric(n + 1)
    postB <- numeric(n + 1)

    postA[1] <- input$priorA
    postB[1] <- input$priorB

    for (i in 1:n) {
      postA[i + 1] <- update_one(postA[i], input$sens, input$fpr)
      postB[i + 1] <- update_one(postB[i], input$sens, input$fpr)
    }

    data.frame(
      Evidenz = rep(0:n, 2),
      Posterior = c(postA, postB),
      Forscher = rep(c("Forscher A", "Forscher B"), each = n + 1)
    )
  })

  output$posteriorPlot <- renderPlot({
    ggplot(posterior_path(),
           aes(x = Evidenz, y = Posterior, group = Forscher, linetype = Forscher)) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      ylim(0, 1) +
      labs(
        x = "Anzahl positiver Evidenzen",
        y = "Posterior P(H | Daten)",
        title = "Gleiche Evidenz, unterschiedliche Priors"
      ) +
      theme_minimal(base_size = 14)
  })

  output$tab <- renderTable({
    d <- posterior_path()
    subset(d, Evidenz == input$n)
  }, digits = 3)

  output$interpretation <- renderText({
    d <- posterior_path()
    last <- subset(d, Evidenz == input$n)

    diff <- abs(diff(last$Posterior))

    paste0(
      "Die beiden Forscher starten mit unterschiedlichen Priors. ",
      "Sie verwenden aber dieselbe Aktualisierungsregel. ",
      "Nach ", input$n, " positiven Evidenzen beträgt der Unterschied der Posterioren noch ",
      round(diff, 3), ". ",
      "Das illustriert den modernen Bayesianismus: Nicht ein eindeutig richtiger Prior steht im Zentrum, ",
      "sondern die rationale Aktualisierung von Unsicherheit durch Evidenz."
    )
  })
}

shinyApp(ui, server)
