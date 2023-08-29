#' Plot the workload
#'
#' Function to plot the workload of an athlete.
#'
#' @param ath athlete id.
#' @param data a data.frame containing workload data.
#'
#' @return plot_workload() returns a ggplot2 object.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom scales rescale
#' @export
plot_workload <- function(ath, data){

  ## ---- Data management ---- ##
  data <- data %>% filter(nom == ath)
  data <- data %>% filter(
    jour > data$jour[min(which(!is.na(data$rpeLoad)))] &
      jour < data$jour[max(which(!is.na(data$rpeLoad)))]
  ) %>% mutate(jour = as.Date(jour))

  ## -- Plot -- ##
  p <- ggplot(data = data) +
    geom_rect(aes(xmin = jour, xmax = jour + 1, ymin = 0, ymax = rescale(rpeLoad, to = c(0,1)),
                  fill = rpeLoad, text = paste("Date:",jour,
                                               "<br>rpeLoad:",rpeLoad,
                                               "<br>rpeLoad:",round(rescale(rpeLoad, to = c(0,1)),2))),
              stat = "identity") +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "2 week") +
    theme_bw() + labs(title = as.character(ath)) +
    scale_fill_gradient(low = "cyan", high = "blue") +
    theme(axis.text.x = element_text(size = 10),
          axis.title.y = element_text(size = 14),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(hjust = 0.5, size = 16),
          legend.position = "none") +
    ylab("Normalized \n workload")

  return(p)
}


#' Plot the workload
#'
#' Function to plot the workload of an athlete.
#'
#' @param ath athlete id.
#' @param data a data.frame containing workload and state data.
#' @param n_state the number of state.
#'
#' @return plot_state() returns a ggplot2 object.
#'
#' @import ggplot2
#' @import dplyr
#' @import grDevices
#' @export
plot_state <- function(ath, data, n_state){

  ## ---- Data management ---- ##
  data <- data %>% filter(nom == ath)
  data <- data %>% filter(
    jour > data$jour[min(which(!is.na(data$rpeLoad)))] &
      jour < data$jour[max(which(!is.na(data$rpeLoad)))]
  ) %>% mutate(jour = as.Date(jour))

  ## ---- Plot ---- ##
  if (n_state == 3){
    col <- c("green","orange","red")
  } else if (n_state == 2){
    col <- c("green","red")
  } else {
    col <- sample(grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)], n_state)
  }
  p <- ggplot(data = data) +
    scale_x_date(date_breaks = "1 month", date_minor_breaks = "2 week") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          axis.title.y = element_text(size = 12))+
    geom_rect(aes(xmin = jour, xmax = jour + 1, ymin = 0, ymax = S1, text = paste("Date:",jour,
                                                                                  "<br>State 1:",round(S1,2))),
              fill = col[1], alpha = 0.5) +
    ylab("MILS state \n probability")

  if (n_state > 1){
    for (i in 2:n_state){
      done_states <- paste0("S",unique(c(1:(i-1))))
      state <- paste0("S",i)
      p <- p +
        geom_rect(aes_string(xmin = "jour", xmax = "jour + 1" , ymin = paste0(done_states, collapse = "+"),
                             ymax = paste0(paste0(done_states, collapse = "+"), "+", state),
                             text = shQuote(paste0("State ",i))),
                  alpha = 0.5, fill = col[i])
    }
  }

  return(p)
}




## ------------------------------------------------- ##
## ------------------- Shiny app ------------------- ##
## ------------------------------------------------- ##

#' Explore a Markov index load state.
#'
#' Function displaying a shiny app to explore a Markov index load state.
#'
#' @param result a dataframe containing workload and state data.
#'
#' @return emils() returns a shiny app.
#'
#' @import shiny
#' @import plotly
#' @importFrom shinyjs hidden show useShinyjs
#' @importFrom shinyWidgets sendSweetAlert
#' @export
emils <- function(result){

  #### ---- UI part ---- ####
  ui <- tagList(
    fluidPage(
      useShinyjs(),

      my_box(
        title = "", width = 12, collapsible = TRUE, closable = FALSE,  maximizable = FALSE,
        fluidRow(
          column(8,selectizeInput("athlete", "Athlete :" , choices = NULL, multiple = FALSE, options = list(placeholder = "Sélectionnez au moins un individu"), width = "100%")),
          column(2,div(actionButton("import", "Update"), style = "padding-top: 30px;"))
        )
      ),

      hidden(
        div(
          id = "body",
          my_box(
            title = "", width = 12, collapsible = TRUE, closable = FALSE,
            fluidRow(
              plotlyOutput("workload", height = "600px")
            )
          ),
          my_box(
            title = "", width = 12, collapsible = TRUE, closable = FALSE,
            fluidRow(
              plotlyOutput("state", height = "600px")
            )
          )
        )
      )

    )
  )


  #### ---- SERVER part ---- ####
  server <- function(input, output){

    ## ---- Reactive ---- ##
    r <- reactiveValues()

    ## ---- Update input ---- ##
    observe({
      indiv <- unique(result$data$nom)
      updateSelectizeInput(
        inputId = "athlete",
        choices = indiv,
        selected = character(0),
        server = TRUE
      )
    })

    ## ---- Import data ---- ##
    observeEvent(input$import,{
      if (length(input$athlete) == 0){
        sendSweetAlert(
          title = "Aucun·e athlète sélectionné·e",
          text = "Veuillez sélectionner au moins un·e athlète.",
          type = "error"
        )
      } else {
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Importation des données", detail = "Patientez un instant...", value = 0.5)

        r$data <- result$data %>% filter(nom == input$athlete)

        shinyjs::show("body")
        progress$inc(1, detail = "Terminé")
      }
    })

    ## ---- Plot ---- ##
    output$workload <- renderPlotly({
      p <- plot_workload(input$athlete, result$data)

      ggplotly(p, tooltip = "text")
    })

    output$state <- renderPlotly({
      p <- plot_state(input$athlete, result$data, length(unique(result$data$state)))

      ggplotly(p, tooltip = "text")
    })

  }


  #### ---- Launch the app ---- ####
  shinyApp(ui, server)
}

