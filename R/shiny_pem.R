## ------------------------------------------------- ##
## --------------------- Utils --------------------- ##
## ------------------------------------------------- ##

#' Import bs4Dash dependencies
#'
#' HMTL object containing all bs4Dash dependencies.
#'
#' @return bs4Dash_ui is a HTML object.
#'
#' @importFrom bs4Dash dashboardPage dashboardHeader dashboardSidebar dashboardBody
#' @export
bs4Dash_ui <- bs4Dash::dashboardPage(
  bs4Dash::dashboardHeader(),
  bs4Dash::dashboardSidebar(),
  bs4Dash::dashboardBody()
)


#' Get bs4Dash dependencies
#'
#' List containing all bs4Dash dependencies.
#'
#' @return bs4Dash_deps is a list of html_dependency.
#'
#' @importFrom htmltools findDependencies
#' @export
bs4Dash_deps <- htmltools::findDependencies(bs4Dash_ui)


#' bs4Dash box
#'
#' Function creating a bs4Dash box.
#'
#' @param ... bs4Dash box arguments.
#' @param title a title.
#' @param width the width of the box (12 by default).
#' @param collapsible boolean. If TRUE, display a button in the upper right that allows the user to collapse the box (TRUE by default).
#' @param maximizable boolean. If TRUE, the card can be displayed in full screen mode (TRUE by default).
#' @param closable boolean.	If TRUE, display a button in the upper right that allows the user to close the box (FALSE by default).
#'
#' @return my_box() returns a HTML object.
#'
#' @importFrom shiny tagList
#' @importFrom bs4Dash box
#' @export
my_box <- function(..., title, width = 12, collapsible = TRUE, maximizable = TRUE, closable = FALSE){
  tagList(
    bs4Dash::box(..., title = title, width = width, collapsible = collapsible, maximizable = maximizable, closable = closable),
    bs4Dash_deps
  )
}


#' Compute estimation
#'
#' Function to compute the estimation of the performance.
#'
#' @param reactive a list (reactive of the shiny app created by 'epem' function).
#' @param type indicates whether performance is to be maximized or minimized ('max' by default).
#'
#' @return compute_estimation() returns a data.frame.
#'
#' @importFrom data.table rbindlist
#' @export
compute_estimation <- function(reactive, type = "max"){
  age <- seq(0,10,0.1)

  ## ---- Estimations ---- ##
  pred <- data.frame(rbindlist(apply(reactive$data, 1, function(x){
    param <- c(t(x[reactive$para]))
    dt <- data.frame(
      age = age,
      perf = do.call(reactive$fun,list(age,as.numeric(param),type)),
      id = unique(x["id"])
    )

    return(dt)
  })))

  ## ---- Création des intervalles d'estimation ---- ##
  quant <- seq(0.05,0.95,0.05)
  pred <- data.frame(rbindlist(by(pred,pred$id,function(y){
    t <- rbindlist(tapply(y$perf,y$age,function(x){
      data.frame(borne = quant, perf = quantile(x,quant))
    }), idcol = "age")
    t$age = as.numeric(t$age)
    return(t)
  }), idcol = "id"))

  pred <- reshape(pred, idvar = c("id","age"), timevar = "borne", direction = "wide")

  return(pred)
}


#' Plot the estimation
#'
#' Function to plot the estimation of the performance.
#'
#' @param reactive a list (reactive of the shiny app created by 'epem' function).
#' @param type indicates whether performance is to be maximized or minimized ('max' by default).
#'
#' @return plot_estimation() returns a plotly object.
#'
#' @import ggplot2
#' @import plotly
#' @export
plot_estimation <- function(reactive, type = "max"){
  if (is.null(reactive$data)){
    p <- ggplot() + theme_classic()
    options(warn = -1)
  } else {
    bdd <- reactive$data

    ## ---- Ajout des estimations ---- ##
    pred <- compute_estimation(reactive, type)
    p <- ggplot() +
      geom_ribbon(data = pred, aes(x = age, ymin = perf.0.25, ymax = perf.0.75,
                                   fill = id), alpha = 0.4) +
      # geom_ribbon(data = pred, aes(x = age, ymin = perf.0.1, ymax = perf.0.9,
      #                              fill = id), alpha = 0.3) +
      geom_ribbon(data = pred, aes(x = age, ymin = perf.0.05, ymax = perf.0.95,
                                   fill = id), alpha = 0.2) +
      geom_line(data = pred, aes(x = age, y = perf.0.5, col = id, group = id, text = paste("Athlète :",id,
                                                                                           "<br>Âge :",round(age,2),
                                                                                           "<br>Estimation :",round(perf.0.5,2))), size = 1.5)
  }


  ## ---- Ggplotly ---- ##
  if (is.null(reactive$data)){
    ggplotly(p, tooltip = "text")
  } else {
    ggplotly(p + labs(x = "Âge", y = "Performance", col = "Athlète") +
               theme_classic() + ylim(0,50) +
               theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)),
             tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0, y = -0.2))
  }
}




## ------------------------------------------------- ##
## ------------------- Shiny app ------------------- ##
## ------------------------------------------------- ##

#' Explore a performance estimation model
#'
#' Function displaying a shiny app to explore a performance estimation model.
#'
#' @param path the path where to find the SQL database.
#' @param type indicates whether performance is to be maximized or minimized ('max' by default).
#' @param table_name name of the SQL table containing the parameters ('parameter' by default).
#'
#' @return epem() returns a shiny app.
#'
#' @import shiny
#' @importFrom shinyjs hidden show useShinyjs
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom shinyWidgets sendSweetAlert
#' @export
epem <- function(path, type = "max", table_name = "parameter"){

  #### ---- UI part ---- ####
  ui <- tagList(
    fluidPage(
      useShinyjs(),

      my_box(
        title = "", width = 12, collapsible = TRUE, closable = FALSE,  maximizable = FALSE,
        fluidRow(
          column(8,selectizeInput("athlete", "Athlete :" , choices = NULL, multiple = TRUE, options = list(placeholder = "Sélectionnez au moins un individu"), width = "100%")),
          column(2,div(actionButton("import", "Update"), style = "padding-top: 30px;"))
        )
      ),

      hidden(
        div(
          id = "body",
          my_box(
            title = "", width = 12, collapsible = TRUE, closable = FALSE,
            fluidRow(
              plotlyOutput("estimation", height = "600px")
            )
          ),
          my_box(
            title = "", width = 12, collapsible = TRUE, closable = FALSE,
            fluidRow(
              plotlyOutput("progression", height = "600px")
            )
          ),
          my_box(
            title = "", width = 12, collapsible = TRUE, closable = FALSE,
            fluidRow(
              plotlyOutput("vitesse", height = "600px")
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
      query <- paste0(
        "SELECT name ",
        "FROM pragma_table_info('",table_name,"')"
      )
      columns <- get_data(query, path)
      if ("e" %in% columns$name){
        r$fun <- f_imap
        r$para <- c("a","b","c","d","e")
      } else {
        r$fun <- f_moore
        r$para <- c("a","b","c","d")
      }

      query <- paste0(
        "SELECT DISTINCT id FROM parameter LIMIT 1000"
      )
      indiv <- get_data(query, path)
      updateSelectizeInput(
        inputId = "athlete",
        choices = indiv$id
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

        query <- paste0(
          "SELECT id, ",paste(r$para, collapse = ", ")," ",
          "FROM ",table_name," ",
          "WHERE id IN ('",paste(input$athlete, collapse = "','"),"')"
        )
        r$data <- get_data(query, path)

        shinyjs::show("body")
        progress$inc(1, detail = "Terminé")
      }
    })

    ## ---- Plot ---- ##
    output$estimation <- renderPlotly({
      plot_estimation(r, type)
    })

  }


  #### ---- Launch the app ---- ####
  shinyApp(ui, server)
}

