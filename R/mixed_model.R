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




## ------------------------------------------------- ##
## ------------------- Shiny app ------------------- ##
## ------------------------------------------------- ##

#' Explore a performance estimation model
#'
#' Function displaying a shiny app to explore a performance estimation model.
#'
#' @param path the path where to find the SQL database.
#'
#' @return epem() returns a shiny app.
#'
#' @import shiny
#' @importFrom shinyjs hidden show useShinyjs
#' @importFrom plotly plotlyOutput
#' @export
epem <- function(path){

  #### ---- UI part ---- ####
  ui <- tagList(
    fluidPage(
      useShinyjs(),

      my_box(
        title = "", width = 12, collapsible = TRUE, closable = FALSE,  maximizable = FALSE,
        fluidRow(
          column(8,selectizeInput("athlete", "Individu :" , choices = NULL, multiple = TRUE, options = list(placeholder = "Sélectionnez au moins un individu"), width = "100%")),
          column(2,div(actionButton("actualiser", "Actualiser", class = "actionbutton"), style = "padding-top: 30px;"))
        )
      ),

      hidden(
        div(
          id = "body",
          my_box(
            title = "", width = 12, collapsible = TRUE, closable = FALSE,
            fluidRow(
              plotlyOutput("estimation")
            )
          ),
          my_box(
            title = "", width = 12, collapsible = TRUE, closable = FALSE,
            fluidRow(
              plotlyOutput("progression")
            )
          ),
          my_box(
            title = "", width = 12, collapsible = TRUE, closable = FALSE,
            fluidRow(
              plotlyOutput("vitesse")
            )
          )
        )

      )
    )
  )


  #### ---- SERVER part ---- ####
  server <- function(input, output){

    observe({
      query <- paste0(
        "SELECT id FROM parameter LIMIT 10"
      )
      indiv <- get_data(query, path)

      updateSelectizeInput(
        inputId = "athlete",
        choices = indiv$id
      )

    })

    observeEvent(input$actualiser,{
      shinyjs::show("body")
    })
  }


  #### ---- Launch the app ---- ####
  shinyApp(ui, server)
}

