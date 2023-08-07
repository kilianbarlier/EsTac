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


#' Compute the estimation
#'
#' Function to compute the estimation of the performance.
#'
#' @param parametre a data.frame of the parameters estimated by the model.
#' @param info a data.frame containing the information of the transformation used on the data to compute the model.
#' @param FUN the function to fit.
#' @param type ("min" by default) indicates whether the function estimates a performance to be maximised ("max") or minimised ("min").
#' @param min_age the minimum age value to compute the estimation (10 by default).
#' @param max_age the maximum age value to compute the estimation (50 by default).
#'
#' @return compute_estimation() returns a data.frame.
#'
#' @importFrom data.table rbindlist
#' @export
compute_estimation <- function(parametre, info, FUN, type = "min", min_age = 10, max_age = 50){

  age <- seq(
    ((min_age-info$mean_age) / info$sd_age) - info$min_age,
    ((max_age-info$mean_age) / info$sd_age) - info$min_age,
    0.1
  )

  if (ncol(parametre) == 6){
    var <- c("a","b","c","d","e")
  } else if (ncol(parametre) == 5){
    var <- c("a","b","c","d")
  }

  estimation <- data.frame(data.table::rbindlist(apply(parametre, 1, function(x){
    param <- as.numeric(c(t(x[var])))
    return(data.frame(
      age = age,
      pred = FUN(age,param,type),
      id = unique(x["id"])
    ))
  })))
  estimation$age <- ((estimation$age + info$min_age) * info$sd_age) + info$mean_age
  estimation$pred <- ((estimation$pred + info$min_perf) * info$sd_perf) + info$mean_perf

  quant <- seq(0.05,0.95,0.05)
  estimation <- data.frame(data.table::rbindlist(by(estimation,estimation$id,function(y){
    t <- data.table::rbindlist(tapply(y$pred,y$age,function(x){
      data.frame(borne = quant, pred = quantile(x,quant))
    }), idcol = "age")
    t$age = as.numeric(t$age)
    return(t)
  }), idcol = "id"))

  estimation <- reshape(estimation, idvar = c("id","age"), timevar = "borne", direction = "wide")

  return(estimation)
}


#' Compute the derivative
#'
#' Function to compute the derivative of the estimation.
#'
#' @param parametre a data.frame of the parameters estimated by the model.
#' @param info a data.frame containing the information of the transformation used on the data to compute the model.
#' @param FUN the function to fit.
#' @param type ("min" by default) indicates whether the function estimates a performance to be maximised ("max") or minimised ("min").
#' @param min_age the minimum age value to compute the estimation (10 by default).
#' @param max_age the maximum age value to compute the estimation (50 by default).
#'
#' @return compute_deriv1() returns a data.frame.
#'
#' @importFrom data.table rbindlist
#' @export
compute_deriv1 <- function(parametre, info, FUN, type = "min", min_age = 10, max_age = 50){

  age <- seq(
    ((min_age-info$mean_age) / info$sd_age) - info$min_age,
    ((max_age-info$mean_age) / info$sd_age) - info$min_age,
    0.1
  )

  if (ncol(parametre) == 6){
    var <- c("a","b","c","d","e")
  } else if (ncol(parametre) == 5){
    var <- c("a","b","c","d")
  }

  estimation <- data.frame(data.table::rbindlist(apply(parametre, 1, function(x){
    param <- as.numeric(c(t(x[var])))
    value <- FUN(age,param,type)
    deriv1 <- diff(value)/diff(age)
    return(data.frame(
      age = age[-1],
      deriv1 = deriv1,
      id = unique(x["id"])
    ))
  })))
  estimation$age <- ((estimation$age + info$min_age) * info$sd_age) + info$mean_age
  estimation$deriv1 <- ((estimation$deriv1 + info$min_perf) * info$sd_perf) + info$mean_perf

  quant <- seq(0.05,0.95,0.05)
  estimation <- data.frame(data.table::rbindlist(by(estimation,estimation$id,function(y){
    t <- data.table::rbindlist(tapply(y$deriv1,y$age,function(x){
      data.frame(borne = quant, deriv1 = quantile(x,quant))
    }), idcol = "age")
    t$age = as.numeric(t$age)
    return(t)
  }), idcol = "id"))

  estimation <- reshape(estimation, idvar = c("id","age"), timevar = "borne", direction = "wide")

  return(estimation)
}


#' Compute the derivative of the derivative
#'
#' Function to compute the derivative of the progression
#'
#' @param parametre a data.frame of the parameters estimated by the model.
#' @param info a data.frame containing the information of the transformation used on the data to compute the model.
#' @param FUN the function to fit.
#' @param type ("min" by default) indicates whether the function estimates a performance to be maximised ("max") or minimised ("min").
#' @param min_age the minimum age value to compute the estimation (10 by default).
#' @param max_age the maximum age value to compute the estimation (50 by default).
#'
#' @return compute_deriv2() returns a data.frame.
#'
#' @importFrom data.table rbindlist
#' @export
compute_deriv2 <- function(parametre, info, FUN, type = "min", min_age = 10, max_age = 50){

  age <- seq(
    ((min_age-info$mean_age) / info$sd_age) - info$min_age,
    ((max_age-info$mean_age) / info$sd_age) - info$min_age,
    0.1
  )

  if (ncol(parametre) == 6){
    var <- c("a","b","c","d","e")
  } else if (ncol(parametre) == 5){
    var <- c("a","b","c","d")
  }

  estimation <- data.frame(data.table::rbindlist(apply(parametre, 1, function(x){
    param <- as.numeric(c(t(x[var])))
    value <- FUN(age,param,type)
    deriv1 <- diff(value)/diff(age)
    deriv2 <- diff(deriv1)/diff(age[-1])
    return(data.frame(
      age = age[c(-1,-2)],
      deriv2 = deriv2,
      id = unique(x["id"])
    ))
  })))
  estimation$age <- ((estimation$age + info$min_age) * info$sd_age) + info$mean_age
  estimation$deriv2 <- ((estimation$deriv2 + info$min_perf) * info$sd_perf) + info$mean_perf

  quant <- seq(0.05,0.95,0.05)
  estimation <- data.frame(data.table::rbindlist(by(estimation,estimation$id,function(y){
    t <- data.table::rbindlist(tapply(y$deriv2,y$age,function(x){
      data.frame(borne = quant, deriv2 = quantile(x,quant))
    }), idcol = "age")
    t$age = as.numeric(t$age)
    return(t)
  }), idcol = "id"))

  estimation <- reshape(estimation, idvar = c("id","age"), timevar = "borne", direction = "wide")

  return(estimation)
}


#' Plot the estimation
#'
#' Function to compute and plot the estimation of the performance.
#'
#' @param data a data.frame of the performance realized by the athlete (can be NULL).
#' @param parametre a data.frame of the parameters estimated by the model.
#' @param info a data.frame containing the information of the transformation used on the data to compute the model.
#' @param FUN the function to fit.
#' @param type ("min" by default) indicates whether the function estimates a performance to be maximised ("max") or minimised ("min").
#' @param min_age minimum value for x axis (10 by default).
#' @param max_age maximum value for x axis (50 by default).
#'
#' @return plot_estimation() returns a ggplot2 object.
#'
#' @import ggplot2
#' @export
plot_estimation <- function(data, parametre, info, FUN, type = "min", min_age = 10, max_age = 50){

  estimation <- compute_estimation(parametre, info, FUN, type, min_age, max_age)

  ## ---- Plot estimation ---- ##
  p <- ggplot(estimation) +
    geom_ribbon(aes(x = age, ymin = pred.0.05, ymax = pred.0.95, fill = id, group = id,
                    text = paste("Athlète :",id,
                                 "<br>Performance min :",round(pred.0.05,2),
                                 "<br>Performance max :",round(pred.0.95,2),
                                 "<br>Âge :",round(age,2))), alpha = 0.2, show.legend = FALSE) +
    geom_ribbon(aes(x = age, ymin = pred.0.1, ymax = pred.0.9, fill = id, group = id,
                    text = paste("Athlète :",id,
                                 "<br>Performance min :",round(pred.0.1,2),
                                 "<br>Performance max :",round(pred.0.9,2),
                                 "<br>Âge :",round(age,2))), alpha = 0.3, show.legend = FALSE)

  ## ---- Add median ---- ##
  if (length(unique(parametre$id)) == 1){
    p <- p +
      geom_line(aes(x = age, y = pred.0.5, col = id, group = 1,
                    text = paste("Athlète :",id,
                                 "<br>Performance :",round(pred.0.5,2),
                                 "<br>Âge :",round(age,2))), show.legend = FALSE, linewidth = 1.5) +
      labs(x = "Âge", y = "Performance", title = paste("Estimation",unique(parametre$id)))
  } else {
    p <- p +
      geom_line(aes(x = age, y = pred.0.5, col = id, group = id,
                    text = paste("Athlète :",id,
                                 "<br>Performance :",round(pred.0.5,2),
                                 "<br>Âge :",round(age,2))), show.legend = FALSE, linewidth = 1.5)+
      labs(x = "Âge", y = "Performance", title = "Estimations")
  }

  ## ---- Add data ---- ##
  if (!is.null(data)){
    p <- p +
      geom_point(data = data, aes(x = age, y = perf, col = id,
                                  text = paste("Athlète :",id,
                                               "<br>Performance :",perf,
                                               "<br>Âge :",round(age,2))), show.legend = FALSE)
  }

  p <- p +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

  return(p)
}


#' Plot the progression
#'
#' Function to compute and plot the progression of an athlete.
#'
#' @param parametre a data.frame of the parameters estimated by the model.
#' @param info a data.frame containing the information of the transformation used on the data to compute the model.
#' @param FUN the function to fit.
#' @param type ("min" by default) indicates whether the function estimates a performance to be maximised ("max") or minimised ("min").
#' @param min_age minimum value for x axis (10 by default).
#' @param max_age maximum value for x axis (50 by default).
#'
#' @return plot_progression() returns a ggplot2 object.
#'
#' @import ggplot2
#' @export
plot_progression <- function(parametre, info, FUN, type = "min", min_age = 10, max_age = 50){

  estimation <- compute_deriv1(parametre, info, FUN, type, min_age, max_age)

  ## ---- Plot estimation ---- ##
  p <- ggplot(estimation) +
    geom_ribbon(aes(x = age, ymin = deriv1.0.05, ymax = deriv1.0.95, fill = id, group = id,
                    text = paste("Athlète :",id,
                                 "<br>Progression min :",round(deriv1.0.05,2),
                                 "<br>Progression max :",round(deriv1.0.95,2),
                                 "<br>Âge :",round(age,2))), alpha = 0.2, show.legend = FALSE) +
    geom_ribbon(aes(x = age, ymin = deriv1.0.1, ymax = deriv1.0.9, fill = id, group = id,
                    text = paste("Athlète :",id,
                                 "<br>Progression min :",round(deriv1.0.1,2),
                                 "<br>Progression max :",round(deriv1.0.9,2),
                                 "<br>Âge :",round(age,2))), alpha = 0.3, show.legend = FALSE)

  ## ---- Add median ---- ##
  if (length(unique(parametre$id)) == 1){
    p <- p +
      geom_line(aes(x = age, y = deriv1.0.5, col = id, group = 1,
                    text = paste("Athlète :",id,
                                 "<br>Progression :",round(deriv1.0.5,2),
                                 "<br>Âge :",round(age,2))), show.legend = FALSE, linewidth = 1.5) +
      labs(x = "Âge", y = "Progression", title = paste("Progression",unique(parametre$id)))
  } else {
    p <- p +
      geom_line(aes(x = age, y = deriv1.0.5, col = id, group = id,
                    text = paste("Athlète :",id,
                                 "<br>Progression :",round(deriv1.0.5,2),
                                 "<br>Âge :",round(age,2))), show.legend = FALSE, linewidth = 1.5) +
      labs(x = "Âge", y = "Progression", title = "Progressions")
  }

  ## ---- Add data ---- ##
  p <- p +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_hline(yintercept = 0, color = "black")

  return(p)
}


#' Plot the speed progression
#'
#' Function to compute and plot the speed progression of an athlete.
#'
#' @param parametre a data.frame of the parameters estimated by the model.
#' @param info a data.frame containing the information of the transformation used on the data to compute the model.
#' @param FUN the function to fit.
#' @param type ("min" by default) indicates whether the function estimates a performance to be maximised ("max") or minimised ("min").
#' @param min_age minimum value for x axis (10 by default).
#' @param max_age maximum value for x axis (50 by default).
#'
#' @return plot_progression() returns a ggplot2 object.
#'
#' @import ggplot2
#' @export
plot_speed_progression <- function(parametre, info, FUN, type = "min", min_age = 10, max_age = 50){

  estimation <- compute_deriv2(parametre, info, FUN, type, min_age, max_age)

  ## ---- Plot estimation ---- ##
  p <- ggplot(estimation) +
    geom_ribbon(aes(x = age, ymin = deriv2.0.05, ymax = deriv2.0.95, fill = id, group = id,
                    text = paste("Athlète :",id,
                                 "<br>Vitesse de progression min :",round(deriv2.0.05,2),
                                 "<br>Vitesse de progression max :",round(deriv2.0.95,2),
                                 "<br>Âge :",round(age,2))), alpha = 0.2, show.legend = FALSE) +
    geom_ribbon(aes(x = age, ymin = deriv2.0.1, ymax = deriv2.0.9, fill = id, group = id,
                    text = paste("Athlète :",id,
                                 "<br>Vitesse de progression min :",round(deriv2.0.1,2),
                                 "<br>Vitesse de progression max :",round(deriv2.0.9,2),
                                 "<br>Âge :",round(age,2))), alpha = 0.3, show.legend = FALSE)

  ## ---- Add median ---- ##
  if (length(unique(parametre$id)) == 1){
    p <- p +
      geom_line(aes(x = age, y = deriv2.0.5, col = id, group = 1,
                    text = paste("Athlète :",id,
                                 "<br>Vitesse de progression :",round(deriv2.0.5,2),
                                 "<br>Âge :",round(age,2))), show.legend = FALSE, linewidth = 1.5) +
      labs(x = "Âge", y = "Vitesse de progression", title = paste("Vitesse de progression",unique(parametre$id)))
  } else {
    p <- p +
      geom_line(aes(x = age, y = deriv2.0.5, col = id, group = id,
                    text = paste("Athlète :",id,
                                 "<br>Vitesse de progression :",round(deriv2.0.5,2),
                                 "<br>Âge :",round(age,2))), show.legend = FALSE, linewidth = 1.5) +
      labs(x = "Âge", y = "Vitesse de progression", title = "Vitesses de progressions")
  }

  ## ---- Add data ---- ##
  p <- p +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_hline(yintercept = 0, color = "black")

  return(p)
}




## ------------------------------------------------- ##
## ------------------- Shiny app ------------------- ##
## ------------------------------------------------- ##

#' Explore a performance estimation model
#'
#' Function displaying a shiny app to explore a performance estimation model.
#'
#' @param args list of arguments required by get_data function.
#' @param type indicates whether performance is to be maximized or minimized ('max' by default).
#' @param table_name name of the SQL table containing the parameters ('parameter' by default).
#' @param info_table_name name of the SQL table containing the transformation informations ('transformation_info' by default).
#'
#' @return epem() returns a shiny app.
#'
#' @import shiny
#' @importFrom shinyjs hidden show useShinyjs
#' @importFrom plotly plotlyOutput renderPlotly
#' @importFrom shinyWidgets sendSweetAlert
#' @export
epem <- function(args, type = "max", table_name = "parameter", info_table_name = "transformation_info"){

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
      columns <- get_data(query, args)
      if ("e" %in% columns$name){
        r$fun <- f_imap
        r$para <- c("a","b","c","d","e")
      } else {
        r$fun <- f_moore
        r$para <- c("a","b","c","d")
      }

      query <- paste0(
        "SELECT * ",
        "FROM ",info_table_name
      )
      r$info <- get_data(query, args)

      query <- paste0(
        "SELECT DISTINCT id FROM parameter LIMIT 1000"
      )
      indiv <- get_data(query, args)
      updateSelectizeInput(
        inputId = "athlete",
        choices = indiv$id,
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

        query <- paste0(
          "SELECT id, ",paste(r$para, collapse = ", ")," ",
          "FROM ",table_name," ",
          "WHERE id IN ('",paste(input$athlete, collapse = "','"),"')"
        )
        r$data <- get_data(query, args)

        shinyjs::show("body")
        progress$inc(1, detail = "Terminé")
      }
    })

    ## ---- Plot ---- ##
    output$estimation <- renderPlotly({
      p <- plot_estimation(data = NULL, parametre = r$data, info = r$info, FUN = r$fun, type = type)

      ggplotly(p + labs(x = "Âge", y = "Performance", col = "Athlète") +
                 theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)),
               tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.2))
    })

    output$progression <- renderPlotly({
      p <- plot_progression(parametre = r$data, info = r$info, FUN = r$fun, type = type)

      ggplotly(p + labs(x = "Âge", y = "Progression", col = "Athlète") +
                 theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)),
               tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.2))
    })

    output$vitesse <- renderPlotly({
      p <- plot_speed_progression(parametre = r$data, info = r$info, FUN = r$fun, type = type)

      ggplotly(p + labs(x = "Âge", y = "Vitesse de progression", col = "Athlète") +
                 theme(axis.title.x = element_text(size = 12), axis.title.y = element_text(size = 12)),
               tooltip = "text") %>%
        layout(legend = list(orientation = "h", x = 0, y = -0.2))
    })

  }


  #### ---- Launch the app ---- ####
  shinyApp(ui, server)
}

