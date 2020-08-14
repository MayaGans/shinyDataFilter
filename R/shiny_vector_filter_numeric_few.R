#' A vector filter for numeric variables with only a few choices
#'
#' @param input requisite shiny module field specifying incoming ui input
#'   reactiveValues
#' @param output requisite shiny module field capturing output for the shiny
#'   data filter ui
#' @param session requisite shiny module field containing the active shiny
#'   session
#' @param x The TODO
#' @param filter_na The \code{logical} TODO
#' @param verbose a \code{logical} value indicating whether or not to print log
#'  statements out to the console
#'
#' @importFrom shiny reactive reactiveValues renderUI div plotOutput sliderInput
#'   isolate tags validate need renderPlot
#' @importFrom ggplot2 ggplot aes aes_ geom_area theme_void scale_x_continuous
#'   scale_y_continuous
#' @importFrom grDevices rgb
#' @importFrom stats density
#' @export
shiny_vector_filter_numeric_few <- function(input, output, session,
            x = shiny::reactive(factor()),  #important: changed x to factor here
           filter_na = shiny::reactive(FALSE), verbose = FALSE) {
    
  ns <- session$ns
  
  x_wo_NA <- shiny::reactive(Filter(Negate(is.na), x()))
  module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
  
  choices <- shiny::reactive(unique(as.character(sort(x_wo_NA()))))
  
  output$ui <- shiny::renderUI({
    filter_log("updating ui", verbose = verbose)
    shiny::div(style = "position: relative;",
               shiny::div(style = "
                          position: absolute; 
                          top: -2px; right: 16px; bottom: -2px; left: 16px;
                          animation: 
                          0.75s ease-out 0s 1 shinyDataFilterEnlargeX, 
                          0.5s ease-in  0s 1 shinyDataFilterFadeIn; 
                          transform-origin: left;" #,
               ),
               shiny::checkboxGroupInput(ns("param"), NULL,
                                         choices = c(choices(), NA),
                                         selected = shiny::isolate(input$param) %||% c(),
                                         width = "100%"))
  })
  
  module_return$code <- shiny::reactive({
    if ("" %in% input$param)
      bquote(.x %in% .(c(if (filter_na()) c() else NA, as.numeric(input$param[input$param != ""]))))
    else if (length(input$param))
      bquote(.x %in% .(c(if (filter_na()) c() else as.numeric(input$param))))
    else if (filter_na())
      bquote(!is.na(.x))
    else
      TRUE
  })
  
  module_return$mask <- shiny::reactive({
    eval(do.call(substitute, list(module_return$code(), list(.x = x())))) # added numeric() to return val, got errors. Then removed
  })
  
  module_return
}