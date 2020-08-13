#' @importFrom shiny NS uiOutput
#' @export
shiny_vector_filter_ui.logical <- function(data, inputId) {
  ns <- shiny::NS(inputId)
  shiny::uiOutput(ns("ui"))
}



#' @export
shiny_vector_filter.logical <- function(data, inputId, ...) {
  function(input, output, session, 
           x = shiny::reactive(logical()), filter_na = shiny::reactive(TRUE), 
           verbose = FALSE) {
    
    ns <- session$ns
    
    x_wo_NA <- shiny::reactive(Filter(Negate(is.na), x()))
    module_return <- shiny::reactiveValues(code = TRUE, mask = TRUE)
    choices <- shiny::reactive({
      Filter(function(i) i %in% x(), c("True" = TRUE, "False" = FALSE, "NA" = NA))
    })
    
    output$ui <- shiny::renderUI({
      filter_log("updating ui", verbose = verbose)
      shiny::div(style = "position: relative;",
                 shiny::div(style = "
          position: absolute; 
          top: -2px; right: 16px; bottom: -2px; left: 16px;
          animation: 
            0.75s ease-out 0s 1 shinyDataFilterEnlargeX, 
            0.5s ease-in  0s 1 shinyDataFilterFadeIn; 
          transform-origin: left;",
                            shiny::plotOutput(ns("plot"), height = "100%")),
                 shiny::checkboxGroupInput(ns("param"), NULL,
                                           choices = choices(),
                                           selected = shiny::isolate(input$param) %||% c(),
                                           width = "100%"))
    })
    
    module_return$code <- shiny::reactive({
      exprs <- list()
      
      # if true return the vector without NAs
      if (TRUE %in% input$param)  exprs <- append(exprs, list(quote(.x)))
      # if false return the vector without NAs
      if (FALSE %in% input$param) exprs <- append(exprs, list(quote(!.x)))
      # if NAs
      if ("" %in% input$param) {
        # if NA, FALSE, and TRUE
        if (length(setdiff(c("", TRUE, FALSE), input$param)) == 0) {
          print("test")
          exprs <- list(quote(is.na(.x)))
          exprs <- append(exprs, list(quote(!.x)))
          exprs <- append(exprs, list(quote(.x)))
        # if NA and FALSE
        } else if (FALSE %in% input$param) {
          exprs <- list(quote(is.na(.x)))
          exprs <- append(exprs, list(quote(!.x)))
        # if NA and TRUE
        } else if (TRUE %in% input$param) {
          exprs <- list(quote(is.na(.x)))
          exprs <- append(exprs, list(quote(.x)))
        # if just NA
        } else {
          exprs <- list(quote(is.na(.x)))
        }
      }
      
      if (length(setdiff(input$param, c("", TRUE, FALSE)))) {
        exprs <- list(quote(is.na(.x)))
        exprs <- append(exprs, list(quote(!.x)))
        exprs <- append(exprs, list(quote(.x)))
      }
        
      if (length(exprs) && length(exprs) < 4) 
        Reduce(function(l, r) bquote(.(l) | .(r)), exprs)
      else 
        TRUE
    })
    
    module_return$mask <- shiny::reactive({
      eval(do.call(substitute, list(module_return$code(), list(.x = x()))))
    })
    
    module_return
  }
}