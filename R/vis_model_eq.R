#' Visualize the functional form of a CSM equation
#'
#' @param model a list vector containing a CSM as created by
#'  [csmbuilder::csm_create_model()]
#'
#' @param name a character string of the variable name whose equation should
#'  be visualized
#'
#'  @param mapping an aesthetic mapping as defined by [ggplot2::aes()]
#'
#' @param ... other arguments corresponding to variables in the model equation
#'  to be visualized
#'
#' @export
#'
vis_model_eq <- function(model, name, mapping, ...){

  if(!requireNamespace("ggplot2", quietly = TRUE)){
    paste0(
      "The vis_model_eq() function requires the ggplot2 package.",
      " Please install and try again.") |>
      stop()
  }else{
    mod_vars <- extract_model_variables(model)
    if(! name %in% names(mod_vars)){
      paste0("Variable '", name, "' was not found in the defined model.") |>
        stop()
    }
    eq <- attr(mod_vars[[name]], "equation")
    if(is.null(eq)){
      paste0("Variable '", name, "' does not have an equation to visualize.") |>
        stop()
    }
  }

  if(length(list(...)) > 0){
    input_df <-
      tryCatch({
        data.frame(...)
      }, error = \(err){
        c("Problem combining other arguments (...) into a data frame: ",
          as.character(err)) |>
          stop()
      })
  }

  if(missing(mapping)){
    mapping <- ggplot2::aes(x = .data[[colnames(input_df)[1]]],
                            y = .data[[name]])
  }

  input_df |>
    within({
      eq |>
        as.list() |>
        getElement(2) |>
        deparse() |>
        paste0(name, " = ", x = _) |>
        parse(text = _) |>
        eval()
    }) |>
    ggplot2::ggplot(data = _,
                    mapping = mapping) +
    ggplot2::geom_line()
}

extract_model_variables <- function(x){
  sub_classes <-
    lapply(x, class) |>
    unlist() |>
    unique()
  if(! "csm_variable" %in% sub_classes &
     is.list(x)){
    lapply(x, extract_model_variables) |>
      unname() |>
      do.call(c, args = _)
  }else if("csm_data_structure" %in% class(x)){
    attr(x, "variables") |>
      lapply(extract_model_variables)
  # }else if(is.list(x)){
  #   lapply(x, extract_model_variables) |>
  #     do.call(c, args = _)
  # }else if("csm_variable" %in% class(x)){
  }else{
    x
  }
}
