#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

library(magrittr)
dt_generate_function <- function(df,shape = "compact",round_col = NULL,perc_col = NULL,caption = "",
                                 editable=FALSE,escape=T, dom = "tp", rownames=FALSE,
                                 digits=0,show_buttons=FALSE,digits_perc=1, pageLength = NULL) {
  result <- DT::datatable(class = shape,data = df,rownames = rownames, escape=escape,
                          caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left;',
                                          caption),editable=editable,extensions = "Buttons",
                          options = list(pageLength = pageLength,
                                         buttons=c("copy","excel"),scrollX=TRUE,dom = ifelse(show_buttons,'Bfrtip',dom),info=FALSE,
                                         paging=ifelse(is.null(pageLength),FALSE,TRUE),searching=FALSE, 
                                         columnDefs = list(list(className = 'dt-center', targets = "_all")))) %>%
    DT::formatStyle(color = "#919191",columns = 1:ncol(df))
  
  
  if (!is.null(round_col) & is.null(perc_col)) {return(result %>% 
                                                         DT::formatRound(columns = round_col,digits = digits,dec.mark = ".") ) }
  
  else if (is.null(round_col) & is.null(perc_col)) { return(result)  }
  
  else if (!is.null(round_col) & !is.null(perc_col)) {return(result %>% 
                                                               DT::formatRound(columns = round_col,digits = digits) %>%
                                                               DT::formatPercentage(columns = perc_col,digits = digits_perc) ) }
  else {return( result %>% DT::formatPercentage(columns = perc_col,digits = digits_perc) )  }
  
}
