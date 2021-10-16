
get_final_values <- function(model = NULL)
{
  if (is.null(model))
  {
    return(NULL)
  }else if (model$parallel){
    to_return <- vector("list", length(model$model))
    for (i in 1:length(model$model))
    {
      to_return[[i]] <- eval(parse(text = (paste0("as.list(model$model$cluster",
                                                  i,
                                                  "$state()[[1]])"))))
    }
    return(to_return)
  }else{
    return(as.list(model$model$state()))
  }
  
  
}
