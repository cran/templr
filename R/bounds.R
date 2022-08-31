#' Helper function to scale from [0,1] to [min,max]
#'
#' @param X values to scale
#' @param inp list containing 'min' and 'max' values
#'
#' @return X scaled in [inp$min, inp$max]
#' @export
#'
#' @examples 
#' from01(data.frame(x=matrix(runif(10))),list(x=list(min=10,max=20)))
from01 = function(X, inp) {
    nX = names(X)
    for (i in 1:ncol(X)) {
        namei = nX[i]
        X[,i] = X[,i] * (inp[[ namei ]]$max[[1]]-inp[[ namei ]]$min[[1]]) + inp[[ namei ]]$min[[1]]
    }
    return(X)
}

#' Helper function to scale from [min,max] to [0,1]
#'
#' @param X values to scale
#' @param inp list containing 'min' and 'max' values
#'
#' @return X scaled in [0,1]
#' @export
#'
#' @examples 
#' to01(10+10*data.frame(x=matrix(runif(10))),list(x=list(min=10,max=20)))
to01 = function(X, inp) {
    nX = names(X)
    for (i in 1:ncol(X)) {
        namei = nX[i]
        X[,i] = (X[,i] - inp[[ namei ]]$min[[1]]) / (inp[[ namei ]]$max[[1]]-inp[[ namei ]]$min[[1]])
    }
    return(X)
}


#' Helper function to get $min from 'input' list
#'
#' @param inp lst of objects containing 'min' field (as list)
#'
#' @return array of inp$...$min values
#' @export
#'
#' @examples 
#' min_input(list(x1=list(min=0,max=1),x2=list(min=2,max=3)))
min_input = function(inp){
    m=list()
    for (xi in names(inp)) m[[xi]]=inp[[xi]]$min
    unlist(m)
}


#' Helper function to get $max from 'input' list
#'
#' @param inp lst of objects containing 'max' field (as list)
#'
#' @return array of inp$...$max values
#' @export
#'
#' @examples 
#' max_input(list(x1=list(min=0,max=1),x2=list(min=2,max=3)))
max_input = function(inp){
    m=list()
    for (xi in names(inp)) m[[xi]]=inp[[xi]]$max
    unlist(m)
}