##################################################################################################
# Below is the R code to be included in the InitErgmTerm.R file from the ergm source code. 
# This creates a version of the term for the binary ERGM from which the valued ERGM will draw from.
# For an introduction to working with the ergm source code, I recommend Hunter, David R., Steven
# M. Goodreau, and Mark S. Handcock. 2013. "ergm.userterms: A Template Package for Extending statnet."
# Journal of Statistical Software 52(2).
##################################################################################################

### nodematchhigh ###

InitErgmTerm.nodematchhigh<-function (nw, arglist, ...) {
  ### First need to check network and arguments
  a <- check.ErgmTerm(nw, arglist, 
                      varnames = c("cut", "attrname", "keep", "levels"),
                      vartypes = c("numeric", "character", "numeric", "character,numeric,logical"),
                      defaultvalues = list(NULL, NULL, NULL, NULL),
                      required = c(TRUE, TRUE, FALSE, FALSE))
  ### Process all arguments
  cutpt<-a$cut # cut point
  nodecov <- # name of actor-level attribute 
    if(length(a$attrname)==1)
      get.node.attr(nw, a$attrname)
  else{
    do.call(paste,c(sapply(a$attrname,function(oneattr) get.node.attr(nw,oneattr),simplify=FALSE),sep="."))
  }
  u <- NVL(a$levels, sort(unique(nodecov)))
  if (!is.null(a$keep)) {
    u <- u[a$keep]
  }
  nodecov <- match(nodecov,u,nomatch=length(u)+1)
  dontmatch <- nodecov==(length(u)+1)
  nodecov[dontmatch] <- length(u) + (1:sum(dontmatch))
  ui <- seq(along=u)
  coef.names <- paste("nodematchhigh", paste(a$attrname,collapse="."), paste(a$cut), sep=".")
  inputs <- c(as.double(cutpt),nodecov)
  
  list(name="nodematchhigh",                                 
       coef.names = coef.names,                          
       inputs =  inputs,
       dependence = FALSE, 
       minval = 0
  )
}

### nodematchlow ###

InitErgmTerm.nodematchlow<-function (nw, arglist, ...) {
  ### First need to check network and arguments
  a <- check.ErgmTerm(nw, arglist, 
                      varnames = c("cut", "attrname", "keep", "levels"),
                      vartypes = c("numeric", "character", "numeric", "character,numeric,logical"),
                      defaultvalues = list(NULL, NULL, NULL, NULL),
                      required = c(TRUE, TRUE, FALSE, FALSE))
  
  ### Process all arguments
  cutpt<-a$cut # cut point
  nodecov <- # name of actor-level attribute 
    if(length(a$attrname)==1)
      get.node.attr(nw, a$attrname)
  else{
    do.call(paste,c(sapply(a$attrname,function(oneattr) get.node.attr(nw,oneattr),simplify=FALSE),sep="."))
  }
  u <- NVL(a$levels, sort(unique(nodecov)))
  if (!is.null(a$keep)) {
    u <- u[a$keep]
  }
  nodecov <- match(nodecov,u,nomatch=length(u)+1)
  dontmatch <- nodecov==(length(u)+1)
  nodecov[dontmatch] <- length(u) + (1:sum(dontmatch))
  ui <- seq(along=u)
  coef.names <- paste("nodematchlow", paste(a$attrname,collapse="."), paste(a$cut), sep=".")
  inputs <- c(as.double(cutpt),nodecov)
  
  list(name="nodematchlow",                                
       coef.names = coef.names,                          
       inputs =  inputs,
       dependence = FALSE,
       minval = 0
  )
}




##################################################################################################
# Below is the R code to be included in the InitWtErgmTerm.R file from the ergm source code. 
# This creates a version of the term for the valued ERGM to use directly.
##################################################################################################

### nodematchhigh ###

InitWtErgmTerm.nodematchhigh<-function (nw, arglist, ...) {
  ### First need to check network and arguments
  a <- check.ErgmTerm(nw, arglist, 
                      varnames = c("cut", "attrname", "keep", "levels", "form"),
                      vartypes = c("numeric", "character", "numeric", "character,numeric,logical", "character"),
                      defaultvalues = list(NULL,  NULL, NULL, NULL, "nonzero"),
                      required = c(TRUE, TRUE, FALSE, FALSE, FALSE))
  binary_dind_wrap("nodematchhigh", nw, a, ...) # links to binary ergm term
}


### nodematchlow ###

InitWtErgmTerm.nodematchlow<-function (nw, arglist, ...) {
  ### First need to check network and arguments
  a <- check.ErgmTerm(nw, arglist, 
                      varnames = c("cut", "attrname", "keep", "levels", "form"),
                      vartypes = c("numeric", "character", "numeric", "character,numeric,logical", "character"),
                      defaultvalues = list(NULL,  NULL, NULL, NULL, "nonzero"),
                      required = c(TRUE, TRUE, FALSE, FALSE, FALSE))
  binary_dind_wrap("nodematchlow", nw, a, ...) # links to binary ergm term
}


