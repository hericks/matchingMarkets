# ----------------------------------------------------------------------------
# R-code (www.r-project.org/) for the Deferred Acceptance Algorithm
#
# Copyright (c) 2013 Thilo Klein
#
# This library is distributed under the terms of the GNU Public License (GPL)
# for full details see the file LICENSE
#
# ----------------------------------------------------------------------------

#' @title Immediate Acceptance Algorithm (a.k.a. Boston mechanism) for two-sided matching markets
#'
#' @description Finds the optimal assignment of students to colleges in the
#' \href{http://en.wikipedia.org/wiki/Hospital_resident}{college admissions} problem
#' based on the Boston mechanism. The algorithmen is also applicable to the stable marriage problem. The option \code{acceptance="deferred"} instead uses the Gale-Shapley
#' (1962) Deferred Acceptance Algorithm with student offer. The function works with either
#' given or randomly generated preferences.
#'
#' @param nStudents integer indicating the number of students (in the college admissions problem)
#' or men (in the stable marriage problem) in the market. Defaults to \code{ncol(s.prefs)}.
#' @param nColleges integer indicating the number of colleges (in the college admissions problem)
#' or women (in the stable marriage problem) in the market. Defaults to \code{ncol(c.prefs)}.
#' @param nSlots vector of length \code{nColleges} indicating the number of places (i.e.
#' quota) of each college. Defaults to \code{rep(1,nColleges)} for the marriage problem.
#' @param s.prefs matrix of dimension \code{nColleges} \code{x} \code{nStudents} with the \code{j}th
#' column containing student \code{j}'s ranking over colleges in decreasing order of
#' preference (i.e. most preferred first).
#' @param c.prefs matrix of dimension \code{nStudents} \code{x} \code{nColleges} with the \code{i}th
#' column containing college \code{i}'s ranking over students in decreasing order of
#' preference (i.e. most preferred first).
#' @param acceptance if \code{acceptance="deferred"} returns the solution found by the student-proposing Gale-Shapley deferred acceptance algorithm; if \code{acceptance="immediate"} (the default) returns the solution found by the Boston mechanism.
#' @param short_match (Optional)  If \code{FALSE} then in the returned matching, free capacities will be indicated with 0 entries. If \code{TRUE}, free capacities will not be reported in the returned matching but an additonal data.frame is returned that contains free capacities. Defaults to \code{TRUE}.
#' @param seed (Optional) integer setting the state for random number generation.
#'
#' @export
#'
#' @section Minimum required arguments:
#' \code{iaa} requires the following combination of arguments, subject to the matching problem.
#' \describe{
#' \item{\code{nStudents, nColleges}}{Marriage problem with random preferences.}
#' \item{\code{s.prefs, c.prefs}}{Marriage problem with given preferences.}
#' \item{\code{nStudents, nSlots}}{College admissions problem with random preferences.}
#' \item{\code{s.prefs, c.prefs, nSlots}}{College admissions problem with given preferences.}
#' }
#' @return
#' \code{iaa} returns a list with the following elements.
#' \item{s.prefs}{student-side preference matrix.}
#' \item{c.prefs}{college-side preference matrix.}
#' \item{iterations}{number of interations required to find the stable matching.}
#' \item{matchings}{edgelist of matches}
#' \item{singles}{identifier of single (or unmatched) students/men.}
#' @author Thilo Klein
#' @keywords algorithms
#' @references Gale, D. and Shapley, L.S. (1962). College admissions and the stability
#' of marriage. \emph{The American Mathematical Monthly}, 69(1):9--15.
#'
#' Kojima, F. and M.U. Unver (2014). The "Boston" school-choice mechanism. \emph{Economic Theory}, 55(3): 515--544.
#'
#' @examples
#' ##\dontrun{
#' ## --------------------------------
#' ## --- College admission problem
#'
#' s.prefs <- matrix(c(1,2,3,
#'                     1,2,3,
#'                     1,2,3,
#'                     2,1,3,
#'                     2,1,3),
#'                   byrow = FALSE, ncol = 5, nrow = 3); s.prefs
#' c.prefs <- matrix(c(1,4,2,3,5,
#'                     5,2,3,4,1,
#'                     1,2,3,4,5),
#'                   byrow = FALSE, ncol = 3, nrow = 5); c.prefs
#' nSlots <- c(2,2,1)
#'
#' ## Boston mechanism
#'  iaa(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots)$matchings
#'
#' ## Gale-Shapley algorithm
#'  iaa(s.prefs = s.prefs, c.prefs = c.prefs, nSlots = nSlots, acceptance="deferred")$matchings
#'
#' ## Same results for the Gale-Shapley algorithm with hri2() function (but different format)
#'  set.seed(123)
#'  iaa(nStudents=7, nSlots=c(3,3), acceptance="deferred")$matchings
#'  set.seed(123)
#'  hri2(nStudents=7, nSlots=c(3,3))$matchings
#'  ##}



hri3 <- function(
    nStudents=ncol(s.prefs),
    nColleges=ncol(c.prefs), 
    nSlots=rep(1,nColleges), 
    s.prefs=NULL,
    c.prefs=NULL,
    acceptance="immediate",
    consent=rep(FALSE, nStudents),
    eaItersBound=1e6,
    short_match = TRUE,
    seed = NULL
){
  # set seed if provided
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  # if 'nColleges' not given, obtain it from `nSlots`
  if(is.null(nColleges)){
    nColleges <- length(nSlots)
  }
  
  # if `s.prefs` not given, generate student preferences randomly
  if(is.null(s.prefs)){
    s.prefs <- replicate(n=nStudents,sample(seq(from=1,to=nColleges,by=1)))
  }
  
  # if `c.prefs` not given, generate college preferences randomly
  if(is.null(c.prefs)){
    c.prefs <- replicate(n=nColleges,sample(seq(from=1,to=nStudents,by=1)))
  }
  
  # consistency checks
  if(
    dim(s.prefs)[1] != dim(c.prefs)[2] | dim(s.prefs)[2] != dim(c.prefs)[1] |
    dim(s.prefs)[1] != nColleges | dim(s.prefs)[2] != nStudents
  ){
    stop("'s.prefs' and 'c.prefs' must be of dimensions 'nColleges x nStudents' and 'nStudents x nColleges'!")
  }
  
  if(length(nSlots) != nColleges) {
    stop("length of 'nSlots' must equal 'nColleges' and the number of columns of 'c.prefs'!")
  }
 
  # run cpp backend 
  res_cpp <- eadam_cpp(
    s.prefs-1, c.prefs-1, 
    nSlots,
    acceptance,
    consent, eaItersBound
  )
  
  
  finish(
    s.prefs, c.prefs,
    res$iter,
    lapply(res_cpp$matches, function(x) x + 1),
    res_cpp$singles,
    short_match
  )
}

finish <- function(s.prefs,c.prefs,iter,c.hist,s.singles,short_match){ #,z=z
  if(short_match == FALSE){
    return(list(s.prefs=s.prefs,c.prefs=c.prefs,iterations=iter,matchings=edgefun(x=c.hist),singles=s.singles)) #,z=z
  }
  else {
    # Format matching
    matching <- edgefun(x=c.hist)
    
    free_caps <- lapply(1:ncol(c.prefs), function(col){
      return(nrow(matching[matching$college == col & matching$student == 0,]))
    })
    free_caps <- data.frame(free_caps)
    colnames(free_caps) <- 1:ncol(c.prefs)
    
    matching <- matching[matching$student != 0, ]
    return(list(s.prefs=s.prefs,c.prefs=c.prefs,iterations=iter,matchings=matching,singles=s.singles, free_cap = free_caps)) #,z=z
  }
}

## convert match matrix to edgelist
edgefun <- function(x){
  res <- data.frame(college = c(unlist( sapply(1:length(x), function(i){
    rep(i,length(x[[i]]))
  }) )),
  student = unlist(x),
  stringsAsFactors = FALSE)
  #browser()
  res <- with(res, res[order(college, student),])
}