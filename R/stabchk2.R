#' @title Stability-Check
#'
#' @description Checks a given two sided matching for blocking pairs.
#'
#' @param matching matrix (or data frame) of matchings. Data frame or matrix with two columns containing in column 1 the colleges and in column 2 the students with each row forming a couple.
#' @param c.prefs college preferences. If \code{withTies} is set to \code{FALSE}, matrix with \code{nColleges} columns. Otherwise, matrix of dimension \code{nStudents} x \code{nColleges}.
#' @param s.prefs student preferences. If \code{withTies} is set to \code{FALSE}, matrix with \code{nStudents} columns. Otherwise, matrix of dimension \code{nColleges} x \code{nStudents}.
#' @param nSlots capacity for each college. \code{NULL} or a vector of length \code{nColleges} giving the capacity for each college.
#' @param withTies logical. If \code{FALSE} (the default), ranks are inferred from the ordering of the preference matrices. Otherwise, ranks must be explicitly given in the preference matrices (see Details).
#' @param envyEmpty logical. Only used if \code{nSlots} is \code{NULL}. If \code{TRUE} (the default), students may have justified envy towards empty colleges (see Details).
#' 
#' @details If \code{withTies} is set to \code{FALSE}, then the default format for the preference matrices is expected, i.e. \code{c.prefs[r,c]} contains the student of rank \code{r} in the ranking of college \code{c}. Missing values are used to indicate unacceptable students/colleges. Especially, this does not allow for ties in the rankings, since ranks are inferred by row. If \code{withTies} is set to \code{TRUE}, then the preference matrices must explicitly contain the ranks, i.e. \code{c.prefs[s,c]} contains the rank of student \code{s} in the ranking of college \code{c}.
#' 
#' @details If \code{nSlots} is not \code{NULL}, then any college with fewer assigned students than available slots is treated as if it was assigned an unacceptable student. Otherwise, this is controlled by \code{envyEmpty}. If \code{envyEmpty} is set to \code{TRUE} (the default), then any college without any matches is treated as if it was assigned an unacceptable student, i.e. it forms a blocking pair with any acceptable student that prefers this college over its assignment. Otherwise, such a college cannot be part of any blocking pair.
#' 
#' @export
#'
#' @return \code{stabchk} returns a data frame with as many rows as blocking pairs were found. Column 1 indicates the college and column 2 indicate the student of the blocking pairs.
stabchk2 <- function(
    matching,
    c.prefs,
    s.prefs,
    nSlots=NULL,
    withTies=FALSE,
    envyEmpty=TRUE
){
  # infer number of colleges and students from preference matrices
  nColleges <- ncol(c.prefs)
  nStudents <- ncol(s.prefs)
  
  # convert preference matrices from standard matchingMarkets format
  if(!withTies) {
    # before:
    #   s.prefs[r,s] contains college with rank r in ranking of student s
    #   (does not allow for ties)
    # after: 
    #   s.prefs[c,s] contains rank of college c in ranking of student s
    #   (does allow for ties)
    transformCol <- function(col, nRows) {
      res <- rep(NA, nRows)
      res[col[!is.na(col)]] <- 1:sum(!is.na(col))
      res
    }
    s.prefs <- apply(s.prefs, 2, transformCol, nColleges)
    c.prefs <- apply(c.prefs, 2, transformCol, nStudents)
  }
  
  # consistency check of dimensions
  if(nrow(c.prefs) != nStudents || nrow(s.prefs) != nColleges) {
    stop("If 'withTies' is set to FALSE, then s.prefs' and 'c.prefs' must be of dimensions 'nColleges x nStudents' and 'nStudents x nColleges'!")
  }
 
  # consistency check of matched colleges
  if(max(matching[,1]) > nColleges) {
    stop(paste0("'matching' matrix contains college ", max(matching[,1]), " that is out of bounds of the preference matrices (inferred 'nColleges' to be ", nColleges, ")!"))
  }
  
  # consistency check of matched students
  if(max(matching[,2]) > nStudents) {
    stop(paste0("'matching' matrix contains student ", max(matching[,2]), " that is out of bounds of the preference matrices (inferred 'nStudents' to be ", nStudents, ")!"))
  }
  
  if(is.null(nSlots)) {
    # when nSlots not explicitly passed
    has_capacity_left <- sapply(1:nColleges, function(c) {
      if(envyEmpty) sum(matching[,1]==c) == 0 else FALSE
    })
  } else {
    # consistency check of nSlots
    if(length(nSlots) != nColleges) {
      stop(paste0("'nSlots' must have length 'nColleges'!")) 
    }
    
    # for each college whether it has more slots than number of admitted students
    has_capacity_left <- sapply(1:nColleges, function(c) {
      sum(matching[,1]==c) < nSlots[c]
    })
  }
  
  # for each college the worst rank among all ranks of admitted students
  college_ranks <- sapply(1:nColleges, function(c) {
    matched_students <- matching[matching[,1]==c,2]
    ranks <- c.prefs[matched_students,c]
    return(if(has_capacity_left[c] || any(is.na(ranks))) Inf else max(ranks)) 
  })
  
  # create empty matrix to append rows of blocking pairs to 
  blocking_pairs <- matrix(
    nrow = 0, ncol = 2, 
    dimnames=list(NULL, c('college', 'student'))
  )
  
  # iterate over all colleges c and students s that are preferred by college c
  # over its worst admitted student
  for(c in 1:nColleges){
    for(s in which(c.prefs[,c] < college_ranks[c])){
      # rank possible for the student s, when switching to college c
      possible_rank <- s.prefs[c,s]
      # rank of currently assigned college in student's ranking
      matched_college <- matching[match(s, matching[,2]),1]
      current_rank <- s.prefs[matched_college,s]
      if(!is.na(possible_rank) && (is.na(current_rank) || possible_rank < current_rank)) {
        blocking_pairs <- rbind(blocking_pairs, c(c, s))
      }
    }
  }
 
  # return blocking pairs as data.frame 
  return(data.frame(blocking_pairs))
}
