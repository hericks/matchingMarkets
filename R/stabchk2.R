#' @title Stability-Check
#'
#' @param matching data frame or matrix of dimension (\code{min[nColleges, nStudents]}) x 2 containing in column 1 the colleges and in column 2 the students with each row forming a couple.
#' @param c.prefs matrix of dimension \code{nStudents} x \code{nColleges} with column c containing the ranks of the students in the ranking of college c (lower ranks are preferred).
#' @param s.prefs matrix of dimension \code{nColleges} x \code{nStudents} with column s containing the ranks of the colleges in the ranking of student s (lower ranks are preferred).
#' 
#' @export
#'
#' @return \code{stabchk} returns a data frame with as many rows as blocking pairs were found. Column 1 indicates the college and column 2 indicate the student of the blocking pairs. Returns \code{NULL} if no blocking pair is found.
stabchk2 <- function(
    matching,
    c.prefs,
    s.prefs,
    nColleges = ncol(c.prefs),
    nStudents = ncol(s.prefs)
){
  # infer number of colleges and students from preference matrices
  nColleges <- ncol(c.prefs)
  nStudents <- ncol(s.prefs)
  
  # consistency check of dimensions
  if(nrow(c.prefs) != nStudents || nrow(s.prefs) != nColleges) {
    stop("'s.prefs' and 'c.prefs' must be of dimensions 'nColleges x nStudents' and 'nStudents x nColleges'!")
  }
 
  # consistency check of matched colleges
  if(max(matching[,1]) > nColleges) {
    stop(paste0("'matching' matrix contains college ", max(matching[,1]), " that is out of bounds of the preference matrices (inferred 'nColleges' to be ", nColleges, ")!"))
  }
  
  # consistency check of matched students
  if(max(matching[,2]) > nStudents) {
    stop(paste0("'matching' matrix contains student ", max(matching[,2]), " that is out of bounds of the preference matrices (inferred 'nStudents' to be ", nStudents, ")!"))
  }
  
  # for each college compute the worst rank among all ranks of admitted students
  college_ranks <- sapply(1:nColleges, function(c){
    matched_students <- matching[matching[,1]==c,2]
    ranks <- c.prefs[matched_students,c]
    return(if(length(ranks) == 0L || any(is.na(ranks))) Inf else max(ranks)) 
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
  
  if(nrow(blocking_pairs) == 0){
    print('No blocking pairs!')
    return(NULL)
  } else{
    return(data.frame(blocking_pairs))
  }
}
