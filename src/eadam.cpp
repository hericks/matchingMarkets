#include <Rcpp.h>

// [[Rcpp::plugins("cpp11")]]
using namespace Rcpp;

void initialPartition(
  const IntegerMatrix& s_prefs,
  std::vector<int>& temp_singles,
  std::vector<int>& final_singles
);

void partitionStudents(
  const std::vector<int>& students,
  const std::vector<int>& n_proposals_made,
  const IntegerMatrix& s_prefs,
  std::vector<int>& temp_singles,
  std::vector<int>& final_singles
);

// [[Rcpp::export]]
List eadam_cpp(
  IntegerMatrix s_prefs,
  IntegerMatrix c_prefs,
  IntegerVector n_slots,
  String acceptance,
  LogicalVector consent,
  int max_ea_iters 
) {
  int n_students = s_prefs.ncol();
  int n_colleges = c_prefs.ncol();
  
  // create vector of maps storing the college preferences more efficiently
  // key: student -> value: rank of student in college ranking
  std::vector<std::unordered_map<int,int>> c_prefs_maps(n_colleges);
  for(int c = 0; c < n_colleges; ++c) {
    for(int rank = 0; rank < n_students; ++rank) {
      if(c_prefs(rank, c) == NA_INTEGER) {
        break;
      }
      c_prefs_maps[c][c_prefs(rank, c)] = rank;
    }
  }
  
  // vector of students that are temporarily single / definitely stay single
  std::vector<int> temp_singles, final_singles;
  initialPartition(s_prefs, temp_singles, final_singles);
  
  // vector of number of proposals that each student made so far (zero)
  std::vector<int> n_props(n_students);
  
  // vector of assignment maps
  std::vector<std::map<int,int>> assignments(n_colleges);
  
  // 
  int iter = 0;
  
  while(!temp_singles.empty()) {
    // let all temporarily single singles make an offer
    std::map<int, std::vector<int>> offers;
    for(auto s : temp_singles) {
      int next_college = s_prefs(n_props[s], s);
      offers[next_college].push_back(s);
      n_props[s]++;
    }
    
    // iterate over approached colleges and assign proposing students
    std::vector<int> rejected;
    
    for(auto const& offer : offers) {
      int approached = offer.first;
      std::vector<int> proposers = offer.second;
   
      if(acceptance == "deferred") {
        // TODO: Copy
      } else {
        std::map<int,int> possible_assignments;
        for(auto const& p: proposers) {
          if(c_prefs_maps[approached].count(p)) {
            int rank = c_prefs_maps[approached][p];
            possible_assignments[rank] = p;
          } else {
            rejected.push_back(p);
          }
        }
        for(auto const& assignment: possible_assignments) {
          if(assignments[approached].size() >= n_slots[approached]) {
            rejected.push_back(assignment.second);
          } else {
            assignments[approached][assignment.first] = assignment.second;
          }
        }
      }
    }
    
    //
    temp_singles.clear();
    partitionStudents(rejected, n_props, s_prefs, temp_singles, final_singles);
    
    ++iter;
  }
  
  // initialize, fill, and return results list
  List results;
 
  // convert vector of assignment maps to list of matches
  List matches(n_colleges);
  for (auto it = assignments.begin(); it != assignments.end(); ++it) {
    int c = std::distance(assignments.begin(), it);
    std::vector<int> accepted;
    for(std::map<int,int>::iterator mit = it->begin(); mit != it->end(); ++mit) {
      accepted.push_back(mit->second);
    }
    matches[c] = accepted;
  }
  results["matches"] = matches;
  results["singles"] = final_singles;
  results["iter"] = iter;
  return results;
}

void initialPartition(
    const IntegerMatrix& s_prefs,
    std::vector<int>& temp_singles,
    std::vector<int>& final_singles
) {
  // vector of all students 
  int n_students = s_prefs.ncol();
  std::vector<int> students(n_students);
  std::iota (std::begin(students), std::end(students), 0);

  // vector of proposals made by each student (zero)
  std::vector<int> n_props(n_students);
  
  // partition vector of all students each of whom made no proposal yet
  partitionStudents(students, n_props, s_prefs, temp_singles, final_singles);
}

void partitionStudents(
    const std::vector<int>& students,
    const std::vector<int>& n_props,
    const IntegerMatrix& s_prefs,
    std::vector<int>& temp_singles,
    std::vector<int>& final_singles
) {
  int n_colleges = s_prefs.nrow();
  for(const auto& s : students) {
    if(n_props[s] < n_colleges && s_prefs(n_props[s], s) != NA_INTEGER) {
      temp_singles.push_back(s);
    } else {
      final_singles.push_back(s);
    }
  }
}
