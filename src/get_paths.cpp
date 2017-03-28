#include <Rcpp.h>
using namespace Rcpp;

//[[Rcpp::export]]
List get_paths(IntegerVector parent) {
  std::deque< std::deque<int> > paths;
  int i, next;
  LogicalVector last = is_na(parent);
  for (i = 0; i < parent.size(); ++i) {
    std::deque<int> path;
    next = i;
    while(!last[next]) {
      next = parent[next] - 1;
      path.push_back(next + 1);
    }
    std::reverse(path.begin(), path.end());
    paths.push_back(path);
  }
  return wrap(paths);
}

//[[Rcpp::export]]
List collect_offspring(ListOf<IntegerVector> offspring, IntegerVector order) {
  std::deque< std::deque<int> > offsprings;
  int i, j, node, n_children, child;
  for (i = 0; i < order.size(); ++i) {
    std::deque<int> off(offspring[i].begin(), offspring[i].end());
    offsprings.push_back(off);
  }
  for (i = 0; i < order.size(); ++i) {
    node = order[i] - 1;
    n_children = offsprings[node].size();
    for (j = 0; j < n_children; ++j) {
      child = offsprings[node][j] - 1;
      offsprings[node].insert(offsprings[node].end(), offsprings[child].begin(), offsprings[child].end());
    }
  }
  return wrap(offsprings);
}
