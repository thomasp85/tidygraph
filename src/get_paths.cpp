#include <vector>
#include <cpp11/list.hpp>
#include <cpp11/list_of.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/logicals.hpp>

[[cpp11::register]]
cpp11::list get_paths(cpp11::integers parent) {
  cpp11::writable::list paths;
  int next = 0;
  for (int i = 0; i < parent.size(); ++i) {
    std::vector<int> path;
    next = i;
    while(!cpp11::is_na(parent[next])) {
      next = parent[next] - 1;
      path.push_back(next + 1);
    }
    std::reverse(path.begin(), path.end());
    paths.push_back(cpp11::integers(path));
  }
  return paths;
}

[[cpp11::register]]
cpp11::list collect_offspring(cpp11::list_of<cpp11::integers> offspring, cpp11::integers order) {
  cpp11::writable::list offsprings;

  for (R_len_t i = 0; i < order.size(); ++i) {
    cpp11::writable::integers off(offspring[i].begin(), offspring[i].end());
    offsprings.push_back(off);
  }

  for (R_len_t i = 0; i < order.size(); ++i) {
    int node = order[i] - 1;

    cpp11::writable::integers off = cpp11::as_cpp<cpp11::writable::integers>(offsprings[node]);
    for (R_len_t j = 0; j < off.size(); ++j) {
      int child = off[j] - 1;
      cpp11::integers child_off = cpp11::as_cpp<cpp11::integers>(offsprings[child]);
      for (R_len_t k = 0; k < child_off.size(); ++k) {
        off.push_back(child_off[k]);
      }
    }
  }
  return offsprings;
}
