#include <Rcpp.h>
using namespace Rcpp;

// Function for string in char vector lookups
bool pystr_in_charvect(CharacterVector x, CharacterVector table) {
  if(is_true(all(is_na(table)))) return false;
  
  IntegerVector res = match(x, table);
  if(res[0] == NA_INTEGER) {
    return false;
  }
  return true;
}

// Template function for char vector in list lookups and list in list lookups
template<typename T>
bool pystr_in_list(const T &x, List &table) {
  unsigned int x_len = x.size();
  bool out = false;
  SEXPTYPE x_sexp = TYPEOF(x);
  
  List::iterator it;
  List::iterator tbl_end = table.end();
  for(it = table.begin(); it != tbl_end; ++it) {
    if(TYPEOF(*it) != x_sexp || LENGTH(*it) != x_len) {
      continue;
    }
    
    T curr_tbl = *it;
    if(is_true(all(curr_tbl == x))) {
      out = true;
      break;
    }
  }
  
  return out;
}

// Exported function
// [[Rcpp::export]]
bool pystr_in_(SEXP &x, SEXP &table) {
  if(LENGTH(x) == 1 && TYPEOF(table) == STRSXP) {
    CharacterVector x_ = CharacterVector(x);
    CharacterVector table_ = CharacterVector(table);
    return pystr_in_charvect(x_, table_);
  }
  
  if(TYPEOF(table) == VECSXP) {
    List table_ = List(table);
    
    switch (TYPEOF(x)) {
      case STRSXP: return pystr_in_list<CharacterVector>(x, table_);
      case VECSXP: return pystr_in_list<List>(x, table_);
    }
  }
  
  return false;
}
