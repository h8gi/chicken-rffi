(module rffi
   *

  (import scheme (chicken base) (chicken foreign) bind coops cplusplus-object)

#>
#include <RInside.h>
using Rcpp::RObject;
RInside rinstance(0, 0, true, false, true);
Rcpp::RObject currentRobj;
<#

(bind-type robject (c-pointer "RObject"))
(bind* #<<EOF

robject rffi_eval(const char * str) {
  Rcpp::RObject x = rinstance.parseEval(str);
  // on heap???
  currentRobj = x;
  return &currentRobj;
}

char * RObject_Type_asString (robject robj) {
    char * type_str;
    Rcpp::RObject x = * robj;
    if(Rcpp::is<Rcpp::NumericVector>(x)){
        if(Rf_isMatrix(x)) type_str = "NumericMatrix";
        else type_str = "NumericVector";
    }
    else if(Rcpp::is<Rcpp::IntegerVector>(x)){
        if(Rf_isFactor(x)) type_str = "factor";
        else type_str = "IntegerVector";
    }
    else if(Rcpp::is<Rcpp::CharacterVector>(x))
        type_str = "CharacterVector";
    else if(Rcpp::is<Rcpp::LogicalVector>(x))
        type_str = "LogicalVector";
    else if(Rcpp::is<Rcpp::DataFrame>(x))
        type_str = "DataFrame";
    else if(Rcpp::is<Rcpp::List>(x))
        type_str = "List";
    else if(x.isS4())
        type_str = "S4";
    else if(x.isNULL())
        type_str = "NULL";
    else
        type_str = "unknown";
    C_return (type_str);
}
EOF
)


(define r-eval rffi_eval)
(define robject->string RObject_Type_asString)

;; Scheme -> R
(define (r-object-to value)
  (cond
   [(integer? value) value]
   [else value]))

;; R -> Scheme
;; (define (r-object-from value)
;;   ())


)
