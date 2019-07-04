(module rffi
   *

  (import scheme (chicken base) (chicken foreign) bind coops cplusplus-object)

#>
#include <RInside.h>
using Rcpp::RObject;

RInside rinstance(0, NULL, false, false, true);
Rcpp::RObject currentRobj;
<#

;; Scheme -> R
(define (scheme-object->r-object value)
  (cond [(integer? value) value]
        [else value]))

(define (r-vector->scheme-vector value ref-proc len-proc)
  (let ([length (len-proc value)])
    (do ((vector (make-vector length))
         (i 0 (+ i 1)))
        ((= i length) vector)
      (vector-set! vector i (ref-proc value i)))))

;; R -> Scheme
(define (r-object->scheme-object x)
  (if (string=? (r-object-type x) "NumericVector")
      (r-vector->scheme-vector x NumericVector_ref R_length)
      "<???>"))

(bind-type robject (c-pointer "RObject") scheme-object->r-object r-object->scheme-object)

(bind*
#<<CPP
robject rffi_eval(const char *str) {
    Rcpp::RObject x = rinstance.parseEval(str);
    // on heap???
    currentRobj = x;
    return &currentRobj;
}

char *RObject_Type_asString(robject robj) {
    char *type_str;
    Rcpp::RObject x = *robj;
    if (Rcpp::is<Rcpp::NumericVector>(x)) {
	if (Rf_isMatrix(x))
	    type_str = "NumericMatrix";
	else
	    type_str = "NumericVector";
    } else if (Rcpp::is<Rcpp::IntegerVector>(x)) {
	if (Rf_isFactor(x))
	    type_str = "factor";
	else
	    type_str = "IntegerVector";
    } else if (Rcpp::is<Rcpp::CharacterVector>(x))
	type_str = "CharacterVector";
    else if (Rcpp::is<Rcpp::LogicalVector>(x))
	type_str = "LogicalVector";
    else if (Rcpp::is<Rcpp::DataFrame>(x))
	type_str = "DataFrame";
    else if (Rcpp::is<Rcpp::List>(x))
	type_str = "List";
    else if (x.isS4())
	type_str = "S4";
    else if (x.isNULL())
	type_str = "NULL";
    else
	type_str = "unknown";
    C_return(type_str);
}

double NumericVector_ref(robject robj, int i) {
    return (Rcpp::as< Rcpp::NumericVector >(*robj))[i];
}

int IntegerVector_ref(robject robj, int i) {
    return (Rcpp::as< Rcpp::IntegerVector >(*robj))[i];
}

char *CharacterVector_ref(robject robj, int i) {
    return (Rcpp::as< Rcpp::CharacterVector >(*robj))[i];
}
int R_length(robject robj) {
    return Rf_length((SEXP) *robj);
}

CPP
)

(define r-eval rffi_eval)

(define (r-object-type robj)
  (RObject_Type_asString robj))

)
