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
  (cons (if (string=? (r-object-type x) "NumericVector")
            (r-vector->scheme-vector x numeric_vector_ref robject_length)
            x)
        (r-object-sexp-type x)))

(bind-type robject (c-pointer "RObject") scheme-object->r-object r-object->scheme-object)

(bind*
#<<CPP
robject rffi_eval(const char *str) {
    try {
	// on heap???
	currentRobj = (Rcpp::as< Rcpp::RObject >(rinstance.parseEval(str)));
    } catch(std::exception& ex) {
	std::cerr << "Exception caught: " << ex.what() << std::endl;
	currentRobj.set__(R_NilValue);
    } catch(...){
	std::cerr << "Unknown exception caught" << std::endl;
	currentRobj.set__(R_NilValue);
    }
    return &currentRobj;
}

char *robject_type_asstring(robject robj) {
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

double numeric_vector_ref(robject robj, int i) {
    return (Rcpp::as< Rcpp::NumericVector >(*robj))[i];
}

int integer_vector_ref(robject robj, int i) {
    return (Rcpp::as< Rcpp::IntegerVector >(*robj))[i];
}

char *character_vector_ref(robject robj, int i) {
    return (Rcpp::as< Rcpp::CharacterVector >(*robj))[i];
}

int robject_sexp_type(robject robj) {
    return robj->sexp_type();
}

int robject_length(robject robj) {
    return Rf_length((SEXP) *robj);
}

CPP
)

(define r-eval rffi_eval)

(define (r-object-type robj)
  (robject_type_asstring robj))

(define +sexp-type-table+
  '(
    (0  . NILSXP     ) ;; /* nil = NULL */
    (1  . SYMSXP     ) ;; /* symbols */
    (2  . LISTSXP    ) ;; /* lists of dotted pairs */
    (3  . CLOSXP     ) ;; /* closures */
    (4  . ENVSXP     ) ;; /* environments */
    (5  . PROMSXP    ) ;; /* promises: [un]evaluated closure arguments */
    (6  . LANGSXP    ) ;; /* language constructs (special lists) */
    (7  . SPECIALSXP ) ;; /* special forms */
    (8  . BUILTINSXP ) ;; /* builtin non-special forms */
    (9  . CHARSXP    ) ;; /* "scalar" string type (internal only)*/
    (10 . LGLSXP     ) ;; /* logical vectors */
    (13 . INTSXP     ) ;; /* integer vectors */
    (14 . REALSXP    ) ;; /* real variables */
    (15 . CPLXSXP    ) ;; /* complex variables */
    (16 . STRSXP     ) ;; /* string vectors */
    (17 . DOTSXP     ) ;; /* dot-dot-dot object */
    (18 . ANYSXP     ) ;; /* make "any" args work */
    (19 . VECSXP     ) ;; /* generic vectors */
    (20 . EXPRSXP    ) ;; /* expressions vectors */
    (21 . BCODESXP   ) ;; /* byte code */
    (22 . EXTPTRSXP  ) ;; /* external pointer */
    (23 . WEAKREFSXP ) ;; /* weak reference */
    (24 . RAWSXP     ) ;; /* raw bytes */
    (25 . S4SXP      ) ;; /* S4 non-vector */
    (30 . NEWSXP     ) ;; /* fresh node creaed in new page */
    (31 . FREESXP    ) ;; /* node released by GC */
    (99 . FUNSXP     ) ;; /* Closure or Builtin */
    ))

(define (r-object-sexp-type robj)
  (alist-ref (robject_sexp_type robj) +sexp-type-table+))

)
