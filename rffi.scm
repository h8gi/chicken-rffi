(module rffi
   *

  (import scheme
          (chicken base)
          (chicken foreign)
          bind
          srfi-69)

#>
#include <RInside.h>
using Rcpp::RObject;
RInside rinstance(0, NULL, false, false, true);
<#

(define +sexp-type-table+
  (alist->hash-table '(
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
                       )))

(define +r-object->scheme-object-table+ (make-hash-table))
(define +scheme-object->r-object-table+ (make-hash-table))

(define (r-eval str)
  (rffi_eval str))

(define (r-object-sexp-type rsxp)
  (hash-table-ref +sexp-type-table+ (rffi_sexp_type rsxp)))


;; Scheme -> R
(define (scheme-object->r-object value)
  (cond [(integer? value) value]
        [else value]))

;; R -> Scheme
(define (r-object->scheme-object x)
  (let* ([type (r-object-sexp-type x)]
         [conv (hash-table-ref/default +r-object->scheme-object-table+ type identity)])
    (cons type (conv x))))

(bind-type rffi_sexp c-pointer scheme-object->r-object r-object->scheme-object)

(bind*
#<<CPP
rffi_sexp rffi_eval(const char *str) {
    try {
	// on heap???
	return rinstance.parseEval(str);
    } catch(std::exception& ex) {
	std::cerr << "Exception caught: " << ex.what() << std::endl;
	return R_NilValue;
    } catch(...){
	std::cerr << "Unknown exception caught" << std::endl;
	return R_NilValue;
    }
}

double numeric_vector_ref(rffi_sexp rsxp, int i) {
    return (REAL((SEXP) rsxp))[i];
}

int integer_vector_ref(rffi_sexp rsxp, int i) {
    return (INTEGER((SEXP) rsxp))[i];
}

int rffi_sexp_type(rffi_sexp rsxp) {
    return TYPEOF((SEXP) rsxp);
}

int rffi_sexp_length(rffi_sexp rsxp) {
    return Rf_length((SEXP) rsxp);
}

CPP
)

(define (make-r-vector->scheme-object ref-proc len-proc)
  (lambda (rvec)
    (let ([length (len-proc rvec)])
      (if (= length 1)
          (ref-proc rvec 0)
          (do ((vector (make-vector length))
               (i 0 (+ i 1)))
              ((= i length) vector)
            (vector-set! vector i (ref-proc rvec i)))))))

(hash-table-set! +r-object->scheme-object-table+
                 'REALSXP
                 (make-r-vector->scheme-object numeric_vector_ref rffi_sexp_length))

(hash-table-set! +r-object->scheme-object-table+
                 'INTSXP
                 (make-r-vector->scheme-object integer_vector_ref rffi_sexp_length))


)
