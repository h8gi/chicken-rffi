(module rffi
   *

  (import scheme
          (chicken base)
          (chicken foreign)
          bind
          srfi-4
          srfi-69)

#>
#include <RInside.h>
#include <stdarg.h>
using Rcpp::RObject;
RInside rinstance(0, NULL, false, false, true);
<#

(define-foreign-type SEXP
  (c-pointer "SEXP"))

(define (r-apply fname lst)
  (assert (and 'r-apply (list? lst)))
  (receive (value err)
      ((foreign-primitive void ((c-string fname) (scheme-object rsxp_list))
         "
Rcpp::Function docall(\"do.call\");
Rcpp::Function f(fname);
Rcpp::List args = Rcpp::List::create();
while(!C_truep(C_i_nullp(rsxp_list))) {
    Rcpp::RObject a = (SEXP)(C_c_pointer_or_null(C_u_i_car(rsxp_list)));
    args.push_back(a);
    rsxp_list = C_u_i_cdr(rsxp_list);
}

C_word *pointer = C_alloc(C_SIZEOF_POINTER);
SEXP value;
int error = 0;
try {
   value = docall(f, args);
} catch(...) {
   error = -1;
}
C_word av[4];
av[0] = C_SCHEME_UNDEFINED;
av[1] = C_k;
av[2] = C_mpointer(&pointer, value);
av[3] = C_fix(error);
C_values(4, av);
") fname lst)
    (if (zero? err)
        value
        (error "r error - r-apply"))))

(define (r-eval-string str)
  (receive (value err)
      ((foreign-primitive void ((c-string str))
         "
C_word *pointer = C_alloc(C_SIZEOF_POINTER);
SEXP value;
int error = 0;
try {
    value = rinstance.parseEval(str);
} catch(...){
    error = -1;
}
C_word av[4];
av[0] = C_SCHEME_UNDEFINED;
av[1] = C_k;
av[2] = C_mpointer(&pointer, value);
av[3] = C_fix(error);
C_values(4, av);
")
       str)
    (if (zero? err)
        value
        (error "r error - r-eval-string"))))

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

(define (r-lambda funname)
  (lambda args
    (rffi_apply funname args)))

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
    (list type (conv x))))

(bind-type rffi_sexp c-pointer
           ;; scheme-object->r-object r-object->scheme-object
           )

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
// rsxpのscheme listであるべき
rffi_sexp rffi_apply(const char *func_name, C_word rsxp_list) {
    Rcpp::Function docall("do.call");
    Rcpp::Function f(func_name);
    Rcpp::List args = Rcpp::List::create();
    while(!C_truep(C_i_nullp(rsxp_list))) {
	Rcpp::RObject a = (SEXP)(C_c_pointer_or_null(C_u_i_car(rsxp_list)));
	args.push_back(a);
	rsxp_list = C_u_i_cdr(rsxp_list);
    }
    return docall(f, args);
}

double numeric_vector_ref(rffi_sexp rsxp, int i) {
    return (REAL((SEXP) rsxp))[i];
}

rffi_sexp r_numeric_vector(double * vec, ___length(vec) int len) {
    return Rcpp::NumericVector(vec, vec + len);
}

int integer_vector_ref(rffi_sexp rsxp, int i) {
    return (INTEGER((SEXP) rsxp))[i];
}

rffi_sexp list_ref(rffi_sexp rsxp, int i) {
    return Rcpp::as<Rcpp::List>((SEXP) rsxp)[i];
    // return VECTOR_ELT((SEXP) rsxp, i);
}

rffi_sexp r_list_append(rffi_sexp rsxp, rffi_sexp x) {
    std::cout << x << "\n";
    (Rcpp::as<Rcpp::List>((SEXP) rsxp)).push_back((SEXP) x);
    return rsxp;
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

(hash-table-set! +r-object->scheme-object-table+
                 'VECSXP
                 (lambda (rsxp)
                   (let ([length (rffi_sexp_length rsxp)])
                     (let loop ([i 0]
                                [lst '()])
                       (if (< i length)
                           (let ([item (list_ref rsxp i)])
                             (loop (+ i 1) (cons (r-object->scheme-object item) lst)))
                           (reverse lst))))))


)
