(module rffi
   *

  (import scheme
          (chicken base)
          (chicken foreign)
          (chicken read-syntax)
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
  (assert (and 'r-apply
               (string? fname)
               (rffi_exists fname "function")
               (list? lst)))
  (let ([lst (map s->r lst)])
    (receive (value err)
        ((foreign-primitive void ((c-string fname) (scheme-object rsxp_list))
           "
C_word *pointer = C_alloc(C_SIZEOF_POINTER);
SEXP value;
int error = 0;
try {
    Rcpp::Function docall(\"do.call\");
    Rcpp::Function f(fname); // can't catch undefined error. why???
    Rcpp::List args = Rcpp::List::create();
    while(!C_truep(C_i_nullp(rsxp_list))) {
        Rcpp::RObject a = (SEXP)(C_c_pointer_or_null(C_u_i_car(rsxp_list)));
        args.push_back(a);
        rsxp_list = C_u_i_cdr(rsxp_list);
    }
    value = docall(f, args);
} catch(...) {
   value = R_NilValue;
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
          (error "r error - r-apply")))))

(define (r-eval expression #!key (convert? #t))
  (define (inner expression)
    (cond [(pair? expression)
           (r-apply (symbol->string (car expression))
                    (map inner (cdr expression)))]
          [(symbol? expression)
           (r-eval-string
            (symbol->string expression)
            #:convert? #f)]
          [else (s->r expression)]))
  ((if convert? r->s identity) (inner expression)))

(set-read-syntax! 'r
  (lambda (port)
    (list 'quote (r-eval (read port)))))

(define (r-eval-string str #!key (convert? #t))
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
        ((if convert? r->s identity) value)
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

(define +r->s-table+ (make-hash-table))
(define +s->r-table+ (make-hash-table))

(define (r-lambda funname)
  (lambda args
    (r-apply funname args)))

(define (r-object-sexp-type rsxp)
  (hash-table-ref +sexp-type-table+ (rffi_sexp_type rsxp)))

;; Scheme -> R
(define (s->r value)
  (cond [(integer? value) (rffi_integer_scalar value)]
        [(number?  value) (rffi_numeric_scalar value)]
        [(boolean? value) (rffi_boolean_scalar value)]
        [(string?  value) (rffi_string_scalar  value)]
        [(f64vector? value)
         (rffi_numeric_vector value)]
        [(s32vector? value)
         (rffi_integer_vector value)]
        [(list? value)
         (r-apply "list" value)]
        [(vector? value)
         (r-apply "c" value)]
        [(eq? 'NULL value) (r-eval-string "NULL" #:convert? #f)]
        [else value]))

;; R -> Scheme
(define (r->s x)
  (let* ([type (r-object-sexp-type x)]
         [conv (hash-table-ref/default +r->s-table+ type
                                       (lambda (x)
                                         (cons type x)))])
    (conv x)))

(bind-type rffi_sexp c-pointer)
(bind*
#<<CPP
double rffi_numeric_vector_ref(rffi_sexp rsxp, int i) {
    return (REAL((SEXP) rsxp))[i];
}

int rffi_integer_vector_ref(rffi_sexp rsxp, int i) {
    return (INTEGER((SEXP) rsxp))[i];
}

bool rffi_logical_vector_ref(rffi_sexp rsxp, int i) {
    return (LOGICAL((SEXP) rsxp))[i];
}

char * rffi_string_vector_ref(rffi_sexp rsxp, int i) {
    return Rcpp::as<Rcpp::StringVector>((SEXP) rsxp)[i];
}

rffi_sexp rffi_list_ref(rffi_sexp rsxp, int i) {
    return Rcpp::as<Rcpp::List>((SEXP) rsxp)[i];
}

rffi_sexp rffi_numeric_vector(double * vec, ___length(vec) int len) {
    return Rcpp::NumericVector(vec, vec + len);
}

rffi_sexp rffi_integer_vector(int * vec, ___length(vec) int len) {
    return Rcpp::IntegerVector(vec, vec + len);
}

rffi_sexp rffi_numeric_scalar(double d) {
    return Rf_ScalarReal(d);
}

rffi_sexp rffi_integer_scalar(int i) {
    return Rf_ScalarInteger(i);
}

rffi_sexp rffi_boolean_scalar(bool b) {
    return Rf_ScalarLogical(b ? 1 : 0);
}

rffi_sexp rffi_string_scalar(char * str) {
    return Rf_ScalarString(Rf_mkChar(str));
}

int rffi_sexp_type(rffi_sexp rsxp) {
    return TYPEOF((SEXP) rsxp);
}

int rffi_sexp_length(rffi_sexp rsxp) {
    return Rf_length((SEXP) rsxp);
}

bool rffi_exists(char * name, char * mode) {
    Rcpp::Function exists("exists");
    Rcpp::String name_sxp(name);
    Rcpp::String mode_sxp(mode);
    Rcpp::LogicalVector bv = exists(name_sxp, Rcpp::Named("mode") = mode_sxp);
    bool b = Rcpp::is_true(all(bv));
    return b;
}

CPP
)

(define (make-r-vector->scheme-object ref-proc len-proc make-vector vector-set!)
  (lambda (rvec)
    (let ([length (len-proc rvec)])
      (if (= length 1)
          (ref-proc rvec 0)
          (do ((vector (make-vector length))
               (i 0 (+ i 1)))
              ((= i length) vector)
            (vector-set! vector i (ref-proc rvec i)))))))

(hash-table-set! +r->s-table+
                 'REALSXP
                 (make-r-vector->scheme-object rffi_numeric_vector_ref rffi_sexp_length make-f64vector f64vector-set!))

(hash-table-set! +r->s-table+
                 'INTSXP
                 (make-r-vector->scheme-object rffi_integer_vector_ref rffi_sexp_length make-s32vector s32vector-set!))

(hash-table-set! +r->s-table+
'VECSXP
                 (lambda (rsxp)
                   (let ([length (rffi_sexp_length rsxp)])
                     (let loop ([i 0]
                                [lst '()])
                       (if (< i length)
                           (let ([item (rffi_list_ref rsxp i)])
                             (loop (+ i 1) (cons (r->s item) lst)))
                           (reverse lst))))))

(hash-table-set! +r->s-table+
                 'LGLSXP
                 (make-r-vector->scheme-object rffi_logical_vector_ref rffi_sexp_length make-vector vector-set!))
(hash-table-set! +r->s-table+
                 'STRSXP
                 (make-r-vector->scheme-object rffi_string_vector_ref rffi_sexp_length make-vector vector-set!))

(hash-table-set! +r->s-table+
                 'NILSXP
                 (lambda (x) 'NULL))
)
