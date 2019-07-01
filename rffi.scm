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
(bind-file* "rffi.cpp")

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
