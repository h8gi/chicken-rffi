(import (chicken format)
        (chicken process)
        (chicken process-context)
        shell
        (prefix srfi-13 srfi-13:))

(define +r-home+ (srfi-13:string-trim-both (capture "R RHOME")))

(define (r-config str)
  (let ([cmd-str (sprintf "~A/bin/R CMD config ~A" +r-home+ str)])
    (srfi-13:string-trim-both (execute (list cmd-str) capture: #t))))

(define (r-slave str)
  (let* ([cmd-str (sprintf "echo '~A' | ~A/bin/R --vanilla --slave" str +r-home+)]
         [result  (execute (list cmd-str) capture: #t)])
    (if (eof-object? result) ""
        (srfi-13:string-trim-both result))))

(define CXX (r-config "CXX"))

(define CPPFLAGS
  (srfi-13:string-join
   (list "-Wall"
         (r-config "CPPFLAGS"))))

(define CXXFLAGS
  (srfi-13:string-join
   (list (r-config "--cppflags")
         (r-slave "Rcpp:::CxxFlags()")
         (r-slave "RInside:::CxxFlags()")
         (r-config "CXXFLAGS"))))

(define LDLIBS
  (srfi-13:string-join
   (list (r-config "--ldflags")
         (r-config "BLAS_LIBS")
         (r-config "LAPACK_LIBS")
         (r-slave "Rcpp:::LdFlags()")
         (r-slave "RInside:::LdFlags()")
         )))

(define full (srfi-13:string-join (list LDLIBS CXXFLAGS CPPFLAGS)))
(define args (command-line-arguments))
(define cmd (srfi-13:string-join
             (append args (list
                           "-c++"
                           "-v"
                           (sprintf "-cxx \"~A\"" CXX)
                           ;; for `clang: error: unsupported option '-fopenmp'`
                           (sprintf "-L \"-Xpreprocessor ~A\"" LDLIBS)
                           (sprintf "-C \"~A ~A\"" CXXFLAGS CPPFLAGS)
                           ))))
(system cmd)
