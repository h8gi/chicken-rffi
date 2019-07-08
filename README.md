# rffi

rffi is a [Chicken Scheme](https://www.call-cc.org/) extension for interacting with [R](https://www.r-project.org/).

## Installation

```bash
Rscript -e "install.packages(\"RInside\")"
git clone https://github.com/h8gi/chicken-rffi
cd chicken-rffi
chicken-install -s
```

## Usage

```scheme
(import rffi)
(r-eval '(sample (c 1 2 3) 10 #t) #:convert? #t)
;; => #s32(2 2 3 2 3 3 2 1 2 2)

(r-eval-string "mean(sample(c(1,2,3), 10, TRUE))" #:convert? #t)
;; => 2.3
```

## Known issues

- XQuartz `plot` freezes on macOS.

## Related repositories

- [https://github.com/eddelbuettel/rinside](https://github.com/eddelbuettel/rinside)
- [https://github.com/klutometis/R](https://github.com/klutometis/R)
- [https://github.com/iraikov/chicken-pyffi](https://github.com/iraikov/chicken-pyffi)
