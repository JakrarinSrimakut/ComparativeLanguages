#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
;; Jakrarin Srimakut - jsrimaku - 1508097
;;
;; NAME
;;      sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;      sbi.scm filename.sbir
;;
;; DESCRIPTION
;;      The file mentioned in argv[1] is read and assumed to be an SBIR
;;      program, which is the executed.   Currently it is only printed.
;;

(define *stderr* (current-error-port))

(define *run-file*
      (let-values
            (((dirpath basepath root?)
                  (split-path (find-system-path 'run-file))))
            (path->string basepath))
)

(define (die list)
      (for-each (lambda (item) (display item *stderr*)) list)
      (newline *stderr*)
      (exit 1)
)

(define (usage-exit)
      (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
      (let ((inputfile (open-input-file filename)))
             (if (not (input-port? inputfile))
                   (die `(,*run-file* ": " ,filename ": open failed"))
                   (let ((program (read inputfile)))
                           (close-input-port inputfile)
                                     program))))

(define (write-program-by-line filename program)
      (printf "==================================================~n")
      (printf "~a: ~s~n" *run-file* filename)
      (printf "==================================================~n")
      (printf "(~n")
      (map (lambda (line) (printf "~s~n" line)) program)
      (printf ")~n"))

(define *function-table* (make-hash))

(define (function-put! key val)
      (hash-set! *function-table* key val)
)

(define (function-get key)
      (hash-ref *function-table* key)
)

(define *label-table* (make-hash))

(define (label-get key)
      (hash-ref *label-table* key)
)

(define (label-put! key val)
      (hash-set! *label-table* key val)
)

(define (label-get key)
      (hash-ref *label-table* key)
)

(define *var-table* (make-hash))

(define (var-get key)
      (hash-ref *var-table* key)
)

(define (var-put! key val)
      (hash-set! *var-table* key val)
)

(for-each
      (lambda (pair) (var-put! (car pair) (cadr pair)))
      `(
            (inputcount 0)
            (pi         3.141592653589793238462643383279502884197169399)
            (e          2.718281828459045235360287471352662497757247093)
      )
)

(define length (lambda (lenl)
      (define length2 (lambda (lenl2 n)
            (if (null? lenl2)
                  n
                  (length2 (cdr lenl2) (+ n 1))
            )
      ))
      (length2 lenl 0)
))

(define (val lenl)
      (if (pair? lenl)
            (apply (function-get (car lenl)) (map val (cdr lenl)))
            (cond ((number? lenl) lenl)
                  (else (var-get lenl))
            )
      )
)

(define (sbir-print tok) 
      (if (not (null?   tok) )
            (begin
                  (if (string? (car tok))   
                        (display (car tok))
                        (display (+ 0.0 (val (car tok))))
                  )
                  (sbir-print (cdr tok))
            )
            (newline)
      )
)

(define (sbir-input expr)
      (var-put! 'inputcount 0)
      (define (get-input expr)
            (when (not (null? (car expr)))
                  (var-put! (car expr) (void))
                  (let ((object (read)))
                        (cond ((eof-object? object)
                                    (var-put! 'inputcount -1))
                              ((number? object)
                                    (var-put! (car expr) (+ object 0.0))
                                    (var-put! 'inputcount
                                          (+ (var-get 'inputcount) 1))
                              )
                              (else
                                    (begin (printf "invalid number: ~a~n" object))
                              )
                        )
                  )
                  (when (not (null? (cdr expr)))
                        (get-input (cdr expr))
                  )
            )
      )
      (get-input expr)
)

(define (sbir-dim expr)
      (var-put! (caar expr) (make-vector (val (cadar expr))))
      (function-put! (caar expr)
            (lambda(x) (vector-ref (var-get (caar expr)) (- x 1)))
      )
)

(define (sbir-let expr)
      (if (pair? (car expr))
            (begin (vector-set! (var-get
                  (caar expr)) (- (val (cadar expr)) 1) (val (cadr expr))
            ))
            (begin (let ((result (val (cadr expr))))
                  (var-put! (car expr) result)
            ))
      )
)

(define (sbir-goto label program)
      (exe-lines program (label-get (car label)))
)

(define (interpret filename program)
      (map (lambda (line) (
            printf "~s~n" (cdr line)
      )) program)
)

(define (func instr program line-nr)
      (if (null? instr)
            (exe-lines program (+ line-nr 1))
            (begin
                  (when (not (hash-has-key? *function-table* (car instr)))
                        (display (car instr))(display " is not valid")(newline)
                        (usage-exit)
                  )
                  (cond
                        ((eqv? (car instr) 'goto)
                              (exe-lines program (- (label-get (cadr instr)) 1))
                        )
                        ((eqv? (car instr) 'if)
                              (if (equal? #t (val (cadr instr)))
                                    (exe-lines program
                                          (- (label-get (caddr instr)) 1))
                                    (exe-lines program (+ line-nr 1))
                              )
                        )
                        (else
                              ((function-get (car instr)) (cdr instr))
                              (exe-lines program (+ line-nr 1))
                        )
                  )
            )
      )
)

(define (eval-labels list)
      (when (not (null? list))
            (let ((first (caar list)))
                  (when (number? first)
                        (if (not (null? (cdar list)))
                              (if (not (symbol? (cadar list)))
                                    (void)
                                    (begin (label-put! (cadar list) (caar list)))
                              )
                              (void)
                        )
                  )
            )
            (eval-labels (cdr list))
      )
)

(define (exe-lines program line-nr)
      (when (< line-nr (length program))
            (let ((line (list-ref program line-nr)))
                  (cond
                        ((= (length line) 3)
                              (set! line (cddr line))
                              (func (car line) program line-nr)
                        )
                        ((and (= (length line) 2) (list? (cadr line)))
                              (set! line (cdr line))
                              (func (car line) program line-nr)
                        )
                        (else (exe-lines program (+ line-nr 1)))
                  )
            )
      )
)

(for-each
      (lambda (pair) (function-put! (car pair) (cadr pair)))
      `(
            (log10    ,(lambda (x) (/ (log x) (log 10.0))))
            (log2      ,(lambda (x) (/ (log x) (log 2.0))))
            (%          ,(lambda (x y) (- x (* (div x y) y))))
            (quot      ,(lambda (x y) (truncate (/ x y))))
            (rem       ,(lambda (x y) (- x (* (quot x y) y))))
            (+          ,+)
            (-          ,-)
            (*          ,*)
            (/          ,(lambda (x y)   (/ (+ x 0.0) (+ y 0.0))))
            (<=         ,<=)
            (>=         ,>=)
            (<          ,<)
            (>          ,>)
            (=          ,=)
            (<>         ,(lambda (x y) (not (equal? x y))))
            (^          ,(lambda (x y) (expt x y)))
            (abs       ,abs)
            (ceil      ,ceiling)
            (floor    ,floor)
            (round    ,round)
            (exp       ,exp)
            (log       ,(lambda(x)(log (if (equal? x 0) 0.0 x))))
            (sqrt      ,sqrt)
            (sin       ,sin)
            (cos       ,cos)
            (tan       ,tan)
            (asin      ,asin)
            (acos      ,acos)
            (atan      ,(lambda(x)(atan (if (equal? x 0) 0.0 x))))
            (print    ,sbir-print)
            (let       ,sbir-let)
            (dim       ,sbir-dim)
            (goto      ,sbir-goto)
            (input    ,sbir-input)
            (if         ,(void))
       )
)

(define (main arglist)
      (if (or (null? arglist) (not (null? (cdr arglist))))
            (usage-exit)
            (let*
                  ((sbprogfile (car arglist))
                        (program (readlist-from-inputfile sbprogfile))
                  )
                  (begin (eval-labels program) (exe-lines program 0))
            )
      )
)

(main (vector->list (current-command-line-arguments)))

