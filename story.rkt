#lang racket

(define (parse-input-new input)
  (match input
    [ "exit" 'exit]
    [(regexp #rx"^go(| to| through| via)(| the) (.+)$" (list _ _ _ what)) 'go]
    [(regexp #rx"^take(| the) (.+)$" (list _ _ what)) 'take]
    [_ 'huh]))

;;;; Old

(define (go where)
  (display (string-join (cons "Will go to" where))))

(define (take what)
  (display (string-join (cons "Picking up" what))))

(define (dispatch-command cmd)
  (if (string=? (car cmd) "go")
      (go (cdr cmd))
      (if (string=? (car cmd) "take")
          (take (cdr cmd))
          (display "Huh?"))))

(define (check-exit)
  (begin
    (display "Really exit? (y/n) ")
    (let ([input (read-line)])
      (if (or (not (string? input)) (string=? "y" (string-downcase input)))
          (void)
          (process-input)))))

(define (process-input)
  (let ([input (read-line)])
    (if (string? input)
      (if (string=? "exit" (string-downcase input))
         (check-exit)
         (begin
          (dispatch-command (string-split (string-downcase input)))
          (newline)
          (process-input)))
      (check-exit))))

(process-input)
