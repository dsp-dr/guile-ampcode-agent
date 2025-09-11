;;; calculator.scm --- Calculator tool implementation

(define-module (tools calculator)
  #:use-module (agent tools)
  #:use-module (ice-9 format)
  #:use-module (ice-9 regex)
  #:export (calculator-tool))

;; Safe evaluation of mathematical expressions
(define (safe-eval-math expr)
  (catch #t
    (lambda ()
      ;; Only allow numbers and basic operators
      (if (string-match "^[0-9+\\-*/()\\. ]+$" expr)
          (let ((result (eval-string expr)))
            (if (number? result)
                (format #f "~a" result)
                "Invalid expression"))
          "Invalid characters in expression"))
    (lambda (key . args)
      (format #f "Error evaluating expression: ~a" args))))

;; Calculator tool definition
(define calculator-tool
  (make-tool
   "calculator"
   "Perform basic mathematical calculations"
   '((type . "object")
     (properties . ((expression . ((type . "string")
                                   (description . "Mathematical expression to evaluate")))))
     (required . #("expression")))
   
   ;; Tool function
   (lambda (input)
     (let ((expression (assoc-ref input 'expression)))
       (format #f "Result: ~a" (safe-eval-math expression))))))