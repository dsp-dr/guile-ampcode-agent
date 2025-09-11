#!/usr/bin/env guile
!#
;;; demo-simple.scm --- Simple demo for recording

(add-to-load-path (dirname (current-filename)))
(add-to-load-path (string-append (dirname (current-filename)) "/src"))

(use-modules (ice-9 readline)
             (ice-9 format))

;; Simple mock responses
(define responses
  '("I'll help you with that calculation. Let me use the calculator tool."
    "The weather in San Francisco is currently 22°C with partly cloudy skies."
    "Let me calculate that for you: Result: 42"
    "I can assist with various tasks including weather queries and calculations."
    "Based on my calculation, the result is 156."))

(define response-index 0)

(define (get-response)
  (let ((resp (list-ref responses (modulo response-index (length responses)))))
    (set! response-index (+ response-index 1))
    resp))

;; Main loop
(define (run-demo)
  (display "\n=== MOCK Backend ===\n")
  (display "Agent initialized. Type 'quit' to exit.\n")
  (display "Available tools: weather, calculator\n")
  (display "==========================================\n")
  
  (let loop ()
    (display "\n> ")
    (force-output)
    (let ((input (readline)))
      (cond
       ((eof-object? input)
        (display "\nGoodbye!\n"))
       ((string=? input "quit")
        (display "Goodbye!\n"))
       ((string=? input "")
        (loop))
       (else
        (display "\nAssistant: ")
        (display (get-response))
        (newline)
        (loop))))))

(activate-readline)
(run-demo)