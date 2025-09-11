#!/usr/bin/env guile
!#
;;; demo-simple.scm --- Simple demo for recording with mock tool calls

(add-to-load-path (dirname (dirname (current-filename))))
(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (ice-9 readline)
             (ice-9 format)
             (ice-9 regex))

;; Enhanced mock with tool invocations
(define (get-response input)
  (cond
   ;; Weather queries
   ((or (string-contains-ci input "weather")
        (string-contains-ci input "temperature")
        (string-contains-ci input "forecast"))
    (format #f "I'll check the weather for you.\n\n🔧 Invoking: get_weather(location: \"San Francisco\")\n💻 Result: {\"temperature\": \"22°C\", \"condition\": \"partly cloudy\"}\n\nThe weather in San Francisco is currently 22°C with partly cloudy skies."))
   
   ;; Math/calculation queries
   ((or (string-contains-ci input "calculate")
        (string-contains-ci input "math")
        (string-match "[0-9].*[+\\-*/].*[0-9]" input))
    (let ((math-expr (or (and (string-match "[0-9+\\-*/ ]+" input)
                             (match:substring (string-match "[0-9+\\-*/ ]+" input)))
                        "5 + 7")))
      (format #f "I'll calculate that for you.\n\n🔧 Invoking: calculator(expression: \"~a\")\n💻 bc command: echo \"~a\" | bc\n💻 Result: 12\n\nThe result of ~a is 12."
              math-expr math-expr math-expr)))
   
   ;; Time queries  
   ((or (string-contains-ci input "time")
        (string-contains-ci input "date"))
    (format #f "Let me get the current time for you.\n\n🔧 Invoking: get_time()\n💻 System call: date '+%%Y-%%m-%%d %%H:%%M:%%S'\n💻 Result: \"2025-09-11 12:34:56\"\n\nCurrent time: 2025-09-11 12:34:56"))
   
   ;; Default helpful response
   (else
    "I can help with weather queries, calculations, and time information. Try asking me about the weather or to calculate something!")))

;; Helper for case-insensitive search
(define (string-contains-ci str substr)
  (let ((str-lower (string-downcase str))
        (substr-lower (string-downcase substr)))
    (string-contains str-lower substr-lower)))

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
        (display (get-response input))
        (newline)
        (loop))))))

(activate-readline)
(run-demo)