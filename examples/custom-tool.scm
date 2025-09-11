#!/usr/bin/env guile
!#
;;; custom-tool.scm --- Example of creating a custom tool

(add-to-load-path (dirname (dirname (current-filename))))
(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (agent tools)
             (agent message)
             (agent backend)
             (ice-9 format)
             (srfi srfi-19))  ; For date/time

;; Define a custom datetime tool
(define datetime-tool
  (make-tool
   "get_datetime"
   "Get current date and time information"
   '((type . "object")
     (properties . ((format . ((type . "string")
                               (enum . #("iso" "human" "unix"))
                               (description . "Output format for the datetime")))
                   (timezone . ((type . "string")
                                (description . "Timezone (not implemented, uses local)")))))
     (required . #()))
   
   ;; Tool implementation
   (lambda (input)
     (let ((format-type (or (assoc-ref input 'format) "human"))
           (current-time (current-date)))
       (case (string->symbol format-type)
         ((iso)
          (date->string current-time "~Y-~m-~dT~H:~M:~S"))
         ((unix)
          (format #f "~a" (time-second (current-time))))
         (else ;; human
          (date->string current-time "~A, ~B ~d, ~Y at ~H:~M:~S")))))))

;; Define a dice rolling tool
(define dice-tool
  (make-tool
   "roll_dice"
   "Roll one or more dice"
   '((type . "object")
     (properties . ((count . ((type . "number")
                              (description . "Number of dice to roll")
                              (minimum . 1)
                              (maximum . 10)))
                   (sides . ((type . "number")
                            (description . "Number of sides on each die")
                            (enum . #(4 6 8 10 12 20 100))))))
     (required . #("sides")))
   
   ;; Tool implementation
   (lambda (input)
     (let ((count (or (assoc-ref input 'count) 1))
           (sides (assoc-ref input 'sides)))
       (if (not sides)
           "Error: Number of sides is required"
           (let ((rolls (map (lambda (_) 
                              (+ 1 (random sides)))
                            (iota count))))
             (format #f "Rolled ~ad~a: ~a (Total: ~a)"
                     count sides rolls 
                     (apply + rolls))))))))

;; Define a string manipulation tool
(define string-tool
  (make-tool
   "string_ops"
   "Perform string operations"
   '((type . "object")
     (properties . ((operation . ((type . "string")
                                  (enum . #("reverse" "uppercase" "lowercase" "length"))
                                  (description . "Operation to perform")))
                   (text . ((type . "string")
                           (description . "Text to operate on")))))
     (required . #("operation" "text")))
   
   ;; Tool implementation
   (lambda (input)
     (let ((op (assoc-ref input 'operation))
           (text (assoc-ref input 'text)))
       (if (not (and op text))
           "Error: Both operation and text are required"
           (case (string->symbol op)
             ((reverse) (list->string (reverse (string->list text))))
             ((uppercase) (string-upcase text))
             ((lowercase) (string-downcase text))
             ((length) (format #f "Length: ~a characters" (string-length text)))
             (else "Unknown operation")))))))

;; Test the custom tools
(format #t "Custom Tools Demo\n")
(format #t "=================\n\n")

;; Create registry and register all tools
(define registry (make-tool-registry))
(register-tool! registry datetime-tool)
(register-tool! registry dice-tool)
(register-tool! registry string-tool)

;; Test datetime tool
(format #t "1. DateTime Tool:\n")
(format #t "   Human format: ~a\n" 
        (execute-tool registry "get_datetime" '((format . "human"))))
(format #t "   ISO format: ~a\n"
        (execute-tool registry "get_datetime" '((format . "iso"))))
(format #t "   Unix timestamp: ~a\n\n"
        (execute-tool registry "get_datetime" '((format . "unix"))))

;; Test dice tool
(format #t "2. Dice Tool:\n")
(format #t "   Single d6: ~a\n"
        (execute-tool registry "roll_dice" '((sides . 6))))
(format #t "   3d20: ~a\n"
        (execute-tool registry "roll_dice" '((count . 3) (sides . 20))))
(format #t "   5d10: ~a\n\n"
        (execute-tool registry "roll_dice" '((count . 5) (sides . 10))))

;; Test string tool
(format #t "3. String Tool:\n")
(format #t "   Reverse 'Hello': ~a\n"
        (execute-tool registry "string_ops" 
                     '((operation . "reverse") (text . "Hello"))))
(format #t "   Uppercase 'guile': ~a\n"
        (execute-tool registry "string_ops"
                     '((operation . "uppercase") (text . "guile"))))
(format #t "   Length of 'AmpCode Agent': ~a\n\n"
        (execute-tool registry "string_ops"
                     '((operation . "length") (text . "AmpCode Agent"))))

;; Show JSON schema for tools
(format #t "4. Tool Schemas (for LLM):\n")
(for-each (lambda (tool-schema)
           (format #t "   - ~a: ~a\n" 
                   (assoc-ref tool-schema 'name)
                   (assoc-ref tool-schema 'description)))
         (tools->json-schema registry))

(format #t "\nCustom tools can be used with any backend!\n")