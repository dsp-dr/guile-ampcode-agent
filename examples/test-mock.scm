#!/usr/bin/env guile
!#
;;; test-mock.scm --- Example using mock backend

(add-to-load-path (dirname (dirname (current-filename))))
(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (agent backend)
             (agent message)
             (agent tools)
             (tools weather)
             (tools calculator)
             (ice-9 format))

;; Create mock backend
(define backend (make-mock-backend))

;; Create tool registry
(define tools (make-tool-registry))
(register-tool! tools weather-tool)
(register-tool! tools calculator-tool)

;; Test conversations
(define messages-1
  (list (make-message "user" 
                     (make-text-content "What's the weather like?"))))

(define messages-2
  (list (make-message "user"
                     (make-text-content "Calculate something for me"))))

;; Test mock responses
(format #t "Testing Mock Backend\n")
(format #t "====================\n\n")

(format #t "Test 1: Weather query\n")
(let ((response (backend-send-message backend messages-1 
                                      (tools->json-schema tools))))
  (format #t "Response: ~a\n\n" response))

(format #t "Test 2: Calculation query\n")
(let ((response (backend-send-message backend messages-2
                                      (tools->json-schema tools))))
  (format #t "Response: ~a\n\n" response))

(format #t "Test 3: Multiple calls (checking randomization)\n")
(do ((i 0 (+ i 1)))
    ((= i 5))
  (let ((response (backend-send-message backend messages-1
                                        (tools->json-schema tools))))
    (format #t "Call ~a: ~a\n" (+ i 1) 
            (if (list? (message-content response))
                "Tool call"
                "Conversation"))))

(format #t "\nMock backend test complete!\n")