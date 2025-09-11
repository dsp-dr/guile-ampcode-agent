#!/usr/bin/env guile
!#
;;; test-all.scm --- Comprehensive test suite runner

(add-to-load-path (dirname (dirname (current-filename))))
(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (srfi srfi-64)
             (ice-9 format))

;; Configure test runner
(test-runner-current (test-runner-simple))

;; Run all test modules
(format #t "\n========================================\n")
(format #t "Running Guile AmpCode Agent Test Suite\n")
(format #t "========================================\n\n")

;; Load and run individual test files
(load "test-messages.scm")
(load "test-tools.scm")

;; Additional integration tests
(test-begin "integration-tests")

(use-modules (agent message)
             (agent tools)
             (tools weather)
             (tools calculator))

(test-group "tool-integration"
  (test-assert "weather tool registration"
    (let ((registry (make-tool-registry)))
      (register-tool! registry weather-tool)
      (tool? (get-tool registry "get_weather"))))
  
  (test-assert "calculator tool registration"
    (let ((registry (make-tool-registry)))
      (register-tool! registry calculator-tool)
      (tool? (get-tool registry "calculator"))))
  
  (test-assert "execute weather tool"
    (let ((registry (make-tool-registry)))
      (register-tool! registry weather-tool)
      (string-contains 
       (execute-tool registry "get_weather" 
                     '((location . "San Francisco, CA")))
       "San Francisco")))
  
  (test-assert "execute calculator tool"
    (let ((registry (make-tool-registry)))
      (register-tool! registry calculator-tool)
      (string-contains
       (execute-tool registry "calculator"
                     '((expression . "5+3")))
       "Result:"))))

(test-group "message-tool-integration"
  (test-assert "tool use in message"
    (let* ((tool-use (make-tool-use-content 
                      "calc-123"
                      "calculator"
                      '((expression . "2+2"))))
           (msg (make-message "assistant" tool-use))
           (alist (message->alist msg)))
      (and (equal? (assoc-ref alist 'role) "assistant")
           (equal? (assoc-ref (assoc-ref alist 'content) 'type) "tool_use")
           (equal? (assoc-ref (assoc-ref alist 'content) 'name) "calculator"))))
  
  (test-assert "tool result in message"
    (let* ((result (make-tool-result-content "calc-123" "Result: 4"))
           (msg (make-message "user" result))
           (alist (message->alist msg)))
      (and (equal? (assoc-ref alist 'role) "user")
           (equal? (assoc-ref (assoc-ref alist 'content) 'type) "tool_result")
           (equal? (assoc-ref (assoc-ref alist 'content) 'content) "Result: 4")))))

(test-end "integration-tests")

;; Summary report
(let ((runner (test-runner-current)))
  (format #t "\n========================================\n")
  (format #t "Test Suite Summary\n")
  (format #t "========================================\n")
  (format #t "Total tests run:    ~a\n" (test-runner-test-count runner))
  (format #t "Tests passed:       ~a\n" (test-runner-pass-count runner))
  (format #t "Tests failed:       ~a\n" (test-runner-fail-count runner))
  (format #t "Tests skipped:      ~a\n" (test-runner-skip-count runner))
  (format #t "========================================\n")
  
  (if (zero? (test-runner-fail-count runner))
      (begin
        (format #t "✓ All tests passed!\n")
        (exit 0))
      (begin
        (format #t "✗ Some tests failed.\n")
        (exit 1))))