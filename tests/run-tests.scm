#!/usr/bin/env guile
!#
;;; run-tests.scm --- Test suite for Guile AmpCode Agent

(use-modules (srfi srfi-64)
             (agent message)
             (agent tools))

;; Initialize test runner
(test-runner-current (test-runner-simple))

;; Test message creation and serialization
(test-begin "message-tests")

(test-assert "create text message"
  (let ((msg (make-message "user" (make-text-content "Hello"))))
    (and (message? msg)
         (string=? (message-role msg) "user")
         (text-content? (message-content msg)))))

(test-equal "message to alist conversion"
  '((role . "user") (content . ((type . "text") (text . "Hello"))))
  (message->alist (make-message "user" (make-text-content "Hello"))))

(test-end "message-tests")

;; Test tool registry
(test-begin "tool-tests")

(test-assert "create tool registry"
  (tool-registry? (make-tool-registry)))

(test-assert "register and retrieve tool"
  (let ((registry (make-tool-registry))
        (tool (make-tool "test" "Test tool" '() (lambda (x) "result"))))
    (register-tool! registry tool)
    (eq? tool (get-tool registry "test"))))

(test-equal "execute tool"
  "result"
  (let ((registry (make-tool-registry))
        (tool (make-tool "test" "Test tool" '() (lambda (x) "result"))))
    (register-tool! registry tool)
    (execute-tool registry "test" '())))

(test-end "tool-tests")

;; Display test results
(let ((runner (test-runner-current)))
  (format #t "\n~a tests run\n" (test-runner-test-count runner))
  (format #t "~a passed\n" (test-runner-pass-count runner))
  (format #t "~a failed\n" (test-runner-fail-count runner))
  (exit (if (zero? (test-runner-fail-count runner)) 0 1)))