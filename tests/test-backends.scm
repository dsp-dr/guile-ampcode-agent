#!/usr/bin/env guile
!#
;;; test-backends.scm --- Test all backends

(add-to-load-path (dirname (dirname (current-filename))))
(add-to-load-path (string-append (dirname (dirname (current-filename))) "/src"))

(use-modules (srfi srfi-64)
             (agent backend)
             (agent message)
             (ice-9 format))

(test-begin "backend-tests")

;; Test mock backend
(test-group "mock-backend"
  (test-assert "create mock backend"
    (let ((backend (make-mock-backend)))
      (and (backend? backend)
           (eq? (backend-type backend) 'mock))))
  
  (test-assert "mock backend returns message"
    (let* ((backend (make-mock-backend))
           (messages (list (make-message "user" 
                                        (make-text-content "Hello"))))
           (response (backend-send-message backend messages '())))
      (message? response)))
  
  (test-assert "mock backend randomness"
    (let* ((backend (make-mock-backend))
           (messages (list (make-message "user"
                                        (make-text-content "Test"))))
           (responses (map (lambda (_)
                            (backend-send-message backend messages '()))
                          (iota 10))))
      ;; Should have some variety in responses
      (> (length (delete-duplicates 
                  (map (lambda (r) 
                         (list? (message-content r)))
                       responses)))
         1))))

;; Test Ollama backend creation
(test-group "ollama-backend"
  (test-assert "create ollama backend with defaults"
    (let ((backend (make-ollama-backend)))
      (and (backend? backend)
           (eq? (backend-type backend) 'ollama))))
  
  (test-assert "create ollama backend with tinyllama"
    (let ((backend (make-ollama-backend #:model "tinyllama")))
      (and (backend? backend)
           (eq? (backend-type backend) 'ollama))))
  
  (test-assert "ollama backend config"
    (let ((backend (make-ollama-backend 
                    #:host "localhost"
                    #:port 11434
                    #:model "tinyllama")))
      (let ((config (backend-config backend)))
        (and (equal? (assoc-ref config 'host) "localhost")
             (equal? (assoc-ref config 'port) 11434)
             (equal? (assoc-ref config 'model) "tinyllama"))))))

;; Test Anthropic backend creation (without actual API calls)
(test-group "anthropic-backend"
  (test-assert "create anthropic backend"
    (let ((backend (make-anthropic-backend "test-key" "test-model")))
      (and (backend? backend)
           (eq? (backend-type backend) 'anthropic))))
  
  (test-assert "anthropic backend config"
    (let ((backend (make-anthropic-backend "sk-test" "claude-3")))
      (let ((config (backend-config backend)))
        (and (equal? (assoc-ref config 'api-key) "sk-test")
             (equal? (assoc-ref config 'model) "claude-3"))))))

(test-end "backend-tests")

;; Summary
(let ((runner (test-runner-current)))
  (format #t "\nBackend Tests Summary:\n")
  (format #t "Total: ~a, Passed: ~a, Failed: ~a\n"
          (test-runner-test-count runner)
          (test-runner-pass-count runner)
          (test-runner-fail-count runner))
  (exit (if (zero? (test-runner-fail-count runner)) 0 1)))