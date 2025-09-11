;;; mock-client.scm --- Mock client for testing without API key

(define-module (agent mock-client)
  #:use-module (srfi srfi-9)
  #:use-module (agent message)
  #:use-module (ice-9 match)
  #:export (make-mock-client
            mock-send-message))

;; Mock client record
(define-record-type <mock-client>
  (make-mock-client model)
  mock-client?
  (model mock-client-model))

;; Simulated responses for testing
(define (generate-mock-response messages)
  (let ((last-msg (car (reverse messages))))
    (if (message? last-msg)
        (let ((content (message-content last-msg)))
          (cond
           ;; Check for weather-related queries
           ((and (text-content? content)
                 (string-contains-ci (text-content-text content) "weather"))
            (make-message "assistant"
                         (list (make-text-content "I'll check the weather for you.")
                               (make-tool-use-content
                                "mock-weather-1"
                                "get_weather"
                                '((location . "San Francisco, CA")
                                  (unit . "celsius"))))))
           
           ;; Check for calculation queries
           ((and (text-content? content)
                 (or (string-contains-ci (text-content-text content) "calculate")
                     (string-contains-ci (text-content-text content) "math")
                     (string-contains-ci (text-content-text content) "compute")))
            (make-message "assistant"
                         (list (make-text-content "Let me calculate that for you.")
                               (make-tool-use-content
                                "mock-calc-1"
                                "calculator"
                                '((expression . "2+2"))))))
           
           ;; Handle tool results
           ((tool-result-content? content)
            (make-message "assistant"
                         (make-text-content 
                          (format #f "Based on the tool result: ~a"
                                  (tool-result-content-content content)))))
           
           ;; Default response
           (else
            (make-message "assistant"
                         (make-text-content 
                          "I understand your request. This is a mock response for testing purposes.")))))
        ;; Fallback
        (make-message "assistant"
                     (make-text-content "Mock response: Message received.")))))

;; Mock send-message function
(define (mock-send-message client messages tools)
  (generate-mock-response messages))

;; Helper function for case-insensitive string search
(define (string-contains-ci str substr)
  (let ((str-lower (string-downcase str))
        (substr-lower (string-downcase substr)))
    (string-contains str-lower substr-lower)))