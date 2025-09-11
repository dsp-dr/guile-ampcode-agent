#!/usr/bin/env guile
!#
;;; test-messages.scm --- Tests for message module

(use-modules (srfi srfi-64)
             (agent message))

(test-begin "message-module")

;; Test text content creation
(test-group "text-content"
  (test-assert "create text content"
    (let ((content (make-text-content "Hello, world!")))
      (and (text-content? content)
           (string=? (text-content-text content) "Hello, world!"))))
  
  (test-equal "text content in message"
    "Hello, world!"
    (let* ((content (make-text-content "Hello, world!"))
           (msg (make-message "user" content)))
      (text-content-text (message-content msg)))))

;; Test tool use content
(test-group "tool-use-content"
  (test-assert "create tool use content"
    (let ((content (make-tool-use-content 
                    "tool-123" 
                    "calculator" 
                    '((expression . "2+2")))))
      (and (tool-use-content? content)
           (string=? (tool-use-content-id content) "tool-123")
           (string=? (tool-use-content-name content) "calculator")
           (equal? (tool-use-content-input content) 
                   '((expression . "2+2"))))))
  
  (test-equal "tool use content name"
    "weather"
    (tool-use-content-name 
     (make-tool-use-content "id" "weather" '()))))

;; Test tool result content
(test-group "tool-result-content"
  (test-assert "create tool result content"
    (let ((content (make-tool-result-content "tool-123" "Result: 4")))
      (and (tool-result-content? content)
           (string=? (tool-result-content-id content) "tool-123")
           (string=? (tool-result-content-content content) "Result: 4"))))
  
  (test-equal "tool result content value"
    "Weather: Sunny"
    (tool-result-content-content
     (make-tool-result-content "id" "Weather: Sunny"))))

;; Test message creation
(test-group "messages"
  (test-assert "create user message"
    (let ((msg (make-message "user" (make-text-content "Hello"))))
      (and (message? msg)
           (string=? (message-role msg) "user"))))
  
  (test-assert "create assistant message"
    (let ((msg (make-message "assistant" (make-text-content "Hi there"))))
      (and (message? msg)
           (string=? (message-role msg) "assistant"))))
  
  (test-assert "message with list content"
    (let ((msg (make-message "assistant" 
                            (list (make-text-content "Part 1")
                                  (make-text-content "Part 2")))))
      (and (message? msg)
           (list? (message-content msg))
           (= 2 (length (message-content msg)))))))

;; Test serialization
(test-group "serialization"
  (test-equal "simple message to alist"
    '((role . "user") 
      (content . ((type . "text") (text . "Hello"))))
    (message->alist 
     (make-message "user" (make-text-content "Hello"))))
  
  (test-equal "tool use message to alist"
    '((role . "assistant")
      (content . ((type . "tool_use")
                  (id . "123")
                  (name . "calc")
                  (input . ((expr . "1+1"))))))
    (message->alist
     (make-message "assistant"
                   (make-tool-use-content "123" "calc" '((expr . "1+1"))))))
  
  (test-assert "round-trip serialization"
    (let* ((original (make-message "user" (make-text-content "Test")))
           (alist (message->alist original))
           (restored (alist->message alist)))
      (and (message? restored)
           (string=? (message-role restored) "user")
           (text-content? (message-content restored))
           (string=? (text-content-text (message-content restored)) "Test")))))

;; Test deserialization
(test-group "deserialization"
  (test-assert "alist to text message"
    (let* ((alist '((role . "user") 
                    (content . ((type . "text") (text . "Hello")))))
           (msg (alist->message alist)))
      (and (message? msg)
           (string=? (message-role msg) "user")
           (text-content? (message-content msg))
           (string=? (text-content-text (message-content msg)) "Hello"))))
  
  (test-assert "alist to tool use message"
    (let* ((alist '((role . "assistant")
                    (content . ((type . "tool_use")
                               (id . "456")
                               (name . "weather")
                               (input . ((location . "NYC")))))))
           (msg (alist->message alist)))
      (and (message? msg)
           (tool-use-content? (message-content msg))
           (string=? (tool-use-content-name (message-content msg)) "weather"))))
  
  (test-assert "handle string content"
    (let* ((alist '((role . "user") (content . "Simple string")))
           (msg (alist->message alist)))
      (and (message? msg)
           (text-content? (message-content msg))
           (string=? (text-content-text (message-content msg)) 
                     "Simple string")))))

(test-end "message-module")