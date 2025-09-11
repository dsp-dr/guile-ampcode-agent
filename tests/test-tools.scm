#!/usr/bin/env guile
!#
;;; test-tools.scm --- Tests for tools module

(use-modules (srfi srfi-64)
             (agent tools))

(test-begin "tools-module")

;; Test tool creation
(test-group "tool-creation"
  (test-assert "create basic tool"
    (let ((tool (make-tool "test" "Test tool" '() (lambda (x) "result"))))
      (and (tool? tool)
           (string=? (tool-name tool) "test")
           (string=? (tool-description tool) "Test tool"))))
  
  (test-equal "tool function execution"
    "Hello, World!"
    (let ((tool (make-tool "greet" "Greeting tool" '() 
                          (lambda (x) "Hello, World!"))))
      ((tool-function tool) '())))
  
  (test-assert "tool with schema"
    (let ((schema '((type . "object")
                    (properties . ((name . ((type . "string")))))
                    (required . #("name")))))
          (tool (make-tool "test" "Test" schema (lambda (x) x))))
      (equal? (tool-input-schema tool) schema))))

;; Test tool registry
(test-group "tool-registry"
  (test-assert "create empty registry"
    (let ((registry (make-tool-registry)))
      (and (tool-registry? registry)
           (null? (list-tools registry)))))
  
  (test-assert "register single tool"
    (let ((registry (make-tool-registry))
          (tool (make-tool "test" "Test" '() (lambda (x) "ok"))))
      (register-tool! registry tool)
      (= 1 (length (list-tools registry)))))
  
  (test-assert "register multiple tools"
    (let ((registry (make-tool-registry))
          (tool1 (make-tool "tool1" "Tool 1" '() (lambda (x) "1")))
          (tool2 (make-tool "tool2" "Tool 2" '() (lambda (x) "2"))))
      (register-tool! registry tool1)
      (register-tool! registry tool2)
      (= 2 (length (list-tools registry)))))
  
  (test-equal "retrieve tool by name"
    "test-tool"
    (let ((registry (make-tool-registry))
          (tool (make-tool "test-tool" "Test" '() (lambda (x) x))))
      (register-tool! registry tool)
      (tool-name (get-tool registry "test-tool"))))
  
  (test-assert "retrieve non-existent tool"
    (let ((registry (make-tool-registry)))
      (not (get-tool registry "non-existent")))))

;; Test tool execution
(test-group "tool-execution"
  (test-equal "execute simple tool"
    "42"
    (let ((registry (make-tool-registry))
          (tool (make-tool "answer" "Answer tool" '() 
                          (lambda (x) "42"))))
      (register-tool! registry tool)
      (execute-tool registry "answer" '())))
  
  (test-equal "execute tool with input"
    "Hello, Alice"
    (let ((registry (make-tool-registry))
          (tool (make-tool "greet" "Greeting" '()
                          (lambda (input)
                            (format #f "Hello, ~a" 
                                    (assoc-ref input 'name))))))
      (register-tool! registry tool)
      (execute-tool registry "greet" '((name . "Alice")))))
  
  (test-assert "execute non-existent tool"
    (let ((registry (make-tool-registry)))
      (string-contains (execute-tool registry "missing" '())
                       "Tool not found")))
  
  (test-assert "handle tool execution error"
    (let ((registry (make-tool-registry))
          (tool (make-tool "error" "Error tool" '()
                          (lambda (x) (error "Intentional error")))))
      (register-tool! registry tool)
      (string-contains (execute-tool registry "error" '())
                       "Error executing tool"))))

;; Test JSON schema generation
(test-group "json-schema"
  (test-equal "empty registry schema"
    '()
    (tools->json-schema (make-tool-registry)))
  
  (test-assert "single tool schema"
    (let ((registry (make-tool-registry))
          (schema '((type . "object")
                    (properties . ((x . ((type . "number")))))))
          (tool (make-tool "math" "Math tool" schema (lambda (x) x))))
      (register-tool! registry tool)
      (let ((json-schema (tools->json-schema registry)))
        (and (= 1 (length json-schema))
             (equal? (assoc-ref (car json-schema) 'name) "math")
             (equal? (assoc-ref (car json-schema) 'description) "Math tool")
             (equal? (assoc-ref (car json-schema) 'input_schema) schema)))))
  
  (test-equal "multiple tools schema"
    2
    (let ((registry (make-tool-registry))
          (tool1 (make-tool "t1" "Tool 1" '() (lambda (x) x)))
          (tool2 (make-tool "t2" "Tool 2" '() (lambda (x) x))))
      (register-tool! registry tool1)
      (register-tool! registry tool2)
      (length (tools->json-schema registry)))))

;; Test complex tool scenarios
(test-group "complex-scenarios"
  (test-assert "calculator tool"
    (let ((registry (make-tool-registry))
          (calc-tool (make-tool 
                      "calc"
                      "Calculator"
                      '((type . "object")
                        (properties . ((expr . ((type . "string")))))
                        (required . #("expr")))
                      (lambda (input)
                        (let ((expr (assoc-ref input 'expr)))
                          (cond
                           ((string=? expr "2+2") "4")
                           ((string=? expr "10/2") "5")
                           (else "unknown")))))))
      (register-tool! registry calc-tool)
      (and (string=? (execute-tool registry "calc" '((expr . "2+2"))) "4")
           (string=? (execute-tool registry "calc" '((expr . "10/2"))) "5"))))
  
  (test-assert "weather tool mock"
    (let ((registry (make-tool-registry))
          (weather-tool (make-tool
                         "weather"
                         "Weather service"
                         '((type . "object")
                           (properties . ((location . ((type . "string")))))
                           (required . #("location")))
                         (lambda (input)
                           (format #f "Weather in ~a: Sunny, 22°C"
                                   (assoc-ref input 'location))))))
      (register-tool! registry weather-tool)
      (string-contains (execute-tool registry "weather" 
                                    '((location . "Boston")))
                       "Boston"))))

(test-end "tools-module")