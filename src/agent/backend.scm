;;; backend.scm --- Backend abstraction for different LLM providers

(define-module (agent backend)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-1)
  #:use-module (agent message)
  #:use-module (agent ollama-client)
  #:use-module (ice-9 format)
  #:export (make-backend
            backend?
            backend-type
            backend-send-message
            
            make-anthropic-backend
            make-ollama-backend
            make-mock-backend))

;; Backend record type
(define-record-type <backend>
  (make-backend type send-fn config)
  backend?
  (type backend-type)
  (send-fn backend-send-fn)
  (config backend-config))

;; Send message through backend
(define (backend-send-message backend messages tools)
  ((backend-send-fn backend) (backend-config backend) messages tools))

;; Create Anthropic backend
(define (make-anthropic-backend api-key model)
  (let ((config `((api-key . ,api-key)
                  (model . ,model))))
    (make-backend 'anthropic anthropic-send config)))

;; Create Ollama backend
(define (make-ollama-backend #:key 
                            (host "localhost")
                            (port 11434)
                            (model "tinyllama"))
  (let ((config `((host . ,host)
                  (port . ,port)
                  (model . ,model))))
    (make-backend 'ollama ollama-send config)))

;; Create Mock backend
(define (make-mock-backend)
  (make-backend 'mock mock-send '()))

;; Anthropic implementation
(define (anthropic-send config messages tools)
  (let ((client (@ (agent client) make-client))
        (send (@ (agent client) send-message)))
    (let ((api-key (assoc-ref config 'api-key))
          (model (assoc-ref config 'model)))
      (send (client api-key model) messages tools))))

;; Ollama implementation
(define (ollama-send config messages tools)
  (let ((model (assoc-ref config 'model))
        (host (assoc-ref config 'host))
        (port (assoc-ref config 'port)))
    (ollama-api-call host port model messages tools)))

;; Mock implementation with 50/50 tool calling
(define (mock-send config messages tools)
  (let ((random-choice (random 100)))
    (if (< random-choice 50)
        ;; Return conversation response
        (make-message "assistant"
                     (make-text-content 
                      (format #f "Mock response to: ~a"
                              (get-last-user-message messages))))
        ;; Return tool call
        (if (and tools (not (null? tools)))
            (let ((random-tool (list-ref tools (random (length tools)))))
              (make-message "assistant"
                           (list (make-text-content "Let me use a tool for that.")
                                 (make-tool-use-content
                                  (format #f "mock-~a" (random 1000))
                                  (assoc-ref random-tool 'name)
                                  (generate-mock-input random-tool)))))
            ;; No tools available, return conversation
            (make-message "assistant"
                         (make-text-content "Mock response (no tools available)"))))))

;; Helper functions
(define (get-last-user-message messages)
  (let ((user-messages (filter (lambda (msg)
                                 (string=? (message-role msg) "user"))
                              messages)))
    (if (null? user-messages)
        "nothing"
        (let ((content (message-content (car (reverse user-messages)))))
          (if (text-content? content)
              (text-content-text content)
              "complex message")))))

(define (generate-mock-input tool)
  ;; Generate mock input based on tool schema
  (let ((schema (assoc-ref tool 'input_schema)))
    (if schema
        (let ((properties (assoc-ref schema 'properties)))
          (if properties
              (map (lambda (prop)
                     (cons (car prop) 
                           (case (assoc-ref (cdr prop) 'type)
                             (("string") "mock-value")
                             (("number") 42)
                             (("boolean") #t)
                             (else "unknown"))))
                   properties)
              '()))
        '())))