;;; agent.scm --- Main agent module

(define-module (agent)
  #:use-module (agent client)
  #:use-module (agent message)
  #:use-module (agent tools)
  #:use-module (ice-9 readline)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (make-agent
            agent?
            agent-client
            agent-tools
            agent-conversation
            
            run-agent
            process-message))

;; Agent record
(define-record-type <agent>
  (make-agent client tools conversation)
  agent?
  (client agent-client)
  (tools agent-tools)
  (conversation agent-conversation set-agent-conversation!))

;; Create a new agent
(define* (create-agent api-key #:key (model "claude-3-sonnet-20240229"))
  (make-agent
   (make-client api-key model)
   (make-tool-registry)
   '()))

;; Process a single message exchange
(define (process-message agent user-input)
  (let* ((conversation (agent-conversation agent))
         (user-msg (make-message "user" (make-text-content user-input)))
         (new-conversation (append conversation (list user-msg))))
    
    ;; Update conversation
    (set-agent-conversation! agent new-conversation)
    
    ;; Send to Claude
    (let* ((tools-schema (tools->json-schema (agent-tools agent)))
           (response (send-message (agent-client agent) 
                                  new-conversation 
                                  tools-schema)))
      
      ;; Add assistant response to conversation
      (set-agent-conversation! agent 
                               (append new-conversation (list response)))
      
      ;; Process any tool calls
      (process-tool-calls agent response)
      
      ;; Return the response
      response)))

;; Process tool calls in a message
(define (process-tool-calls agent message)
  (let ((content (message-content message)))
    (when (list? content)
      (for-each
       (lambda (block)
         (when (tool-use-content? block)
           (handle-tool-use agent block)))
       content))))

;; Handle a single tool use
(define (handle-tool-use agent tool-use)
  (let* ((tool-id (tool-use-content-id tool-use))
         (tool-name (tool-use-content-name tool-use))
         (tool-input (tool-use-content-input tool-use))
         (result (execute-tool (agent-tools agent) tool-name tool-input))
         (tool-result-msg (make-message 
                          "user"
                          (make-tool-result-content tool-id result)))
         (conversation (agent-conversation agent)))
    
    ;; Add tool result to conversation
    (set-agent-conversation! agent 
                             (append conversation (list tool-result-msg)))
    
    ;; Get follow-up response from Claude
    (let* ((tools-schema (tools->json-schema (agent-tools agent)))
           (response (send-message (agent-client agent)
                                  (agent-conversation agent)
                                  tools-schema)))
      (set-agent-conversation! agent
                               (append (agent-conversation agent) 
                                      (list response)))
      (process-tool-calls agent response))))

;; Display message to user
(define (display-message message)
  (let ((content (message-content message)))
    (cond
     ((text-content? content)
      (display (text-content-text content))
      (newline))
     ((list? content)
      (for-each
       (lambda (block)
         (when (text-content? block)
           (display (text-content-text block))
           (newline)))
       content)))))

;; Main agent loop
(define (run-agent agent)
  (activate-readline)
  (display "Agent initialized. Type 'quit' to exit.\n")
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
        (catch #t
          (lambda ()
            (let ((response (process-message agent input)))
              (display "\nAssistant: ")
              (display-message response)
              (loop)))
          (lambda (key . args)
            (format #t "\nError: ~a ~a\n" key args)
            (loop)))))))