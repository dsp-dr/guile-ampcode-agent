;;; message.scm --- Message types and handling for agent

(define-module (agent message)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (json-simple)
  #:export (make-message
            message?
            message-role
            message-content
            
            make-text-content
            text-content?
            text-content-text
            
            make-tool-use-content
            tool-use-content?
            tool-use-content-id
            tool-use-content-name
            tool-use-content-input
            
            make-tool-result-content
            tool-result-content?
            tool-result-content-id
            tool-result-content-content
            
            message->alist
            alist->message))

;; Message record type
(define-record-type <message>
  (make-message role content)
  message?
  (role message-role)
  (content message-content))

;; Text content
(define-record-type <text-content>
  (make-text-content text)
  text-content?
  (text text-content-text))

;; Tool use content
(define-record-type <tool-use-content>
  (make-tool-use-content id name input)
  tool-use-content?
  (id tool-use-content-id)
  (name tool-use-content-name)
  (input tool-use-content-input))

;; Tool result content
(define-record-type <tool-result-content>
  (make-tool-result-content id content)
  tool-result-content?
  (id tool-result-content-id)
  (content tool-result-content-content))

;; Convert message to alist for JSON serialization
(define (message->alist msg)
  (define (content->alist content)
    (cond
     ((text-content? content)
      `((type . "text")
        (text . ,(text-content-text content))))
     ((tool-use-content? content)
      `((type . "tool_use")
        (id . ,(tool-use-content-id content))
        (name . ,(tool-use-content-name content))
        (input . ,(tool-use-content-input content))))
     ((tool-result-content? content)
      `((type . "tool_result")
        (tool_use_id . ,(tool-result-content-id content))
        (content . ,(tool-result-content-content content))))
     ((list? content)
      (map content->alist content))
     (else content)))
  
  `((role . ,(message-role msg))
    (content . ,(content->alist (message-content msg)))))

;; Convert alist to message
(define (alist->message alist)
  (define (alist->content content-alist)
    (match (assoc-ref content-alist 'type)
      ("text"
       (make-text-content (assoc-ref content-alist 'text)))
      ("tool_use"
       (make-tool-use-content
        (assoc-ref content-alist 'id)
        (assoc-ref content-alist 'name)
        (assoc-ref content-alist 'input)))
      ("tool_result"
       (make-tool-result-content
        (assoc-ref content-alist 'tool_use_id)
        (assoc-ref content-alist 'content)))
      (_ content-alist)))
  
  (let ((role (assoc-ref alist 'role))
        (content (assoc-ref alist 'content)))
    (make-message role
                  (if (list? content)
                      (map alist->content content)
                      (if (string? content)
                          (make-text-content content)
                          (alist->content content))))))