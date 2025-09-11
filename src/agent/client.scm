;;; client.scm --- Anthropic API client

(define-module (agent client)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (json)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:use-module (rnrs bytevectors)
  #:use-module (agent message)
  #:export (make-client
            client?
            client-api-key
            client-model
            set-client-model!
            
            send-message))

;; Client record
(define-record-type <client>
  (make-client api-key model)
  client?
  (api-key client-api-key)
  (model client-model set-client-model!))

;; API endpoint
(define *api-endpoint* "https://api.anthropic.com/v1/messages")
(define *api-version* "2023-06-01")

;; Send message to Claude API
(define (send-message client messages tools)
  (let* ((api-key (client-api-key client))
         (model (client-model client))
         (request-body 
          `((model . ,model)
            (max_tokens . 4096)
            (messages . ,(map message->alist messages))
            ,@(if (and tools (not (null? tools)))
                  `((tools . ,tools))
                  '())))
         (json-body (scm->json-string request-body))
         (headers `((content-type . "application/json")
                   (x-api-key . ,api-key)
                   (anthropic-version . ,*api-version*))))
    
    (catch #t
      (lambda ()
        (receive (response body)
            (http-post *api-endpoint*
                      #:body json-body
                      #:headers headers
                      #:decode-body? #f)
          (let ((status (response-code response)))
            (if (= status 200)
                (let* ((json-str (bytevector->string body "UTF-8"))
                       (result (json-string->scm json-str)))
                  (parse-api-response result))
                (error "API request failed" status 
                       (bytevector->string body "UTF-8"))))))
      (lambda (key . args)
        (error "Failed to send message" key args)))))

;; Parse API response into message
(define (parse-api-response response)
  (let ((content (assoc-ref response 'content)))
    (make-message "assistant" 
                  (map parse-content-block content))))

(define (parse-content-block block)
  (match (assoc-ref block 'type)
    ("text"
     (make-text-content (assoc-ref block 'text)))
    ("tool_use"
     (make-tool-use-content
      (assoc-ref block 'id)
      (assoc-ref block 'name)
      (assoc-ref block 'input)))
    (_ block)))