;;; ollama-client.scm --- Ollama API client for local LLMs

(define-module (agent ollama-client)
  #:use-module (srfi srfi-1)
  #:use-module (web client)
  #:use-module (web uri)
  #:use-module (web response)
  #:use-module (json-simple)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 iconv)
  #:use-module (rnrs bytevectors)
  #:use-module (agent message)
  #:export (ollama-api-call
            ollama-list-models
            ollama-pull-model))

;; Make API call to Ollama
(define (ollama-api-call host port model messages tools)
  (let* ((endpoint (format #f "http://~a:~a/api/chat" host port))
         (prompt (messages->ollama-prompt messages))
         (request-body 
          `((model . ,model)
            (messages . ,prompt)
            (stream . #f)))
         (json-body (scm->json-string request-body))
         (headers '((content-type . "application/json"))))
    
    (catch #t
      (lambda ()
        (receive (response body)
            (http-post endpoint
                      #:body json-body
                      #:headers headers
                      #:decode-body? #f)
          (let ((status (response-code response)))
            (if (= status 200)
                (let* ((json-str (bytevector->string body "UTF-8"))
                       (result (json-string->scm json-str)))
                  (parse-ollama-response result tools))
                (make-message "assistant"
                             (make-text-content 
                              (format #f "Ollama error: ~a" 
                                      (bytevector->string body "UTF-8"))))))))
      (lambda (key . args)
        (make-message "assistant"
                     (make-text-content 
                      (format #f "Failed to connect to Ollama: ~a" args)))))))

;; Convert messages to Ollama format
(define (messages->ollama-prompt messages)
  (map (lambda (msg)
         `((role . ,(message-role msg))
           (content . ,(extract-text-content (message-content msg)))))
       messages))

(define (extract-text-content content)
  (cond
   ((text-content? content)
    (text-content-text content))
   ((list? content)
    (string-join 
     (filter string?
             (map (lambda (c)
                    (if (text-content? c)
                        (text-content-text c)
                        ""))
                  content))
     " "))
   (else "")))

;; Parse Ollama response
(define (parse-ollama-response response tools)
  (let ((message (assoc-ref response 'message)))
    (if message
        (let ((content (assoc-ref message 'content)))
          ;; Simple heuristic for tool detection in response
          (if (and tools 
                   (not (null? tools))
                   (string-contains-ci content "use")
                   (string-contains-ci content "tool"))
              ;; Simulate tool call based on content
              (make-message "assistant"
                           (list (make-text-content content)
                                 (detect-and-create-tool-call content tools)))
              ;; Regular text response
              (make-message "assistant" (make-text-content content))))
        (make-message "assistant" 
                     (make-text-content "No response from Ollama")))))

;; Detect tool mentions and create tool call
(define (detect-and-create-tool-call content tools)
  (let ((mentioned-tool 
         (find (lambda (tool)
                 (string-contains-ci content (assoc-ref tool 'name)))
               tools)))
    (if mentioned-tool
        (make-tool-use-content
         (format #f "ollama-~a" (random 1000))
         (assoc-ref mentioned-tool 'name)
         '())
        (make-text-content ""))))

;; List available models
(define (ollama-list-models host port)
  (let ((endpoint (format #f "http://~a:~a/api/tags" host port)))
    (catch #t
      (lambda ()
        (receive (response body)
            (http-get endpoint #:decode-body? #f)
          (let ((status (response-code response)))
            (if (= status 200)
                (let* ((json-str (bytevector->string body "UTF-8"))
                       (result (json-string->scm json-str)))
                  (assoc-ref result 'models))
                '()))))
      (lambda (key . args)
        '()))))

;; Pull a model
(define (ollama-pull-model host port model-name)
  (let* ((endpoint (format #f "http://~a:~a/api/pull" host port))
         (request-body `((name . ,model-name)))
         (json-body (scm->json-string request-body))
         (headers '((content-type . "application/json"))))
    
    (catch #t
      (lambda ()
        (receive (response body)
            (http-post endpoint
                      #:body json-body
                      #:headers headers
                      #:decode-body? #f)
          (let ((status (response-code response)))
            (= status 200))))
      (lambda (key . args)
        #f))))

;; Helper for case-insensitive string search
(define (string-contains-ci str substr)
  (let ((str-lower (string-downcase str))
        (substr-lower (string-downcase substr)))
    (string-contains str-lower substr-lower)))