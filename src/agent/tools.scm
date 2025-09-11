;;; tools.scm --- Tool registry and base tool functionality

(define-module (agent tools)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (ice-9 match)
  #:use-module (json-simple)
  #:export (make-tool
            tool?
            tool-name
            tool-description
            tool-input-schema
            tool-function
            
            make-tool-registry
            register-tool!
            get-tool
            list-tools
            tools->json-schema
            execute-tool))

;; Tool record type
(define-record-type <tool>
  (make-tool name description input-schema function)
  tool?
  (name tool-name)
  (description tool-description)
  (input-schema tool-input-schema)
  (function tool-function))

;; Tool registry
(define-record-type <tool-registry>
  (%make-tool-registry tools)
  tool-registry?
  (tools tool-registry-tools set-tool-registry-tools!))

(define (make-tool-registry)
  (%make-tool-registry '()))

(define (register-tool! registry tool)
  (set-tool-registry-tools! 
   registry
   (cons tool (tool-registry-tools registry))))

(define (get-tool registry name)
  (find (lambda (tool)
          (string=? (tool-name tool) name))
        (tool-registry-tools registry)))

(define (list-tools registry)
  (tool-registry-tools registry))

;; Convert tools to JSON schema format for Claude
(define (tools->json-schema registry)
  (map (lambda (tool)
         `((name . ,(tool-name tool))
           (description . ,(tool-description tool))
           (input_schema . ,(tool-input-schema tool))))
       (list-tools registry)))

;; Execute a tool
(define (execute-tool registry name input)
  (let ((tool (get-tool registry name)))
    (if tool
        (catch #t
          (lambda ()
            ((tool-function tool) input))
          (lambda (key . args)
            (format #f "Error executing tool ~a: ~a" name args)))
        (format #f "Tool not found: ~a" name))))