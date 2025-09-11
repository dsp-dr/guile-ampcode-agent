;;; json-simple.scm --- Simple JSON implementation for compatibility

(define-module (json-simple)
  #:export (scm->json-string
            json-string->scm
            json))

;; For compatibility - re-export under (json) name
(define-module (json)
  #:re-export (scm->json-string
               json-string->scm))

;; Simple JSON serialization
(define (scm->json-string obj)
  (cond
   ((null? obj) "null")
   ((boolean? obj) (if obj "true" "false"))
   ((number? obj) (number->string obj))
   ((string? obj) (format #f "~s" obj))
   ((symbol? obj) (format #f "~s" (symbol->string obj)))
   ((list? obj)
    (if (and (pair? obj) (pair? (car obj)))
        ;; It's an alist (object)
        (string-append "{"
                      (string-join
                       (map (lambda (pair)
                              (format #f "~s:~a"
                                     (symbol->string (car pair))
                                     (scm->json-string (cdr pair))))
                            obj)
                       ",")
                      "}")
        ;; It's an array
        (string-append "["
                      (string-join
                       (map scm->json-string obj)
                       ",")
                      "]")))
   ((vector? obj)
    (string-append "["
                  (string-join
                   (map scm->json-string (vector->list obj))
                   ",")
                  "]"))
   (else "null")))

;; Simple JSON parsing (basic implementation)
(define (json-string->scm str)
  ;; Very basic parser - in production, use a proper JSON library
  (let ((trimmed (string-trim-both str)))
    (cond
     ((string=? trimmed "null") '())
     ((string=? trimmed "true") #t)
     ((string=? trimmed "false") #f)
     ((string-prefix? "\"" trimmed)
      (substring trimmed 1 (- (string-length trimmed) 1)))
     ((string-prefix? "{" trimmed)
      ;; Parse as alist - simplified
      '())
     ((string-prefix? "[" trimmed)
      ;; Parse as list - simplified
      '())
     ((string->number trimmed)
      => (lambda (n) n))
     (else trimmed))))

;; Helper to trim whitespace
(define (string-trim-both str)
  (let* ((chars (string->list str))
         (trimmed (drop-while char-whitespace? chars)))
    (list->string (reverse (drop-while char-whitespace? 
                                       (reverse trimmed))))))