;;; weather.scm --- Weather tool implementation

(define-module (tools weather)
  #:use-module (agent tools)
  #:use-module (ice-9 format)
  #:export (weather-tool))

;; Weather tool definition
(define weather-tool
  (make-tool
   "get_weather"
   "Get the current weather for a specified location"
   '((type . "object")
     (properties . ((location . ((type . "string")
                                 (description . "The city and state, e.g. San Francisco, CA")))
                   (unit . ((type . "string")
                           (enum . #("celsius" "fahrenheit"))
                           (description . "Temperature unit")))))
     (required . #("location")))
   
   ;; Tool function
   (lambda (input)
     (let ((location (assoc-ref input 'location))
           (unit (or (assoc-ref input 'unit) "celsius")))
       ;; This is a mock implementation
       ;; In a real app, this would call a weather API
       (format #f "The weather in ~a is currently 22° ~a with partly cloudy skies."
               location
               (if (string=? unit "celsius") "C" "F"))))))