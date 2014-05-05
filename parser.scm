;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; File: PARSER.SCM
;; Author: Hoa Long Tam (hoalong.tam@berkeley.edu)
;;
;; Adapted for use in Python from a Logo-in-Scheme interpreter written by Brian
;; Harvey (bh@cs.berkeley.edu), available at ~cs61a/lib/logo.scm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Selectors for the list returned by py-read.
(define indentation car)
(define tokens cdr)

(define (make-line-obj line)
  (instantiate line-obj (indentation line) (tokens line)))

;; A class to represent a sequence of tokens to be used by the evaluator.
(define-class (line-obj indentation tokens)
  (method (empty?)
    (null? tokens))
  (method (exit?)
    (member tokens '((exit |(| |)|) (quit |(| |)|))))
  (method (peek)
    (car tokens))
  (method (push token)
    (set! tokens (cons token tokens)))
  (method (next)
    (let ((token (car tokens)))
      (set! tokens (cdr tokens))
      token)))

;; Parser utility functions
(define (char->symbol ch) (string->symbol (make-string 1 ch))) ;1 is # of ch in string
(define operators '(#\+ #\- #\* #\/ #\% #\< #\> #\! #\=))
(define (comma? symbol) (eq? symbol '|,|))
(define (colon? symbol) (eq? symbol '|:|))
(define open-brace-symbol (char->symbol #\{))
(define close-brace-symbol (char->symbol #\}))
(define open-paren-symbol (char->symbol #\())
(define close-paren-symbol (char->symbol #\)))
(define open-bracket-symbol (char->symbol #\[))
(define close-bracket-symbol (char->symbol #\]))
(define (char-newline? char)
  (or (eq? char #\newline) ;; you're in
      (and (eq? char #\return)
	   (eq? (peek-char) #\newline)
	   (read-char))))  ;; chomp off newline
;; B[#3] GET-NUM
(define (dot-symbol? symbol) (eq? symbol '|.|))  ;for symbols
(define (dot-char? char) (eq? char #\.))   ; for characters

;;;;
;; The main tokenizer.  Reads in a line from standard input and returns a list
;; of the (indentation token1 token2 token3 ...).  Turns the line
;; 'def foo(a,b):' into (def foo |(| a |,| b |)| :).
;;;

(define (py-read)

 ;;;; Comm[#2] GET-AND-INDENT-TOKENS
  (define (get-indent-and-tokens)  ;;; person A implementation
    (let ((indent (get-indent 0)))    ;; call indent with 0 as starting indentation
    (cons indent (get-tokens '()))) )  ;; start list with indentation and tokens  
  ;;; person B implementation
   ; (let ((indent (count-indent 0)))  
   ;   (cons indent (get-tokens '() )))  )

  (define (reverse-brace char)
    (let ((result (assq char '((#\{ . #\}) (#\} . #\{)
			       (#\( . #\)) (#\) . #\()
			       (#\[ . #\]) (#\] . #\[)))))
      (if result
	  (cdr result)
	  (read-error "SyntaxError: bad closing brace: " char))))
  
  (define (get-tokens braces)
    ;; Reads in until the end of the line and breaks the stream of input into a
    ;; list of tokens.  Braces is a list of characters representing open brace
    ;; ([, (, and {) tokens, so it can throw an error if braces are mismatched.
    ;; If it reaches the endof a line while inside braces, it keeps reading
    ;; until the braces are closed.
    (let ((char (peek-char)))
      (cond
       ((char-newline? char)
	(if (not (null? braces))
	    (begin (read-char) (get-tokens braces))
	    (begin (read-char) '())))
       ((eof-object? char)
	(if (not (null? braces))
            (read-error "SyntaxError: End of file inside expression")
            '()))

       ((eq? char #\space)
        (read-char)
        (get-tokens braces))
       ((eq? char #\#)       ;; Ignore everything after python comment symbol '#'
        (ignore-comment)     ;;;; Comm[#1] IGNORE-COMMENT implemented
        '())
       ((memq char (list #\[ #\( #\{))
        (let ((s (char->symbol (read-char))))
          (cons s (get-tokens (cons char braces)))))
       ((memq char (list #\] #\) #\}))
	(if (and (not (null? braces)) (eq? char (reverse-brace (car braces))))
            (let ((t (char->symbol (read-char))))
              (cons t (get-tokens (cdr braces))))
            (read-error "SyntaxError: mismatched brace: " char)))
       ((memq char (list #\, #\:))
        (let ((t (char->symbol (read-char))))
          (cons t (get-tokens braces))))
     ;;;; A[#3] GET-STRING implemented 
       ((memq char (list #\" #\')) ;; check if char is opening a string
	(let ((t (list->string (get-string (read-char))))) ;; pass in type of quote
	  (cons t (get-tokens braces))))
       ;((memq char (list #\" #\'))  
        ;(let ((t (list->string (get-string (read-char)))))
         ; (cons t (get-tokens braces))))
       
       ((memq char operators)
        (let ((t (get-operator)))
          (cons t (get-tokens braces))))
      
       ((char-numeric? char)         ;;;; B[#3 ] GET-NUM implemented
	(let ((num (get-num "" #f)))   ;; dot-flag initally false
	  (if (string? num)
              (cons (string->number num) (get-tokens braces))
              (cons num (get-tokens braces)))))
       
       (else
	(let ((token (get-token (char->symbol (read-char)))))
	  (cond
           ((and (string? token)
                 (DOT-CHAR? (string-ref token 0))   ;; more clear with this
                 (char-numeric? (string-ref token 1)))
	    (cons (word (string->symbol (string-append "0" token)))
		  (get-tokens braces)))
           ((string? token)
	    (cons (string->symbol token) (get-tokens braces)))
           (else (cons token (get-tokens braces)))))))))
  (define (get-token so-far)
    (let ((char (peek-char)))
      (if (not (or (char-alphabetic? char)
		   (char-numeric? char)
		   (eq? char #\_)))
	  so-far
	  (get-token (word so-far (char->symbol (read-char)))))))

    ;; Reads in a number.  Num-so-far a Scheme word (we will convert back into
    ;; a Scheme number in the get-tokens procedure).
    ;; maintain a dot-flag - boolean tracking if there has already been a dot
    ;; in the number

  (define (get-num num-so-far dot-flag)    ;;;; B[#4]  GET-NUM
    (let ((char (peek-char)))
      (cond ((char-numeric? char)
	     (get-num (word num-so-far (char->symbol (read-char))) dot-flag))
	    ((dot-char? char) 	    
	     (if dot-flag    
		 num-so-far   ;; do not throw error, just return current number
			      ;; set dot-flag to true
		 (get-num (word num-so-far (char->symbol (read-char))) #t)))
	    (else num-so-far)))  )

  (define (get-operator)
    (let ((char (read-char))
	  (next (peek-char)))
      (cond ((eq? char #\+) (if (eq? next #\=) (begin (read-char) '+=) '+))
	    ((eq? char #\-) (if (eq? next #\=) (begin (read-char) '-=) '-))
	    ((eq? char #\%) (if (eq? next #\=) (begin (read-char) '%=) '%))
	    ((eq? char #\<) (if (eq? next #\=) (begin (read-char) '<=) '<))
	    ((eq? char #\>) (if (eq? next #\=) (begin (read-char) '>=) '>))
	    ((eq? char #\=) (if (eq? next #\=) (begin (read-char) '==) '=))
	    ((eq? char #\/) (if (eq? next #\=) (begin (read-char) '/=) '/))
	    ((eq? char #\!)
	     (if (eq? next #\=)
		 (begin (read-char) '!=)
		 (read-error "Unknown operator: !")))
	    ((eq? char #\*)
	     (cond ((eq? next #\*)
		    (read-char)
		    (if (eq? (peek-char) #\=)
			(begin (read-char) '**=)
			'**))
		   ((eq? next #\=) (read-char) '*=)
		   (else '*))))))
;;;; A[#3] GET-STRING person A
  (define (get-string type)   ;; type == quote passed  (A3)                      
    (let ((char (read-char))) ;; char == first char after the quote 
      (if (eq? char type)     ;; if char is the same as the quote passed in
	  '()                 ;; finish the token!
	  (cons char (get-string type))))) ;;  start a token list with char 

    ;; Reads in a string and returns a list of Scheme characters, up to, but not
    ;; including the closing quote.  Type is the Scheme character that opened
    ;; the string.  The first character returned by (read-char) when this
    ;; function is executed will be the first character of the desired string.

   (define (ignore-comment)  ;;;; Comm[#1] IGRNORE-COMMENT person A
     (helper-ignore-comment))
#|                           ;;;; Comm[#1] IGRNORE-COMMENT person B
   (define (ignore-comment)  
     (read-ignore '*ignore-comment*))
|#  
  (define (get-string type)   ;; type == quote passed  (A3)                      
    (let ((char (read-char))) ;; char == first char after the quote 
      (if (eq? char type)     ;; if char is the same as the quote passed in
	  '()                 ;; finish the token!
	  (cons char (get-string type))))) ;; else, start making a token list with char 
  
   (get-indent-and-tokens)     
    )  ;; end PY-READ

;; Error handler for py-read.  Needs to eat remaining tokens on the line from
;; user input before throwing the error.

(define (read-error . args)
  (define (loop)
    (let ((char (read-char)))
      (if (or (char-newline? char) (eof-object? char))
	  (apply py-error args)
	  (loop))))
  (loop))

;;;;;;;;;;  Added Utility Procedures for problems

;; Comm[#1] person B   READ-IGNORE 
    ;; Initially written for ignore-comment. Takes one argument of a commentary word.
    ;; Used for recursively reading, igrnoring, and printing a commentary word
    ;; If comment argument is 'no-comment, it does not print anything.

(define (read-ignore comment)
  (define (print-comment?)
    (if (not (equal? comment 'no-comment))
	comment) )    ;; used as a pred for comment printing
	    
  (let ((input-char (read-char)))
    (cond ((or (eof-object? input-char) (char-newline? input-char))
	   (print-comment?) )
	  (else (read-ignore comment))))   )

;; Comm[#2] person B (get-indent-and-tokens)
    ;; recursively reads the spaces at the begging of a line until a character.
    ;; returns number of spaces as the size of the indent to support
    ;; Python's dependency on indentation structure
    ;; returns number of indents to be paired with line phrase.
(define (count-indent current-indent)
  (let ((input-char (peek-char)))
    (if (eq? input-char #\space)
	(begin (read-char) (count-indent (+ 1 current-indent)))
	current-indent))  )
		   
;; B[#3] (GET-NUM)  


;; Comm[#1] person A  HELPER-IGNORE-COMMENT (ignore-comment) 
(define (helper-ignore-comment)
  (let ((char (read-char)))         ;; eat the char! (hashtag eaten first time thru) 
    (cond ((eof-object? char)       ;; reach the end of obj? return nothing
	   '*comment-ignored*)
	  ((char-newline? char)     ;; reach a new line? return nothing 
	   '*comment-ignored*)
	  (else (helper-ignore-comment)))))  ;; inside the comment, loop back through


;; Comm[#1] person A  GET-INDENT (get-indent-and-tokens)
;; 2. Helper proc for (get-indent-and-tokens)
(define (get-indent counter)
  (let ((char (peek-char)))         ;; look at the next char
    (if (not (eq? char #\space))    ;; if its not equal to a space
	counter                     ;; return counter, i.e. number of spaces
	(begin (read-char)          ;; else, eat the space
	       (get-indent (+ counter 1)))))) ;; recurse on get-indent
