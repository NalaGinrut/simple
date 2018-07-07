;;  Copyright (C) 2014
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  This file is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.

;;  This file is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define-module (language simple simple)
  #:use-module (system base lalr)
  #:use-module (language tree-il)
  #:use-module (ice-9 and-let-star)
  #:use-module (ice-9 match)
  #:export (make-simple-tokenizer make-parser compile-tree-il))

;; Two helper macros to create the token struct for returning
(define-syntax-rule (port-source-location port)
  (make-source-location (port-filename port)
                        (port-line port)
                        (port-column port)
                        (false-if-exception (ftell port))
                        #f))
(define-syntax-rule (return port category value)
  (make-lexical-token category (port-source-location port) value))

(define (is-whitespace? c) (char-set-contains? char-set:whitespace c))
(define (is-number? c) (char-set-contains? char-set:hex-digit c))
;; operators, in this simple case, we just have four operators
(define (is-op? c) (string-contains "+-*/" (string c)))
(define (is-delimiter? c)
  (or (eof-object? c) (string-contains " +-*/;\n" (string c))))
(define (get-number port)
 (let lp((c (peek-char port)) (ret '()))
   (cond
    ((is-delimiter? c) ; encounter delimiter, finish to read a number
     ;; convert to a number representation
     (string->number (list->string (reverse ret))))
    (else
     (read-char port) ; next char
     (lp (peek-char port) (cons c ret))))))
(define (get-op port) (string->symbol (string (read-char port))))
(define (next-token port)
  (let ((c (peek-char port)))
    (cond
     ((or (eof-object? c) (char=? c #\nl)) ; end of line, or end src
      '*eoi*) ; return '*eoi* because LALR module need it
     ((is-whitespace? c)
      (read-char port)
      (next-token port)) ; skip white space
     ((is-number? c)
      (return port 'number (get-number port)))
     ((is-op? c)
      (return port (get-op port) #f))
     (else
      (read-char port)
      (next-token port)))))
(define (make-simple-tokenizer port) (lambda () (next-token port)))
(define (make-parser)
  (lalr-parser
   (number + - * /)
   ;;((number #;(left: + -) #;(left: * /))
   (program (exp) : $1
            (*eoi*) : (call-with-input-string "" read)) ; *eof-object*
   (exp  (exp + term) : `(+ ,$1 ,$3)
         (exp - term) : `(- ,$1 ,$3)
         (term) : $1)
   (term (term * factor) : `(* ,$1 ,$3)
         (term / factor) : `(/ ,$1 ,$3)
         (factor) : $1)
   (factor (number) : `(number ,$1))))
(define (compile-tree-il exp env opts)
  (values (parse-tree-il (comp exp '())) env env))
(define (comp src e)
  (and-let* (((supports-source-properties? src))
             (loc (source-property src 'loc)))
            (format (current-error-port) "LOC [~a]: ~a\n" loc src))
  (match src
    (('number x) `(const ,x))
    ((op x y) `(call (primitive ,op) ,(comp x e) ,(comp y e)))
    ((h t ...) (comp h e)))) ;; for driver glr
