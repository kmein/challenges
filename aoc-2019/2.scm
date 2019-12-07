#!/usr/bin/env -S csi -script
(import (chicken string)
        (chicken io))

(define (set-nth index item list)
  (if (= index 1)
      (cons item (cdr list))
      (cons (car list)
            (set-nth (- index 1) item (cdr list)))))

(define (opcode f index program)
  (let ((argument1 (list-ref program (+ index 1)))
        (argument2 (list-ref program (+ index 2))))
    (set-nth (+ index 3) (f argument1 argument2) program)))

(define (run index program)
  (let ((current (list-ref program index)))
    (cond ((= current 1) (run (+ index 4) (opcode + index program)))
          ((= current 2) (run (+ index 4) (opcode * index program)))
          ((= current 99) program)
          (else (exit 1)))))

(call-with-input-file
  "2.input"
  (lambda (input-file)
    (let*
      ((instructions (map string->number
                          (string-split (read-string #f input-file) ",")))
       (program (compress (map number? instructions) instructions))
       (final (run 0 program))
       (final-string (string-intersperse (map number->string final) ",")))
      (begin
        (display program)
        (display final)
        (newline)))))
