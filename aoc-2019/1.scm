#!/usr/bin/env -S csi -script
(import (chicken io))

(define (mass->fuel mass)
  (- (floor (/ mass 3)) 2))

(call-with-input-file
  "1.input"
  (lambda (input-file)
    (let*
      ((masses (map string->number (read-lines input-file)))
       (fuels (map mass->fuel masses))
       (total-fuel (apply + fuels)))
      (begin
        (display total-fuel)
        (newline)))))
