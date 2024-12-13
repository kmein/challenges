(import (chicken process-context)
        (chicken io)
        (chicken string))

(define (in-bounds grid coordinates)
  (let ((row (car coordinates))
        (column (cdr coordinates))
        (total-rows (vector-length grid))
        (total-columns (vector-length (vector-ref grid row))))
    (and (>= row 0) (>= column 0) (<= row total-rows) (<= column total-columns))))

(define (grid-at grid coordinates)
  (let
    ((current-row (vector-ref grid (car coordinates)))
     (current-cell (vector-ref current-row (cdr coordinates))))
    current-cell))


(define (flood-fill grid x y visited region)
  (let* ((rows (vector-length grid))
         (cols (vector-length (vector-ref grid x)))
         (current-color (vector-ref (vector-ref grid x) y)))
    (when (and (>= x 0) (< x rows)
               (>= y 0) (< y cols)
               (not (member (list x y) visited)) ; Check if already visited
               (or (null? region)
                   (equal? current-color (vector-ref (vector-ref grid (car (car region))) (cadr (car region)))))) ; Check if the same color
      (set! visited (cons (list x y) visited)) ; Mark as visited
      (set! region (cons (list x y) region)) ; Add to the current region
      ;; Recursively call flood-fill on adjacent cells
      (flood-fill grid (+ x 1) y visited region) ; down
      (flood-fill grid (- x 1) y visited region) ; up
      (flood-fill grid x (+ y 1) visited region) ; right
      (flood-fill grid x (- y 1) visited region)))) ; left

(define (group-regions grid)
  (let loop ((x 0) (y 0) (visited '()) (regions '()))
    (if (< x (vector-length grid))
        (if (< y (vector-length (vector-ref grid x)))
            (let ((current-color (vector-ref (vector-ref grid x) y)))
              (if (not (member (list x y) visited)) ; If not visited
                  (let ((region '()))
                    (flood-fill grid x y visited region) ; Find the region
                    (loop x (+ y 1) visited (cons region regions))) ; Add region to regions
                  (loop x (+ y 1) visited regions))) ; Move to the next cell
            (loop (+ x 1) 0 visited regions)) ; Move to the next row
        regions))) ; Return the list of regions


(define (read-grid)
  (let ((filename (if (string? (get-environment-variable "AOC_TEST"))
                    "12.txt.test"
                    "12.txt")))
    (with-input-from-file
      filename
      (lambda ()
        (let ((content (read-string)))
          (apply vector
                 (map (lambda (line) (apply vector (string->list line)))
                      (string-split content "\n"))))))))

;; Group the regions

;; Print the grouped regions
(define (print-regions regions)
  (for-each (lambda (region)
              (display "Region: ")
              (for-each (lambda (cell)
                          (display cell)
                          (display " "))
                        region)
              (newline))
            regions))

(define grid (read-grid))
(define grouped-regions (group-regions grid))
(print-regions grouped-regions)


; (define (directions)
;   (list (-1 . 1) (1 . 0) (0 . -1) (0 . 1))
;
; (define (add-coordinates a b)
;   ((+ (car a) (car b)) . (+ (cdr a) (cdr b))))
;
; (define (neighbours grid coordinates)
;   (filter (in-bounds grid)
;           (map (lambda (dcoord)
;                  (grid-at grid (add-coordinates dcoord coordinates)))
;                directions)))
;
;
; (define (in-bounds grid coordinates)
;   (let ((row (car coordinates))
;         (column (cdr coordinates))
;         (total-rows (length grid))
;         (total-columns (length (car grid))))
;     (and (<= row total-rows) (<= column total-columns))))
;
; (define (depth-first-search (grid coordinates current-area areas))
;   (let
;     ((current-cell (grid-at grid coordinates)))
;     (neighbours grid coordinates)
;
; (define (flood-fill set? grid coordinates)
;   (let ((node (grid-at grid coordinates)))
;     (if (set? node) '(coordinates . node)
;       (append-map (lambda (adjacent) (flood-fill set? grid adjacent)) (neighbours grid coordinates)))))
;
; (define (read-grid)
;   (let ((filename (if (string? (getenv "AOC_TEST"))
;                     "12.txt.test"
;                     "12.txt")))
;     (with-input-from-file
;       filename
;       (lambda ()
;         (let ((content (read-string)))
;           (map (lambda (line) (string->list line))
;                (string-split content "\n")))))))
;
; (let (grid (read-grid))
;   (display (depth-first-search grid (0 . 0) "A" '())))
