; 1. Show the following lists in box notation:
; a. (a b (c d))
; b. (a (b (c (d))))
; c. (((a b) c) d)
; d. (a (b . c) . d)
; Solution: img/3.1.png

; 2. Write a version of union that preserves the order of the elements in
; the original lists:
; > (new-union '(a b c) '(b a d))
; (A B C D)

(defun new-union (x y)
  (reverse (union y (reverse x))))

; 3. Define a function that takes a list and returns a list indicating the
; number of times each (eql) element appears, sorted from most common
; element to least common:
; > (occurrences '(a b a d a c d c a))
; ((A . 4) (C . 2) (D . 2) (B . 1))

(defun occurrences (x)
  (if (consp x)
      (occ x '())
      x))

(defun occ (x y)
  (if (null x)
      (sort y #'(lambda (x y) (> (cdr x) (cdr y))))
      (if (non-member (car x) y)
          (occ (cdr x) (cons (cons (car x) (cnt (car x) x)) y))
          (occ (cdr x) y))))

(defun cnt (x y)
  (if (null (consp y))
      y
      (let ((res 0))
        (do ((i y (cdr i)))
            ((eql nil (car i)) res)
          (if (eql x (car i))
              (setf res (+ res 1)))))))

(defun non-member (elem data)
  (if (null data)
      t
      (if (eql (car (car data)) elem)
          nil
          (non-member elem (cdr data)))))
