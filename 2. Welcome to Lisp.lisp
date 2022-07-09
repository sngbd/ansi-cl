;; 1. Describe what happens when the following expressions are evaluated:
;; a. 
(+ (- 5 1) (+ 3 7))
;; (+ 4 (+ 3 7))
;; (+ 4 10)
;; 14
;; b. 
(list 1 (+ 2 3))
;; (list 1 5) -> will return a list with its arguments as the elements
;; (1 5)
;; c.
(if (listp 1) (+ 1 2) (+ 3 4))
;; (listp 1) -> evaluated to nil, because 1 is not a list
;; (+ 3 4) so, the latter argument is evaluated
;; 7
;; d. 
(list (and (listp 3) t) (+ 1 2)) ; (and nil t) -> nil
;; (list nil 3)
;; (NIL 3)

;; 2. Give three distinct cons expressions that return (a b c).
(cons 'a '(b c))
(cons 'a (cons 'b '(c)))
(cons 'a (cons 'b (cons 'c ())))

;; 3. Using car and cdr, define a function to return the fourth element of a list.
(defun four-th (lst)
  (car (cdr (cdr (cdr lst)))))

;; 4. Define a function that takes two arguments and returns the greater of the two.
(defun greater (a b)
  (if (> a b)
      a
      b))

;; 5. What do these functions do?
;; a. 
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x)))))
;; always returns nil for a list argument
;; b.
(defun mystery (x y)
  (if (null y)
      nil
      (if (eql (car y) x)
          0
          (let ((z (mystery x (cdr y))))
            (and z (+ z 1))))))
;; returns nil if y is an empty list or x is not found in y, 
;; otherwise it returns the index (starting from 0) of x in y.

;; 6. What could occur in place of the x in each of the following exchanges?
;; a. > (car (x (cdr '(a (b c) d)))) -> B
;; car
;; b. > (x 13 (/ 1 0)) -> 13
;; or
;; c. (x #'list 1 nil) -> 1
;; apply

;; 7. Using only operators introduced in this chapter, define a function that
;; takes a list as an argument and returns true if one of its elements is a list.
(defun elemlistp (x)
  (if (null x)
      nil
      (if (listp (car x))
          t
          (elemlistp (cdr x)))))

;; 8. Give iterative and recursive definitions of a function that
;; a. takes a positive integer and prints that many dots.
;; b. takes a list and returns the number of times the symbol a occurs in it.

;; a.
;; recursive:
(defun dots-rec (n)
  (if (eql n 0)
      'done
      (progn
        (format t ".")
        (dots-rec (- n 1)))))
;; iterative
(defun dots-iter (n) 
  (do ((i n (- i 1)))
      ((eql 0 i) 'done)
    (format t ".")))
;; b.
;; recursive
(defun counta-rec (x)
  (if (null x)
      0
      (if (eql (car x) 'a)
          (+ 1 (counta-rec (cdr x)))
          (counta-rec (cdr x)))))
;; iterative
(defun counta-iter (x) 
  (let ((z 0))
    (do ((i x (cdr i)))
        ((eql nil (car i)) z)
      (if (eql 'a (car i))
          (setf z (+ z 1))
          (setf z (+ z 0))))))

;; 9. A friend is trying to write a function that returns the sum of all the
;; non-nil elements in a list. He has written two versions of this function,
;; and neither of them work. Explain what's wrong with each, and give a correct version:
;; a. 
;; (defun summit (lst) 
;;   (remove nil lst) 
;;   (apply #'+ lst))
;;(remove nil lst) only returns the list without nil.
;; fix: use setf to remove nil from list.
;; correct version:
(defun summit-fix (lst)
  (setf lst (remove nil lst))
  (apply #'+ lst))
; b.
;; (defun summit (lst)
;;   (let ((x (car lst)))
;;     (if (null x)
;;         (summit (cdr lst))
;;         (+ x (summit (cdr lst))))))
;; (summit (cdr lst)) when x is null will lead to infinite recursion.
;; fix: first, check if lst is empty. If it is return 0, else check if x is nil.
;; If x is nil then return (summit (cdr lst)), otherwise return x + (summit (cdr lst)).
;; correct version:
(defun summit-fix2 (lst)
  (let ((x (car lst)))
    (if (not lst)
        0
        (if (null x)
            (summit-fix (cdr lst))
            (+ x (summit-fix (cdr lst)))))))
