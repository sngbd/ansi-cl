;; 1. Show the following lists in box notation:
;; a. (a b (c d))
;; b. (a (b (c (d))))
;; c. (((a b) c) d)
;; d. (a (b . c) . d)
;; Solution: img/3.1.png

;; 2. Write a version of union that preserves the order of the elements in
;; the original lists:
;; > (new-union '(a b c) '(b a d))
;; (A B C D)
(defun new-union (x y)
  (reverse (union y (reverse x))))

;; 3. Define a function that takes a list and returns a list indicating the
;; number of times each (eql) element appears, sorted from most common
;; element to least common:
;; > (occurrences '(a b a d a c d c a))
;; ((A . 4) (C . 2) (D . 2) (B . 1))
;; (occurrences '(a b a d a c d c a))
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

;; 4. Why does (member '(a) '((a) (b))) return nil?
;; member's test is eql by default, hence why it return nil for comparing
;; two list (those two list are distinct objects).

;; 5. Suppose the function pos+ takes a list and returns a list of each element
;; plus its position:
;; > (pos+ '(7 5 1 4))
;; (7 6 3 7)
;; Define this function using (a) recursion, (b) iteration, (c) mapcar.
;; a. Recursion
(defun pos+rec (x)
  (if (null x)
      nil
      (append 
        (pos+rec (reverse (cdr (reverse x)))) 
        (cons (+ (car (reverse x)) (- (length x) 1)) nil))))
;; b. Iteration
(defun pos+iter (x)
  (let ((res nil))
    (do ((i x (cdr i)))
        ((null i) (reverse res))
      (setf res (cons (+ (length res) (car i)) res)))))
;; c. Mapcar
(defun pos+map (l)
  (let ((i -1))
    (mapcar #'(lambda (x) (+ (incf i) x)) l)))

;; 6. After years of deliberation, a government commission has decided that
;; lists should be represented by using the cdr to point to the first element
;; and the car to point to the rest of the list. Define the government
;; versions of the following functions:
;; (a) cons
;; (b) list
;; (c) length (for lists)
;; (d) member (for lists; no keywords)
;; a. cons
(defun cons-swap (x y)
  (if (null (consp y))
      (cons y x)
      (cons (cons-swap (car y) (cdr y)) x)))
;; b. list
(defun list-swap (x y)
  (if (null (consp y))
      (cons y x)
      (cons (cons y nil) x)))
;; c. length
(defun swap-back (x)
  (if (null x)
      nil
      (cons (cdr x) (swap-back (car x)))))

(defun length-swap (x)
  (length (swap-back x)))
;; d. member
(defun member-swap (x y)
  (let ((m (member x (swap-back y))))
    (cons-swap (car m) (cdr m))))
