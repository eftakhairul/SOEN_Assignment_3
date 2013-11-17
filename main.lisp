;;;; 2013-11-16 02:16:11
;;;; This is your lisp file. May it serve you well.
;;;; author Eftakhairul Islam <eftakhairul@gmail.com>

;;class
(defclass dictionary()
  ((size :accessor dictionary-size
         :initform 0)
   (els  :accessor dictionary-elements
         :initform '()))
)

;;Display all elements
(defmethod display ((d dictionary))
  (dictionary-elements d)
)

;;Inserts an item with element e and key k into the dictionary
(defmethod insert( el (d dictionary))
  ( setf (dictionary-elements d) (cons el (dictionary-elements d)))
  (setf (dictionary-size d) (+ 1 (dictionary-size d)))
)

;;Returns the number of items in the dictionary.
(defmethod size((d dictionary))
  (dictionary-size d)  
)

;;Tests whether the dictionary is empty. Returns True or False.
(defmethod isEmpty((d dictionary))
  (if(= (dictionary-size d) 0)
      T
      Nil)
)

;;Returns an item associated with key, OR False if an item does not exist.
(defmethod ﬁndItem(key (d dictionary))
  (searchByKey key (dictionary-elements d))
  )

;;Returns a collection with all items by key.
(defmethod ﬁndAll(key (d dictionary))
  (searchAllByKey key (dictionary-elements d))
 )

;;Removes an item with key equal to k and returns True, or if an item does not exist then it returns False
(defmethod removeItem(key (d dictionary))
 (setf (dictionary-elements d)(removeOneItemBykey key (dictionary d))))


;;Removes every item with key equal to k and returns True, or if no items exist then it returns False
(defmethod removeAllItems(key (d dictionary))
 (setf (dictionary-elements d)(removeAllBykey (d dictionary))))

(defun removeOneItemBykey(key lst)
 (cond ((null lst) nil)
  ((equal key(car(car lst)))(cdr lst)) 
   (t (cons (car lst) (removeOneItemBykey key (cdr lst)))))
 )

(defun removeAllBykey(key lst)
 (cond ((null lst) nil)
       ((equal key(car(car lst))) (removeAllBykey (cdr lst)))
       (t (cons (car lst) (removeAllBykey key (cdr lst)))))
)


(defun searchByKey(key lst)
 (cond ((null lst)   nil)
       ((equal (car(car lst)) key) (append(cdr(car lst))))
       (T (searchByKey key (cdr lst))))
)

(defun searchAllByKey(key lst)
 (cond ((null lst) nil)
       ((equal (car(car lst)) key) (append (cdr(car lst))(searchAllByKey key (cdr lst))))
       (T (searchAllByKey key (cdr lst))))
)

;;================================== Testing Code==============================
(setq c ( make-instance 'dictionary))
(insert '(1 a) c)
(insert '(2 b) c)
(print(display c))
(print (isEmpty c))
(print (size c))
(print (ﬁndItem '1 c))
