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

;;Returns a collection with all keys stored in the dictionary, including possible duplicates.
(defmethod keys((d dictionary))
 (append (getAllkeys(dictionary-elements d))))


;;Returns a collection with all keys stored in the dictionary, including possible duplicates.
(defmethod elements((d dictionary))
 (append(getAllElements(dictionary-elements d))))

;;Inserts an item with element e and key k into the dictionary
(defmethod insertItem(key element (d dictionary))
  (setf (dictionary-elements d) (cons (list key element) (dictionary-elements d)))
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
 (setf (dictionary-elements d)(removeAllBykey (d dictionary)))
)


;;Sub Class of Dictonary
(defclass restricted-dictionary(dictionary)
  ((capacity :accessor restricted-dictionary-capacity             
             :initform 10
             :allocation :class)
   )
)

;;Inserts an item with element e and key k into the dictionary
(defmethod insertItem :around(key element (rd restricted-dictionary))
  (cond ((equal (stringp key) nil) "error: key is not string")
        ((equal (stringp element) nil) "error: element is not string")
        ((equal key element) "error: key and element are same")
        ((memberp key (keys rd)) "error: key already exist")
        ((> (dictionary-size rd) (restricted-dictionary-capacity rd)) "error: you don't have capacity")
        (T (call-next-method))
    )  
 )



;;implementation of memberp
(defun  memberp(key lst)
  (cond ((null lst) nil)
    ((equal key (car lst)) T)
    (T (memberp key ( cdr lst)))
    )
 )


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

(defun getAllkeys(lst)
 (if (or (null lst)(not(listp lst))) 
      nil
      (append (cons (car(car lst)) (getAllkeys (cdr lst)))))
) 

(defun getAllElements(lst)
 (if (or (null lst)(not(listp lst))) 
      nil
     (append (cdr(car lst)) (getAllElements (cdr lst))))
 )

;;================================== Testing Code==============================

;;Checking task 2
(setq rd ( make-instance 'restricted-dictionary))
(print (insertItem "b" "rain"  rd))
(print (display rd))
;(print (insertItem 'rain "roman" rd)
;(print (insertItem 'rasel "bhai" rd)))
;(print (insertItem 'rain "rain" rd))




