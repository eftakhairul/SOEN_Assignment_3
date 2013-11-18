;; 2013-11-16 02:16:11
;; author Eftakhairul Islam <eftakhairul@gmail.com>

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
  (getAllkeys(dictionary-elements d))
)

;;Returns a collection with all keys stored in the dictionary, including possible duplicates.
(defmethod elements((d dictionary))
 (getAllElements(dictionary-elements d))
)

;;Inserts an item with element e and key k into the dictionary
(defmethod insertItem(key element (d dictionary))
  (setf (dictionary-elements d) (cons (list key element) (dictionary-elements d)))
  (setf (dictionary-size d) (+ 1 (dictionary-size d)))
  T
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
  (if(memberp key (keys d))
      (searchByKey key (dictionary-elements d))
      Nil)  
)

;;Returns a collection with all items by key.
(defmethod ﬁndAll(key (d dictionary))
  (if(memberp key (keys d))
      (searchAllByKey key (dictionary-elements d))
      Nil) 
 )

;;Removes an item with key equal to k and returns True, or if an item does not exist then it returns False
(defmethod removeItem(key (d dictionary))
  (cond ((isEmpty d) nil)
         ((memberp key (keys d)) (setf (dictionary-elements d)(removeOneItemBykey key (dictionary-elements d)))
          (setf (dictionary-size d) (- (dictionary-size d) 1))
          T
         )             
    )
  )

;;Removes every item with key equal to k and returns True, or if no items exist then it returns False
(defmethod removeAllItems(key (d dictionary))
  (cond ((isEmpty d) nil)
         ((memberp key (keys d)) (setf (dictionary-elements d)(removeAllBykey key (dictionary-elements d)))
          (setf (dictionary-size d) (length (dictionary-elements d)))
          T
          )             
    )
)




;;Inheritance
;;Sub Class of Dictonary
(defclass restricted-dictionary(dictionary)
  ((capacity :accessor restricted-dictionary-capacity             
             :initform 10
             :allocation :class)
   )
)

;;Inserts an item with element e and key k into the dictionary
(defmethod insertItem :around(key element (rd restricted-dictionary))
  (cond ((equal (stringp key) nil)      "error: key is not string")
        ((equal (stringp element) nil)  "error: element is not string")
        ((equal key element)            "error: key and element are same")
        ((memberp key (keys rd))        "error: key is already exist")
        ((> (dictionary-size rd) (restricted-dictionary-capacity rd)) "error: you don't have capacity")
        (T (call-next-method key element rd))
    )  
 )

;;implementation of memberp
(defun  memberp(key lst)
  (cond ((null lst) nil)
    ((equal key (car lst)) T)
    (T (memberp key ( cdr lst)))
    )
 )


;;======================================== Auxilary functions ======================================

;;return the list after removing the item by key
(defun removeOneItemBykey(key lst)
 (cond ((null lst) nil)
       ((equal key(car(car lst)))(cdr lst)) 
       (T (cons (car lst) (removeOneItemBykey key (cdr lst)))))
 )

;;return the list after removing all items by key
(defun removeAllBykey(key lst)
 (cond ((null lst) nil)
       ((equal key(car(car lst))) (removeAllBykey key (cdr lst)))
       (t (cons (car lst) (removeAllBykey key (cdr lst)))))
)

;;return the element by key from the list
(defun searchByKey(key lst)
 (cond ((null lst)   nil)
       ((equal (car(car lst)) key) (append(cdr(car lst))))
       (T (searchByKey key (cdr lst))))
)

;;return all elements by key from the list
(defun searchAllByKey(key lst)
 (cond ((null lst) nil)
       ((equal (car(car lst)) key) (append (cdr(car lst))(searchAllByKey key (cdr lst))))
       (T (searchAllByKey key (cdr lst))))
)

;;return all keys
(defun getAllkeys(lst)
 (if (or (null lst)(not(listp lst))) 
      nil
      (append (cons (car(car lst)) (getAllkeys (cdr lst)))))
) 


;;return all elements
(defun getAllElements(lst)
 (if (or (null lst)(not(listp lst))) 
      nil
     (append (cdr(car lst)) (getAllElements (cdr lst))))
 )
;;================================== Testing Code==============================

;;Check task 1
(setq d ( make-instance 'dictionary))
(print (insertItem "a" "Newyork"  d))
(print (insertItem "b" "Montreal"  d))
(print (keys  d))

(print (removeItem "b" d))
(print (size d))
(print (insertItem "b" "Montreal"  d))
(print (insertItem "b" "Qubec City"  d))
(print (insertItem "c" "Toronto"  d))
(print (display d))
(print (size d))
(print (removeAllItems "b" d))
(print (size d))



;;Checking task 2
(setq rd ( make-instance 'restricted-dictionary))
(write-line "One Insertion")
(print (insertItem "b" "rain"  rd))
(write-line "Display")
(print (display rd))
(print (insertItem "b" "roman" rd))
(print (insertItem "a" "a" rd))
(write-line "2nd Insertion")
(print (insertItem "c" "ap" rd))
(write-line "Display")
(print (display rd))
(write-line "Size")
(print (size rd))
(write-line "isEmpty Check")
(print (isEmpty rd))
(write-line "Search by key C")
(print (ﬁndItem "c"  rd))
(print (display rd))
(write-line "Size")
(print (size rd))
(write-line "Remove one element by key C")
(print (removeItem "c" rd))
(write-line "Size")
(print (size rd))

