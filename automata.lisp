
;;;;;;;;;;;;; Dumb array util functions

; fuck common lisp - copy an array
(defun copy-array (array)
;  (copy-seq array))
  (let ((dims (array-dimensions array)))
    (adjust-array
     (make-array dims :displaced-to array)
     dims)))

(defun rotate90 (arr w h)
  (let* ((o (make-array (list h w))))
    (loop 
       repeat h
       for y from 0
       do (loop 
             repeat w
               for x from 0
               do (setf (aref o y x) (aref arr x y))))
    o))

;; get all rotations of an array
(defun rotations (arr w h)
  (let* ((arr90 (rotate90 arr w h))
         (arr180 (rotate90 arr90 h w))
         (arr270 (rotate90 arr180 w h)))
    (list arr arr90 arr180 arr270)))

(defun reflect-horizontal (arr w h)
  (let ((o (make-array (list w h))))
    (loop 
         repeat h
         for y from 0
         do (loop
                 repeat w
                 for x from 0
                 do (setf (aref o x (- (- h 1) y)) (aref arr x y))))
    o))

(defun reflect-vertical (arr w h)
  (let ((o (make-array (list w h))))
    (loop 
         repeat h
         for y from 0
         do (loop
                 repeat w
                 for x from 0
                 do (setf (aref o (- (- w 1) x) y) (aref arr x y))))
    o))

;; get all reflections of an array
(defun reflections (arr w h)
  (let* ((hor (reflect-horizontal arr w h))
         (ver (reflect-vertical arr w h))
         (both (reflect-horizontal ver w h)))
    (list arr hor ver both)))

;; get all reflections and rotations of an array
(defun reflections-and-rotations (arr w h)
  (loop for rot in (rotations arr w h)
       append (reflections rot w h) into everything
       finally (return everything)))

(defun test-it-all ()
    (let ((arr (make-array '(3 3) :initial-contents '((1 2 3) (4 5 6) (7 8 9)))))
      (reflections-and-rotations arr 3 3)))

(defun list-to-array (l w h)
  (let ((arr (make-array (list w h))))
    (loop for elm in l
         for i from 0
         for v = (multiple-value-list  (truncate i w))
         for y = (car v)
         for x = (cadr v)
       do (setf (aref arr x y) elm))
    arr))

(defun remove-duplicate-arrays (l)
  (loop 
     for elm in l
     when (loop for x in out never (equalp elm x))
     collect elm into out
     finally (return out)))
; automata schema
; (automata 
;   (entities (
;           (<entity-full-name1> :asymbol <alt-symbol> :ascii <a-char-rep> :value <an-int>)
;           ...
;           (<entity-full-name2> ...))
;   (match pattern do)
;   (match pattern do)
;   ...
;   )
; 

; :* is a wildcard
; :* in a replace-with pattern is just not touched
(defun test-automata-1 ()
;; grow lines
  '(automata
    (entities
     ((NOTHING   :asymbol :_ :ascii '_' :value 0 )) ;asymbol means alt symbol
      (SOMETHING :asymbol :@ :ascii '@' :value 222))
    (match 
     (pattern (:_ :_ :_  :_ :@ :_  :_ :_ :_) 3 3)
     (replace-with
      (pattern (:_ :@ :_  :_ :@ :_  :_ :_ :_) 3 3)))
    (match 
     (pattern (:_ :_ :_  :_ :@ :_  :_ :@ :_) 3 3)
     (replace-with
      (pattern (:_ :@ :_  :_ :@ :_  :_ :@ :_) 3 3)))))

;; Generate code like this:
; if ((entity_at 0 0 y x) == NOTHING) {
;   if ((entity_at 0 1 y x) == NOTHING) {
;     if ((entity_at 0 2 y x) == NOTHING) {
;       if ((entity_at 1 0 y x) == NOTHING) {
;         if ((entity_at 1 1 y x) == SOMETHING) {
;           if ((entity_at 1 2 y x) == NOTHING) {
;             if ((entity_at 2 0 y x) == NOTHING) {
;               if ((entity_at 2 1 y x) == NOTHING) {
;                if ((entity_at 2 2 y x) ==  NOTHING) {
;                  replace_at 0 0 y x pattern_1 pattern_1_replace
;                }
;               } else if ((entity_at 2 1 y x) == SOMETHING) {
;                if ((entity_at 2 2 y x) ==  NOTHING) {
;                  replace_at 0 0 y x pattern_2 pattern_2_replace
;                }
;               }
;             }                                      
;           }                
;         }      
;       }
;     }
;   }
; }

(defun automata-eval (a)
  ; ensure that it is an automata
  ; parse entities
  ; - build a symbol table 
  ; - symbol table should use asymbol to ref itself as well
  ; parse matches
  ; parse patterns
  ; generate enum for entities
  ; generate patterns
  ; generate to-char function
  ; generate big select block
  ; - find same matches

  ; TODO
  ; - randomness: decisions should be randomly followed
  ; - ambiguity - multiple matches
  ; - precedence
  ; - layers
)
