
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
     ((NOTHING   :asymbol :_ :ascii "_" :value 0 )) ;asymbol means alt symbol
      (SOMETHING :asymbol :@ :ascii "@" :value 222))
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

;     ((NOTHING   :asymbol :_ :ascii '_' :value 0 )) ;asymbol means alt symbol
;      (SOMETHING :asymbol :@ :ascii '@' :value 222))
(defun flat-assoc (key l)
  (if l
      (if (equalp (car l) key)
          (cadr l)
          (flat-assoc key (cddr l)))
      nil))

(defun test-flat-assoc ()
  (let ((l '(:heh :heh1 :what :what1)))
    (list
     (equalp (flat-assoc :heh l) :heh1)
     (equalp (flat-assoc :what l) :what1)
     (equalp nil (flat-assoc :zuh l)))))

(defclass entity ()
  ((esymbol        :initarg :esymbol  :accessor esymbol)
   (asymbol       :initarg :asymbol :accessor asymbol)
   (ascii       :initarg :ascii :accessor ascii)
   (value         :initarg :value   :accessor value)))

(defclass pattern-match ()
  ((pattern-name :initarg :name :accessor pattern-name)
   (pattern      :initarg :pattern :accessor match-pattern)
   (action       :initarg :action  :accessor action)))

(defclass action () ())
(defclass replace-with (action) 
  ((match-pattern :initarg :match-pattern :accessor match-pattern)))

(defun parse-entity (ed)
  (let* ((name-sym (first ed))
         (rested (rest ed))
         (asymbol  (flat-assoc :asymbol rested))
         (ascii  (flat-assoc :ascii rested))
         (value    (flat-assoc :value rested)))
    (make-instance 'entity :esymbol name-sym :asymbol asymbol :ascii ascii :value value)))

(defun parse-pattern (p)
  (list-to-array (nth 1 p) (nth 2 p) (nth 3 p)))

(defgeneric parse-args (action))


; this is unsafe!
(defun parse-action (a)
  (let ((action (make-instance (first a))))
    (progn 
      (parse-args action (rest a))
      action)))


(defun parse-match (m)
  (let* ((match-sym (first m))
         (match-pattern (parse-pattern (assoc 'pattern m)))
         (name (or (assoc 'name m) (string (gensym))))
         (action (parse-action (first (last m)))))
    (if (not (equalp 'match match-sym)) (error "not a match!"))
    (make-instance 'pattern-match 
                   :name name
                   :pattern match-pattern
                   :action action)))

           
(defclass symbol-table ()
  ((table :initarg :table :accessor table)))

(defmethod resolve-symbol ((symt symbol-table) sym)
  (cadr (assoc sym (table symt))))

(defun build-symbol-table (entities) ; could use a better data structure
  (make-instance 'symbol-table :table
                 (append
                  (mapcar (lambda (e) (list (esymbol e) (esymbol e))) entities)
                  (mapcar (lambda (e) (list (asymbol e) (esymbol e))) entities))))

(defun s+ (l)
  (format nil "~{ ~A~}" l))


(defun automata-eval (a)
  ; ensure that it is an automata
  ; parse entities
  ; - build a symbol table 
  ; - symbol table should use asymbol to ref itself as well

  (labels ((get-entity-defs (l) (cdr (assoc 'entities l)))
           (get-match-defs (l) (find-if 
                             (lambda (x) (case (car x)
                                           (match T)
                                           (otherwise NIL))) l))
           (generate-enum (ents)
             (format nil "typdef enum ENTITY { ~{ ~A~^,~} } Entity;"
                     (mapcar 
                      (lambda (e) (s+ 
                                   (list (string (esymbol e)) 
                                         "=" 
                                         (format nil "~D" (value e)))))
                      ents)))
           (generate-palette (ents)
             (format nil "Entity types[] = { ~{ ~A~^,~} };"
                     (mapcar (lambda (e) (string (esymbol e))) ents)))
           (generate-to-char-function (ents)
             (format nil "char entity_to_char( Entity e ) { ~%  select ( e ) { ~{ ~A~} default: return '?'; } }~%"
                     (mapcar (lambda (e) 
                               (format nil "case ~A: return '~A';~%" (string (esymbol e)) (ascii e))) ents))) 

           )
      (if (not (eq 'AUTOMATA (car a))) (error "not an automata!"))
      (let* ((l (rest a))
             (entity-defs (get-entity-defs l))
             (match-defs (get-match-defs l))
             (entities (mapcar #'parse-entity entity-defs))
             (symbol-table (build-symbol-table entities))
             (matches (mapcar #'parse-match match-defs))
             (enum (generate-enum entities)) ; string
             (palette (generate-palette entities)) ; string
             (to-char-fun (generate-to-char-function entities)) ; string
             )
        (s+ (list enum palette to-char-fun)))))
  ; X parse matches
  ; X parse patterns
  ; X generate enum for entities
  ; X generate patterns
  ; X generate to-char function
  ; generate big select block
  ; - find same matches

  ; TODO
  ; - randomness: decisions should be randomly followed
  ; - ambiguity - multiple matches
  ; - precedence
  ; - layers
  ; - fairness in pattern matching :(

(defun test-automata-eval ()
  (automata-eval (test-automata-1)))
