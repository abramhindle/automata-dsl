
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
     (NOTHING   :asymbol :_ :ascii "_" :value 0 ) ;asymbol means alt symbol
     (SOMETHING :asymbol :@ :ascii "@" :value 222))
    (match 
     (pattern (:_ :_ :_  :_ :@ :_  :_ :_ :_) 3 3)
     (replace-with
      (pattern (:_ :@ :_  :_ :@ :_  :_ :_ :_) 3 3)))
    (match 
     (pattern (:_ :_ :_  :_ :@ :_  :_ :@ :_) 3 3)
     (replace-with
      (pattern (:_ :@ :_  :_ :@ :_  :_ :@ :_) 3 3)))))

(defun test-automata-2 ()
;; grow lines
  '(automata
    (entities
     (NOTHING   :asymbol :_ :ascii "_" :value 0 ) ;asymbol means alt symbol
     (SOMETHING :asymbol :@ :ascii "@" :value 222))
    (match 
     (pattern (:_ :_ :_  :_ :@ :_  :_ :_ :_) 3 3)
     (replace-with
      (pattern (:_ :@ :_  :_ :@ :_  :_ :_ :_) 3 3)))
    (match 
     (pattern (:_ :_ :_  :_ :@ :_  :_ :@ :_) 3 3)
     (replace-with
      (pattern (:_ :@ :_  :_ :@ :_  :_ :@ :_) 3 3)))
    (match 
     (pattern (:_ :@ :_  :_ :@ :_  :_ :_ :_) 3 3)
     (replace-with
      (pattern (:_ :@ :_  :_ :@ :_  :@ :_ :@) 3 3)))
    (match 
     (pattern (:@ :_ :@  :_ :_ :_  :_ :_ :_) 3 3)
     (replace-with
      (pattern (:@ :_ :@  :_ :_ :_  :@ :_ :@) 3 3)))))

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
;;;;;;;;;;;;;;; CLASSES ;;;;;;;;;;;;;;;;;
(defclass entity ()
  ((esymbol        :initarg :esymbol  :accessor esymbol)
   (asymbol       :initarg :asymbol :accessor asymbol)
   (ascii       :initarg :ascii :accessor ascii)
   (value         :initarg :value   :accessor value)))

(defgeneric pattern-name (pattern))
(defclass pattern-match ()
  ((pattern-name :initarg :name :accessor pattern-name)
   (pattern      :initarg :pattern :accessor match-pattern)
   (action       :initarg :action  :accessor action)))

(defclass pattern ()
  ((pattern-name :initarg :name :accessor pattern-name)
   (pattern      :initarg :pattern :accessor pattern-arr)))

(defclass action () ())
(defclass replace-with (action) 
  ((match-pattern :initarg :match-pattern :accessor match-pattern)))


;;;;;;;;;;;; CLASSES END ;;;;;;;;;;;;


(defun rand-name () (symbol-name (gensym)))

(defun make-pattern (p)
  (make-instance 'pattern :name (rand-name) :pattern p))

(defun make-pattern-of-list (l w h)
  (make-pattern (list-to-array l w h)))


(defun parse-entity (ed)
  (let* ((name-sym (first ed))
         (rested (rest ed))
         (asymbol  (flat-assoc :asymbol rested))
         (ascii  (flat-assoc :ascii rested))
         (value    (flat-assoc :value rested)))
    (make-instance 'entity :esymbol name-sym :asymbol asymbol :ascii ascii :value value)))

(defun parse-pattern (p)
  (make-pattern
   (list-to-array (nth 1 p) (nth 2 p) (nth 3 p))))

(defgeneric replace-pattern (p))
(defmethod replace-pattern (pattern-match)
  (match-pattern (action pattern-match)))

;; this seems like bad news
(defgeneric patterns-of-match (pattern-match))
(defmethod  patterns-of-match (pattern-match)
  ; Not clear: get the action  attribute, then get the match-pattern attribute from action
  ; then make a  list
  (list 
   (match-pattern pattern-match)
   (replace-pattern pattern-match)))
  

(defgeneric parse-args (action args))
(defmethod parse-args ((action replace-with) args) 
           (setf (match-pattern action) (parse-pattern (first args)))
           action)
; this is unsafe!
(defun parse-action (a)
  (let ((action (make-instance (first a))))
    (parse-args action (rest a))
    action))


(defun parse-match (m)
  (let* ((match-sym (first m))
         (mr (rest m))
         (match-pattern (parse-pattern (assoc 'pattern mr)))
         (name (or (assoc 'name mr) (symbol-name (gensym))))
         (action (parse-action (first (last mr)))))
    (if (not (equalp 'match match-sym)) (error "not a match!"))
    (make-instance 'pattern-match 
                   :name name
                   :pattern match-pattern
                   :action action)))

(defclass symbol-table ()
  ((table :initarg :table :accessor table)))

(defgeneric resolve-symbol(symbol-table sym))
(defmethod resolve-symbol ((symt symbol-table) sym)
  (cadr (assoc sym (table symt))))



(defun build-symbol-table (entities) ; could use a better data structure
  (make-instance 'symbol-table :table
                 (append
                  (mapcar (lambda (e) (list (esymbol e)  e)) entities)
                  (mapcar (lambda (e) (list (asymbol e)  e)) entities))))

; string concat by space
(defun s+ (l)
  (format nil "~{~A~^ ~}" l))
(defun s% (l)
  (format nil "~{~A~^~%~}" l))

(defun flatten-2d-array (arr w h) 
  (let ((n (* w h)))
    (loop repeat n
       for i from 0
       for v = (multiple-value-list  (truncate i w))
       for y = (first v)
       for x = (second v)
       collect (aref arr x y) into out
         finally (return out))))

     
(defun c-define-pattern (symbol-table pattern)
  (let* ((name (pattern-name pattern))
         (arr  (pattern-arr pattern))
         (dims (array-dimensions arr))
         (width (first dims))
         (height (second dims))
         (arr-list (flatten-2d-array arr width height))
         (names (mapcar (lambda (s) (symbol-name (esymbol (resolve-symbol symbol-table s)))) arr-list)))

    (concatenate 'string
                 (format nil "~%#define ~A_width ~D~%#define ~A_height ~D~%" name width name height)
                 (format nil "#define ~A_len ~D~%" name (* width height))
                 (format nil "Entity ~A[] = { ~{ ~A~^,~} };~%" name names))))
    
(defgeneric width (x))
(defgeneric height (x))
(defmethod width ((m pattern))
 (first (array-dimensions (pattern-arr m))))
(defmethod height ((m pattern))
 (second (array-dimensions (pattern-arr m))))
(defmethod width ((m pattern-match))
  (width (match-pattern m)))
(defmethod height ((m pattern-match))
  (height (match-pattern m)))
(defun safe-get-xy (arr x y)
  (let* ((d  (array-dimensions arr))
         (w (first d))
         (h (second d)))
    (if (and (>= x 0) (< x w) (>= y 0) (< y h))
        (aref arr x y)
        nil)))

(defgeneric get-xy (p x y))
(defmethod get-xy ((m pattern) x y)
  (safe-get-xy (pattern-arr m) x y))
(defmethod get-xy ((m pattern-match) x y)
  (get-xy (match-pattern m) x y))

(defun next-xy (x y w h)
    (let* ((nx (+ 1 x))
           (ny (+ 1 y)))
      (if (>= nx w)
          (if (>= ny h) nil (list 0 ny))
          (list nx y))))


;; assume everything is the same size
;; (defun generate-logic-block  (symbol-table entities matches)
;;   (let ((mwidth (loop for x in matches maximize (width x)))
;;         (mheight (loop for x in matches maximize (height x))))
;;     ; (cond (0 0) (NOTHING (cond (0 1) (NOTHING) (SOMETHING))) (SOMETHING (cond (0 1) (NOTHING) (SOMETHING))))
;;     (labels ((helper (xdepth ydepth subset) 
;;                (let* ((nset (remove-if-not (lambda (z) () (get-xy z xdepth ydepth)) subset)) ; remove those matches which are not defined here
;;                       (sentities (mapcar (lambda (z) (get-xy z xdepth ydepth)) nset)) ; symbols
;;                       (suniq (remove-duplicates sentities)) ; remove duplicates
;;                       (xy (next-xy xdepth ydepth mwidth wheight))
;;                       )                 
;;                  (if (not xy)
;;                      (loop 
;;                         for sym in suniq
;;                         (
;;                      (list :replace (mapcar #'mk-replace suniq))
;;                      (list :replace ); we're done!
;;                      ; we're not done :(
;;                      )
;;                  (list xdepth ydepth )
;;                  "if ((ENTITY_AT(~D,~D, y, x)) == ~A)"
;;                  )
;;                
;;                ))
;;       )
;;     )
 ;; )

 (defun tuple (x y) (list x y))
 (defun tfst (x) (car x))
 (defun tsnd (x) (cadr x))
 (defun tlsnd (l) (mapcar #'tsnd l))
;;     ; (cond (0 0) (NOTHING (cond (0 1) (NOTHING) (SOMETHING))) (SOMETHING (cond (0 1) (NOTHING) (SOMETHING))))
;;;; XXX TEST THIS IN THE MORNING
(defun mk-logic-tree (symbol-table entities matches)
  (let ((mwidth (loop for x in matches maximize (width x)))
        (mheight (loop for x in matches maximize (height x))))
    (labels ((get-entries-at (x y l)
               (loop for z in l
                  for c = (get-xy z x y)
                  when c collect (tuple c z)))
             (get-symbols-of-tuples (tuples)
               (remove-duplicates (mapcar #'tfst tuples)))
             (grep-set (sym sset)
               (remove-if-not (lambda (x) (equalp (tfst x) sym)) sset))
             (partition-set (syms sset)
               (loop
                  for sym in syms
                  collect (tuple sym (grep-set sym sset))))
             (per-partition (x y part)
               (let* ((sym (tfst part))
                      (sset (tsnd part))
                      (xy (next-xy x y mwidth mheight))
                      (tl (tlsnd sset)))
                 (if xy                        
                     (list sym (tree-helper (first xy) (second xy) tl))
                     (list sym (append (list 'matches) (mapcar (lambda (x) (pattern-name (match-pattern x))) tl))))))
             (tree-helper (x y sset) ; set is of matches
               (if (and sset (< x mwidth) (< y mheight))
                   (let* ((nset (get-entries-at x y sset))
                          (syms (get-symbols-of-tuples nset))
                          (parts (partition-set syms nset)))
                     (append (list 'case (list x y))
                             (mapcar (lambda (p) (per-partition x y p)) parts)))
                   '())))
      (tree-helper 0 0 matches))))

(defun logic-tree-to-C (symt entities matches tree)
  (labels ((indentation (depth) (make-string (* 2 depth) :initial-element #\  ))
           (helper (depth leaf)
             (case (first leaf)
               ('case (let ((dxdy (second leaf))
                           (cases (cddr leaf))
                           (in (indentation depth)))
                       (concatenate 'string
                                    (format nil "~Aent = ENTITY_AT(~D,~D, x, y) ;~%" in (first dxdy) (second dxdy))
                                    (format nil "~{~%~A~}"  ; join w/ newline
                                            (mapcar 
                                             (lambda (c) 
                                               (let* ((sym (first c))
                                                      (ssym (symbol-name (esymbol (resolve-symbol symt sym))))
                                                      (exp (second c)))
                                                 (format nil "~Aif ( ent == ~A) {~%~A~%~A} else~%" 
                                                         in 
                                                         ssym 
                                                         (helper (+ 1 depth) exp) 

                                                         in))) 
                                             cases))
                                    (format nil "~%~A{ return;~%~A}~%" in in))))
               ('matches (let (;(matches-sym (first leaf))
                              (matching-pattern-names (rest leaf))
                              (in (indentation depth)))
                           (s%
                            (mapcar (lambda (pattern)
                                        ; might have to put an index here!
                                      (format nil "~APATTERN_MATCH_AT( ~A_INDEX , x, y );~%" in pattern))
                                    matching-pattern-names))))
               (otherwise (error (s+ "What was this" (string leaf)))))))
    (helper 0 tree)))
                                    
  
  

(defun mk-logic-tree-test ()
  (let* ((e1 (parse-entity '(NOTHING   :asymbol :_ :ascii "_" :value 0 )))
         (e2 (parse-entity '(SOMETHING :asymbol :@ :ascii "@" :value 222)))
         (entities (list e1 e2))
         (symt (build-symbol-table entities))
         (m1 (parse-match 
             '(match 
               (pattern (:_ :_ :_  :_ :@ :_  :_ :_ :_) 3 3)
               (replace-with
                (pattern (:_ :@ :_  :_ :@ :_  :_ :_ :_) 3 3))))
           )
         (m2 (parse-match 
             '(match 
               (pattern (:@ :@ :@  :@ :@ :@  :@ :@ :@) 3 3)
               (replace-with
                (pattern (:_ :_ :_  :_ :@ :_  :_ :_ :_) 3 3))))
           )
         (m3 (parse-match 
             '(match 
               (pattern (:@ :@ :@  :@ :@ :@  :@ :@ :_) 3 3)
               (replace-with
                (pattern (:_ :_ :_  :_ :@ :_  :_ :_ :_) 3 3))))
           )
         (n1 (parse-match 
             '(match 
               (pattern (:_ :_  :_ :@) 2 2)
               (replace-with
                (pattern (:_ :@ :_  :@) 2 2))))
           )
         (n2 (parse-match 
             '(match 
               (pattern (:_ :_  :@ :@) 2 2)
               (replace-with
                (pattern (:_ :@ :_  :@) 2 2))))
           )

         (n3 (parse-match 
             '(match 
               (pattern (:@ :@ :@ :@) 2 2)
               (replace-with
                (pattern (:_ :_ :_  :@ ) 2 2))))
           )
         (n4 (parse-match 
             '(match 
               (pattern (:@ :@ :@ :_) 2 2)
               (replace-with
                (pattern (:_ :_ :_ :@) 3 3))))
           )
         (matches (list m1 m2 m3))
         (matches2 (list n1 n2 n3 n4)))
    (let ((tree  (mk-logic-tree symt entities matches2)))
      (list tree 
            (logic-tree-to-C symt entities matches2 tree)))))

    

(defun make-logic-function (logic-str)
  (format nil "void automata_matcher( Entity * entities, int * matches, int x, int y) {~%  Entity ent;~%~A~%}~%" logic-str))
    

; basically we just copy the classes
(defgeneric prototype-action-replace (action r))
(defmethod prototype-action-replace ((action replace-with) r)
  (make-instance 'replace-with :match-pattern r))
;basically we just copy the classes @_@
(defgeneric prototype-pattern-replace (match p r))
(defmethod prototype-pattern-replace ((match pattern-match) p r)
  (make-instance 'pattern-match
                 :name (format nil "~A~A" (pattern-name match) (rand-name))
                 :pattern p
                 :action (prototype-action-replace (action match) r)))

(defun rotate-and-reflect-match (match)
  (let* ((pattern (match-pattern match))
         (rpattern (replace-pattern match))
         (pparr (pattern-arr pattern))
         (rparr (pattern-arr rpattern))
         (pprr (reflections-and-rotations pparr (width pattern) (height pattern)))
         (rprr (reflections-and-rotations rparr (width rpattern) (height rpattern))))
    (loop 
       for p in pprr
       for r in rprr
       collect (prototype-pattern-replace match (make-pattern p) (make-pattern r)))))

(defun rotate-and-reflect-matches (matches)
  (loop for m in matches
     append (rotate-and-reflect-match m) into all-matches
     finally (return all-matches)))


(defun automata-eval (a)
  ; ensure that it is an automata
  ; parse entities
  ; - build a symbol table 
  ; - symbol table should use asymbol to ref itself as well

  (labels ((get-entity-defs (l) (cdr (assoc 'entities l)))
           (get-match-defs (l) (remove-if-not
                             (lambda (x) (case (car x)
                                           (match T)
                                           (otherwise NIL))) l))
           (generate-enum (ents)
             (format nil "typedef enum ENTITY { ~{ ~A~^,~} } Entity;"
                     (mapcar 
                      (lambda (e) (s+ 
                                   (list (symbol-name (esymbol e)) 
                                         "=" 
                                         (format nil "~D" (value e)))))
                      ents)))
           (generate-palette (ents)
             (format nil "Entity types[] = { ~{ ~A~^,~} };"
                     (mapcar (lambda (e) (symbol-name (esymbol e))) ents)))
           (generate-to-char-function (ents)
             (format nil "char entity_to_char( Entity e ) { ~%switch ( e ) {~%~{    ~A~^~%~} default: return '?'; } }~%"
                     (mapcar (lambda (e) 
                               (format nil "case ~A: return '~A';~%" (symbol-name (esymbol e)) (ascii e))) ents)))
           (get-patterns-of-matches (matches)
             (loop
                for m in matches
                append (patterns-of-match m) into patterns
                finally (return patterns)))          
           ; returns a string list
           (generate-replacement-patterns (symbol-table matches)
             (print "generate-replacement-patterns")
             (let ((patterns (get-patterns-of-matches matches)))
               (print "got patterns")
               (mapcar (lambda (x) (c-define-pattern symbol-table x)) patterns)))

           (generate-pattern-match-arr (matches)
             (let ((l (mapcar (lambda (m) (pattern-name (match-pattern m))) matches)))
               (concatenate 'string
                            (format nil "~{~A~}"
                                    (loop
                                       for m in l
                                       for y from 0
                                       collect (format nil "#define ~A_INDEX ~D~%" m y)))

                            (format nil "~%")
                            (format nil "#define match_patterns_len ~D~%Entity * match_patterns[] = { ~{ ~A~^,~} };~%" (length l) l))))
           (generate-replace-match-arr (matches)
             (let ((l (mapcar (lambda (m) (pattern-name (replace-pattern m))) matches)))
               (format nil "#define match_patterns_len ~D~%Entity * replace_patterns[] = { ~{ ~A~^,~} };~%" (length l) l)))

           (generate-n-types (entities)
             (format nil "#define NTYPES ~D~%" (length entities)))



           )
      (if (not (eq 'AUTOMATA (car a))) (error "not an automata!"))
      (let* ((l (rest a))

             (incstr (format nil "#include \"auto-test-h-include.c\"~%"))

             (entity-defs (get-entity-defs l))
             (match-defs (get-match-defs l))
             (entities (mapcar #'parse-entity entity-defs))
             (symbol-table (build-symbol-table entities))
             (matches-alpha (mapcar #'parse-match match-defs))
             (matches (rotate-and-reflect-matches matches-alpha))
             ; TODO rotate matches etc.
             (def-ntypes (generate-n-types entities)) ; string
             (enum (generate-enum entities)) ; string
             (palette (generate-palette entities)) ; string
             (to-char-fun (generate-to-char-function entities)) ; string
             (replacements (generate-replacement-patterns symbol-table matches)) ; string list
             (pattern-match-arr (generate-pattern-match-arr matches))
             (replace-match-arr (generate-replace-match-arr matches))
             (logic (mk-logic-tree symbol-table entities matches))
             (logic-str (logic-tree-to-C symbol-table entities matches logic))
             (logic-function (make-logic-function logic-str))
             ;  (logic-block (generate-logic-block symbol-table entities matches))
             )
        (with-open-file (f "auto-test.h" :direction :output)
          (format f "~A"
           (s% (append (list def-ntypes enum incstr  palette to-char-fun) replacements (list pattern-match-arr replace-match-arr logic-function)))
           )))))
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
