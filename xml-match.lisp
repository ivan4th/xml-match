;;; -*- show-trailing-whitespace: t; indent-tabs-mode: nil -*-

;;; Copyright (c) 2010 Ivan Shvedunov. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :xml-match)

(defparameter *store-source* nil)
(defparameter *trace-match* nil)
(defparameter *trace-indent* 2)
(defvar *pattern-aliases* '(()))
(defvar *trace-level*)
(defvar *current-attribute-owner*)
(defvar *nested-start*)
(defvar *rep-saved-bindings*)
(defvar *rep-accumulated-bindings*)
(defvar *deep-end*)

(defun xml-pattern-debug (debug-p)
  (setf debug-p (and debug-p t)
        *store-source* debug-p
        *trace-match* debug-p))

;;; parsing

(defmacro define-xml-pattern-alias (name pattern)
  (with-gensyms (prev)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,(once-only (name pattern)
          `(progn
             (if-let ((,prev (assoc ,name (first *pattern-aliases*))))
               (setf (cdr ,prev) ,pattern)
               (setf (first *pattern-aliases*)
                     (acons ,name ,pattern (first *pattern-aliases*))))
             ,name)))))

(defun qname->local-name (qname)
  (if-let ((p (position #\: qname)))
    (subseq qname (1+ p))
    qname))

(defun preprocess-element (el &optional
                           (keyword-converter (make-keyword-converter "")))
  (if (and (proper-list-p el) (keywordp (first el)))
      (destructuring-bind (namespace-uri . qname)
          (funcall keyword-converter (first el) nil)
        (iter (with rest = (rest el))
              (for l on (rest el) by #'cddr)
              (for attr-name = (and (rest l) (keywordp (first l))
                                     (funcall keyword-converter (first l) t)))
              (while attr-name)
              (collect (cons attr-name (second l)) into attributes)
              (setf rest (cddr l))
              (finally (return (values namespace-uri qname attributes rest)))))
      (values nil nil nil)))

(defun trim (str)
  (string-trim '(#\space #\tab #\newline #\return) (or str "")))

(defun make-keyword-converter (&optional (target-prefix "") (target-namespace "")
                               (attr-prefix "") (attr-namespace ""))
  #'(lambda (kw attribute-p)
      (assert (keywordp kw) ()
              "cannot translate keyword (attribute-p: ~s): ~s"
              attribute-p kw)
      (values
        (cons (if attribute-p attr-namespace target-namespace)
              (concatenate 'string
                           (if attribute-p attr-prefix target-prefix)
                           (princ-to-string ;; SIMPLE-BASE-STRING may cause some problems
                            (string-downcase kw)))))))

(defun next-sibling (child)
  (when (and (not (typep child 'stp:attribute)) (stp:parent child))
    (let ((p (stp:child-position child (stp:parent child))))
      (when (< p (1- (stp:number-of-children (stp:parent child))))
        (stp:next-sibling child)))))

(defun element-p (node)
  (typep node 'stp:element))

(defclass pattern ()
  ((source :accessor source-of :initarg :source :initform nil)))

(defun variable-p (x)
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun valid-attribute-pattern-p (pattern)
  (if (typep pattern '(cons (eql :+opt) (cons t null)))
      (valid-attribute-pattern-p (second pattern))
      (or (stringp pattern)
          (variable-p pattern)
          (and (consp pattern)
               (eq ':+rx (first pattern))))))

(defun aliased-pattern (name)
  (iter (for frame in *pattern-aliases*)
        (thereis (cdr (assoc name frame)))))

;; TBD: xml pattern-specific conditions
(defun %parse-xml-pattern (pattern &key (keyword-converter (make-keyword-converter "")))
  (flet ((parse-sub (subpattern)
           (parse-xml-pattern subpattern :keyword-converter keyword-converter)))
    (cond ((null pattern) (make-instance 'end-anchor))
          ((variable-p pattern)
           (make-instance 'bind
                          :content-pattern (make-instance 'everything)
                          :variable pattern))
          ((and (symbolp pattern) (aliased-pattern pattern))
           (parse-sub (aliased-pattern pattern)))
          ((eq pattern '*) (make-instance 'everything))
          ((stringp pattern) (make-instance 'text :text pattern))
          ((and (proper-list-p pattern)
                (symbolp (first pattern))
                (not (eq (first pattern) '*))
                (not (variable-p (first pattern)))
                (not (aliased-pattern (first pattern))))
           (case (first pattern)
             (:+alias
                (unless (and (proper-list-p (second pattern))
                             (every (rcurry #'typep '(cons symbol (cons t t)))
                                    (second pattern)))
                  (error "invalid :+alias bindings: ~s" (second pattern)))
                (let ((*pattern-aliases*
                       (cons (second pattern)
                             *pattern-aliases*)))
                  (parse-sub (cddr pattern))))
             (:+bind
                (unless (variable-p (lastcar pattern))
                  (error "invalid pattern: ~s" pattern))
                (make-instance 'bind
                               :content-pattern (parse-sub (rest (butlast pattern)))
                               :variable (lastcar pattern)))
             (:+deep
                (make-instance 'deep
                               :content-pattern (parse-sub (rest pattern))))
             (:+opt
                (make-instance 'opt
                               :content-pattern (parse-sub (rest pattern))))
             (:+or
                (make-instance 'alternation
                               :patterns (mapcar #'parse-sub (rest pattern))))
             (:*
                (make-instance 'greedy-repetition
                               :content-pattern (parse-sub (rest pattern))))
             (:+
                (make-instance 'greedy-repetition-no-zero
                               :content-pattern (parse-sub (rest pattern))))
             ((:+rx :+erx)
                (unless (and (stringp (second pattern))
                             (every #'variable-p (cddr pattern)))
                  (error "invalid pattern: ~s" pattern))
                (make-instance (if (eq (first pattern) :+rx) 'regex 'eregex)
                               :regex (second pattern)
                               :variables (cddr pattern)))
             (:+ns
                (unless (and (proper-list-p pattern)
                             (stringp (second pattern)))
                  (error "invalid pattern: ~s" pattern))
                (make-instance 'element-pattern
                               :local-name nil
                               :namespace-uri (second pattern)
                               :attribute-patterns '()
                               :content-pattern (parse-sub (cddr pattern))))
             (t
                (multiple-value-bind (namespace-uri qname attributes rest)
                    (preprocess-element pattern keyword-converter)
                  (make-instance 'element-pattern
                                 :local-name (qname->local-name qname)
                                 :namespace-uri namespace-uri
                                 :attribute-patterns
                                 (iter (for ((attr-namespace-uri . attr-qname) . value) in attributes)
                                       (for opt-p = (and (consp value) (eq (first value) :+opt)))
                                       (assert (valid-attribute-pattern-p value)
                                               () "invalid attribute pattern: ~s" value)
                                       (collect (make-instance
                                                 'attribute
                                                 :source (when *store-source*
                                                           (list (list :attribute attr-namespace-uri
                                                                       (qname->local-name attr-qname) value)))
                                                 :local-name (qname->local-name attr-qname)
                                                 :namespace-uri attr-namespace-uri
                                                 :value-pattern (parse-sub (if opt-p (second value) value))
                                                 :optional-p opt-p)))
                                 :content-pattern (parse-sub rest))))))
          ((proper-list-p pattern)
           (if (null (rest pattern))
               (parse-sub (first pattern))
               (make-instance 'seq :patterns (mapcar #'parse-sub pattern))))
          (t
           (error "invalid pattern: ~s" pattern)))))

(defun parse-xml-pattern (pattern &rest pattern-options)
  (if (typep pattern 'pattern)
      pattern
      (let ((parsed (apply #'%parse-xml-pattern pattern pattern-options)))
        (when *store-source*
          (setf (source-of parsed) (list pattern)))
        parsed)))

;;; compilation

(defmacro matcher-closure (pattern name args &body body)
  `(wrap-matcher-closure ,pattern ',name (named-lambda ,name ,args ,@body)))

(defun wrap-matcher-closure (pattern name thunk)
  (if (not *trace-match*)
      thunk
      (let ((src (if (source-of pattern)
                     (first (source-of pattern))
                     (type-of pattern))))
        (named-lambda trace-closure (node bindings)
          (let ((indent (* *trace-level* *trace-indent*)))
            (format *debug-io* "~&~v@TENTER: ~s ~s NODE ~s~%~v@TBINDINGS ~s~%"
                    indent name src node indent bindings)
            (let ((result (let ((*trace-level* (1+ *trace-level*)))
                            (funcall thunk node bindings))))
              (format *debug-io* "~&~v@TLEAVE: ~s ~s~%~v@TRESULT ~s~%"
                      indent name src indent result)
              result))))))

(defun get-binding (var bindings)
  (assoc var bindings))

(defun binding-var (binding)
  (car binding))

(defun binding-val (binding)
  (cdr binding))

(defun make-binding (var val)
  (cons var val))

(defun xmlp-lookup (var bindings)
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  (cons (make-binding var val) bindings))

(defun collect-added-bindings (new-bindings old-bindings collected-bindings)
  (let ((added-bindings (ldiff new-bindings old-bindings)))
    (iter (for (var . val) in added-bindings)
          (if-let ((binding (get-binding var collected-bindings)))
            (appendf (cdr binding) (list val))
            (setf collected-bindings
                  (extend-bindings var (list val) collected-bindings))))
    collected-bindings))

(defun binding-value->string (value)
  (etypecase value
    (string value)
    (stp:comment "")
    (stp:node (stp:string-value value))
    (proper-list (format nil "~{~a~}" (mapcar #'binding-value->string value)))))

(defun binding-values-match-p (v1 v2)
  (flet ((ensure-lol (v)
           (if (and (consp v) (consp (car v)))
               v
               (list v))))
    (iter (for a in (ensure-lol v1))
          (thereis
           (iter (for b in (ensure-lol v2))
                 (thereis
                  (string-equal (trim (binding-value->string a))
                                (trim (binding-value->string b)))))))))
(defun text-segments (node)
  (let (positions next-nodes)
    (values
      (with-output-to-string (out)
        (iter (with p = 0)
              (for cur-node initially node then (next-sibling cur-node))
              (while cur-node)
              (unless (typep cur-node 'stp:comment)
                (let ((str (stp:string-value cur-node)))
                  (write-string str out)
                  (incf p (length str))
                  (collect p into c-positions)
                  (collect (next-sibling cur-node) into c-next-nodes)))
              (finally (setf positions c-positions)
                       (setf next-nodes c-next-nodes))))
      (reverse positions)
      (reverse next-nodes))))

(defun skip-whitespace-nodes (node)
  (iter (while (and node (typep node 'stp:text)
                    (string= "" (trim (stp:string-value node)))))
        (setf node (next-sibling node))
        (finally (return node))))

(define-constant xmlp-empty-bindings '((t . t)) :test #'equal)

(defgeneric xml-pattern-variables (pattern))

(defgeneric create-matcher (pattern next-fn))

(defmethod create-matcher :before (pattern next-fn)
  (assert next-fn () "error: null next-fn"))

(defun chain-matchers (patterns next-fn)
  (if (null patterns)
      next-fn
      (iter (for pattern in (reverse patterns))
            (for cur initially next-fn then next)
            (for next = (create-matcher pattern cur))
            (finally (return next)))))

(defmethod xml-pattern-variables ((pattern pattern)) '())

(defclass compound-pattern (pattern)
  ((content-pattern :accessor content-pattern-of :initarg :content-pattern)))

(defmethod xml-pattern-variables ((pattern compound-pattern))
  (xml-pattern-variables (content-pattern-of pattern)))

(defclass element-pattern (compound-pattern)
  ((local-name         :accessor local-name-of         :initarg :local-name)
   (namespace-uri      :accessor namespace-uri-of      :initarg :namespace-uri)
   (attribute-patterns :accessor attribute-patterns-of :initarg :attribute-patterns)
   (content-pattern    :accessor content-pattern-of    :initarg :content-pattern)))

(defmethod xml-pattern-variables ((pattern element-pattern))
  (union (call-next-method)
         (when (attribute-patterns-of pattern)
           (reduce #'union
                   (mapcar #'xml-pattern-variables (attribute-patterns-of pattern))))))

;; NOTE!!! namespace-uri is never NIL. It may be an empty string though
(defmethod create-matcher ((pattern element-pattern) next-fn)
  (let* ((local-name (local-name-of pattern))
         (namespace-uri (namespace-uri-of pattern))
         (content-matcher (create-matcher
                           (content-pattern-of pattern)
                           (matcher-closure pattern element-content-end (node bindings)
                             (unless (skip-whitespace-nodes node) bindings))))
         (match-attributes-and-content
          (chain-matchers (attribute-patterns-of pattern)
                          (matcher-closure pattern element-inner (node bindings)
                            (when-let ((new-bindings
                                        (funcall content-matcher
                                                 (stp:first-child node)
                                                 bindings)))
                              (funcall next-fn (next-sibling node) new-bindings))))))
    (matcher-closure pattern element-outer (node bindings)
      (flet ((match-it (node)
               (and node (element-p node)
                    (or (null local-name) (string= (stp:local-name node) local-name))
                    (string= (stp:namespace-uri node) namespace-uri)
                    (funcall match-attributes-and-content node bindings))))
        (typecase node
          (stp:element (match-it node))
          (stp:document (match-it (stp:document-element node)))
          (stp:text (when (string= "" (trim (stp:string-value node)))
                      (match-it (next-sibling node)))))))))

(defclass attribute (pattern)
  ((local-name :accessor local-name-of :initarg :local-name)
   (namespace-uri :accessor namespace-uri-of :initarg :namespace-uri)
   (value-pattern :accessor value-pattern-of :initarg :value-pattern)
   (optional-p :accessor optional-p :initarg :optional-p)))

(defmethod xml-pattern-variables ((pattern attribute))
  (xml-pattern-variables (value-pattern-of pattern)))

(defmethod create-matcher ((pattern attribute) next-fn)
  (let ((local-name (local-name-of pattern))
        (namespace-uri (namespace-uri-of pattern))
        (optional-p (optional-p pattern))
        (value-matcher (create-matcher
                        (value-pattern-of pattern)
                        (matcher-closure pattern attribute-inner (node bindings)
                          ;; non-null node may mean * or var that refused to match
                          (unless node
                            (funcall next-fn *current-attribute-owner* bindings))))))
    (matcher-closure pattern attribute-outer (node bindings)
      (and node (element-p node)
           (let ((attr-node (stp:find-attribute-named node local-name namespace-uri)))
             (cond (attr-node
                    (let ((*current-attribute-owner* node))
                      (funcall value-matcher attr-node bindings)))
                   (optional-p
                    (funcall next-fn node bindings))))))))

(defclass everything (pattern) ())

(defmethod create-matcher ((pattern everything) next-fn)
  (matcher-closure pattern everything (node bindings)
    (if-let ((parent (and node (next-sibling node) (stp:parent node))))
      (iter (for from-node initially nil then sibling)
            (for sibling initially (stp:last-child parent)
                 then (stp:previous-sibling sibling))
            (thereis (funcall next-fn from-node bindings))
            (until (eq sibling node)))
      (or (funcall next-fn nil bindings)
          (funcall next-fn node bindings)))))

(defclass bind (compound-pattern)
  ((variable :accessor variable-of :initarg :variable)))

(defmethod xml-pattern-variables ((pattern bind))
  (union (call-next-method)
         (list (variable-of pattern))))

(defmethod create-matcher ((pattern bind) next-fn)
  (let* ((var (variable-of pattern))
         (content-matcher
          (create-matcher
           (content-pattern-of pattern)
           (matcher-closure pattern bind-inner (node bindings)
             (let ((prev-value (xmlp-lookup var bindings))
                   (new-value (iter (for sibling initially (first *nested-start*)
                                         then (next-sibling sibling))
                                    (until (eq sibling node))
                                    (collect sibling))))
               (cond ((null prev-value)
                      (let ((*nested-start* (rest *nested-start*)))
                        (funcall next-fn node
                                 (extend-bindings var new-value bindings))))
                     ((binding-values-match-p prev-value new-value)
                      (let ((*nested-start* (rest *nested-start*)))
                        (funcall next-fn node bindings)))))))))
    (matcher-closure pattern bind-outer (node bindings)
      (let ((*nested-start* (cons node *nested-start*)))
        (funcall content-matcher node bindings)))))

(defclass end-anchor (pattern) ())

(defmethod create-matcher ((pattern end-anchor) next-fn)
  (matcher-closure pattern end-anchor (node bindings)
    (unless (skip-whitespace-nodes node)
      (funcall next-fn nil bindings))))

(defclass multi-pattern (pattern)
  ((patterns :accessor patterns-of :initarg :patterns)))

(defmethod xml-pattern-variables ((pattern multi-pattern))
  (when (patterns-of pattern)
    (reduce #'union (mapcar #'xml-pattern-variables (patterns-of pattern)))))

(defclass seq (multi-pattern) ())

(defmethod create-matcher ((pattern seq) next-fn)
  (chain-matchers (patterns-of pattern) next-fn))

(defclass text (pattern)
  ((text :accessor text-of :initarg :text)))

(defmethod create-matcher ((pattern text) next-fn)
  (let ((text (text-of pattern)))
    (matcher-closure pattern text (node bindings)
      (cond ((and (null node) (string= text ""))
             (funcall next-fn nil bindings))
            (node
             (multiple-value-bind (str positions next-nodes) (text-segments node)
               (iter (for p in positions)
                     (for next-node in next-nodes)
                     (thereis (and (string= text (trim (subseq str 0 p)))
                                   (funcall next-fn next-node bindings))))))))))

(defclass regex-base (pattern)
  ((regex :accessor regex-of :initarg :regex)
   (variables :accessor variables-of :initarg :variables)))

(defmethod xml-pattern-variables ((pattern regex-base))
  (variables-of pattern))

(defgeneric regex-matcher (pattern))

(defmethod create-matcher ((pattern regex-base) next-fn)
  (let* ((regex (cl-ppcre:create-scanner (regex-of pattern)
                                         :single-line-mode t
                                         :case-insensitive-mode t))
         (vars (variables-of pattern))
         (gen-new-bindings (regex-matcher pattern)))
    (matcher-closure pattern regex (node bindings)
      (when node
        (multiple-value-bind (str positions next-nodes) (text-segments node)
          (iter (for p in positions)
                (for next-node in next-nodes)
                (thereis
                 (multiple-value-bind (whole subs)
                     (cl-ppcre:scan-to-strings regex str :start 0 :end p)
                   (when whole
                     (funcall next-fn next-node
                              (funcall gen-new-bindings vars subs bindings node next-node)))))))))))

(defclass regex (regex-base) ())

(defmethod regex-matcher ((pattern regex))
  #'(lambda (vars subs bindings start-node end-node)
      (declare (ignore start-node end-node))
      (iter (for var in (reverse vars))
            (for sub in-vector subs downto 0)
            (setf bindings (extend-bindings var sub bindings))
            (finally (return bindings)))))

(defclass eregex (regex-base) ())

(defmethod regex-matcher ((pattern eregex))
  #'(lambda (vars subs bindings start-node end-node)
      (declare (ignore subs))
      (extend-bindings (first vars)
                       (iter (for node initially start-node
                                  then (next-sibling node))
                             (while (and node (not (eq node end-node))))
                             (collect node))
                       bindings)))

(defclass alternation (multi-pattern) ())

(defmethod create-matcher ((pattern alternation) next-fn)
  (let ((matchers (mapcar (rcurry #'create-matcher next-fn)
                          (patterns-of pattern))))
    (matcher-closure pattern alternation (node bindings)
      (iter (for matcher in matchers)
            (thereis (funcall matcher node bindings))))))

(defclass opt (compound-pattern) ())

(defmethod create-matcher ((pattern opt) next-fn)
  (let ((content-matcher
         (create-matcher (content-pattern-of pattern) next-fn)))
    (matcher-closure pattern opt (node bindings)
      (or (funcall content-matcher node bindings)
          (funcall next-fn node bindings)))))

(defclass greedy-repetition (compound-pattern) ())

(defmethod create-matcher ((pattern greedy-repetition) next-fn)
  (let* ((content-matcher nil)
         (inner-closure
          (matcher-closure pattern greedy-repetition-inner (node bindings)
            (setf *rep-accumulated-bindings*
                  (collect-added-bindings bindings *rep-saved-bindings*
                                          *rep-accumulated-bindings*))
            (or (funcall content-matcher node *rep-saved-bindings*)
                (funcall next-fn node (append *rep-accumulated-bindings*
                                              *rep-saved-bindings*))))))
    (setf content-matcher (create-matcher (content-pattern-of pattern) inner-closure))
    (matcher-closure pattern greedy-repetition (node bindings)
      (let ((*rep-accumulated-bindings* '())
            (*rep-saved-bindings* bindings))
        (funcall inner-closure node bindings)))))

(defclass greedy-repetition-no-zero (compound-pattern) ())

(defmethod create-matcher ((pattern greedy-repetition-no-zero) next-fn)
  (let (content-matcher)
    (setf content-matcher
          (create-matcher (content-pattern-of pattern)
                          (matcher-closure pattern repetition (node bindings)
                            (setf *rep-accumulated-bindings*
                                  (collect-added-bindings bindings *rep-saved-bindings*
                                                          *rep-accumulated-bindings*))
                            (or (funcall content-matcher node *rep-saved-bindings*)
                                (funcall next-fn node (append *rep-accumulated-bindings*
                                                              *rep-saved-bindings*))))))
    (matcher-closure pattern greedy-repetition (node bindings)
      (let ((*rep-accumulated-bindings* '())
            (*rep-saved-bindings* bindings))
        (funcall content-matcher node bindings)))))

(defclass deep (compound-pattern) ())

(defmethod create-matcher ((pattern deep) next-fn)
  (let ((content-matcher
         (create-matcher
          (content-pattern-of pattern)
          (matcher-closure pattern deep-inner (node bindings)
            (setf *deep-end* node)
            bindings))))
    (matcher-closure pattern deep-outer (node bindings)
      (let (*deep-end*)
        (when node
          (or (when-let ((binds (funcall content-matcher node bindings)))
                (funcall next-fn *deep-end* binds))
              (labels ((scan (node)
                         (or (funcall content-matcher node bindings)
                             (some #'scan (stp:list-children node)))))
                (when-let ((binds (scan node)))
                  (iter (for sibling initially (next-sibling node) then (next-sibling sibling))
                        (thereis (funcall next-fn sibling binds)))))
              (deep-outer (next-sibling node) bindings)))))))

;;; compilation/matching

(defun fix-attributes-in-bindings (bindings)
  (labels ((fix-value (v)
             (typecase v
               ((cons stp:attribute null) (stp:string-value (first v)))
               (proper-list (mapcar #'fix-value v))
               (t v))))
    (iter (for (name . value) in bindings)
          (collect (cons name (fix-value value))))))

(defun compile-xml-pattern (pattern &rest pattern-options &key next-node-handler &allow-other-keys)
  (let ((compiled 
         (create-matcher (apply #'parse-xml-pattern pattern
                                (remove-from-plist pattern-options :next-node-handler))
                         (named-lambda final-matcher (node bindings)
                           (when next-node-handler
                             (funcall next-node-handler node))
                           (fix-attributes-in-bindings bindings)))))
    (named-lambda pattern-wrapper (node bindings)
      ;; protect global variables to make the code threadsafe
      (let ((*trace-level* 0)
            *current-attribute-owner*
            (*nested-start* '())
            *rep-saved-bindings*
            *rep-accumulated-bindings*
            *deep-end*)
        (funcall compiled node bindings)))))

(defun xml-match (pattern node &rest pattern-options)
  (funcall (if (functionp pattern)
               pattern
               (apply #'compile-xml-pattern pattern pattern-options))
           node xmlp-empty-bindings))
