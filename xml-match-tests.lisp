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

(defpackage :xml-match-tests
    (:use :cl :alexandria :iterate :xml-match :fiveam))

(in-package :xml-match-tests)

(def-suite xml-match)

(defclass attribute-sorter (cxml:sax-proxy) ())

(defmethod sax:start-element ((handler attribute-sorter) uri lname qname attrs)
  (call-next-method handler uri lname qname
                    (sort (copy-list attrs)
                          #'cxml::rod<
                          :key #'sax:attribute-qname)))

(defun make-attribute-sorter (chained-handler)
  (make-instance 'attribute-sorter :chained-handler chained-handler))

(defun print-stp (stp &optional (stream *standard-output*) sink)
  (if (typep stp 'stp:text)
      (write-string (stp:data stp) stream)
      (let ((sink
             (or sink
                 (make-attribute-sorter
                  (cxml:make-whitespace-normalizer
                   (cxml:make-character-stream-sink
                    stream
                    :omit-xml-declaration-p t
                    :canonical t))))))
        (etypecase stp
          (stp:element
             (sax:start-document sink)
             (stp:serialize stp sink)
             (sax:end-document sink))
          (stp:document
             (stp:serialize stp sink)))
        (values))))

(defun stringify-bindings (bindings)
  (iter (for (name . value) in bindings)
        (unless (eq name t)
          (collect (cons name
                         (labels ((convert-value (v)
                                    (etypecase v
                                      (string v)
                                      (stp:element
                                         (with-output-to-string (out)
                                           (print-stp v out)))
                                      (stp:node
                                         (stp:string-value v))
                                      (cons
                                         (if (or (consp (car v))
                                                 (stringp (car v)))
                                             (mapcar #'convert-value v)
                                             (apply #'concatenate 'string
                                                    (mapcar #'convert-value v)))))))
                           (convert-value value)))))))

(defun verify-match (pattern xml &optional (expected-bindings xmlp-empty-bindings))
  (let ((actual-bindings (xml-match pattern (cxml:parse-rod xml (stp:make-builder)))))
    (is (not (null actual-bindings))
        "failed to match ~a with ~s" xml pattern)
    (when actual-bindings
      (is (equal (stringify-bindings expected-bindings)
                 (stringify-bindings actual-bindings))
          "bad bindings for matching ~a~% with ~s:~%~s~% -- instead of --~%~s"
          xml pattern
          (stringify-bindings actual-bindings)
          (stringify-bindings expected-bindings)))))

(defun verify-mismatch (pattern xml)
  (is (null (xml-match pattern (cxml:parse-rod xml (stp:make-builder))))
      "wrong match of ~a with ~s" xml pattern))

(test (test-matching-elements :suite xml-match)
  (verify-match '(:a) "<a/>")
  (verify-mismatch '(:a) "<a><b/></a>")
  (verify-match '(:a *) "<a/>")
  (verify-match '(:a *) "<a>dfd</a>")
  (verify-match '(:a * (:p)) "<a><p/></a>")
  (verify-match '(:a * (:p)) "<a><f/><p/></a>")
  (verify-match '(:a * (:p (:q) (:r))) "<a><p><q/><r/></p><f/><p><q/><r/></p></a>")
  (verify-mismatch '(:a *) "<b/>")
  (verify-match '(:a (:b (:c))) "<a>  <b>  <c>  </c>  </b>  </a>")
  (verify-match '(:a (:+ns "http://example.com/somenamespace" (:qq "x")))
                "<a><z:b xmlns:z='http://example.com/somenamespace'><qq>x</qq></z:b></a>")
  (verify-mismatch '(:a (:+ns "http://example.com/somenamespace" (:qq "x")))
                   "<a><z:b xmlns:z='http://example.com/etc'><qq>x</qq></z:b></a>")
  (verify-mismatch '(:a (:+ns "http://example.com/somenamespace" (:qq "x")))
                   "<a/>")
  (verify-mismatch '(:a (:+ns "http://example.com/somenamespace" (:qq "x")))
                   "<a><z:b xmlns:z='http://example.com/somenamespace'><qq>x</qq>misc</z:b></a>"))

(test (test-match-bindings :suite xml-match)
  (verify-match '(:a (:b (:+bind * ?x))) "<a><b>xcfds<z/>qweqweq</b></a>"
                '((?x . "xcfds<z></z>qweqweq")))
  (verify-mismatch '(:a (:b (:+bind * ?x))) "<a><c>xcfds<z/>qweqweq</c></a>")
  (verify-mismatch '(:a (:b (:+bind * ?x))) "<a><b>xcfds<z/>qweqweq</b>fff</a>")
  (verify-match '(:a (:b (:+bind * (:c) ?x) (:d)) (:e))
                "<a><b>xcfds<z/>qweqweq<c/><d/></b><e/></a>"
                '((?x . "xcfds<z></z>qweqweq<c></c>")))
  (verify-mismatch '(:a (:b (:+bind * (:c) ?x) (:d)) (:e))
                   "<a><b>xcfds<z/>qweqweq<c/></b><e/></a>")
  (verify-mismatch '(:a (:b (:+bind * (:c) ?x) (:d)) (:e))
                   "<a><b>xcfds<z/>qweqweq<d/></b><e/></a>")
  (verify-mismatch '(:a (:b (:+bind * (:c) ?x) (:d)) (:e))
                   "<a><b>xcfds<z/>qweqweq<c/><d/></b><f/></a>"))

(test (test-matching-strings :suite xml-match)
  (verify-match '(:a "qwerty") "<a>qwerty</a>")
  (verify-match '(:a "qwerty") "<a>  qwerty  </a>")
  (verify-match '(:a (:b) "") "<a><b/><c/></a>")
  (verify-match '(:a (:b "") "") "<a><b/><c/></a>")
  (verify-match '(:a (:b) "") "<a><b/><d></d><e><f></f></e><c/></a>")
  (verify-mismatch '(:a (:b) "") "<a><b/><d></d><e><f>zz</f></e><c/></a>")
  (verify-mismatch '(:a (:b) "") "<a><b/>zz</a>")
  (verify-match '(:a (:+bind "qwerty" ?x)) "<a>  qwerty  <f/></a>"
                '((?x . "  qwerty  <f></f>")))
  (verify-match '(:a "qwerty" (:f "")) "<a>  qwerty  <f>   </f></a>")
  (verify-mismatch '(:a "qwerty") "<a>  qwerty  x</a>"))

(test (test-matching-rx :suite xml-match)
  (verify-match '(:a (:b (:+rx "^(\\d+)$" ?num)))
                "<a><b>1234</b></a>" '((?num . "1234")))
  (verify-match '(:a (:b (:+rx "^(\\d+)([xyz])$" ?num ?v) (:x *)))
                "<a><b>1234z<x>qqq</x></b></a>" '((?num . "1234") (?v . "z")))
  (verify-mismatch '(:a (:b (:+rx "^(\\d+)$" ?num)))
                   "<a><b>1234z</b></a>")
  (verify-mismatch '(:a (:b (:+rx "^(\\d+)$" ?num)))
                   "<a><b>1234<x>qqq</x></b></a>")
  (verify-match '(:a (:bcd :xyz (:+rx "^(\\d+)$" ?num) *) *)
                "<a><bcd xyz='1234'>zzz</bcd>rrr</a>"
                '((?num . "1234")))
  (verify-match '(:a (:bcd :xyz (:+rx "^(\\d+)$") *) *)
                "<a><bcd xyz='1234'>zzz</bcd>rrr</a>")
  (verify-mismatch '(:a (:bcd :xyz (:+rx "^(\\d+)$" ?num) *) *)
                   "<a><bcd xyz='zz1234'>zzz</bcd>rrr</a>")
  (verify-match '(:a (:b (:c (:+erx "(123-\\w+)" ?fff))))
                "<a><b><c><r/>123-zzz<x/></c></b></a>"
                '((?fff . "<r></r>123-zzz<x></x>")))
  (verify-match '(:a (:b (:c (:+rx "z123-\\w+"))))
                "<a><b><c><r/>Z123-zzz<x/></c></b></a>")
  (verify-mismatch '(:a (:b (:c (:+rx "123-\\w+"))))
                   "<a><b><c><r/>aaa-zzz<x/></c></b></a>"))

(test (test-matching-attributes :suite xml-match)
  (verify-mismatch '(:a :x ?x) "<a/>")
  (verify-match '(:a :x (:+opt ?x)) "<a/>")
  (verify-match '(:a :x (:+opt ?x)) "<a x='42'/>"
                '((?x . "42")))
  (verify-match '(:a :x (:+opt (:+rx "(\\d+)" ?x))) "<a x='42'/>"
                '((?x . "42")))
  (verify-match '(:a :x (:+opt (:+rx "(\\d+)" ?x))) "<a/>")
  (verify-match '(:a :a "1234" (:b :x ?x ?b) (:c :z ?x "ff") (:d *))
                "<a a='1234'><b x='4'>dfde<q/><r/></b><c z='4'>ff</c><d/></a>"
                '((?b . "dfde<q></q><r></r>") (?x . "4")))
  (verify-match '(:a :a "1234" (:b :x ?x ?b) (:c ?x) (:d *))
                "<a a='1234'><b x='4'>dfde<q/><r/></b><c><misc>4</misc> </c><d/></a>"
                '((?b . "dfde<q></q><r></r>") (?x . "4")))
  (verify-mismatch '(:a :a "1234" (:b :x ?x ?b) (:c :z ?x "ff") (:d *))
                   "<a a='1234'><b x='4'>dfde<q/><r/></b><c z='45'>ff</c><d/></a>")
  (verify-match '(:img :src "x") "<img src=\"x\"/>")
  (is (equal '((?x . "x") (t . t))
             (xml-match '(:img :src ?x)
                        (cxml:parse-rod "<img src=\"x\"/>" (stp:make-builder))))))

(test (test-matching-opt :suite xml-match)
  (verify-match '(:a (:+opt (:b ?x))) "<a><b>qqq</b></a>"
                '((?x . "qqq")))
  (verify-match '(:a (:+opt (:b ?x) (:qqq ?y))) "<a><b>qqq</b><qqq>zzz</qqq></a>"
                '((?y . "zzz") (?x . "qqq")))
  (verify-match '(:a (:+opt (:b ?x)) (:c)) "<a><c/></a>")
  (verify-match '(:a (:+opt (:b ?x))) "<a/>")
  (verify-mismatch '(:a (:+opt (:b ?x)) (:c)) "<a/>"))

(test (test-matching-or :suite xml-match)
  (verify-match '(:a (:b (:c (:+or (:+rx "123-\\w+") (:d) (:e)))))
                "<a><b><c><r/>123-zzz<x/></c></b></a>")
  (verify-match '(:a (:b (:c (:+or (:+rx "123-\\w+") (:d) (:e)))))
                "<a><b><c><d/></c></b></a>")
  (verify-match '(:a (:b (:c (:+or (:+rx "123-\\w+") (:d) (:e)))))
                "<a><b><c><e/></c></b></a>")
  (verify-mismatch '(:a (:b (:c (:+or (:+rx "123-\\w+") (:d) (:e)))))
                   "<a><b><c><f/></c></b></a>"))

(test (test-repetition :suite xml-match)
  (verify-match '(:a (:* (:b "qwerty") "zzz")) "<a/>")
  (verify-match '(:a (:* (:b "qwerty") "zzz"))
                "<a><b>qwerty</b>zzz</a>")
  (verify-match '(:a (:* (:b "qwerty") "zzz"))
                "<a><b>qwerty</b>zzz<b>qwerty</b>zzz</a>")
  (verify-match '(:a (:* (:b "qwerty") "zzz"))
                "<a><b>qwerty</b>zzz<b>qwerty</b>zzz<b>qwerty</b>zzz</a>")
  (verify-mismatch '(:a (:* (:b "qwerty") "zzz"))
                   "<a><b>qwerty</b></a>")
  (verify-mismatch '(:a (:* (:b "qwerty") "zzz"))
                   "<a><q/></a>")
  (verify-mismatch '(:a (:* (:b "qwerty") "zzz"))
                   "<a><b>qwerty</b>zzz<x>f</x></a>")
  (verify-match '(:a (:+ (:b "qwerty") "zzz"))
                "<a><b>qwerty</b>zzz</a>")
  (verify-match '(:a (:+ (:b "qwerty") "zzz"))
                "<a><b>qwerty</b>zzz<b>qwerty</b>zzz</a>")
  (verify-match '(:a (:+ (:b "qwerty") "zzz"))
                "<a><b>qwerty</b>zzz<b>qwerty</b>zzz<b>qwerty</b>zzz</a>")
  (verify-match '(:ul (:* (:li ?x)))
                "<ul><li>abc</li><li>def</li><li><em>qqq</em></li></ul>"
                '((?x "abc" "def" "<em>qqq</em>")))
  (verify-match '(:ul (:* (:li ?x)))
                "<ul><li>abc</li></ul>"
                '((?x "abc")))
  (verify-match '(:ul (:* (:li ?x))) "<ul/>")
  (verify-match '(:ul (:+ (:li ?x)))
                "<ul><li>abc</li><li>def</li><li><em>qqq</em></li></ul>"
                '((?x "abc" "def" "<em>qqq</em>")))
  (verify-match '(:ul (:+ (:li ?x)))
                "<ul><li>abc</li></ul>"
                '((?x "abc")))
  (verify-match '(:div (:p ?x) (:ul (:+ (:li ?x))))
                "<div><p>zzz</p><ul><li>zzz</li><li>zzz</li></ul></div>"
                '((?x . "zzz")))
  (verify-match '(:div (:+ (:img :src ?src)))
                "<div><img src='a.jpg'/><img src='b.jpg'/><img src='c.jpg'/></div>"
                '((?src "a.jpg" "b.jpg" "c.jpg")))
  ;; if the same variable is used both inside and outside repetition,
  ;; the value used outside must occur among values produced by the binding
  ;; inside repetition
  (verify-match '(:div (:ul (:+ (:li ?x))) (:p ?x))
                "<div><ul><li>zzz</li><li>zzz</li></ul><p>zzz</p></div>"
                '((?x "zzz" "zzz")))
  (verify-match '(:div (:ul (:+ (:li ?x))) (:p ?x))
                "<div><ul><li>qqq</li><li>zzz</li></ul><p>zzz</p></div>"
                '((?x "qqq" "zzz")))
  (verify-match '(:div (:ul (:+ (:li ?x))) (:p ?x))
                "<div><ul><li>zzz</li><li>qqq</li></ul><p>zzz</p></div>"
                '((?x "zzz" "qqq")))
  ;; nested bindings
  (verify-match '(:ul (:* (:li (:+bind (:a :href ?x *) ?items))))
                "<ul><li><a href=\"zzz\">1</a></li><li><a href=\"qqq\">2</a></li></ul>"
                '((?x "zzz" "qqq")
                  (?items "<a href=\"zzz\">1</a>" "<a href=\"qqq\">2</a>")))
  ;; TBD: as of now, using the same variable in different repetitions
  ;; causes the matcher to fail. It should be:
  ;; bindings in different repetitions must have at least one value in common.
  ;; But I don't know how to implement it cleanly right now
  #+nil
  (verify-match '(:div (:ul (:+ (:li ?x))) (:+ (:div ?x)))
                "<div><ul><li>zzz</li><li>qqq</li></ul><div>zzz</div><div>fff</div></div>"
                '((?x "zzz" "qqq")))
  (verify-mismatch '(:div (:p ?x) (:ul (:+ (:li ?x))))
                   "<div><p>zzz</p><ul><li>zzz</li><li>qqq</li></ul></div>")
  (verify-mismatch '(:div (:ul (:+ (:li ?x))) (:p ?x))
                "<div><ul><li>rrr</li><li>qqq</li></ul><p>zzz</p></div>")
  (verify-mismatch '(:a (:+ (:b "qwerty") "zzz")) "<a/>")
  (verify-mismatch '(:a (:+ (:b "qwerty") "zzz"))
                   "<a><b>qwerty</b></a>")
  (verify-mismatch '(:a (:+ (:b "qwerty") "zzz"))
                   "<a><q/></a>")
  (verify-mismatch '(:a (:+ (:b "qwerty") "zzz"))
                   "<a><b>qwerty</b>zzz<x>f</x></a>"))

(define-xml-pattern-alias 'some-pattern '(:q (:z "qwerty")))

(test (test-aliases :suite xml-match)
  (verify-match 'some-pattern "<q><z>qwerty</z></q>")
  (verify-match '(:a some-pattern "rrr") "<a><q><z>qwerty</z></q>rrr</a>")
  (verify-match '(:+alias ((another-pattern (:q "fff") "zzz")
                           (misc-pattern (:f "rrr")))
                  (:a another-pattern misc-pattern another-pattern (:x)))
                "<a><q>fff</q>zzz<f>rrr</f><q>fff</q>zzz<x/></a>")
  (verify-mismatch '(:+alias ((another-pattern (:q "fff") "zzz")
                              (misc-pattern (:f "rrr")))
                     (:a another-pattern misc-pattern another-pattern (:x)))
                   "<a><q>fff</q>zzz<f>rrr</f><q>fff</q>zzz</a>")
  (verify-mismatch '(:+alias ((another-pattern (:q "fff") "zzz")
                              (misc-pattern (:f "rrr")))
                     (:a another-pattern misc-pattern another-pattern (:x)))
                   "<a><q>fff</q>zzz</a>")
  (verify-mismatch '(:a some-pattern "rrr") "<a><q><z>qwertyz</z></q>rrr</a>")
  (verify-mismatch '(:a some-pattern "rrr") "<a><q><z>qwerty</z></q>fff</a>"))

(test (test-xml-pattern-variables :suite xml-match)
  (is (equal '(?v1 ?v2 ?v3 ?v4 ?v5 ?v6 ?v7 ?v8 ?v9 ?v10 ?v11 ?v12 ?v13 ?v14)
             (sort
              (xml-pattern-variables
               (parse-xml-pattern
                '(:a ?v1
                  (:+or (:+bind (:b ?v3 (:* (:x ?v4)) (:+ (:y ?v5))) ?v2)
                   (:z ?v6) (:+opt "qqq" ?v7) (:r :f ?v8 :qq (:+opt ?v9)
                                               :rr (:+rx "(\\d+)" ?v10)
                                               (:+rx "(\\w+)" ?v11))
                   (:qq (:+erx "\\w+" ?v12))
                   (:+ns "http://example.com/somenamespace" (:x ?v13 "fff"))
                   (:+alias ((zzz (:zzz ?v14))) zzz zzz ?v1)
                   *))))
              #'<
              :key #'(lambda (var)
                       (assert (or (xml-match::variable-p var)
                                   (starts-with-subseq "?V" (symbol-name var))))
                       (parse-integer (subseq (symbol-name var) 2)))))))

(test (test-deep-matching :suite xml-match)
  (verify-match '(:a (:+deep (:q :x ?x "qwerty") "zzz"))
                "<a><r>aaa<q>afff<q x='zxc'>qwerty</q>zzz</q>...</r>...</a>"
                '((?x . "zxc")))
  (verify-match '(:a (:+deep (:q :x ?x "qwerty") "zzz"))
                "<a>zzz<f/><r>aaa<q>afff<q x='zxc'>qwerty</q>zzz</q>...</r>...</a>"
                '((?x . "zxc")))
  (verify-match '(:a (:+deep (:q :x ?x "qwerty") "zzz"))
                "<a>zzz<f><q x='zxc'>qwerty</q>zzz</f><r>aaa<q>afff<q x='zxc'>qwerty</q>zzz</q>...</r>...</a>"
                '((?x . "zxc")))
  (verify-match '(:a (:+deep (:q :x ?x "qwerty") "zzz"))
                "<a><q x='zxc123'>qwerty</q>zzz</a>"
                '((?x . "zxc123")))
  (verify-match '(:a (:+deep (:q :x ?x "qwerty") "zzz"))
                "<a>ffffffff<q x='zxc123'>qwerty</q>zzz</a>"
                '((?x . "zxc123")))
  (verify-match '(:a (:+deep (:q :x ?x "qwerty") "zzz") (:bb ?y))
                "<a><r>aaa<q>afff<q x='zxc'>qwerty</q>zzz</q>...</r>...<bb>fff</bb></a>"
                '((?y . "fff") (?x . "zxc")))
  (verify-match '(:a (:+deep (:q :x ?x "qwerty") "zzz") (:bb ?y))
                "<a><q x='zxc'>qwerty</q>zzz<bb>fff</bb></a>"
                '((?y . "fff") (?x . "zxc")))
  (verify-mismatch '(:a (:+deep (:q :x ?x "qwerty") "zzz"))
                   "<a><q x='zxc123'>qwertyf</q>zzz</a>"))

(test (test-matching-real-cases :suite xml-match)
  (verify-match
   '(:table :align "left" :border "0"
     (:tbody
      (:tr
       (:td
        (:img :src ?src :width ?width :height ?height)))
      (:tr
       (:td
        (:font :size "1" ?description)))))
   "<table hspace=\"5\" align=\"left\" border=\"0\">
        <tbody>
         <tr><td><img src=\"/@@155785\" height=\"130\" hspace=\"0\" width=\"135\" border=\"0\"/></td></tr>
         <tr><td><font face=\"verdana\" size=\"1\">Игрушка<br/>(одушевлённое сущ.)</font></td></tr>
        </tbody>
      </table>"
   '((?description . "Игрушка<br></br>(одушевлённое сущ.)")
     (?height . "130")
     (?width . "135")
     (?src . "/@@155785"))))

;; TBD: if repeated block binds some variables, the number of items in each of these
;; variables should be equal
;; TBD: XPath integration as separate lib

;; TBD: namespace URI translation spec for nodes and attributes
;; TBD: rename element-pattern
;; TBD: default end of pattern
;; TBD: proper handling of text/rx sequences (boundaries inside text nodes)
