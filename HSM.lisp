(defpackage hsm
  (:use :cl))
(in-package :hsm)
(require :uiop)
(ql:quickload "alexandria")


;DATABASE
;Anything to do with Postres goes here.

(defun start-up-local ();wrap this in the right messages
   (postmodern:connect-toplevel "hsm" "hsm" "Databa$3" "localhost")
   (postmodern:query "CREATE TABLE IF NOT EXISTS hsm_table (id uuid, hsmtype text, contains text[], contained_by text[], string text[], timestamp timestamp without time zone, checkec boolean);")
)

(defun hsm-insert (inputs)
   (postmodern:query (:insert-rows-into 'hsm_table :columns 'id 'hsmtype 'contains 'contained_by 'string :values inputs))
)

(defun hsm-retrieve (inputs)
   (let ((query (concatenate 'string "select id, hsmtype, contains, contained_by, string from hsm_table where id in (" (list-to-comma-single-quoted inputs)  ")")))

   (postmodern:query query)
   )
)

(defun hsm-select (inputs)
   (let ((columns (list ''id ''hsmtype ''contains ''contained_by ''string))
         (criteria-list nil)
         (criteria-string nil)
         )
      (dolist (input inputs)
         (print input)
         (if (not (equal input :*))
            (if (equal input :null)
               (setf criteria-list (concatenate 'list criteria-list (list (list :is-null (car columns)))))
               (if (equal (type-of input) 'cons)
                  (setf criteria-list (concatenate 'list criteria-list (list input)))
                  (setf criteria-list (concatenate 'list criteria-list (list (list := (car columns) input))))
               )
            )
         )
         (setf columns (cdr columns))
      )
      (setf criteria-list (concatenate 'list (list :and) criteria-list))
      (setf query (concatenate 'list (list :select 'id 'hsmtype 'contains 'contained_by 'string :from 'hsm_table :where criteria-list)))
      (print "criteria-list")
      (print criteria-list)
      (print "query")
      (print query)
      ;(setf criteria-string (write-to-string criteria-list))
      (if (equal nil criteria-list)
         (postmodern:query (:select 'id 'hsmtype 'contains 'contained_by 'string :from 'hsm_table))
         (postmodern:query (postmodern:sql-compile query))
      )
   )
)

;hsm-copy
;hsm-delete

;SERVER
(defun launch-server ()
(setq *acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))
(hunchentoot:start *acceptor*)

(hunchentoot:define-easy-handler (hsm :uri "/hsm") (name)
(let ((return-val nil)
      (*print-pretty* nil)
      (request-type (hunchentoot:request-method*)))
(setf (hunchentoot:content-type*) "text/plain; charset=utf-8")
(setf name (concatenate 'string "hsm::" name))
;(print-spesh (read-from-string name))
;(print-spesh (cdr (car (hunchentoot:post-parameters*))))
;(print-spesh request-type)
(if (equal request-type :post)   ;(hunchentoot:post-parameters*)
   (setf return-val (eval (read-from-string (concatenate 'string "hsm::" (cdr (car (hunchentoot:post-parameters*)))))))
   (setf return-val (eval (read-from-string name)))
)

;(if (equal request-type :post)   ;(hunchentoot:post-parameters*)
   ;(print-spesh (cdr (car (hunchentoot:post-parameters*))))
   ;(print-spesh request-type)
;)
;(log-message*)


;(format nil "~s" return-val)
(format nil "~s" return-val)
;(print-spesh return-val)
)
)

)


;UTILITIES

(defun list-to-comma-single-quoted (original-list)
   (let ((return-string nil))
      (dolist (item original-list)
         (if (stringp item)
            (setf return-string (concatenate 'string return-string ", '" item "'"))
            (setf return-string (concatenate 'string return-string ", '" (write-to-string item) "'"))
         )
         
      )
      (setf return-string (string-trim ", " return-string))
      (return-from list-to-comma-single-quoted return-string)
   )
)