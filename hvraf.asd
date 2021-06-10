(asdf:defsystem #:hvraf
  :description "A Common Lisp Jupyter inspector."
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (:common-lisp-jupyter :cytoscape-clj :resizable-box-clj)
  :components
    ((:module lisp
      :serial t
      :components
        ((:file "packages")
         (:file "inspect"))))
  . #+asdf3
      (:version "0.1"
       :homepage "https://github.com/yitzchak/hvraf/"
       :bug-tracker "https://github.com/yitzchak/hvraf/issues")
    #-asdf3 ())


#+asdf3.1
  (asdf:register-system-packages "hvraf"
                                 '(:tree))


