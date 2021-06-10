(in-package :hvraf)


(defparameter +graph-style+
  "node {
     shape: round-rectangle;
     width: label;
     height: label;
     padding: 10;
     label: data(label);
     text-valign: center;
     text-halign: center;
   }
   edge {
     curve-style: bezier;
     target-arrow-shape: triangle;
   }
   :parent {
     text-valign: top;
     text-halign: center;
   }")


(defparameter *graph-layouts*
  '((:label "Circle layout"
     :args (cytoscape:circle-layout))
    (:label "Concentric layout"
     :args (cytoscape:concentric-layout))
    (:label "Breadth-First layout"
     :args (cytoscape:breadth-first-layout))
    (:label "COLA layout"
     :args (cytoscape:cola-layout))
    (:label "CoSE layout"
     :args (cytoscape:cose-layout))
    (:label "Dagre layout"
     :args (cytoscape:dagre-layout))
    (:label "fCoSE layout"
     :args (cytoscape:fcose-layout))
    (:label "Grid layout"
     :args (cytoscape:grid-layout))
    (:label "Klay layout"
     :args (cytoscape:klay-layout))))


(defclass inspector ()
  ((ids
     :accessor ids
     :initform (make-hash-table))
   (expand-functions
     :accessor expand-functions
     :initform (make-hash-table :test #'equal))
   (expand-command
     :reader expand-command
     :initform (make-instance 'cytoscape:menu-command
                              :content "<span class='fa fa-plus fa-2x'></span>"))
   (collapse-command
     :reader collapse-command
     :initform (make-instance 'cytoscape:menu-command
                              :content "<span class='fa fa-minus fa-2x'></span>"))
   (layout-select
     :reader layout-select
     :initform (make-instance 'jw:dropdown ; A dropdown to select the layout
                              :%options-labels (mapcar (lambda (layout)
                                                         (getf layout :label)) *graph-layouts*)
                              :index 3
                              :layout (make-instance 'jw:layout :width "max-content" :margin ".5em")))
   (fit-button
     :reader fit-button
     :initform (make-instance 'jw:button ; A button to refit to the whole graph.
                              :description "Fit"
                              :layout (make-instance 'jw:layout :width "max-content" :margin ".5em")))
   (graph
     :accessor graph
     :initform (make-instance 'cytoscape:cytoscape-widget
                              :graph-style +graph-style+
                              :layout (make-instance 'jw:layout
                                                     :height "600px"
                                                     :align-self "stretch"
                                                     :grid-area "graph")))))


(defun set-layout (instance)
  (setf (cytoscape:graph-layouts (graph instance))
        (list (apply #'make-instance (getf (elt *graph-layouts* (jw:widget-index (layout-select instance))) :args)))))


(defmethod initialize-instance :after ((instance inspector) &rest initargs &key &allow-other-keys)
  (with-slots (expand-command expand-functions collapse-command graph fit-button layout-select)
              instance
    (setf (cytoscape:context-menus graph)
          (list (make-instance 'cytoscape:context-menu
                               :selector "node.collapsed"
                               :commands (list expand-command))
                (make-instance 'cytoscape:context-menu
                               :selector "node.expanded"
                               :commands (list collapse-command))))
    (set-layout instance)
    (jw:observe layout-select :index
      (lambda (inst type name old-value new-value source)
        (declare (ignore inst type name old-value new-value source))
        (set-layout instance)))
    (jw:on-button-click fit-button
      (lambda (inst)
        (declare (ignore inst))
        (cytoscape:fit-elements graph)))
    (cytoscape:on-menu-command-select expand-command
      (lambda (inst id)
        (declare (ignore inst))
        (let ((fun (gethash id expand-functions)))
          (when fun
            (funcall fun)))))
    instance))


(defun add-element (inspector element &key expand collapse)
  (setf (cytoscape:elements (graph inspector))
        (append (when (equal "nodes" (cytoscape:group element))
                  (list element))
                (cytoscape:elements (graph inspector))
                (when (equal "edges" (cytoscape:group element))
                  (list element))))
  (when expand
    (setf (gethash (gethash "id" (cytoscape:data element)) (expand-functions inspector))
          expand)))


(defgeneric add-to-graph (inspector object id))


(defmethod add-to-graph (inspector object id)
  (add-element inspector
               (make-instance 'cytoscape:element
                              :group "nodes"
                              :data (j:make-object "id" id
                                                   "label" (write-to-string object)))))


(defmethod add-to-graph (inspector (object cons) id)
  (let ((car-source-id (j:make-uuid))
        (car-target-id (j:make-uuid))
        (cdr-source-id (j:make-uuid))
        (cdr-target-id (j:make-uuid)))
    (add-element inspector
                 (make-instance 'cytoscape:element
                                :group "nodes"
                                :classes (list "collapsed")
                                :data (j:make-object "id" cdr-source-id
                                                     "parent" id
                                                     "label" "cdr"))
                       :expand (lambda ()
                                 (add-to-graph inspector (cdr object) cdr-target-id)
                                 (add-element inspector
                                              (make-instance 'cytoscape:element
                                                             :group "edges"
                                                             :data (j:make-object "source" cdr-source-id
                                                                                  "target" cdr-target-id)))))
    (add-element inspector
                 (make-instance 'cytoscape:element
                                :group "nodes"
                                :classes (list "collapsed")
                                :data (j:make-object "id" car-source-id
                                                     "parent" id
                                                     "label" "car"))
                       :expand (lambda ()
                                 (add-to-graph inspector (car object) car-target-id)
                                 (add-element inspector
                                              (make-instance 'cytoscape:element
                                                             :group "edges"
                                                             :data (j:make-object "source" car-source-id
                                                                                  "target" car-target-id)))))
    (add-element inspector
                 (make-instance 'cytoscape:element
                                :group "nodes"
                                :data (j:make-object "id" id
                                                     "label" (write-to-string object))))))


(defmethod add-to-graph (inspector (object hash-table) id)
    (trivial-do:dohash (key value object)
      (let* ((source-id (j:make-uuid))
             (target-id (j:make-uuid))
             (value value)
             (element (make-instance 'cytoscape:element
                                      :classes (list "collapsed")
                                      :data (j:make-object "id" source-id
                                                           "parent" id
                                                           "label" (write-to-string key)))))
        (add-element inspector
                     element
                     :expand (lambda ()
                               (setf (cytoscape:classes element) (list "expanded"))
                               (add-to-graph inspector value target-id)
                               (add-element inspector
                                            (make-instance 'cytoscape:element
                                                           :group "edges"
                                                           :data (j:make-object "source" source-id
                                                                                "target" target-id)))))))
    (add-element inspector
                 (make-instance 'cytoscape:element
                                :group "nodes"
                                :data (j:make-object "id" id
                                                     "label" (write-to-string object)))))


(defun inspect (object)
  (let ((inspector (make-instance 'inspector))
        (id (j:make-uuid)))
    (setf (gethash object (ids inspector)) id)
    (add-to-graph inspector object id)
    (j:display (make-instance 'resizable-box:resizable-grid-box
                   :children (list (graph inspector)
                                   (make-instance 'jw:box
                                                  :children (list (layout-select inspector)
                                                                  (fit-button inspector))
                                                  :layout (make-instance 'jw:layout
                                                                         :flex-flow "row wrap"
                                                                         :justify-content "center"
                                                                         ;:margin "-.5em"
                                                                         :align-items "baseline"
                                                                         :align-content "flex-start"
                                                                         :grid-area "controls")))
                   :enable-full-screen t
                   :layout (make-instance 'resizable-box:resizable-layout
                                          :resize "vertical"
                                          :grid-gap "1em"
                                          :min-height "480px"
                                          :overflow "hidden"
                                          :padding "0 24px 0 0"
                                          :grid-template-rows "1fr min-content"
                                          :grid-template-columns "1fr"
                                          :grid-template-areas "'graph' 'controls")))))


#+sbcl
(setf sb-impl::*inspect-fun* (lambda (object input output)
                               (declare (ignore input output))
                               (inspect object)))


#+(or abcl clasp ecl)
(setf ext:*inspector-hook* #'inspect)
