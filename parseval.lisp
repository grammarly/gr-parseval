;;;; Parseval implementation
;;;; (c) 2014 Grammarly Inc.

(defpackage #:parseval
  (:use :common-lisp #:rutilsx #+dev #:should-test)
  (:export #:parseval
           #:tree-depth))

(in-package #:parseval)
(named-readtables:in-readtable rutils-readtable)


(defun parseval (test-tree gold-tree
                 &key unlabelled split leafless (weight (constantly 1)))
  "Calculate precision, recall, and f1-measure according to the Parseval metric
   for the TEST-TREE compared to GOLD-TREE (represented as Lisp lists).
   These are returned as a list.
   As second value returns a list of correct nodes in the TEST-TREE,
   total counts of nodes in the TEST-TREE and GOLD-TREE.

   Example:

       (parseval '(NP (NN \"foo\") (CC \"and\") (NN \"bar\"))
                 '(NP (NP (NN \"foo\")) (CC \"and\") (NN \"bar\")))
       => (1 4/5 8/9)
       => (4 4 5)

   NB. Words are not accounted by the metric.

   Different variants of Parseval may be used
   (see http://tech.grammarly.com/blog/posts/The-Dirty-Little-Secret-of-Constituency-Parser-Evaluation.html
    for details):

   - UNLABELLED compares only structure, not considering node labels
   - SPLIT uses a more strict metric of node identity
   - LEAFLESS doesn't count differences in tags (which are leaf nodes for Parseval)
   - WEIGHT calculating function may be provided to adjust individual error weights
     based on the node's properies (such as, distance from the leaves).
     One example may be a linear function that assigns 0.5 weight to leaf node
     errors and 1.5 to errors in the tree top:

         (let ((max-d (1- (tree-depth test-tree 'max))))
           (lambda (node) (- 3/2 (/ (tree-depth noed 'min) max-d))))
  "
  (assert (and (listp test-tree)
               (listp gold-tree)))
  (let ((total-gold 0)
        (total-test 0)
        (matched 0)
        nodes gold-nodes test-nodes
        i)
    (labels ((traverse (tree)
               (if (atom (cadr tree))
                   (let ((node (pair (first tree)
                                     (pair (:+ i) (1+ i)))))
                     (unless leafless
                       (push node nodes))
                     (list node))
                   (let* ((children (mapcar #'traverse (rest tree)))
                          (node (pair (first tree)
                                      (cons (first (rt (caar children)))
                                            (mapcar #`(last1 (rt (first %)))
                                                    children)))))
                     (push node nodes)
                     (cons node children)))))
      (doindex (j tree (list gold-tree test-tree))
        (:= i -1
            nodes (list))
        (let ((tree (traverse tree)))
          (if (zerop j)
              (:= gold-nodes nodes
                  total-gold (length nodes))
              (let ((key (cond
                           ((and unlabelled split) #'cadr)
                           (unlabelled #`(list (first (cadr %))
                                               (last1 (cadr %))))
                           ((not split) #`(cons (car %)
                                                (list (first (cadr %))
                                                      (last1 (cadr %)))))
                           (t #'identity))))
                (:= total-test (length nodes))
                (rutil:dotree (sub tree)
                  (unless (and leafless (atom (cadr sub)))
                    (:+ matched
                        (- 1 (if-it (position (funcall key (first sub)) gold-nodes
                                              :test #'equal :key key)
                                    (progn
                                      (void (nth it gold-nodes))
                                      0)
                                    (funcall weight sub)))))))))))
    (let ((prec (/ matched total-gold))
          (rec  (/ matched total-test)))
      (values (list prec rec (if (and (zerop prec) (zerop rec))
                                 0
                                 (/ (* 2 prec rec) (+ prec rec))))
              (list matched total-test total-gold)))))

(defun tree-depth (tree &optional (predicate 'max))
  "Calculate tree depth of TREE according to PREDICATE (default: max)."
  (labels ((traverse (tree &optional (depth 0))
             (cond ((atom tree) depth)
                   ((null (rest tree)) (1+ depth))
                   (t (reduce predicate
                              (mapcar #`(traverse % (1+ depth))
                                      (rest tree)))))))
    (traverse tree)))
