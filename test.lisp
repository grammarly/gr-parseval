;;;; Parseval testing
;;;; (c) 2014 Grammarly Inc.

(in-package #:parseval)
(named-readtables:in-readtable rutils-readtable)


(deftest tree-depth ()
  (should be = 0
          (tree-depth "foo"))
  (should be = 1
          (tree-depth '(NN "foo")))
  (should be = 3
          (tree-depth '(NP (NP (NN "foo")) (CC "and") (NN "bar"))))
  (should be = 2
          (tree-depth '(NP (NP (NN "foo")) (CC "and") (NN "bar"))
                      'min)))

(defparameter *fail-cases*
  (mapcar (lambda (pair)
            #h(:test (first pair)
               :gold (second pair)))
          (group 2 (read-from-string
                    (strcat "("
                            (read-file (asdf:system-relative-pathname
                                        'gr-parseval "test-cases.sexp"))
                            ")")))))

(defparameter *measurements*
  (mapcar #`(mapcar #'read-from-string
                    (split #\Tab % :remove-empty-subseqs t))
          (split #\Newline
                 (read-file (asdf:system-relative-pathname 'gr-parseval
                                                           "measurements.txt"))
                 :remove-empty-subseqs t)))

(defun equal-approx (x y)
  (< (abs (- x y)) 0.01))

(defun every-equal-approx (xs ys)
  (every #'equal-approx xs ys))

(deftest parseval ()
  (loop :for case :in *fail-cases*
        :for msrm :in *measurements* :do
    (let* (#+nil (max-d (1- (tree-depth (? case :test))))
           (weight `(:weight ,(lambda (node)
                                (- 3/2 (/ 1 (expt 3/2 (1- (tree-depth node 'max)))))
                                #+nil (+ 1/2 (/ (1- (tree-depth node 'max)) max-d))))))
      (should be every-equal-approx msrm
              (mapcar (lambda (args)
                        (* 100 (float (third (apply #'parseval
                                                    (? case :test)
                                                    (? case :gold)
                                                    args)))))
                      `(()
                        (:unlabelled t :split t)
                        (:split t)
                        ,weight
                        (:unlabelled t :split t ,@weight)
                        (:split t ,@weight)
                        (:split t :leafless t)))))))
