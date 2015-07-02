(in-package :pddl.component-planner)
(cl-syntax:use-syntax :annot)

;;; normalized score

(defun basedata (pair)
  (ematch pair
    ((vector (and bag (list* (abstract-component-task-
                              (ac (abstract-component components))) _))
             (macro-action actions))
     (vector (length bag)
             (length components)
             (length actions)))))


(defun map-column (data fn index)
  (flet ((v (datum) (aref datum index)))
    (funcall fn (map 'vector #'v data))))

(defun norminfo (data)
  (iter (for i below 3)
        (collect
            (vector (map-column data #'mean i)
                    (map-column data #'standard-deviation i)))))
(defun normalize (mean stdev x)
  (/ (- x mean) (if (zerop stdev) 1 stdev)))
(defun normalize-data (data)
  (let ((norminfo (norminfo data)))
    (map 'vector
         (lambda (datum)
           (iter (for x in-vector datum)
                 (for v in norminfo)
                 (ematch v
                   ((vector mean stdev)
                    (collect (normalize mean stdev x)
                      result-type vector)))))
         data)))

(defun ^2 (x) (* x x))

(defvar *weights* (vector 1.0 0.5 0.1))
(defun ms-diff-score (d)
  (- (mean d) (standard-deviation d)))
(defun ms-rate-score (d)
  (let ((stdev (standard-deviation d)))
    (if (zerop stdev)
        (if (plusp (reduce #'+ d))
            MOST-POSITIVE-SINGLE-FLOAT
            MOST-NEGATIVE-SINGLE-FLOAT)
        (/ (mean d) stdev))))

(defun sum-score (d) (reduce #'+ d))
(defun score (datum)
  (sum-score
   (map 'vector #'* datum *weights*)))

(defun filter-macros-normalized (pairs)
  (when pairs
    (let* ((data (map 'vector #'basedata pairs))
           (ndata (normalize-data data))
           (scores (map 'vector #'score ndata))
           (threshold (+ (mean scores)
                         (* (standard-deviation scores)
                             (z *threshold*))))
           (all (iter (for pair in pairs)
                      (for datum in-vector data)
                      (for ndatum in-vector ndata)
                      (for score in-vector scores)
                      (collect (list pair datum ndatum score))))
           (all (sort all #'> :key #'fourth))
           (results (remove-if-not
                     (curry #'< threshold) all
                     :key #'fourth)))
      (print-results all threshold)
      (format t "~&Pruning percentage is ~3,1f%." (* 100 *threshold*))
      (format t "~&Score threshold is ~a." threshold)
      (when (< (length results) 2)
        (format t "~&This threshold value prunes too many macros. ~
                   ~&Recovering at least 2.")
        (setf results (subseq all 0 (min 2 (length all)))))
      (format t "~&~40@<~>... ~a remaining." (length results))
      (mapcar #'first results))))

(defvar *sep*
    "----------------------------------------------------------------")
(defun print-results (results th)
  (format t "~&~a macros, status:" (length results))
  (when results
    (iter (for r in results)
          (for s = (get-score r))
          (for ps previous s)
          (when (and ps (< s th ps))
            (format t "~&~a" *sep*))
          (print-result r))))
(defun get-score (result)
  (ematch result
    ((list _ _ _ score) score)))
(defun print-result (result)
  (ematch result
    ((list (vector (list* (abstract-component-task-
                           (ac (abstract-component seed))) _) _)
           (vector bag components actions)
           _ ; (vector nbag ncomponents nactions)
           score)
     (format t "~&(:tasks ~a :objs ~a :macro-length ~a ~
                   :score ~,4f :seed ~a)"
             ;; :tasks ~,4f :objs ~,4f :macro-length ~,4f ~
             bag components actions
             ;; nbag ncomponents nactions
             score
             (name (type seed))))))

;;; old implementation of fixed and normdist filtering

(defun score-pair (pair)
  (ematch pair
    ((vector (and bag (list* (abstract-component-task-
                              (ac (abstract-component components))) _))
             (macro-action actions))
     (let ((sample (vector (length bag)
                           (length components)
                           (length actions))))
       (- (mean sample)
          (standard-deviation sample))))))

(defun print-pair-status (pair)
  (ematch pair
    ((vector (and bag (list* (abstract-component-task-
                              (ac (abstract-component seed components))) _))
             (macro-action actions))
     (format t "~&(:tasks ~a :objs ~a :macro-length ~a :score ~a :seed ~a)"
             (length bag) (length components) (length actions)
             (score-pair pair)
             (name (type seed))))))

(defun filter-macros-fixed (pairs)
  (when (< 2 (length pairs))
    (format t "~&~a macros are filtered down to 2 (fixed number)."
            (length pairs)))
  (subseq pairs 0 (min 2 (length pairs))))

(defvar *threshold* 0)

(defun sort-and-print-macros (pairs)
  (format t "~&~a macros, status:" (length pairs))
  (when pairs
    (setf pairs (sort pairs #'> :key #'score-pair))
    (mapc #'print-pair-status pairs)
    pairs))

(defun filter-macros-normdist (pairs)
  (when pairs
    (let* ((scores (map 'vector #'score-pair pairs))
           (threshold (+ (mean scores)
                         (* (standard-deviation scores) *threshold*)))
           (results (remove-if-not (curry #'< threshold) pairs
                                   :key #'score-pair)))
      (format t "~&Pruning threshold is ~a." threshold)
      (when (< (length results) 2)
        (format t "~&This threshold value prunes too many macros. Recovering at least 2.")
        (setf results (subseq pairs 0 (min 2 (length pairs)))))
      (format t "~&~a macros are filtered down to ~a." (length pairs) (length results))
      results)))

;;; trivial filters

(defun remove-null-macros (pairs)
  (format t "~&~40@<Filtering null macros.~>")
  (let ((filtered (remove-if (lambda-match ((vector _ nil) t)) pairs)))
    (format t "... ~a remaining." (length filtered))
    filtered))
(defun remove-single-macros (pairs)
  (format t "~&~40@<Filtering macros with length 1.~>")
  (let ((filtered (remove-if (lambda-match ((vector _ (macro-action actions))
                                            (= 1 (length actions))))
                             pairs)))
    (format t "... ~a remaining." (length filtered))
    filtered))
