(in-package :trivial-algebra)

(defun make-vector (length &optional (element-type *default-type*))
  (make-array length :element-type element-type))

(defmacro do-vector ((vector &optional pos) &body body)
  (let ((i (or pos (gensym))))
    `(dotimes (,i (length ,vector))
       ,@body)))

(defun copy-vector (vector &optional destination)
  (if destination
      (setf destination (copy-seq vector))
      (copy-seq vector)))

(defun map-vector (vector func)
  (do-vector (vector i)
    (setf (aref vector i) (funcall func (aref vector i))))
  vector)

(defun make-random-vector (length &optional
			   (element-type *default-type*)
			   (rng #'(lambda (x)
				    (declare (ignore x))
				    (random 
				     (case element-type
				       (number 10)
				       (t (coerce 1 element-type)))))))
  (map-vector (make-vector length element-type) rng))

(defun map-two-vectors (v1 v2 func)
  (do-vector (v1 i)
    (setf (aref v1 i)
	  (funcall func (aref v1 i) (aref v2 i))))
  v1)

(defun v+ (&rest vectors)
  (reduce #'(lambda (v1 v2)
	      (map-two-vectors v1 v2 #'+))
	  vectors))

(defun v- (&rest vectors)
  (reduce #'(lambda (v1 v2)
	      (map-two-vectors v1 v2 #'-))
	  vectors))

(defun v* (&rest vectors)
  (reduce #'(lambda (v1 v2)
	      (map-two-vectors v1 v2 #'*))
	  vectors))

(defun v+c (vector const)
  (do-vector (vector i)
    (incf (aref vector i) const))
  vector)

(defun v-c (vector const)
  (do-vector (vector i)
    (decf (aref vector i) const))
  vector)

(defun v*c (vector const)
  (do-vector (vector i)
    (setf (aref vector i)
	  (* (aref vector i) const)))
  vector)

(defun inner-prod (v1 v2)
  (let ((prod 0))
    (do-vector (v1 i)
      (incf prod (* (aref v1 i) (aref v2 i))))
    prod))

(defun e-norm (vec)
  (let ((norm 0))
    (do-vector (vec i)
      (incf norm (square (aref vec i))))
    (sqrt norm)))

(defun vector-min (vec)
  (let ((val (aref vec 0)))
    (do-vector (vec i)
      (let ((cur-val (aref vec i)))
	(when (< cur-val val)
	  (setf val cur-val))))
    val))

(defun vector-max (vec)
  (let ((val (aref vec 0)))
    (do-vector (vec i)
      (let ((cur-val (aref vec i)))
	(when (> cur-val val)
	  (setf val cur-val))))
    val))

(defun norm-vector (vec)
  (let ((norm (e-norm vec)))
    (setf vec (map 'vector #'(lambda (x) (/ x norm)) vec))
    vec))

(defun vector-sum (vec)
  (let ((sum 0))
    (do-vector (vec i)
      (incf sum (aref vec i)))
    sum))

(defun vector-mean (vec)
  (/ (vector-sum vec) (length vec)))

(defun covariance (vec1 vec2)
  (- (vector-mean (v* (copy-vector vec1) vec2))
     (* (vector-mean vec1)
	(vector-mean vec2))))
