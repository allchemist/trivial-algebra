(in-package :trivial-algebra)

;;; quaternion q=w+ix+jy+kz is represented as 4-vector: [w x y z]
;;; every operation is destructive (if it can be destructive), except q-prod

(defparameter *quaternion-type* 'double-float)

(defun make-zero-q (&optional (type *quaternion-type*))
  (make-array 4 :element-type type))

(defun make-identity-q (&optional (type *quaternion-type*))
  (let ((q (make-zero-q type)))
    (setf (aref q 0) (coerce 1.0 type))
    q))

(defun make-rotation-q (axis angle)
  (let ((a/2 (/ angle 2)))
    (let ((sin-a/2 (sin a/2)))
      (vector (cos a/2)
	      (* (aref axis 0) sin-a/2)
	      (* (aref axis 1) sin-a/2)
	      (* (aref axis 2) sin-a/2)))))

(defun q+ (q1 q2)
  (incf (aref q1 0) (aref q2 0))
  (incf (aref q1 1) (aref q2 1))
  (incf (aref q1 2) (aref q2 2))
  (incf (aref q1 3) (aref q2 3))
  q1)

(defun q- (q1 q2)
  (decf (aref q1 0) (aref q2 0))
  (decf (aref q1 1) (aref q2 1))
  (decf (aref q1 2) (aref q2 2))
  (decf (aref q1 3) (aref q2 3))
  q1)

(defun q* (q1 q2)
  (setf (aref q1 0) (* (aref q1 0) (aref q2 0)))
  (setf (aref q1 1) (* (aref q1 1) (aref q2 1)))
  (setf (aref q1 2) (* (aref q1 2) (aref q2 2)))
  (setf (aref q1 3) (* (aref q1 3) (aref q2 3)))
  q1)

(defun q*c (q const)
  (setf (aref q 0) (* (aref q 0) const))
  (setf (aref q 1) (* (aref q 1) const))
  (setf (aref q 2) (* (aref q 2) const))
  (setf (aref q 3) (* (aref q 3) const))
  q)

(defun q-norm (q)
  (+ (square (aref q 0))
     (square (aref q 1))
     (square (aref q 2))
     (square (aref q 3))))

(defun q-magnitude (q)
  (sqrt (q-norm q)))

(defun q-prod (q1 q2)
  (let ((q (make-zero-q (array-element-type q1))))
    (setf (aref q 1)
	  (+ (* (aref q1 2) (aref q2 3))
	     (- (* (aref q1 3) (aref q2 2)))
	     (* (aref q1 1) (aref q2 0))
	     (* (aref q1 0) (aref q2 1))))
    (setf (aref q 2)
	  (+ (* (aref q1 3) (aref q2 1))
	     (- (* (aref q1 1) (aref q2 3)))
	     (* (aref q1 2) (aref q2 0))
	     (* (aref q1 0) (aref q2 2))))
    (setf (aref q 3)
	  (+ (* (aref q1 1) (aref q2 2))
	     (- (* (aref q1 2) (aref q2 1)))
	     (* (aref q1 3) (aref q2 0))
	     (* (aref q1 0) (aref q2 3))))
    (setf (aref q 0)
	  (- (* (aref q1 0) (aref q2 0))
	     (* (aref q1 1) (aref q2 1))
	     (* (aref q1 2) (aref q2 2))
	     (* (aref q1 3) (aref q2 3))))
    q))

(defun q-conjugate (q)
  (setf (aref q 1) (- (aref q 1)))
  (setf (aref q 2) (- (aref q 2)))
  (setf (aref q 3) (- (aref q 3)))
  q)

(defun q-invert (q)
  (q*c (q-conjugate q)
       (/ (q-norm q))))

(defun q-rotate-vector (v q)
  ;;; it could be imporved seriously
  (let ((type (array-element-type q)))
    (let ((new-v (make-zero-q type)))
      (setf (aref new-v 1) (coerce (aref v 0) type))
      (setf (aref new-v 2) (coerce (aref v 1) type))
      (setf (aref new-v 3) (coerce (aref v 2) type))
      (subseq (q-prod (q-prod q new-v) (q-invert (copy-vector q))) 1 4))))