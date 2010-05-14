(in-package :trivial-algebra)

(defparameter *default-type* 'number)
(defun in-type (num) (coerce num *default-type*))

(defun square (x) (* x x))

(defun random-value (max-value)
  (- max-value (random (* max-value 2))))

(defun dim0 (array) (array-dimension array 0))
(defun dim1 (array) (array-dimension array 1))

(defun make-matrix (dimensions &optional (element-type *default-type*))
  (make-array dimensions :element-type element-type))

(defmacro with-matrix-destination ((matrix &optional destination) &body body)
  (let ((result (or destination
		    (make-matrix (array-dimensions matrix)
				 (array-element-type matrix)))))
    `(progn
       ,@body
       ,result)))

(defmacro do-matrix ((matrix &optional row col) &body body)
  (let ((i (or row (gensym)))
	(j (or col (gensym))))
    `(dotimes (,i (dim0 ,matrix))
       (dotimes (,j (dim1 ,matrix))
	 ,@body))))

(defmacro do-row ((matrix &optional pos) &body body)
  (let ((i (or pos (gensym))))
    `(dotimes (,i (dim1 ,matrix))
       ,@body)))

(defmacro do-col ((matrix &optional pos) &body body)
  (let ((i (or pos (gensym))))
    `(dotimes (,i (dim0 ,matrix))
       ,@body)))

(defun row (matrix row &optional destination)
  (let ((vec (or destination (make-matrix (array-dimension matrix 1)))))
    (do-row (matrix i)
      (setf (aref vec i) (aref matrix row i)))
        vec))

(defun row-bind (matrix row)
  (make-array (dim1 matrix)
	      :displaced-to matrix
	      :displaced-index-offset (* (dim1 matrix) row)))

(defun col (matrix col &optional destination)
  (let ((vec (or destination (make-matrix (array-dimension matrix 0)))))
    (do-col (matrix i)
      (setf (aref vec i) (aref matrix i col)))
    vec))

(defun (setf row) (vec matrix row)
  (do-row (matrix i)
    (setf (aref matrix row i) (aref vec i)))
  matrix)

(defun (setf col) (vec matrix col)
  (do-col (matrix i)
    (setf (aref matrix i col) (aref vec i)))
  matrix)

(defun print-matrix (matrix &optional (control-string " ~,4f ") (destination t))
  (do-col (matrix i)
    (format destination "~%")
    (do-row (matrix j)
      (format destination control-string (aref matrix i j)))
    (format destination "~%")))

(defun copy-matrix (matrix &optional destination)
  (let ((new-matrix
	 (or destination
	     (make-matrix (array-dimensions matrix)
			  (array-element-type matrix)))))
    (do-matrix (matrix i j)
      (setf (aref new-matrix i j)
	    (aref matrix i j)))
    new-matrix))

(defun map-matrix (matrix func)
  (do-matrix (matrix i j)
    (setf (aref matrix i j) (funcall func (aref matrix i j))))
  matrix)

(defun make-random-matrix (dimensions &optional
			   (element-type *default-type*)
			   (rng #'(lambda (x)
				    (random 
				     (case element-type
				       (number 10)
				       (t (coerce 1 element-type)))))))
  (map-matrix (make-matrix dimensions element-type) rng))

(defun map-two-matrices (m1 m2 func)
  (do-matrix (m1 i j)
    (setf (aref m1 i j)
	  (funcall func (aref m1 i j) (aref m2 i j))))
  m1)

(defun m+ (&rest matrices)
  (reduce #'(lambda (m1 m2)
	      (map-two-matrices m1 m2 #'+))
	  matrices))

(defun m- (&rest matrices)
  (reduce #'(lambda (m1 m2)
	      (map-two-matrices m1 m2 #'-))
	  matrices))

(defun m* (&rest matrices)
  (reduce #'(lambda (m1 m2)
	      (map-two-matrices m1 m2 #'*))
	  matrices))

(defun m+c (matrix const)
  (do-matrix (matrix i j)
    (incf (aref matrix i j) const))
  matrix)

(defun m-c (matrix const)
  (do-matrix (matrix i j)
    (decf (aref matrix i j) const))
  matrix)

(defun m*c (matrix const)
  (do-matrix (matrix i j)
    (setf (aref matrix i j)
	  (* (aref matrix i j) const)))
  matrix)

(defun m-prod (m1 m2 &optional destination)
  (let ((rows1 (array-dimension m1 0))
	(cols1 (array-dimension m1 1))
	(cols2 (array-dimension m2 1))
	(rows2 (array-dimension m2 0)))
    (if (or (/= rows2 cols1)
	    (when destination
	      (or (/= (dim0 destination) rows1)
		  (/= (dim1 destination) cols2))))
	(error "matrix multiplucation dimensions error")
	(let ((result (or destination
			  (make-matrix (list rows1 cols2)
				       (array-element-type m1)))))
	  (dotimes (row rows1 result)
	    (dotimes (col cols2)
	      (setf (aref result row col)
		    (loop for k below cols1
			  summing (* (aref m1 row k) (aref m2 k col))))))))))

(defun mv-prod (matrix vector &optional destination)
  (let ((rows1 (array-dimension matrix 0))
	(cols1 (array-dimension matrix 1))
	(rows2 (length vector)))
    (if (or (/= rows2 cols1)
	    (when destination
	      (/= (length destination) rows1)))
	(error "matrix to vector multiplication dimensions error")
	(let ((result (or destination
			  (make-matrix rows1 (array-element-type matrix)))))
	  (dotimes (row rows1 result)
	    (setf (aref result row)
		  (loop for k below cols1
			summing (* (aref matrix row k) (aref vector k)))))))))

(defun v-prod (vec1 vec2 &optional destination)
  (if (and destination
	   (or (/= (dim0 destination) (length vec1))
	       (/= (dim1 destination) (length vec2))))
      (error "vector to vector multiplication dimensions error")
      (let ((matrix (make-matrix (list (length vec1)
				       (length vec2))
				 (array-element-type vec1))))
	(do-matrix (matrix i j)
	  (setf (aref matrix i j)
		(* (aref vec1 i)
		   (aref vec2 j))))
	matrix)))

(defun transpose-matrix (matrix &optional destination)
  (let ((new-matrix
	 (or destination
	     (make-matrix (reverse (array-dimensions matrix))
			  (array-element-type matrix)))))
    (do-matrix (matrix i j)
      (setf (aref new-matrix j i)
	    (aref matrix i j)))
    new-matrix))

(defun transpose-vector (vector &optional destination)
  (let ((new-matrix
	 (or destination
	     (make-matrix (list 1 (length vector))
			  (array-element-type vector)))))
    (do-vector (vector i)
      (setf (aref new-matrix 0 i)
	    (aref vector i)))
    new-matrix))

(defun transpose-vector-into (vector)
  (make-array (list 1 (length vector))
	      :displaced-to vector))

(defun sub-matrix (matrix dimensions offset)
  (let ((submatrix (make-matrix dimensions (array-element-type matrix)))
	(row-offset (first offset))
	(col-offset (second offset)))
    (do-matrix (submatrix i j)
      (setf (aref submatrix i j)
	    (aref matrix (+ i row-offset) (+ j col-offset))))
    submatrix))

(defun (setf sub-matrix) (submatrix matrix offset)
  (let ((row-offset (first offset))
	(col-offset (second offset)))
    (do-matrix (submatrix i j)
      (setf (aref matrix (+ i row-offset) (+ j col-offset))
	    (aref submatrix i j)))
    matrix))

(defun invert-matrix (matrix)
  (let ((size (array-dimension matrix 0))
        (temp (coerce 0 (array-element-type matrix))))
    (dotimes (i size matrix)
      (setf temp (aref matrix i i))
      (dotimes (j size)
        (setf (aref matrix i j)
              (if (= i j)
                  (/ (aref matrix i j))
                  (/ (aref matrix i j) temp))))
      (dotimes (j size)
        (unless (= i j)
          (setf temp (aref matrix j i)
                (aref matrix j i) (coerce 0 (array-element-type matrix)))
          (dotimes (k size)
            (setf (aref matrix j k)
                  (- (aref matrix j k)
                     (* temp (aref matrix i k))))))))))

(defun diag (vector)
  (let ((result (make-matrix (list (length vector) (length vector))
				   (array-element-type vector))))
    (do-vector (vector i)
      (setf (aref result i i) (aref vector i)))
    result))

(defun make-identity-matrix (size &optional (element-type *default-type*))
  (let ((matrix (make-matrix (list size size) element-type)))
    (dotimes (i size)
      (setf (aref matrix i i) 1))
    matrix))
