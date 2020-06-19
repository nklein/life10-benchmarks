;;; ccl  --load life.lisp --eval '(make-exe)' ; time ./life-ccl.exe ; rm -f ./life-ccl.exe
;;; sbcl --load life.lisp --eval '(make-exe)' ; time ./life-sbcl.exe; rm -f ./life-sbcl.exe


(defconstant +n+ 40)
(defconstant +m+ 80)
(defconstant +g+ 66000)

(deftype element-type () '(unsigned-byte 8))
(deftype array-type () `(simple-array element-type (,+n+ ,+m+)))

(declaim (ftype (function (array-type fixnum fixnum) element-type) aeref)
         (inline aeref))
(defun aeref (a i j)
  (declare (optimize (speed 3)
                     (safety 1))
           (type array-type a)
           (type fixnum i j))
  (the element-type (aref a i j)))

(declaim (ftype (function (fixnum array-type fixnum fixnum) element-type) (setf aeref))
         (inline (setf aeref)))
(defun (setf aeref) (v a i j)
  (declare (optimize (speed 3)
                     (safety 1))
           (type array-type a)
           (type element-type v)
           (type fixnum i j))
  (setf (aref a i j) v))

(defun display (b)
  (declare (optimize (speed 3)
                     (safety 1)))
  (declare (type array-type b))
  (dotimes (i +n+)
    (declare (type fixnum i))
    (dotimes (j +m+)
      (declare (type fixnum j))
      (write-char (if (plusp (aeref b i j)) #\* #\Space)))
    (terpri)))

(defun main ()
  (declare (optimize (speed 3)
                     (safety 1)))
  (let ((b (make-array (list +n+ +m+)
                       :element-type 'element-type
                       :initial-element 0))
        (nextb (make-array (list +n+ +m+)
                           :element-type 'element-type
                           :initial-element 0)))
    (declare (type array-type b nextb))
    (setf (aeref b 19 41) 1
          (aeref b 20 40) 1
          (aeref b 21 40) 1
          (aeref b 22 40) 1
          (aeref b 22 41) 1
          (aeref b 22 42) 1
          (aeref b 22 43) 1
          (aeref b 19 44) 1)

    (format t "Before:~%")
    (display b)

    (let ((n-1 (1- +n+))
          (m-1 (1- +m+)))
      (declare (type fixnum n-1 m-1))
      (dotimes (k +g+)
        (declare (type fixnum k))
        (dotimes (i +n+)
          (declare (type fixnum i))
          (let ((up (if (plusp i)
                        (1- i)
                        n-1))
                (down (if (< i n-1)
                          (1+ i)
                          0)))
            (declare (type fixnum up down))
            (dotimes (j +m+)
              (declare (type fixnum j))
              (let ((left (if (plusp j)
                              (1- j)
                              m-1))
                    (right (if (< j m-1)
                               (1+ j)
                               0)))
                (declare (type fixnum left right))
                (let ((count (+ (aeref b up left)
                                (aeref b up j)
                                (aeref b up right)
                                (aeref b i right)
                                (aeref b down right)
                                (aeref b down j)
                                (aeref b down left)
                                (aeref b i left))))
                  (declare (type fixnum count))
                  (setf (aeref nextb i j) (if (or (and (= count 2)
                                                      (plusp (aeref b i j)))
                                                 (= count 3))
                                             1
                                             0)))))))
        (rotatef nextb b)))
    (format t "After ~D generations:~%" +g+)
    (display b)))


(defun make-exe ()
  #+sbcl
  (sb-ext:save-lisp-and-die "life-sbcl.exe" :toplevel 'main :executable t)
  #+ccl
  (ccl:save-application "life-ccl.exe" :toplevel-function 'main :init-file nil :prepend-kernel t))
