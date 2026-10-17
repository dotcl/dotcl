;;; Structure slots declared (UNSIGNED-BYTE n) / (SIGNED-BYTE n), directly or
;;; through a DEFTYPE: stores at and past the bounds, fixnums and bignums.

(deftype %sswc-u64 () '(unsigned-byte 64))

(defstruct %sswc
  (a 0 :type %sswc-u64)
  (b 0 :type (signed-byte 64))
  (c 0 :type (unsigned-byte 7))
  (d 0 :type (signed-byte 8))
  (e 0 :type (unsigned-byte *))
  (f 0 :type (signed-byte 200)))

(defun %sswc-store (slot v)
  (let ((s (make-%sswc)))
    (handler-case
        (progn
          (ecase slot
            (:a (setf (%sswc-a s) v))
            (:b (setf (%sswc-b s) v))
            (:c (setf (%sswc-c s) v))
            (:d (setf (%sswc-d s) v))
            (:e (setf (%sswc-e s) v))
            (:f (setf (%sswc-f s) v)))
          :ok)
      (type-error () :type-error))))

(deftest struct-slot-integer-width-bounds
  (list (%sswc-store :a (1- (expt 2 64)))
        (%sswc-store :a (expt 2 64))
        (%sswc-store :a (expt 2 63))
        (%sswc-store :a 0)
        (%sswc-store :a -1)
        (%sswc-store :a (- (expt 2 70)))
        (%sswc-store :a 1.0)
        (%sswc-store :a :x)
        (%sswc-store :b most-negative-fixnum)
        (%sswc-store :b (- (expt 2 63)))
        (%sswc-store :b (1- (expt 2 63)))
        (%sswc-store :b (expt 2 63))
        (%sswc-store :b (- -1 (expt 2 63)))
        (%sswc-store :c 127)
        (%sswc-store :c 128)
        (%sswc-store :d -128)
        (%sswc-store :d 127)
        (%sswc-store :d 128)
        (%sswc-store :d -129)
        (%sswc-store :e (expt 2 100))
        (%sswc-store :e -1)
        (%sswc-store :f (- (expt 2 199)))
        (%sswc-store :f (- -1 (expt 2 199)))
        (%sswc-store :f (1- (expt 2 199)))
        (%sswc-store :f (expt 2 199)))
  (:ok :type-error :ok :ok :type-error :type-error :type-error :type-error
   :ok :ok :ok :type-error :type-error
   :ok :type-error
   :ok :ok :type-error :type-error
   :ok :type-error
   :ok :type-error :ok :type-error))
