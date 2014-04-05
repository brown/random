;;;; Copyright 2011, Google Inc.  All rights reserved.

;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are
;;;; met:

;;;;     * Redistributions of source code must retain the above copyright
;;;; notice, this list of conditions and the following disclaimer.
;;;;     * Redistributions in binary form must reproduce the above
;;;; copyright notice, this list of conditions and the following disclaimer
;;;; in the documentation and/or other materials provided with the
;;;; distribution.
;;;;     * Neither the name of Google Inc. nor the names of its
;;;; contributors may be used to endorse or promote products derived from
;;;; this software without specific prior written permission.

;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;;;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;;;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;;;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;;;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;;;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;;; Author: brown@google.com (Robert Brown)

;;;; ACM random number generator.

(in-package #:acm-random)

(defconst +a+ 16807)
(defconst +m+ (1- (expt 2 31)))
(defconst +maximum-acm-random-value+ (1- +m+))
(defconst +acm-default-seed+ 301)

(deftype acm-random-value () '(integer 1 #.+maximum-acm-random-value+))

(defclass acm-random ()
  ((state :accessor state
          :initarg :seed
          :initform +acm-default-seed+
          :documentation "ACM random number generator state."))
  (:documentation
   "ACM minimal standard random number generator.  Use only for tests, not for
applications where security is important, since the next value output by the
generator is completely determined by the current value.  ACM random is a
multiplicative generator and outputs A^k mod M, where A = 16807 and M = 2^31-1.
Output repeats with period M-1."))

(defmethod initialize-instance :after ((random acm-random) &key)
  (let ((seed (ldb (byte 31 0) (state random))))
    (when (or (zerop seed) (= seed +m+))
      (setf seed 1))
    (setf (state random) seed)))

(declaim (ftype (function (acm-random) (values acm-random-value &optional)) next-uint31)
         (inline next-uint31))

(defun next-uint31 (random)
  "Returns a pseudo-random integer in the range [1, 2^31 - 2].  Note that this
is one number short on both ends of the full range of non-negative 32-bit
integers."
  (declare (type acm-random random))
  (let* ((state (state random))
         (product (* state +a+))
         (new-state (+ (ash product -31) (logand product +m+))))
    (declare (type acm-random-value state))
    (when (> new-state +m+) (decf new-state +m+))
    (setf (state random) new-state)
    new-state))

(declaim (ftype (function (acm-random) (values (unsigned-byte 62) &optional)) next-uint62)
         (inline next-uint62))

(defun next-uint62 (random)
  "Returns a pseudo-random integer in the range [1, (2^31 - 2)^2].  Do not use
this function if a short cycle length is important.  It is implemented by
calling NEXT-UINT31 twice, so it cycles with period 2^30 - 1."
  (let ((next31 (next-uint31 random)))
    (+ (* (1- next31) (1- +m+)) (next-uint31 random))))

(declaim (ftype (function (acm-random uint32) (values uint32 &optional)) uniform-uint32)
         (inline uniform-uint32))

;;;; Implement the RANDOM interface.

(defmethod random:reset ((random acm-random))
  (setf (state random) +acm-default-seed+))

(defmethod random:next-uint8 ((random acm-random))
  (ldb (byte 8 1) (next-uint31 random)))

(defmethod random:next-uint16 ((random acm-random))
  (ldb (byte 16 1) (next-uint31 random)))

(defmethod random:next-uint32 ((random acm-random))
  "Returns a pseudo-random integer in the range [0, 2^31 - 3]."
  (1- (next-uint31 random)))

(defmethod random:next-uint64 ((random acm-random))
  "Returns a pseudo-random integer in the range [0, (2^31 - 2)^2 - 1]."
  (1- (next-uint62 random)))
