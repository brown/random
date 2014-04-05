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

;;;; Random number generator interface.

(in-package #:random)

(defgeneric reset (random)
  (:documentation "Resets RANDOM to a default state."))

(defgeneric next-uint8 (random)
  (:documentation "Returns a non-negative pseudo-random 8-bit integer."))

(defgeneric next-uint16 (random)
  (:documentation "Returns a non-negative pseudo-random 16-bit integer."))

(defgeneric next-uint32 (random)
  (:documentation "Returns a non-negative pseudo-random 32-bit integer."))

(defgeneric next-uint64 (random)
  (:documentation "Returns a non-negative pseudo-random 64-bit integer."))

(declaim (ftype (function (t uint32) (values uint32 &optional)) uniform-uint32)
         (inline uniform-uint32))

(defun uniform-uint32 (random n)
  "Returns a pseudo-random non-negative 32-bit integer modulo the non-negative
integer N.  When N is zero, zero is returned."
  (declare (type uint32 n))
  (let ((next (next-uint32 random)))
    (declare (type uint32 next))
    (if (zerop n)
        0
        (mod next n))))

(declaim (ftype (function (t uint32) (values boolean &optional)) one-in)
         (inline one-in))

(defun one-in (random n)
  "Return true when (UNIFORM-UINT32 RANDOM N) is zero."
  (declare (type uint32 n))
  (let ((x (uniform-uint32 random n)))
    (declare (type uint32 x))
    (zerop x)))

(declaim (ftype (function (t (integer 0 32)) (values uint32 &optional)) skewed-uint32)
         (inline skewed-uint32))

(defun skewed-uint32 (random maximum-log)
  "Picks a base uniformly from the range [0, MAXIMUM-LOG] and then returns base
random bits.  The effect is the same as picking a number in the range
[0, (2^MAXIMUM-LOG) - 1] with a bias toward smaller numbers."
  (declare (type (integer 0 32) maximum-log))
  (let* ((random1 (next-uint32 random))
         (base (mod random1 (1+ maximum-log)))
         (random2 (next-uint32 random)))
    (declare (type uint32 random1 random2))
    (mod random2 (ash 1 base))))
