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

;;;; ACM random number generator unit tests.

(in-package #:common-lisp-user)

(defpackage #:acm-random-test
  (:documentation "Tests the ACM random number generator in the ACM-RANDOM package.")
  (:use #:common-lisp
        #:acm-random
        #:com.google.base
        #:hu.dwim.stefil)
  (:export #:test-acm-random))

(in-package #:acm-random-test)
(declaim #.*optimize-default*)

(defsuite (test-acm-random :in root-suite) ()
  (run-child-tests))

(in-suite test-acm-random)

(deftest seed-special-cases ()
  "Tests that several problem seed values are handled correctly."
  (let ((random-minus-one (make-instance 'acm-random :seed -1))
        (random-zero (make-instance 'acm-random :seed 0))
        (random-m (make-instance 'acm-random :seed acm-random::+m+))
        (expected (mod acm-random::+a+ acm-random::+m+)))
    (is (= (next-uint31 random-minus-one)
           (next-uint31 random-zero)
           (next-uint31 random-m)
           expected))))

(deftest expected-32-bit-values ()
  "Tests expected random values from the default seed."
  (let ((m #x7fffffff)
        (a 16807)
        (r +acm-default-seed+)
        (random (make-instance 'acm-random)))
    (loop repeat 1000 do
      (setf r (mod (* r a) m))
      (is (= (next-uint31 random) r)))))

(deftest expected-64-bit-values ()
  "Tests expected 64-bit random values from the default seed."
  (let ((m #x7fffffff)
        (expected '(#x002698ad4b48ead0 #x1bfb1e0316f2d5de #x173a623c9725b477 #x0a447a02823ad868
                    #x1df74948b3fbea7e #x1bc8b594bcf01a39 #x07b767ca9520e99a #x05e28b4320bfd20e
                    #x0105906a24823f57 #x1a1e7d14a6d24384 #x2a7326df322e084d #x120bc9cc3fac4ec7
                    #x2c8f193a1b46a9c5 #x2b9c95743bbe3f90 #x0dcfc5b1d0398b46 #x006ba47b3448bea3
                    #x3fe4fbf9a522891b #x23e1a50ad6aebca3 #x1b263d39ea62be44 #x13581d282e643b0e))
        (random1 (make-instance 'acm-random))
        (random2 (make-instance 'acm-random)))
    (loop repeat 1000
          for golden in expected
          do (let ((value (next-uint62 random1)))
               (when golden (is (= value golden)))
               (let* ((first (next-uint31 random2))
                      (second (next-uint31 random2)))
                 (is (= value (+ (* (1- first) (1- m)) second))))))))

(deftest random-api-next ()
  (let ((random (make-instance 'acm-random)))
    (dotimes (i 1000)
      (is (<= 0 (random:next-uint8 random) (1- (expt 2 8))))
      (is (<= 0 (random:next-uint16 random) (1- (expt 2 16))))
      (is (<= 0 (random:next-uint32 random) (- (expt 2 31) 3)))
      (is (<= 0 (random:next-uint64 random) (1- (* (- (expt 2 31) 2) (- (expt 2 31) 2))))))))

(deftest random-api-uniform-and-one-in ()
  (let* ((random (make-instance 'acm-random))
         (first (random:next-uint32 random)))
    (random:reset random)
    (is (zerop (random:uniform-uint32 random 0)))
    (random:reset random)
    (is (= (random:uniform-uint32 random 10000) (mod first 10000)))
    (random:reset random)
    (is (random:one-in random first))
    (random:reset random)
    (is (not (random:one-in random (1+ first))))))
