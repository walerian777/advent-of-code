;; https://adventofcode.com/2019/day/22

(require "cl-ppcre")

(defun read-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))

(defun extract-number (text)
  (map 'list #'parse-integer
    (cl-ppcre::all-matches-as-strings "(-{0,1}\\d+)" text)))

(setq deck-length 10007)
(setq desired-card 2019)
(setq input (read-file "input"))
(setq numbers (map 'list #'extract-number input))

(defun range (size)
  (loop for n from 0 below size by 1
        collect n))

(defun starts-with? (str1 str2)
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

(defun deal-condition? (text)
  (equal text "deal into new stack"))

(defun cut-condition? (text)
  (starts-with? text "cut"))

(defun inc-condition? (text)
  (starts-with? text "deal with increment"))

(defun deal-command ()
  (- deck-length desired-card 1))

(defun cut-command (value)
  (mod (- (+ desired-card deck-length) value) deck-length))

(defun inc-command (value)
  (loop while (not (equal (mod desired-card value) 0))
        do (setq desired-card (+ desired-card deck-length))
        return (mod (* value desired-card) deck-length)))

(loop for pair in (mapcar 'list input numbers)
      do
      (if (deal-condition? (first pair))
        (setq desired-card (deal-command))
        (if (cut-condition? (first pair))
          (setq desired-card (cut-command (first (second pair))))
          (if (inc-condition? (first pair))
            (setq desired-card (inc-command (first (second pair))))))))

(print desired-card)
