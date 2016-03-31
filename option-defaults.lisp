(in-package #:chronicler)

(defvar *default-ignore-behavior* nil
  "Whether to ignore all headings by default.
Defaults to nil (count all headings by default).")

(defvar *day-start-time* 4
  "The time a day is considered to start (0-23).
For the purpose of this program, the concept of a day may be more accurately
thought of as a period of being awake or as a writing session.")

(defvar *base-store-dir* ".chronicler/"
  "The path to store statistics in.
It can be relative to the current directory (that of the file being analyzed)
or absolute.")
