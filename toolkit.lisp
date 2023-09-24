(in-package #:org.shirakumo.fraf.trial.space)

(declaim (inline clamp))
(defun clamp (min x max)
  (max min (min x max)))

(declaim (inline ensure-function))
(defun ensure-function (function-designator)
  (etypecase function-designator
    (function function-designator)
    (symbol (fdefinition function-designator))))
