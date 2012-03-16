(ns DiskProf.core
  ;(:use 'seesaw.core)
  )
(use 'seesaw.core)
(import 'java.io.File)
;(import 'javax.swing.JPanel)

(defrecord file-rec [file size children])

(defn make-file-rec [file]
  "Takes a java.io.File and returns a file-rec"
  (let [child-recs (map make-file-rec (seq (.listFiles file)))]
    (file-rec. file
	       (reduce + (.length file) (map :size child-recs))
	       child-recs)))

