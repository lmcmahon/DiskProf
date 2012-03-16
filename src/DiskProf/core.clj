(ns DiskProf.core
  ;(:use 'seesaw.core)
  )
(:use 'seesaw.core)
(import 'java.io.File)

(defrecord file-rec [file size children])

(defn make-file-rec [file]
  "Takes a java.io.File and returns a file-rec"
  (let [children (seq (.listFiles file))
	child-dirs (filter #(.isDirectory %) children)
	child-files (filter #(.isFile %) children)
	child-file-recs (map make-file-rec child-dirs)]
    (file-rec. file
	       (reduce + (.length file)
		       (concat (map #(.length %) child-files)
			     (map :size child-file-recs)))
	       child-dirs)))

