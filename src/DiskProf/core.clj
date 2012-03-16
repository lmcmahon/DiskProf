(ns DiskProf.core
  ;(:use 'seesaw.core)
  )
(use 'seesaw.core)
(import 'java.io.File)
(import 'javax.swing.JFileChooser)
;(import 'javax.swing.JPanel)

(defrecord file-rec [file size children rep expanded-rep])

(defn pos-listener [proportion]
  [:component-resized (fn [e] (do (println proportion) (config! e :divider-location proportion)))])

(defn make-expanded-rep [size childs]
  (delay (if-let [n (first childs)]
	   (left-right-split (:rep n)
			     @(make-expanded-rep (- size (:size n)) (rest childs))
			     :listen (pos-listener (/ (:size n) size))))))

(defn make-button [rec]
  (if (:expanded-rep rec)
    (let [b (button :text "Expand")
	  grid (:rep rec)]
      (listen b :action (fn [e]
			  (config! grid :items [@(:expanded-rep rec)])))
      (config! grid :items [b])
      rec)
    rec))

(defn make-file-rec [file]
  "Takes a java.io.File and returns a file-rec"
  (let [child-recs (map make-file-rec (seq (.listFiles file)))
	child-sorted (sort #(> (:size %) (:size %2)) child-recs)
	size (reduce + (.length file) (map :size child-recs))
	rep (grid-panel :border (.getName file))]
    (make-button (file-rec. file size child-sorted rep
			    (make-expanded-rep size child-sorted)))))

(defn run [root]
  (let [root-rec (make-file-rec root)
	f (frame :title "Disk Profiler"
		 :content @(:expanded-rep root-rec))]))

(defn -main []
  (let [chooser (JFileChooser.)]
    (.setFileSelectionMode chooser JFileChooser/FILES_AND_DIRECTORIES)
    (let [result (.showOpenDialog chooser)
	  file (.getSelectedFile chooser)]
      (if (= result JFileChooser/APPROVE_OPTION)
	(run file)))))