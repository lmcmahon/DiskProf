(ns DiskProf.core
  (:gen-class)
  )
(use 'seesaw.core)
(import 'java.io.File)
(import 'javax.swing.JFileChooser)
					;(import 'javax.swing.JPanel)

(native!)

(defn pos-listener [proportion]
  [:component-resized (fn [e] (config! e :divider-location proportion))])

(defprotocol Pfile-rec (def-button [_]) (def-expanded-rep [this] [this rem-size rem-children]) (def-rep [this]))

(defrecord filerec [name size children rep]
  Pfile-rec
  ;;must not be called before def-rep
  (def-button [this]
    (when (seq children)
      (let [b (button :text "Expand")]
 	(listen b :action (fn [e]
 			    (config! @rep :items [(def-expanded-rep this)])))
 	(config! @rep :items [b])
 	b)))
  (def-expanded-rep [this rem-size rem-children]
    (let [[f s & r] rem-children]
      (if s
 	(if (seq r)
 	  (left-right-split (def-rep f) (def-expanded-rep this (- rem-size (:size f)) (cons s r)) :listen (pos-listener (/ (:size f) rem-size)))
 	  (left-right-split (def-rep f) (def-rep s) :listen (pos-listener (/ (:size f) rem-size))))
 	(def-rep f))))
  (def-expanded-rep [this] (def-expanded-rep this size children))
  (def-rep [this]
    (swap! rep (fn [_] (grid-panel :border name)))
    (when-let [item (def-button this)]
      (config! @rep :items [item]))
    @rep))

;; (defn make-expanded-rep [size childs]
;;   (delay (if-let [n (first childs)]
;; 	   (left-right-split (:rep n)
;; 			     @(make-expanded-rep (- size (:size n)) (rest childs))
;; 			     :listen (pos-listener (/ (:size n) size))))))

;; (defn make-button [rec]
;;   (if (:expanded-rep rec)
;;     (let [b (button :text "Expand")
;; 	  grid (:rep rec)]
;;       (listen b :action (fn [e]
;; 			  (config! grid :items [@(:expanded-rep rec)])))
;;       (config! grid :items [b])
;;       rec)
;;     rec))

(defn ^filerec make-file-rec [^File file]
  "Takes a java.io.File and returns a file-rec"
  (let [child-recs (map make-file-rec (seq (.listFiles file)))
	child-sorted (sort #(> (:size %) (:size %2)) child-recs)
	size (reduce + (.length file) (map :size child-recs))]
    (filerec. (.getName file) size child-sorted (atom nil))))

(defn run [root]
  (let [root-rec (make-file-rec root)
	f (frame :title "Disk Profiler"
		 :content (def-rep root-rec)
		 :on-close :exit
		 )]
    (-> f pack! show!)))

(defn -main [& args]
  (let [chooser (JFileChooser.)]
    (.setFileSelectionMode chooser JFileChooser/FILES_AND_DIRECTORIES)
    (let [result (.showOpenDialog chooser nil)
	  file (.getSelectedFile chooser)]
      (if (= result JFileChooser/APPROVE_OPTION)
	(run file)))))