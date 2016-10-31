(ns obi-assays.core
  (:require [clojure.string :as str])
  (:import (org.obolibrary.robot IOHelper OntologyHelper)
           (org.semanticweb.owlapi.model IRI)
           (org.semanticweb.owlapi.apibinding OWLManager)
           (org.semanticweb.owlapi.manchestersyntax.renderer
            ManchesterOWLSyntaxOWLObjectRendererImpl)
           (org.semanticweb.owlapi.reasoner OWLReasonerFactory)
           (org.semanticweb.owlapi.reasoner.structural StructuralReasonerFactory)
           (org.semanticweb.owlapi.util
            ShortFormProvider
            AnnotationValueShortFormProvider))
  (:gen-class))

(defn obo
  [& strings]
  (apply str "http://purl.obolibrary.org/obo/" strings))

(defn quote-string
  [s]
  (if (re-find #"\s" s) (str "'" s "'") s))

;; ## OWL Stuff
; Man, OWLAPI is ugly, even with ROBOT to help.

(defn quoted-provider
  "This is just a wrapper to put single-quotes around short forms
   that contain spaces."
  [short-form-provider]
  (reify ShortFormProvider
    (dispose [_] (.dispose short-form-provider))
    (getShortForm
      [_ entity]
      (quote-string (.getShortForm short-form-provider entity)))))

(def io-helper (IOHelper.))
(def obi-path "/Users/james/Repositories/OBO/OBI/trunk/src/ontology/branches/obi.owl")
(def ontology (.loadOntology io-helper obi-path))
(def manager (.getOWLOntologyManager ontology))
(def data-factory (.getOWLDataFactory manager))
(def reasoner-factory (StructuralReasonerFactory.))
(def reasoner (.createReasoner reasoner-factory ontology))
(def short-form-provider
  (quoted-provider
   (AnnotationValueShortFormProvider.
    [(.getRDFSLabel data-factory)]
    {}
    manager)))
(def renderer (new ManchesterOWLSyntaxOWLObjectRendererImpl))
(.setShortFormProvider renderer short-form-provider)

(defn sort-axioms
  "Sort axioms, ignoring single quotes."
  [axioms]
  (->> axioms
       (map (fn [x] [(str/replace x "'" "") x]))
       sort
       (map second)))

(defn render-axioms
  "Given the IRI for a class in the ontology,
   return a sequence of Manchester string representations
   of its equivalentTo and subClassOf assertions."
  [iri]
  (let [cls (.getOWLClass data-factory iri)]
    (concat
     (->> (.getEquivalentClassesAxioms ontology cls) 
          (map #(.render renderer %))
          (map #(str/replace % #"^.*EquivalentTo " ""))
          (map #(str/replace % "\n" "\n  "))
          sort-axioms
          (map (partial str "equivalent to: ")))
     (->> (.getSubClassAxiomsForSubClass ontology cls) 
          (map #(.render renderer (.getSuperClass %)))
          (map #(str/replace % "\n" "\n  "))
          sort-axioms
          (map (partial str "subclass of: "))))))

; Define the annotations to report, in preferred order.

(def annotation-properties
  [["label"                 "http://www.w3.org/2000/01/rdf-schema#label"]
   ["alternative term"      (obo "IAO_0000118")]
   ["IEDB alternative term" (obo "OBI_9991118")]
   ["definition"            (obo "IAO_0000115")]
   ["definition source"     (obo "IAO_0000119")]
   ["example of usage"      (obo "IAO_0000112")]
   ["editor note"           (obo "IAO_0000116")]
   ["term editor"           (obo "IAO_0000117")]
   ])

(def annotation-property-map
  (->> annotation-properties
       (map (fn [[label iri-string]]
              [label
               (.getOWLAnnotationProperty
                data-factory
                (IRI/create iri-string))]))
       (into {})))

(defn render-annotation
  "Given a property name and a term IRI,
   return a sequence of annotation blocks."
  [property iri]
  (->> (OntologyHelper/getAnnotationStrings
        ontology
        (get annotation-property-map property)
        iri)
       sort
       (map #(str/replace % "\n" "  \n"))
       (map (partial str property ": "))))

(defn render-annotations
  "Given a term IRI,
   return a sequence of annotation blocks."
  [iri]
  (mapcat
    #(render-annotation % iri)
    (map first annotation-properties)))


;; ## Template Stuff

(defn apply-template
  "Given a ROBOT template string and a value,
   substitute the value into the template
   and return the result."
  [[template value]]
  (str/replace
   (str/replace template "C " "")
   "%"
   (quote-string value)))

(defn render-row
  "Given a row from the TSV file,
   return a sequence of logic strings."
  [templates row]
  (->> (map vector templates row)
       (remove #(str/blank? (second %)))
       (filter #(.startsWith (first %) "C "))
       (map apply-template)
       (map (partial str "subclass of: "))
       sort-axioms))


;; ## Report Stuff

(defn render-report
  "Given an IRI, and before-and-after logic strings,
   return a string with the term's annotations
   the before logic and the after logic."
  [iri before after]
  (str/join
   "\n"
   (concat
    [(.toString iri)]
    (render-annotations iri)
    [""]
    (if (= before after) ["SAME LOGIC"] ["DIFFERENT LOGIC"])
    ["" "BEFORE"]
    before
    ["" "AFTER"]
    after)))

(defn compare-axioms
  "Given the template row and a term row,
   compare the term in the ontology (before)
   to the term in the table (after).
   If their logic differs, return a report."
  [templates row]
  (let [id     (nth row 2)
        ontid  (str/replace id ":" "_") 
        label  (nth row 3)
        iri    (IRI/create (obo ontid))
        before (render-axioms iri)
        after  (render-row templates row)]
    (when-not (= before after)
      [ontid
       label
       (render-report iri before after)])))


;; ## Table Stuff

(defn read-rows
  "Given a path to a TSV file, return a sequence of sequences."
  [path]
  (->> path
       slurp
       str/split-lines
       (remove (partial str/blank?))
       (map #(str/split % #"\t"))))

(defn process-table
  "Given a path to a TSV file of assays,
   compare the term in the ontology (before)
   to the term in the table (after).
   If their logic differs, write a report."
  [path]
  (let [rows      (read-rows path)
        headers   (first rows)
        templates (second rows)]
    (->> rows
         (drop 2) ; drop headers
         (take 10) ; testing
         (remove #(= (nth % 1) "TRUE"))
         (map (partial compare-axioms templates))
         (remove nil?)
         (map (fn [[ontid label report]]
                (spit
                 (str "reports/" ontid ".txt")
                 report)
                ontid))
         doall)))

(defn -main [& args]
  (process-table "assays.tsv"))

