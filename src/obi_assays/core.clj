(ns obi-assays.core
  (:require [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            clojure.set)
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
(def obi-path "obi-merged.owl")
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
       (map (fn [x] [(string/replace x "'" "") x]))
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
          (map #(string/replace % #"^.*EquivalentTo " ""))
          (map #(string/replace % "\n" "\n  "))
          sort-axioms
          (map (partial str "equivalent to: ")))
     (->> (.getSubClassAxiomsForSubClass ontology cls) 
          (map #(.render renderer (.getSuperClass %)))
          (map #(string/replace % "\n" "\n  "))
          sort-axioms
          (map (partial str "subclass of: "))))))

; Define the annotations to report, in preferred order.

(def annotation-properties
  [["label"                  "http://www.w3.org/2000/01/rdf-schema#label"]
   ["alternative term"       (obo "IAO_0000118")]
   ["FGED alternative term"  (obo "OBI_9991119")]
   ["IEDB alternative term"  (obo "OBI_9991118")]
   ["ISA alternative term"   (obo "OBI_0001847")]
   ["NIAID GSCID-BRC alternative term" (obo "OBI_0001886")]
   ["definition"             (obo "IAO_0000115")]
   ["definition source"      (obo "IAO_0000119")]
   ["example of usage"       (obo "IAO_0000112")]
   ["editor note"            (obo "IAO_0000116")]
   ["curator note"           (obo "IAO_0000232")]
   ["term editor"            (obo "IAO_0000117")]
   ["has curation status"    (obo "IAO_0000114")]])

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
       (filter string?)))

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
  [template value]
  (when (and
         (not (string/blank? template))
         (not (string/blank? value)))
    (string/replace
     (string/replace template "C " "")
     "%"
     (quote-string value))))

(defn render-row-logic
  "Given a row from the TSV file,
   return a sequence of logic strings."
  [templates row]
  (->> templates
       (filter #(.startsWith (val %) "C "))
       (map #(apply-template (val %) (get row (key %))))
       (remove string/blank?)
       (map (partial str "subclass of: "))
       sort-axioms))


;; ## Report Stuff

(defn report-difference
  [before after]
  (let [before (set before)
        after  (set after)]
    (->> (clojure.set/union before after)
         sort
         (map
          (fn [item]
            (cond
              (and (before item) (after item)) (str "  " item)
              (before item) (str "- " item)
              (after item) (str "+ " item))))
         (map #(string/replace % #"(?m)\r?\n" "\n    "))
         sort)))

(defn report-annotation-difference
  [row iri property]
  (report-difference
   (->> (render-annotation property iri)
        (map (partial str property ": ")))
   (->> (string/split (get row property) #"\|")
        (remove string/blank?)
        sort
        (map (partial str property ": ")))))

(defn report-logic-difference
  [templates row iri]
  (report-difference
   (render-axioms iri)
   (render-row-logic templates row)))

(defn report-differences
  "Given the template row and a term row,
   compare the term in the ontology (before)
   to the term in the table (after).
   If their logic differs, return a report."
  [templates row]
  (let [ontid (string/replace (get row "ID") ":" "_") 
        iri   (IRI/create (obo ontid))]
    [ontid (get row "label") 
     (->> (concat
           [iri
            (str "Reviewed: " (get row "reviewed"))
            (when-not (string/blank? (get row "review comments"))
              (str "Review Comments: " (get row "review comments")))
            (str "Curation Status: " (get row "has curation status"))
            ""]
           (mapcat
            (partial report-annotation-difference row iri)
            (map first (butlast annotation-properties)))
           [""]
           (report-logic-difference templates row iri))
          (remove nil?)
          (string/join "\n"))]))


;; ## Table Stuff

(defn read-rows
  "Given a path to a TSV file, return a sequence of sequences."
  [path]
  (with-open [in-file (io/reader path)]
    (doall
     (csv/read-csv in-file))))

(defn process-table
  "Given a path to a TSV file of assays,
   compare the term in the ontology (before)
   to the term in the table (after).
   If their logic differs, write a report."
  [path]
  (let [rows      (read-rows path)
        headers   (first rows)
        templates (zipmap headers (second rows))]
    (->> rows
         (drop 2) ; drop headers
         ;(take 10) ; testing
         (map (partial zipmap headers))
         ;(remove #(= (get % "reviewed") "TRUE"))
         (map (partial report-differences templates))
         (remove nil?)
         (map (fn [[ontid label report]]
                (spit
                 (str "reports/" ontid ".txt")
                 report)
                ontid))
         doall)))

(defn -main [& args]
  (process-table "assays.csv"))
