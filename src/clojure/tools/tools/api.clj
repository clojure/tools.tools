;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns clojure.tools.tools.api
  "This api provides functions that can be executed from the Clojure tools using -Ttools."
  (:refer-clojure :exclude [list remove])
  (:require
    [clojure.pprint :as pprint]
    [clojure.tools.deps.alpha :as deps]
    [clojure.tools.deps.alpha.tool :as tool]
    [clojure.tools.deps.alpha.extensions :as ext]
    [clojure.tools.deps.alpha.extensions.git :as git]
    [clojure.tools.gitlibs :as gitlibs]))

(defn install
  "Install a tool for later use, taking the following required fields:
    lib - lib name, and coord value
    :as - tool name
  On install, the tool is procured, and persisted with the tool name for later use."
  [{:keys [as] :as args}]
  (let [lib (first (filter qualified-symbol? (keys args)))
        coord (get args lib)]
    (when (or (not lib) (not coord) (not as))
      (println "Missing required args: lib to coord or :as"))
    (let [{:keys [root-edn user-edn]} (deps/find-edn-maps)
          master-edn (deps/merge-edns [root-edn user-edn])
          coord (let [{:git/keys [url sha tag]} coord
                      url (if (nil? url) (git/auto-git-url lib) url)]
                  (cond-> coord
                    (and tag (nil? sha))
                    (assoc :git/sha (gitlibs/resolve url tag))))
          [lib coord] (ext/canonicalize lib coord master-edn)]
      (when-not (and lib coord)
        (throw (ex-info (format "Could not resolve tool: %s" (pr-str args)) args)))
      (tool/install-tool lib coord as)
      (println "Installed" as))))

(defn- max-len
  [vals]
  (apply max (map #(count (str %)) vals)))

(defn- pad
  [s len]
  (format (str "%-" len "s") s))

(defn- print-table
  [ks rows]
  (let [widths (map (fn [k] (+ 2 (max-len (map k rows)))) (butlast ks))]
    (run!
      (fn [row]
        (let [padded-cols (map #(pad (get row %1) %2) ks widths)]
          (println (str (apply str padded-cols) (get row (last ks))))))
      rows)))

(defn list
  "List available tools"
  [_]
  (print-table [:tool :lib :type :version]
    (cons
      {:tool "TOOL" :lib "LIB" :type "TYPE" :version "VERSION"}
      (map (fn [tool]
             (let [{:keys [lib coord]} (tool/resolve-tool tool)
                   ctype (ext/coord-type coord)
                   {mver :mvn/version gtag :git/tag} coord]
               {:tool tool :lib lib :type ctype :version (or mver gtag)}))
        (tool/list-tools)))))

(defn show
  "Print info and usage for this :tool"
  [{:keys [tool] :as args}]
  (if-let [{:keys [lib coord] :as info} (tool/resolve-tool tool)]
    (do
      (binding [*print-namespace-maps* false]
        (pprint/pprint info))
      (let [{:keys [ns-default ns-aliases]} (tool/usage tool)]
        (println "Default namespace: " ns-default)
        (doseq [[a n] ns-aliases]
          (println "Namespace alias: " a))))))

(comment
  (show {:tool 'tools})
  )

(defn remove
  "Remove :tool, if it exists."
  [{:keys [tool] :as args}]
  (if tool
    (if (tool/remove-tool tool)
      (println "Tool removed")
      (println "Tool not found or could not be removed"))))

(comment
  (install '{io.github.seancorfield/clj-new
                  {:git/tag "v1.1.243"} :as "clj-new"})

  (list nil)
  )
