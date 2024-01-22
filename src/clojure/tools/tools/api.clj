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
    [clojure.tools.deps :as deps]
    [clojure.tools.deps.tool :as tool]
    [clojure.tools.deps.extensions :as ext]
    [clojure.tools.deps.extensions.git :as git]
    [clojure.tools.gitlibs :as gitlibs]
    [clojure.string :as str]))

(defn install
  "Install a tool under a local tool name for later use. On install, the tool is procured, and
  persisted with the tool name for later use.

  Options:
    lib-name (required) - value is coord map (git coords may omit sha)
    :as (required) - tool name

  Example:
    clj -Ttools install io.github.clojure/tools.deps.graph '{:git/tag \"v1.0.63\"}' :as deps-graph

  Also see:
    clj -X:deps find-versions :lib <lib>"
  [{:keys [as] :as args}]
  (let [lib (first (filter qualified-symbol? (keys args)))
        coord (get args lib)]
    (when (or (not lib) (not coord) (not as))
      (throw (ex-info "Missing required args, syntax: clj -Ttools install lib-name coord :as tool-name" (or args {}))))
    (let [{:keys [root-edn user-edn]} (deps/find-edn-maps)
          master-edn (deps/merge-edns [root-edn user-edn])
          coord (let [{:git/keys [url sha tag]} coord
                      url (if (nil? url) (git/auto-git-url lib) url)]
                  (cond-> coord
                    (and tag (nil? sha))
                    (assoc :git/sha (gitlibs/resolve url tag))))
          [lib coord] (ext/canonicalize lib coord master-edn)]
      (when-not (and lib coord)
        (throw (ex-info (format "Could not resolve tool: %s" (pr-str args)) (or args {}))))
      (tool/install-tool lib coord as)
      (println (str as ":") "Installed" (ext/coord-summary lib coord)))))

(defn- parse-install-latest-args
  [{:keys [lib tool as] :as args}]
  (cond (or (zero? (count args)) (and lib as)) args
        tool (let [tool-info (tool/resolve-tool tool)]
               (if tool-info
                 (assoc tool-info :as tool)
                 (throw (ex-info (str "Tool not found: " tool) {}))))
        :else (throw (ex-info "Missing required args, install-latest requires either :tool or both :lib and :as" (or args {})))))

(defn- release-version?
  "Release version does not contain any of:
    a, alpha, b, beta, m, milestone, rc, cr, snapshot"
  [{:keys [git/tag mvn/version]}]
  (let [v (or tag version)]
    (when v
      (let [vl (str/lower-case v)]
        (not
          (boolean
            (some (fn [s] (str/includes? vl s))
              ;; check subset of list in docstring as a is in alpha and snapshot, etc
              ["a" "b" "m" "rc" "cr"])))))))

(defn- install-1
  [lib coord as master-edn]
  (let [current (tool/resolve-tool as)
        coord (or coord (->> (ext/find-all-versions lib (:coord current) master-edn)
                             (filter release-version?)
                             last
                             (merge (:coord current))))]
    (if coord
      (if (and current (= lib (:lib current)) (zero? (ext/compare-versions lib (:coord current) coord master-edn)))
        (println (str as ":") "Skipping, newest installed" (ext/coord-summary lib coord))
        (do
          (tool/install-tool lib coord as)
          (println (str as ":") "Installed" (ext/coord-summary lib coord)
                   (binding [*print-namespace-maps* false]
                     (pr-str coord)))))
      (println (str as ":") "Did not find versions for" lib))))

(defn install-latest
  "Install the latest version of a tool under a local tool name for later use.
  On install, the tool is procured, and persisted with the tool name for later use.

  If :tool is provided, the latest version of that existing tool is updated.
  If :lib and :as are provided, the latest version of that lib will be installed
  with that tool name, replacing any existing tool by that name.
  If no args are provided, install the newest version of all tools.
  Optionally :coord may be provided to accommodate privately hosted repositories
  or other unresolvable urls.

  The latest version is determined by listing the versions in semver order,
  filtering out versions with special strings, and choosing the last one.
  Special strings that cause a version to be ignored are:
      alpha a beta b miletone m rc cr snapshot
  Note that for git deps, the newest tagged version will be installed.

  It is recommended that Maven tool releases use Maven release version
  conventions, and that git tool releases use tags in the format \"vA.B.C\".

  Options:
    :tool tool-name - currently installed tool
    :lib lib-name - mvn lib or git lib with inferrable url
    :coord - map (git coords may omit sha)
    :as - tool name

  Example:
    clj -Ttools install-latest :lib io.github.clojure/tools.deps.graph :as deps-graph
    clj -Ttools install-latest :tool tools

  Also see:
    clj -X:deps find-versions :lib <lib>
    clj -Ttools install <lib> <coord> :as tool-name"
  [args]
  (let [{:keys [lib coord as]} (parse-install-latest-args args)
        {:keys [root-edn user-edn]} (deps/find-edn-maps)
        master-edn (deps/merge-edns [root-edn user-edn])]
    (if lib
      (install-1 lib coord as master-edn)
      (run!
        (fn [tool-name]
          (try
            (let [{:keys [lib]} (tool/resolve-tool tool-name)]
              (install-1 lib coord tool-name master-edn))
            (catch Exception e
              (println (str tool-name ":") "Failed:" (.getMessage e)))))
        (tool/list-tools)))))

(comment
  (tool/list-tools)

  (install-latest {:lib 'io.github.seancorfield/clj-new :as 'new})
  (install-latest {:lib 'io.github.seancorfield/deps-new :as 'new})

  (tool/resolve-tool "new")
  (def master-edn
    (let [{:keys [root-edn user-edn]} (deps/find-edn-maps)]
      (deps/merge-edns [root-edn user-edn])))
  (install-1 'com.github.seancorfield/deps-new "deps-new" master-edn)
  (ext/find-all-versions 'com.github.seancorfield/deps-new nil master-edn)
  (install-latest nil)
  (install {'com.github.seancorfield/deps-new {:git/tag "v0.4.9" :git/sha "ba30a76"} :as "deps-new"})

  (install {'org.clojure/data.json {:mvn/version "0.2.0"} :as "json"})
  (tool/resolve-tool "json")
  (install-latest {:tool "json"})

  (filter release-version? (ext/find-all-versions 'io.github.clj-holmes/clj-watson nil master-edn))
  (filter release-version? (ext/find-all-versions 'org.clojure/clojure nil master-edn))
  (filter release-version? (ext/find-all-versions 'io.github.clojure/data.json nil master-edn))
  )

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
  "List available tools.

  Options:
    none

  Example:
    clj -Ttools list"
  [_]
  (print-table [:tool :lib :type :version]
    (cons
      {:tool "TOOL" :lib "LIB" :type "TYPE" :version "VERSION"}
      (map (fn [tool]
             (let [{:keys [lib coord]} (tool/resolve-tool tool)
                   ctype (ext/coord-type coord)
                   {mver :mvn/version gtag :git/tag gsha :git/sha} coord
                   gsha (if (and gsha (> (count gsha) 7)) (subs gsha 0 7) gsha)]
               {:tool tool :lib lib :type ctype :version (or mver gtag gsha)}))
        (tool/list-tools)))))

(defn show
  "Print info and usage for this :tool.

  Options:
    :tool (required) - tool name to show

  Example:
    clj -Ttools show :tool deps-graph"
  [{:keys [tool] :as args}]
  (when (nil? tool)
    (throw (ex-info "Missing required arg :tool" (or args {}))))
  (if-let [info (tool/resolve-tool tool)]
    (do
      (binding [*print-namespace-maps* false]
        (pprint/pprint info))
      (let [{:keys [ns-default ns-aliases]} (tool/usage tool)]
        (println "Default namespace: " ns-default)
        (doseq [[a _n] ns-aliases]
          (println "Namespace alias: " a))))
    (println (str "Tool not found: " tool))))

(defn remove
  "Remove :tool, if it exists.

  Options:
    :tool (required) - tool name to remove"
  [{:keys [tool] :as args}]
  (when (nil? tool)
    (throw (ex-info "Missing required arg :tool" (or args {}))))
  (if (tool/remove-tool tool)
    (println "Tool removed")
    (println "Tool not found or could not be removed")))
