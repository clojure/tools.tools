(ns clojure.tools.tools.api-test
  (:require
    [clojure.edn :as edn]
    [clojure.java.io :as jio]
    [clojure.string :as str]
    [clojure.test :refer :all]
    [clojure.tools.deps.extensions.git :as git]
    [clojure.tools.tools.api :as api])
  (:import
    [java.io File]
    [java.nio.file Files StandardCopyOption]
    [java.util UUID]))

(def tool-install-name "temporary-ci-test-tool")

(deftest install-update-list-and-remove
  (testing "Install specific version that is not the latest"
    (let [expected-response (str tool-install-name ": Installed com.github.seancorfield/deps-new v0.4.9")
          actual-response (with-out-str (api/install {'com.github.seancorfield/deps-new {:git/url "https://github.com/seancorfield/deps-new"
                                                                                         :git/tag "v0.4.9"} :as tool-install-name}))]
      (is (str/includes? actual-response expected-response))))

  (testing "list installed tools"
    (let [expected-response (re-pattern (str tool-install-name "\\s*com\\.github\\.seancorfield/deps-new\\s*:git\\s*v0\\.4\\.9"))
          actual-response (with-out-str (api/list nil))]
      (is (re-find expected-response actual-response))))

  (testing "update installed tool to latest version"
    ;; Would be more desirable if we had a test tool to install and could check the specific version that wouldn't change
    (let [expected-response (re-pattern (str tool-install-name ": Installed com\\.github\\.seancorfield/deps-new"))
          actual-response (with-out-str (api/install-latest {:tool tool-install-name}))]
      (is (re-find expected-response actual-response))))

  (testing "list installed tools reflects updated version"
    (let [old-tool (re-pattern (str tool-install-name "\\s*com\\.github\\.seancorfield/deps-new\\s*:git\\s*v0\\.4\\.9"))
          expected-response (re-pattern (str tool-install-name "\\s*com\\.github\\.seancorfield/deps-new"))
          actual-response (with-out-str (api/list nil))]
      (is (not (re-find old-tool actual-response)) "The old tool was not successfully updated")
      (is (re-find expected-response actual-response))))

  (testing "remove installed tool"
    (let [removal-response (with-out-str (api/remove {:tool tool-install-name}))
          list-response (with-out-str (api/list nil))]
      (is (str/includes? removal-response "Tool removed"))
      (is (not (str/includes? list-response tool-install-name)) "The tool was not successfully removed"))))

(deftest install-latest-git-procurer-with-provided-coord
  (testing "Installed latest version of tool, for git procurer providing coord"
    (let [expected-response (re-pattern (str tool-install-name ": Installed com\\.github\\.seancorfield/deps-new"))
          actual-response (with-out-str (api/install-latest {:lib 'com.github.seancorfield/deps-new
                                                             :coord {:git/url "https://github.com/seancorfield/deps-new"}
                                                             :as tool-install-name}))]
      (is (re-find expected-response actual-response))))

  (testing "remove installed tool"
    (let [removal-response (with-out-str (api/remove {:tool tool-install-name}))
          list-response (with-out-str (api/list nil))]
      (is (str/includes? removal-response "Tool removed"))
      (is (not (str/includes? list-response tool-install-name)) "The tool was not successfully removed"))))

(defn copy-dir [^String src ^String dest]
  (let [dest-dir (.toPath (jio/file dest))]
    (doseq [^File f (.listFiles (jio/file src)) :when (.isFile f)]
      (Files/copy (.toPath f)
        (.resolve dest-dir (.getName f))
        (into-array [StandardCopyOption/REPLACE_EXISTING])))))

(defmacro let-tools-dir
  [[dir-sym dir-path] & body]
  (if (or (System/getenv "CLJ_CONFIG") (System/getenv "XDG_CONFIG_HOME"))
    (println "Skipping let-tools-dir test due to environment")
    `(let [tmp# (jio/file (System/getProperty "java.io.tmpdir") (str (UUID/randomUUID)))
           tools# (jio/file tmp# ".clojure" "tools")
           old-home# (System/getProperty "user.home")
           ~dir-sym (.getAbsolutePath tools#)]
       (try
         (.mkdirs tools#)
         (.deleteOnExit tmp#)
         (copy-dir ~dir-path (.getAbsolutePath tools#))
         (System/setProperty "user.home" (.getAbsolutePath tmp#))
         ~@body
         (finally
           (System/setProperty "user.home" old-home#))))))

;; when new tool has both git and maven versions, prefer git and install
(deftest install-latest-new-mixed
  (let-tools-dir [tools "test-data/empty"]
    (api/install-latest {:lib 'com.github.seancorfield/clj-new
                         :as 'clj-new})
    (let [new-edn (edn/read-string (slurp (jio/file tools "clj-new.edn")))]
      (clojure.pprint/pprint new-edn)
      ;; has a :git/tag
      (is (= 'com.github.seancorfield/clj-new (-> new-edn :lib)))
      (is (= "https://github.com/seancorfield/clj-new.git" (-> new-edn :lib git/auto-git-url)))
      (is (-> new-edn :coord :git/tag))
      (is (-> new-edn :coord :git/sha)))))

;; when out of date tool has both git and maven versions, only consider updates
;; to version with the same coordinate type. here, git is installed
(deftest install-latest-existing-tool-mixed
  (let-tools-dir [tools "test-data/tools1"]
    (let [old-edn (edn/read-string (slurp (jio/file tools "clj-new.edn")))
          old-tag (-> old-edn :coord :git/tag)]
      (api/install-latest {:tool "clj-new"})
      (let [new-edn (edn/read-string (slurp (jio/file tools "clj-new.edn")))
            new-tag (-> new-edn :coord :git/tag)]
        (is new-tag) ;; has a :git/tag
        (is (not= old-tag new-tag)))))) ;; ... that has been updated
