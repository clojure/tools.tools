(ns clojure.tools.tools.api-test
  (:require
    [clojure.string :as str]
    [clojure.test :refer :all]
    [clojure.tools.tools.api :as sut]))

(def tool-install-name "temporary-ci-test-tool")

(deftest install-update-list-and-remove
  (testing "Install specific version that is not the latest"
    (let [expected-response (str tool-install-name ": Installed com.github.seancorfield/deps-new v0.4.9")
          actual-response (with-out-str (sut/install {'com.github.seancorfield/deps-new {:git/url "https://github.com/seancorfield/deps-new"
                                                                                         :git/tag "v0.4.9"} :as tool-install-name}))]
      (is (str/includes? actual-response expected-response))))

  (testing "list installed tools"
    (let [expected-response (re-pattern (str tool-install-name "\\s*com\\.github\\.seancorfield/deps-new\\s*:git\\s*v0\\.4\\.9"))
          actual-response (with-out-str (sut/list nil))]
      (is (re-find expected-response actual-response))))

  (testing "update installed tool to latest version"
    ;; Would be more desirable if we had a test tool to install and could check the specific version that wouldn't change
    (let [expected-response (re-pattern (str tool-install-name ": Installed com\\.github\\.seancorfield/deps-new"))
          actual-response (with-out-str (sut/install-latest {:tool tool-install-name}))]
      (is (re-find expected-response actual-response))))

  (testing "list installed tools reflects updated version"
    (let [old-tool (re-pattern (str tool-install-name "\\s*com\\.github\\.seancorfield/deps-new\\s*:git\\s*v0\\.4\\.9"))
          expected-response (re-pattern (str tool-install-name "\\s*com\\.github\\.seancorfield/deps-new"))
          actual-response (with-out-str (sut/list nil))]
      (is (not (re-find old-tool actual-response)) "The old tool was not successfully updated")
      (is (re-find expected-response actual-response))))

  (testing "remove installed tool"
    (let [removal-response (with-out-str (sut/remove {:tool tool-install-name}))
          list-response (with-out-str (sut/list nil))]
      (is (str/includes? removal-response "Tool removed"))
      (is (not (str/includes? list-response tool-install-name)) "The tool was not successfully removed"))))

(deftest install-latest-git-procurer-with-provided-coord
  (testing "Installed latest version of tool, for git procurer providing coord"
    (let [expected-response (re-pattern (str tool-install-name ": Installed com\\.github\\.seancorfield/deps-new"))
          actual-response (with-out-str (sut/install-latest {:lib 'com.github.seancorfield/deps-new
                                                             :coord {:git/url "https://github.com/seancorfield/deps-new"}
                                                             :as tool-install-name}))]
      (is (re-find expected-response actual-response))))

  (testing "remove installed tool"
    (let [removal-response (with-out-str (sut/remove {:tool tool-install-name}))
          list-response (with-out-str (sut/list nil))]
      (is (str/includes? removal-response "Tool removed"))
      (is (not (str/includes? list-response tool-install-name)) "The tool was not successfully removed"))))