(import ./sbb :as s)
(import ../bin/tweake :as t)

(defn install
  [manifest &]
  (s/ddumpf "bundle script: %s hook" "install")
  (def [tos s] (s/get-os-stuff))
  (s/add-binscripts manifest [tos s]))

(defn check
  [&]
  (s/ddumpf "bundle script: %s hook" "check")
  (s/run-tests))

# cwd is project root
(defn prep
  [&]
  (s/ddumpf "bundle script: %s hook" "prep")
  # patch remarkable.janet
  (def target-path "src/remarkable/remarkable.janet")
  (try
    (do
      (pp [:hi-there target-path])
      (def src (slurp target-path))
      # expect to find in src:
      #
      #   (import ./renderers/html)
      #           ^^^^^^^^^^^^^^^^
      #
      # i.e. the fourth top-level form of file, 1st argument
      # should be a symbol with name `./renderers/html`
      (def [found-value _ _] (t/peek src 4 [1]))
      (assertf (= './renderers/html found-value)
               "unexpected import path in source code, check: %s"
               target-path)
      (def new-src (t/tweak src 4 [0] "comment"))
      # 6th form:
      #
      #   (defn render-html ...)
      #
      # needs to be commented out
      (def [found-value-2 _ _] (t/peek new-src 6 [1]))
      (assertf (= 'render-html found-value-2)
               "did not find symbol render-html, check: %s"
               target-path)
      (def new-new-src (t/tweak new-src 6 [0] "comment"))
      (spit target-path new-new-src))
    ([e]
      (eprintf "bundle script: prep hook error\n  %s" e))))

