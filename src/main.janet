(import ./args :as a)
(import ./cm-zipper :as c)
(import ./empathy :as e)
(import ./find :as f)
(import ./remarkable/remarkable :as r)
(import ./utils :as u)

(def version "DEVEL")

(def usage
  `````
  Usage: jdcheck <file-or-dir>...

         jdcheck [-h|--help]|[-v|--version]

  Check Janet docstrings.

  Parameters:

    <file-or-dir>          path to file or directory

  Options:

    -h, --help             show this output
    -v, --version          show version information

  Examples:

    Check docstrings in the file `data/has-heading.janet`:

    $ jdcheck data/has-heading.janet

    Check docstrings in `data` directory:

    $ jdheck data
  `````)

########################################################################

(defn check-node
  [a-node]
  (def head (get a-node 0))
  (when (not (keyword? head))
    (break [true nil]))
  #
  (case head
    # unsupported blocks
    :blockquote [false "block quote"]
    :heading [false "heading"]
    :html [false "html block"]
    :linkdef [false "link reference definition"]
    :t-break [false "thematic break"]
    # unsupported inlines
    :autolink [false "autolink"]
    :hardbreak [false "hard line break"]
    :link [false "link"]
    :rawhtml [false "raw html"]
    # partially supported
    :codeblock [(not (and (= :fenced (get-in a-node [1 :kind]))
                          (= "~" (get-in a-node [1 :delim]))))
                "fenced code block with tilde delimiters"]
    # supported
    :document [true nil]
    :blank [true nil]
    :codespan [true nil]
    :emphasis [true nil]
    :list [true nil]
    :list-item [true nil]
    :paragraph [true nil]))

(defn main
  [_ & args]
  (def opts (a/parse-args args))
  #
  (when (get opts :show-help)
    (print usage)
    (os/exit 0))
  #
  (when (get opts :show-version)
    (print version)
    (os/exit 0))
  #
  (def paths (get opts :rest))
  #
  (def src-filepaths (filter |(and (= :file (os/stat $ :mode))
                                   (u/looks-like-janet? $))
                             (e/itemize ;paths)))
  (when (empty? src-filepaths)
    (eprintf "failed to find relevant paths for: %s"
             (string/join paths " "))
    (os/exit 1))
  #
  (each sfp src-filepaths
    (def src (slurp sfp))
    (when (not (empty? src))
      (def results
        (try
          (f/find-docs src)
          ([e]
            (eprint e)
            (eprintf "find-docs failed for: %s" sfp))))
      (each r results
        (def text (get r :text))
        (when (and text (not (empty? text)))
          (def tree (r/parse-md text))
          (var t-zloc (c/zip tree))
          (while (def a-zloc (c/df-next t-zloc))
            (when (c/end? a-zloc)
              (break))
            #
            (def a-node (c/node a-zloc))
            (def [supported? feat-name] (check-node a-node))
            (when (not supported?)
              (def {:bc col :bl line :name name} r)
              (eprintf "%s:%d:%d: docstring of `%s` has: %s"
                       sfp line col name feat-name))
            (set t-zloc a-zloc)))))))

