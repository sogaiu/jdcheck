(import ../src/remarkable/remarkable :as r)
(import ../src/cm-zipper :as c)

(comment

  (def sample
    ````
    # Nice Title

    Some _background_ text.

    * Item one
    * Item two
      * Subitem A
      * Subitem V
    * Item three

    ---

    ```
    # fenced

    (def a 1)

    (defn f [x] (+ x 8))
    ```

        # indented

        (var s 11)

    He **said**:

    > You must have goofed up somewhere!
    > ...or something :)
    ````)

  (def tree
    (r/parse-md sample))

  tree
  # =>
  [:document @{:container? true :open? true}
   @[[:heading @{:inlines? true :kind :atx
                 :level 1 :open? false}
      @[@"Nice Title"]]
     [:paragraph @{:inlines? true :open? false}
      @[@"Some " [:emphasis @{} @[@"background"]] @" text."]]
     [:list @{:container? true :has-blank? true
              :kind :bullet :marker "*"
              :open? false :tight? true}
      @[[:list-item @{:container? true :open? false :width 2}
         @[[:paragraph @{:inlines? true :open? false} @[@"Item one"]]]]
        [:list-item @{:container? true :open? false :width 2}
         @[[:paragraph @{:inlines? true :open? false} @[@"Item two"]]
           [:list @{:container? true :kind :bullet
                    :marker "*" :open? false
                    :tight? true}
            @[[:list-item @{:container? true :open? false :width 2}
               @[[:paragraph @{:inlines? true :open? false}
                  @[@"Subitem A"]]]]
              [:list-item @{:container? true :open? false :width 2}
               @[[:paragraph @{:inlines? true :open? false}
                  @[@"Subitem V"]]]]]]]]
        [:list-item @{:container? true :open? false :width 2}
         @[[:paragraph @{:inlines? true :open? false}
            @[@"Item three"]]]]]]
     [:thematic-break
       @{:char 45}
       @["---"]]
     [:codeblock @{:delim "`" :indent 0
                   :kind :fenced :num 3
                   :open? false}
      @["# fenced\n" "\n" "(def a 1)\n" "\n" "(defn f [x] (+ x 8))\n"]]
     [:codeblock @{:kind :indented :open? false}
      @["# indented\n" "\n" "(var s 11)\n"]]
     [:paragraph @{:inlines? true :open? false}
      @[@"He " [:strong @{} @[@"said"]] @":"]]
     [:blockquote @{:container? true :open? false}
      @[[:paragraph @{:inlines? true :open? false}
         @[@"You must have goofed up somewhere!\n...or something :)"]]]]]]

  (def [the-node the-state]
    (c/zip tree))

  the-node
  # =>
  tree

  (merge {} the-state)
  # =>
  @{}

  (-> (c/zip tree)
      c/down
      c/node)
  # =>
  [:heading @{:inlines? true :kind :atx
              :level 1 :open? false}
   @[@"Nice Title"]]

  (-> (c/zip tree)
      c/down
      c/down
      c/node)
  # =>
  @"Nice Title"

  (-> (c/zip tree)
      c/down
      c/right
      c/node)
  # =>
  [:paragraph @{:inlines? true :open? false}
   @[@"Some " [:emphasis @{} @[@"background"]] @" text."]]

  (-> (c/zip tree)
      c/down
      c/right
      c/right
      c/down
      c/node)
  # =>
  [:list-item @{:container? true :open? false :width 2}
   @[[:paragraph @{:inlines? true :open? false}
      @[@"Item one"]]]]

  )

(comment

  (c/attrs (c/zip tree))
  # =>
  @{:container? true :open? true}

  )

(comment

  (-> (c/zip-down tree)
      c/node)
  # =>
  [:heading @{:inlines? true :kind :atx :level 1 :open? false}
   @[@"Nice Title"]]

  )

(comment

  # start at :document
  (var t-zloc (c/zip tree))

  (def node-types @[])

  # first visited node ends up being :heading for the example
  (while (def a-zloc (c/df-next t-zloc))
    (when (c/end? a-zloc)
      (break))
    #
    (def a-node (c/node a-zloc))
    (def head (get a-node 0))
    (when (keyword? head)
      (array/push node-types head))
    (set t-zloc a-zloc))

  node-types
  # =>
  @[:heading
    :paragraph
    :emphasis
    :list
    :list-item
    :paragraph
    :list-item
    :paragraph
    :list
    :list-item
    :paragraph
    :list-item
    :paragraph
    :list-item
    :paragraph
    :thematic-break
    :codeblock
    :codeblock
    :paragraph
    :strong
    :blockquote
    :paragraph]

  )

