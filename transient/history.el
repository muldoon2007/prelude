((magit-blame
  ("-w"))
 (magit-branch nil)
 (magit-cherry-pick
  ("--ff"))
 (magit-commit nil
               ("--all"))
 (magit-diff
  ("--no-ext-diff" "--stat")
  (("--" "src/resources/dev-config.edn")
   "--no-ext-diff")
  (("--" "sap-for-mss.js")
   "--no-ext-diff" "--stat")
  nil)
 (magit-dispatch nil)
 (magit-ediff nil)
 (magit-fetch nil)
 (magit-log
  ("-n256" "--graph" "--decorate" "--patch")
  ("-n256" "--graph" "--decorate")
  (("--" "wtdoc-to-wtparts.js"))
  (("--" "project.clj")))
 (magit-merge nil)
 (magit-patch nil)
 (magit-patch-apply nil)
 (magit-patch-create nil)
 (magit-pull nil
             ("--rebase"))
 (magit-push
  ("--force-with-lease")
  nil
  ("--force"))
 (magit-rebase
  ("--interactive")
  nil
  ("--autosquash")
  ("--autosquash" "--autostash"))
 (magit-reset nil)
 (magit-revert
  ("--edit"))
 (magit-stash nil)
 (magit-submodule nil)
 (magit-worktree nil)
 (magit:-- "src/resources/dev-config.edn" "" "sap-for-mss.js"))
