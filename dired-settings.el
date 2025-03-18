(setq dired-recursive-deletes 'always)

(setq dired-listing-switches "-alh")

(setq dired-auto-revert-buffer t)

(setq dired-guess-shell-alist-user
      '(("\\.tar\\.gz\\'"
         "b=$(basename ? .tar.gz); mkdir -p \"$b\"; tar xzf ? -C \"$b\"")
        ("\\.tar\\.bz2\\'"
         "b=$(basename ? .tar.bz2); mkdir -p \"$b\"; tar xjf ? -C \"$b\"")
        ("\\.tar\\.xz\\'"
         "b=$(basename ? .tar.xz); mkdir -p \"$b\"; tar xJf ? -C \"$b\"")
        ("\\.tar\\'"
         "b=$(basename ? .tar); mkdir -p \"$b\"; tar xf ? -C \"$b\"")
        ("\\.zip\\'"
         "b=$(basename ? .zip); mkdir -p \"$b\"; unzip ? -d \"$b\"")
        ("\\.rar\\'"
         "b=$(basename ? .rar); mkdir -p \"$b\"; unrar ? \"$b\"")))

(add-to-list 'dired-guess-shell-alist-user
             '("\\.jpg\\'"
               "xdg-open ?"))

(setq dired-dwim-target t)

(setq dired-recursive-copies 'always)
