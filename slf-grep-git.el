
(defvar slf-grep-max-lines 400 "Do not show matches when there are more than this number.")
(defvar slf-grep-max-chars 200 "Do not search for text longer than this value.")
(defvar slf-grep-git-root nil "The default git root folder. Used when the folder cannot be found automatically.")

(defun slf-grep-git()
"grep for text in source files.

The search text comes from the current selection, or when no
selection, you type it in. The typed in text is treated as a
regular expression and the selected text is treated as a fixed
string.

The matching lines are appended to a buffer called slf-grep-<num>.

The source files to search come from the git ls-files
command. The ls-files command is run from the git root directory
so by default all source files of the project are searched.

The command finds the git root directory by looking relative to
the current buffer. So you can have multiple git projects on your
machine and the correct set of files will be searched. Each
project gets its own results buffer.

If you have files in your project that you do not want to search,
add them to the ignore.txt file. The files in ignore.txt are
skipped. Enter one file or pattern per line. The ignore.txt file
in the git root folder is used when it exists, else the one in
the home folder is used.

If the number of matching lines is more than
slf-grep-max-lines (400), the lines are not appended to the
results buffer. If the number of characters is greater than
slf-grep-max-chars (200), the command is ignored. You can
configure these values if you want.

The command runs the shell command:

git ls-files | grep -v -f ignore.txt | xargs grep -n
"
  (interactive)
  (let (word buffer buffer-name cmd num-lines temp-buffer
             ignore-file git-root use-regex qword searcher switches)
    (catch 'slf-grep-tag
      ;; Get the git root folder or prompt for it.
      (setq slf-grep-git-root (slf-find-root-folder))
      (when (not slf-grep-git-root)
        (message "no git root folder")
        (throw 'slf-grep-tag nil))

      ;; Get the selected text or prompt for it.  The selected text is
      ;; treated as a fixed string and it is quoted. Typed in text is
      ;; treated as a regular expression and is not quoted.  You can
      ;; add extra switches this way and you can quote or not.
      (if mark-active
        (progn
          (setq use-regex nil)
          (slf-mark-then-point)
          (setq word (slf-read-region)))
        (setq use-regex t)
        (setq word (read-from-minibuffer "grep for: ")))

      ;; Make sure the search word is not too long.
      (when (or (equal word "") (> (length word) slf-grep-max-chars))
        (message (format "To many characters selected. Select less than %d (slf-grep-max-chars)."
                         slf-grep-max-chars))
        (throw 'slf-grep-tag nil))

      ;; Name the buffer. Base the name off the root directory so it is
      ;; unique but always the same.
      (setq buffer-name (concat "slf-grep-" (substring (md5 slf-grep-git-root) 0 8) ".txt"))

      ;; Open the results buffer and append to it.
      (setq results-buffer (get-buffer-create buffer-name))
      (set-buffer results-buffer)
      (setq default-directory slf-grep-git-root)
      (end-of-buffer)
      (insert (format "\n\nGrepping for: %s\n" word))

      ;; Put the info in a temp buffer. Then the search word can be
      ;; highlighted and rows counted before commiting to the
      ;; results buffer.
      (setq temp-buffer (generate-new-buffer "slf-grep-temp"))
      (set-buffer temp-buffer)

      ;; The subprocess uses the default-directory as its current
      ;; directory. Set it to the git root directory.
      (setq default-directory slf-grep-git-root)

      ;; Look for the ignore.txt file first in the git root directory
      ;; then in the home dir.
      (setq ignore-file "ignore.txt")
      (if (not (file-exists-p ignore-file))
        (setq ignore-file "~/ignore.txt"))

      ;; Quote the search word when using a selection.
      (if use-regex
        (progn
          (setq switches "")
          (setq qword word))
        (setq switches " -F")
        (setq qword (concat "\"" word "\"")))

      ;; Run the shell commands.
      (setq cmd (format "git ls-files | grep -v -f %s | xargs grep -n%s %s" ignore-file switches qword))
      (message cmd)
      (call-process "/bin/bash" nil temp-buffer nil "-c" cmd)

      ;; Count the lines in the buffer.  If too many, don't add them to
      ;; the grep buffer.
      (setq num-lines (count-lines (point-min) (point-max)))
      (if (> num-lines slf-grep-max-lines)
        (progn
          (set-buffer results-buffer)
          (insert (format "There are more than %d (slf-grep-max-lines) matching lines.\n"
                          slf-grep-max-lines)))

        (defun searcher (use-regex word)
          (if use-regex
            (search-forward-regexp word nil t)
            (search-forward word nil t)))

        ;;;; Color the words in the buffer.
        (goto-char (point-min))
        (while (funcall #'searcher use-regex word)
          (put-text-property (match-beginning 0) (point) 'face '(:foreground "red")))

        ;; Insert the lines into the grep buffer.
        (set-buffer results-buffer)
        (insert (format "%d lines\n" num-lines))
        (insert-buffer-substring temp-buffer)
        (insert (format "%d lines\n" num-lines)))
        (setq default-directory slf-grep-git-root)

      (kill-buffer temp-buffer)
      (switch-to-buffer results-buffer))))


(defun slf-folder-part(path)
" Return the folder of the given path.
"
  ;; /home/steve/code/file.py ->
  ;; /home/steve/code/ ->
  ;; /home/steve/ ->
  ;; /home/ ->
  ;; / ->
  ;; nil
  (if (equal (substring path -1) "/")
    (file-name-directory (substring path 0 -1))
    (file-name-directory path)))


(defun slf-look-for-git()
" Look for the .git folder relative to the current file. Return
  the folder or nil when not found.
"
  (let (root folder name)
    (setq root nil)
    (setq name (buffer-file-name))
    (when name
      (setq folder (slf-folder-part name))
      (while folder
        (if (file-exists-p (concat folder ".git"))
          (progn
            (setq root folder)
            (setq folder nil))
          (setq folder (slf-folder-part folder))))
      root)))


(defun slf-find-root-folder()
" Return the git root folder to use for searching or nil when nothing can be found.
"
  (let (git-root)
    ;; Look for the git root folder relative to the current file.
    (setq git-root (slf-look-for-git))

    ;; When the git root folder cannot be found, use the last one
    ;; found.
    (when (not git-root)
      (setq git-root slf-grep-git-root))

    ;; If no last one, prompt for it.
    (when (not git-root)
      (setq git-root (read-from-minibuffer "git root folder: ")))

    (setq slf-grep-git-root git-root)
    slf-grep-git-root))


(when (>= emacs-major-version 24)
  (defvar run-test-code nil)

  (when run-test-code
    (assert (equal (slf-folder-part "/home/steve/code/file.py") "/home/steve/code/"))
    (assert (equal (slf-folder-part "/home/steve/code/") "/home/steve/"))
    (assert (equal (slf-folder-part "/home/steve/") "/home/"))
    (assert (equal (slf-folder-part "/home/") "/"))
    (assert (equal (slf-folder-part "/") nil))
    (assert (equal (slf-folder-part "/home/steve/code/y") "/home/steve/code/")))

  (when run-test-code
    (assert (equal (slf-look-for-git) "/home/steve/emacs/")))

  (when run-test-code
    (assert (equal (slf-find-root-folder) "/home/steve/emacs/")))

  (when run-test-code
    (assert (equal (md5 "/home/steve/emacs/") "e459440a8d79d467f39ea3e9fbeb8c37")))

  (when run-test-code
    (assert (equal (substring (md5 "/home/steve/emacs/") 0 8) "e459440a"))))
