
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
  (let (word use-regex buffer-name results-buffer temp-buffer folder r)
    ;; Get the git root folder or prompt for it.
    (setq folder (slf-find-root-folder))
    ;; (message "folder = %s" folder)

    ;; Get the word to search and whether to use regex.
    (setq r (slf-get-search-word))
    (setq word (car r))
    (setq use-regex (nth 1 r))
    ;; (message "word = %s" word)
    ;; (message "use-regex = %s" use-regex)
    
    ;; Name the buffer. Base the name off the folder name so it is
    ;; unique but always the same.
    (setq buffer-name (concat "slf-grep-" (substring (md5 folder) 0 8) ".txt"))
    ;; (message "buffer-name = %s" buffer-name)

    ;; Open the results buffer and append to it.
    (setq results-buffer (get-buffer-create buffer-name))
    (set-buffer results-buffer)
    (setq default-directory folder)
    (end-of-buffer)
    (insert (format "\n\nGrepping for: %s\n" word))

    ;; Grep and return a new buffer with the results.
    (setq temp-buffer (slf-grep folder use-regex word))
    (set-buffer temp-buffer)
    (setq num-lines (count-lines (point-min) (point-max)))
    ;; (message "num-lines = %s" num-lines)

    ;; Check for too many lines.
    (if (> num-lines slf-grep-max-lines)
      (progn
        (set-buffer results-buffer)
        (setq default-directory folder)
        (insert (format "Found %d matches which is over the maximum of %d, see slf-grep-max-lines.\n"
                         num-lines slf-grep-max-lines)))
      ;; Color the matching words.
      (slf-color-words use-regex word)
      (set-buffer results-buffer)
      (setq default-directory folder)
      ;; Insert the matching lines into the results buffer.
      (insert (format "%d lines\n" num-lines))
      (insert-buffer-substring temp-buffer)
      (when (> num-lines 0)
        (insert (format "%d lines\n" num-lines))))

    (kill-buffer temp-buffer)
    (switch-to-buffer results-buffer)))


;; (setq slf-grep-git-root nil)


(defun slf-get-search-word()
" Return list containing the word to search and whether it is a regex or not.

Get the selected text or prompt for it.  The selected text is
treated as a fixed string and it is quoted. Typed in text is
treated as a regular expression and is not quoted.  You can add
extra switches this way and you can quote or not.
"
  (interactive)
  (let (word use-regex lword)
    ;; Use the selected text or when nothing selected, prompt for it.
    (if mark-active
      (progn
        (setq use-regex nil)
        (setq word (buffer-substring-no-properties (mark) (point))))
      (setq use-regex t)
      (setq word (read-from-minibuffer "grep for: ")))

    (setq lword (length word))
    (when (equal lword 0)
      (error ""))

    ;; Make sure the search word is not too long.
    (when (> lword slf-grep-max-chars)
      (error (format "You selected %d characters which is over the maximum of %d, see slf-grep-max-chars."
                       lword slf-grep-max-chars)))

    (list word use-regex)))


(defun slf-grep(folder use-regex word)
"Grep for the given word in the files in the given folder using
regex or not. Return a buffer containing the results.
"
  (let (cmd temp-buffer ignore-file qword searcher switches)
    ;; Put the info in a temp buffer. Then the search word can be
    ;; highlighted and rows counted before commiting to the
    ;; results buffer.
    (setq temp-buffer (generate-new-buffer "slf-grep-temp"))
    (set-buffer temp-buffer)
    ;; The subprocess uses the default-directory as its current
    ;; directory. Set it to the git root directory.
    (setq default-directory folder)

    ;; Use the ignore.txt file if it's in the git root directory
    ;; otherwise use the one in the home folder.
    (if (file-exists-p "ignore.txt")
      (setq ignore-file "ignore.txt")
      (setq ignore-file "~/ignore.txt"))
    ;; (message "ignore-file = %s" ignore-file)

    ;; Quote the search word when not using a regex.
    (if use-regex
      (progn
        (setq switches "")
        (setq qword word))
      (setq switches " -F")
      (setq qword (concat "\"" word "\"")))
    ;; (message "switches = %s" switches)
    ;; (message "qword = %s" qword)

    ;; Run the shell commands.
    (setq cmd (format "git ls-files | grep -v -f %s | xargs grep -n%s %s" ignore-file switches qword))
    (message cmd)
    (call-process "/bin/bash" nil temp-buffer nil "-c" cmd)

    temp-buffer))


(defun slf-color-words(use-regex word)
" Color all instances of the given word in the current buffer.
"
  (let (searcher)
    (defun searcher (use-regex word)
      (if use-regex
        (search-forward-regexp word nil t)
        (search-forward word nil t)))

    ;;;; Color the words in the buffer.
    (goto-char (point-min))
    (while (funcall #'searcher use-regex word)
      (put-text-property (match-beginning 0) (point) 'face '(:foreground "red")))))


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


(defun slf-look-for-git(filename)
" Look for the .git folder relative to the given filename. Return
  the folder or nil when not found.
"
  (let (root folder name)
    (setq root nil)
    (setq name filename)
    (when name
      (setq folder (slf-folder-part name))
      ;; (message folder)
      (while folder
        (if (file-exists-p (concat folder ".git"))
          (progn
            (setq root folder)
            (setq folder nil))
          ;; (message folder)
          (setq folder (slf-folder-part folder))))
      root)))


(defun slf-find-root-folder()
" Return the git root folder to use for searching.
"
  (interactive)
  (let (git-root)
    ;; Look for the git root folder relative to the current file.
    (setq git-root (slf-look-for-git (buffer-file-name)))
    ;; (message "git-root = %s" git-root)

    ;; When the git root folder cannot be found, use the last one
    ;; found.
    (when (not git-root)
      (setq git-root slf-grep-git-root)
      ;; (message "git-root = %s" git-root)
      )

    ;; If no last one, prompt for it.
    (when (not git-root)
      (setq git-root (read-from-minibuffer "git root folder: "))
      ;; (message "git-root = %s" git-root)
      (when (equal (length git-root) 0)
        (error ""))
      ;; Add an ending slash if needed.
      (when (not (equal (substring git-root -1) "/"))
        (setq git-root (concat git-root "/"))
        ;; (message "git-root = %s" git-root)
        )
      ;; Make sure the entered directory is really a git root.
      (when (not (file-exists-p (concat git-root ".git")))
        (error (format "'%s' is not a git root directory." git-root))))

    (setq slf-grep-git-root git-root)
    slf-grep-git-root))

(setq slf-grep-git-root nil)
