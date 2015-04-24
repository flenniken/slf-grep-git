

(ert-deftest slf-folder-part-test ()
  "Test slf-folder-part"
  (should (equal (slf-folder-part "/home/steve/code/file.py") "/home/steve/code/"))
  (should (equal (slf-folder-part "/home/steve/code/") "/home/steve/"))
  (should (equal (slf-folder-part "/home/steve/") "/home/"))
  (should (equal (slf-folder-part "/home/") "/"))
  (should (equal (slf-folder-part "/") nil))
  (should (equal (slf-folder-part "/home/steve/code/y") "/home/steve/code/")))


(ert-deftest slf-md5-test ()
  "Test md5"
  (should (equal (md5 "/home/steve/emacs/") "e459440a8d79d467f39ea3e9fbeb8c37")))

(ert-deftest slf-md5-test2 ()
  "Test md5 and substring"
  (should (equal (substring (md5 "/home/steve/emacs/") 0 8) "e459440a")))

(ert-deftest slf-look-for-git-test ()
  "Test slf-look-for-git"
  (should (equal (slf-look-for-git "/home/steve/code/slf-grep-git/path/hello.el") 
                 "/home/steve/code/slf-grep-git/"))
  (should (equal (slf-look-for-git "/home/steve/code/slf-grep-git/hello.el") 
                 "/home/steve/code/slf-grep-git/"))
  (should (equal (slf-look-for-git "/home/steve/code/metar/test-files/saometing.py") 
                 "/home/steve/code/metar/"))
  (should (equal (slf-look-for-git "/not/found/") nil)))


(ert-deftest slf-slf-grep-test ()
  "Test slf-grep"
  (let (temp-buffer)
    (setq temp-buffer (slf-grep "/home/steve/code/slf-grep-git/" nil "hello"))
    (kill-buffer temp-buffer)))

