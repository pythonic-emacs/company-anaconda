;;; company-anaconda-test.el --- company-anaconda test suite

;;; Commentary:

;;; Code:

(defvar counter 0)

(defvar fixtures
  '(("foo" "foo")
    ("def test(x=1" 'stop)
    ("1234." 'stop)
    ("\"test\"." '("\"test\"." . t))))

(dolist (fixture fixtures)
  (cl-destructuring-bind (content expected)
      fixture
    (eval
     `(ert-deftest ,(intern (format "test-company-anaconda-%s" (cl-incf counter))) ()
        (with-current-buffer (generate-new-buffer (format "*fixture-%s*" ,counter))
          (insert ,content)
          (goto-char (point-max))
          (python-mode)
          (anaconda-mode)
          (should (equal ,expected (company-anaconda 'prefix))))))))

(provide 'company-anaconda-test)

;;; company-anaconda-test.el ends here
