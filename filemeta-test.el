;;; filemeta-test.el -mode -*- coding: utf-8; lexical-binding: t; -*-

(defvar filemeta:testdir "/tmp/filemeta/testing/")
(defvar filemeta:testfile (f-join filemeta:testdir "hello.txt"))
(mkdir filemeta:testdir t)
(filemeta:init filemeta:testdir)
(f-write-text "" 'utf-8 filemeta:testfile)
(loop for tag in '(math physics cs nerdy techie)
      do (filemeta:+tag! tag filemeta:testfile))
(loop for tag in '(nerdy techie)
      do (filemeta:-tag! tag filemeta:testfile))

(filemeta:db-dump filemeta:testdir)
(filemeta:hashes-in-repo filemeta:testdir)
(filemeta:tags-in-repo filemeta:testdir)
(filemeta:update-file-history! filemeta:testfile)

(provide 'filemeta-test)
