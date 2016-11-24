(defmodule db
	(export (new 0) (destroy 1) (write 3) (delete 2) (read 2) (match 2)))

(defun new ()
	())

(defun destroy (DbRef) 'ok)

(defun write (Key Element DbRef)
	(cons (tuple Key Element) DbRef))

(defun delete 
	(Key DbRef) (do_delete Key DbRef ())
)

(defun do_delete 
	([Key (cons (tuple TKey _) tl) Res] (when (== Key TKey)) (++ Res tl))
        ([Key (cons hd tl) Res] (do_delete Key tl (++ Res (list hd))))
)

(defun read
        ([Key (cons (tuple TKey TValue) tl)] (when (== Key TKey)) 
		(tuple 'ok (tuple TKey TValue)))
        ([Key (cons _ tl)] (read Key tl))
	([_ _] (tuple 'error 'instance))
)

(defun match
	([Value DbRef] (do_match Value DbRef ()))
)

(defun do_match
	([Value () Res] Res)
	([Value (cons (tuple Tkey TValue) Tail) Res] (when (== Value TValue))
		(do_match Value Tail (++ Res (list Tkey))))
	([Value (cons (tuple Tkey TValue) Tail) Res]
		(do_match Value Tail Res))
)
