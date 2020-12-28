;;    Copyright: Markus Kollmar (december2020)
;;    Home: https://github.com/mkollmar/textplayer.el
;;
;;    Thanks Andrea Rossetti aka "The Software Bin"
;;    (http://github.com/thesoftwarebin)
;;    for teletype which helpes this project as skeleton.
;;
;;    This program is free software: you can redistribute it and/or modify
;;    it under the terms of the GNU General Public License as published by
;;    the Free Software Foundation, either version 3 of the License, or
;;    (at your option) any later version.
;; 
;;    This program is distributed in the hope that it will be useful,
;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;    GNU General Public License for more details.
;; 
;;    You should have received a copy of the GNU General Public License
;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;;;;;;;;

(defconst supported-movie-format 0 "Format in the movie header which is supoorted. In general newer textplayer.el should handle all older (smaller version-number) formats.")

; movie: (header frame0 frame1 frame2...)
; header: (format-version title copyright date description default-frame-duration)
; title: String, copyright: string
; frame: (text-line0 format-line0 text-line1 format-line1...)
; text-line: String to print, format-line: String with colorname for string (r=red, g=green, h=gray/grey, b=blue, p=pink, y=yellow, w=white (default)).
(defconst textplayer-demo-movie
      '((0 "textplayer-demo" "Markus Kollmar" "december 2020" "-no description-" 0.1) (
"#"
"r"
""
""
""
"") (
     
"##"
"rr"
"#"
"r"
"#"
"r") (
      
"###"
"rrr"
" #"
" r"
" #"
" r") (
       
" ###         #"
" rrr         b"
"  #          #"
"  r          b"
"  #          #"
"  r          b") (
		   
"  ###       ##"
"  rrr       bb"
"   #        #"
"   r        b"
"   #        #"
"   r        b") (
		  
"   ###     ##"
"   rrr     bb"
"    #      ##"
"    r      bb"
"    #      #"
"    r      b") (
		 
"    ### e ##    l"
"    rrr r bb    b"
"    #     ##"
"    r     bb"
"    #     #"
"    r     b") (
		
"   ###  x ##    a"
"   rrr  r bb    b"
"    #   e ##   l"
"    r   r bb   b"
"    #     #"
"    r     b") (
		
"   ###  t ##    y"
"   rrr  r bb    b"
"    #   x ##   a"
"    r   r bb   b"
"    #  e  #   l"
"    r  r  b   b") (
		    
"   ###    ##    e"
"   rrr    bb    b"
"    #   t ##   y"
"    r   r bb   b"
"    # e x #  la"
"    r r r b  bb") (
		    
"   ###    ##    r"
"   rrr    bb    b"
"    #   t ##   e"
"    r   r bb   b"
"    # ex  # lay"
"    r rr  b bbb") (
		    
"   ###    ##"
"   rrr    bb"
"    #   t ##    r"
"    r   r bb    b"
"    # ex  # laye"
"    r rr  b bbbb") (
		     
"   ###    ##"
"   rrr    bb"
"    #     ##"
"    r     bb"
"    # ext # layer"
"    r rrr b bbbbb") (

"   ###    ##"
"   rrr    bb"
"    #     ##"
"    r     bb"
"    # ext # layer"
"    r rrr b bbbbb"
"    -------------"
"    hhhhhhhhhhhhh") (
		      
"   ###    ##"
"   rrr    bb"
"    #     ##"
"    r     bb"
"    # ext # layer"
"    r rrr b bbbbb"
"    -------------"
"    hhhhhhhhhhhhh"
"    -------------"
"    hhhhhhhhhhhhh") (
		      
"   ###    ##"
"   rrr    bb"
"    #     ##"
"    r     bb"
"    # ext # layer"
"    r rrr b bbbbb"
"    -------------"
"    yyyyyyyyyyyyy"
"    -------------"
"    hhhhhhhhhhhhh"
"    -------------"
"    ppppppppppppp")
) "sample demo-movie for text-player.")


;;;;
(defun textplayer-print-char (c color)
  "Prints one char and a property for it."
  (cond
   ((equal color ?r)
    (insert (propertize (char-to-string c) 'face '(:foreground "red")))
    ;(insert (char-to-string c))
    ;(put-text-property (- (point) 1) (point) 'face (cons 'foreground-color "red"))
    )
   ((equal color ?g)
    (insert (propertize (char-to-string c) 'face '(:foreground "green"))))
   ((equal color ?y)
    (insert (propertize (char-to-string c) 'face '(:foreground "yellow"))))
   ((equal color ?h)
    (insert (propertize (char-to-string c) 'face '(:foreground "gray"))))
   ((equal color ?p)
    (insert (propertize (char-to-string c) 'face '(:foreground "deep pink"))))
   ((equal color ?b)
    (insert (propertize (char-to-string c) 'face '(:foreground "blue"))))
   ((equal color ?\n)
    (insert "?"))
   (t
    (insert (char-to-string c)))))
		       
(defun textplayer-print-line (line-string line-format-string)
  "Prints line (row)."
  (mapcar* (lambda (strchar colchr)
	     (textplayer-print-char strchar colchr))
	   (string-to-list line-string)
	   (string-to-list line-format-string)))

(defun textplayer-print-frame (frame)
  "Prints one frame (area)."
  (dotimes (line-nr (/ (length frame) 2))
    (textplayer-print-line (nth (* line-nr 2) frame) (nth (+ 1 (* line-nr 2)) frame))
    (newline)))

(defun textplayer-get-frame-count (movie)
  "Get amount of video frames in movie."
  (- (length movie) 1))

(defun textplayer-get-frame (movie frame-number)
  "Get frame with index starting at 0."
  (let ((last-frame-nr (- (textplayer-get-frame-count movie) 1)))
    (if (< frame-number 0)
	(nth 1 movie)
      (if (> frame-number last-frame-nr)
	  (nth last-frame-nr movie)
	(nth (+ 1 frame-number) movie)))))

(defun textplayer-get-format (movie)
  "Get textplayer movie format."
  (nth 0 (car movie)))

(defun textplayer-get-title (movie)
  "Get movie title."
  (nth 1 (car movie)))

(defun textplayer-get-copyright (movie)
  "Get movie copyright."
  (nth 2 (car movie)))

(defun textplayer-get-date (movie)
  "Get movie title."
  (nth 3 (car movie)))

(defun textplayer-get-description (movie)
  "Get movie description."
  (nth 4 (car movie)))

(defun textplayer-get-duration (movie)
  "Get frame view duration."
  (nth 5 (car movie)))

(defun textplayer-show-movie-info (movie)
  (message "Title: %s | Copyright: %s | Date: %s | Note: %s" (textplayer-get-title movie) (textplayer-get-copyright movie) (textplayer-get-date movie) (textplayer-get-description movie)))

;;;;
(defun textplayer-in-new-buffer (movie &optional frame-duration-in-seconds)
  "Create a new buffer and play the movie. You can provide a optional frame-duration in second(s) to overwrite the default given by the movie."
  (let ((frame-duration (if frame-duration-in-seconds
			  frame-duration-in-seconds
			  (textplayer-get-duration movie))))
    (switch-to-buffer (get-buffer-create (textplayer-get-title movie)))
    (dotimes (frame-nr (textplayer-get-frame-count movie))
      (erase-buffer)
      (textplayer-print-frame (textplayer-get-frame movie frame-nr))
      (sit-for frame-duration))))

(defun textplayer-run-demo ()
  "Run a internal textplayer-demo movie."
  (interactive)
  (textplayer-show-movie-info textplayer-demo-movie)
  (textplayer-in-new-buffer textplayer-demo-movie))

(defun textplayer-file ()
  "Ask for a filename and a frame-duration-time, then play the movie in the file."
  (interactive
   (let ((filename-to-play (read-file-name "Choose a filename: " nil "textplayer.txt" t "textplayer.txt"))
	 (movie '())
	 (movie-format nil))
     (textplayer-in-new-buffer (with-temp-buffer
				 (insert-file-contents filename-to-play)
				 (setq movie (car (read-from-string (buffer-string))))
				 (textplayer-show-movie-info movie)
				 (setq movie-format (textplayer-get-format movie))
				 (when (< supported-movie-format movie-format)
				   (message "Warning: Your movie has a newer format (%s) than your textplayer knows. Try to get newer version of textplayer, otherwise the movie may not work as excpected!" movie-format))
				 movie)))))
