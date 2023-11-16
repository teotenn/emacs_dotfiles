;; Function to beautify with all-the-icons package 
(defun custom-modeline-time ()
  (let* ((iweek (all-the-icons-octicon "calendar" 
				       :height 1.1 
				       :v-adjust -0.0 
				       :face 'all-the-icons-green))
	 (hour (string-to-number (format-time-string "%I"))))
    (concat
     (propertize iweek)
     (propertize (format-time-string "%W|%H:%M ") 'face `(:height 0.9)))))

;; Count (lines, words)
(defun custom-modeline-region-info ()
  (when mark-active
    (let ((words (count-lines (region-beginning) (region-end)))
          (chars (count-words (region-end) (region-beginning))))
      (concat
       (propertize (format "   %s" (all-the-icons-octicon "pencil") words chars)
                   'face `(:family ,(all-the-icons-octicon-family))
                   'display '(raise -0.0))
       (propertize (format " (%s, %s)" words chars)
                   'face `(:height 0.9))))))

;; version control NOT SO GOOD
(defun -custom-modeline-github-vc ()
  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
    (concat
     (propertize (format " %s" (all-the-icons-alltheicon "git")) 
		 'display '(raise -0.1))
     " Â· "
     (propertize (format "%s" (all-the-icons-octicon "git-branch"))
                 'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
                 'display '(raise -0.1))
     (propertize (format " %s" branch) 'face `(:height 0.9)))))

(defun -custom-modeline-svn-vc ()
  (let ((revision (cadr (split-string vc-mode "-"))))
    (concat
     (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
     (propertize (format " Â· %s" revision) 'face `(:height 0.9)))))

(defun custom-modeline-icon-vc ()
  (when vc-mode
    (cond
      ((string-match "Git-" vc-mode) (-custom-modeline-github-vc))
      ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
      (t (format "%s" vc-mode)))))

;; --------------- From powerline --------------- ;;
;; Define sections using powerline
(use-package powerline)

(defun tt-powerline-lhs ()
	 (let* ((active (powerline-selected-window-active))
                (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                (mode-line (if active 'mode-line 'mode-line-inactive))
                (face0 (if active 'powerline-active0 'powerline-inactive0))
                (face1 (if active 'powerline-active1 'powerline-inactive1))
                (separator-left (intern (format "powerline-%s-%s"
                                                (powerline-current-separator)
                                                (car powerline-default-separator-dir))))
		(lhs (list (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                           (funcall separator-left face0 face1)))
		)
	   (concat (powerline-render lhs))))

(defun tt-powerline-rhs ()
  (let* ((active (powerline-selected-window-active))
         (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
         (mode-line (if active 'mode-line 'mode-line-inactive))
         (face0 (if active 'powerline-active0 'powerline-inactive0))
         (face1 (if active 'powerline-active1 'powerline-inactive1))
         (face2 (if active 'powerline-active2 'powerline-inactive2))
         (separator-left (intern (format "powerline-%s-%s"
                                         (powerline-current-separator)
                                         (car powerline-default-separator-dir))))
         (separator-right (intern (format "powerline-%s-%s"
                                          (powerline-current-separator)
                                          (cdr powerline-default-separator-dir))))
	 (rhs (list (powerline-raw "%4lL:%6p" face1 'l)
		    (powerline-raw " " face1 'l)
                    (funcall separator-right face1 face0)
		    (powerline-raw " " face0)
                    
                    (when powerline-display-hud
                      (powerline-hud face0 face2))
		    (powerline-raw prot-modeline-misc-info face0 'r)
                    (powerline-fill face0 0)
                    )))
    (concat (powerline-render rhs))))


(provide 'tt-modeline)
