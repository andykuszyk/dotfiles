;;; osawm.el --- a Mac OS window manager for Emacs  -*- lexical-binding:t; -*-
;;; Commentary:
;;; Code:

(require 'f)

(defgroup osawm
  nil
  "A Mac OS window manager for Emacs."
  :group 'applications
  :prefix "osawm-")

(defcustom osawm-titlebar-height
  68
  "The height of the title bar, which is taken into account when resizing windows."
  :type 'number)

(defcustom osawm-modeline-height
  20
  "The height of the modeline, which is taken into account when resizing windows."
  :type 'number)

(defun osawm--list-installed-applications ()
  "Lists installed applications."
  (interactive)
  (mapcar
   (lambda (s)
     (string-trim (string-trim-right s "\.app.*") " "))
   (split-string (shell-command-to-string "
osascript <<EOF
set appList to {}
tell application \"Finder\"
	set appList to name of every application file in folder \"Applications\" of startup disk
end tell
return appList
EOF
") ",")))

(defun osawm--start-application (application)
  "Start APPLICATION."
  (shell-command (format "
osascript <<EOF
tell application \"%s\"
	activate
end tell
EOF" application)))

(defun osawm--resize-application (application left top right bottom)
  "Resize APPLICATION to LEFT, RIGHT, BOTTOM, TOP."
  (shell-command
   (format
    "
osascript <<EOF
tell application \"%s\"
	activate
	tell window 1
		set bounds to {%d, %d, %d, %d} -- {left, top, right, bottom}
	end tell
end tell
EOF"
    application left top right bottom)))

(defun osawm-launch-application (application)
  "Launch application APPLICATION."
  (interactive
   (list
    (completing-read
     "Application: "
     (osawm--list-installed-applications)
     nil
     t)))
  (osawm--start-application application)
  (let* ((left (window-pixel-left))
	 (right (+ (window-pixel-left) (window-pixel-width)))
	 (top (+  (window-pixel-top) osawm-titlebar-height))
	 (bottom (- (+
		     (window-pixel-top)
		     (window-pixel-height)
		     osawm-titlebar-height)
		    osawm-modeline-height))
	 (new-buffer-name (format "*osawm %s*" application))
	 (temp-file-name
	  (f-join
	   (temporary-file-directory)
	   (format "%s.png" application))))
    (osawm--resize-application application left top right bottom)
    (shell-command
     (format
      "screencapture -R %d,%d,%d,%d \"%s\""
      left
      top
      (- right left)
      (- bottom top)
      temp-file-name))
    (find-file temp-file-name)
    (display-line-numbers-mode -1)
    (rename-buffer new-buffer-name)))

(defun osawm-select-application (application)
  "Select APPLICATION and display it."
  (interactive (list (completing-read "Application: " (osawm-list-applications) nil t)))
  (shell-command (format "
osascript <<EOF
tell application \"%s\"
    activate
end tell
EOF
" application)))

(defun osawm-bring-to-front (application)
  "Bring APPLICATION to the front."
  (interactive (list (completing-read "Application: " (osawm-list-applications) nil t)))
  (shell-command (format "
osascript <<EOF
tell application \"System Events\"
    set frontmost of process \"%s\" to true
end tell
EOF
" application)))

(defun osawm-send-to-back (application)
  "Send APPLICATION to the back."
  (interactive (list (completing-read "Application: " (osawm-list-applications) nil t)))
  (shell-command (format "
osascript <<EOF
tell application \"System Events\"
    set frontmost of process \"%s\" to false
end tell
EOF" application)))

(defun osawm-list-applications ()
  "Lists open applications."
  (interactive)
  (string-split (shell-command-to-string "
osascript <<EOF
tell application \"System Events\"
    -- Get the names of all open (non-background) applications
    set openApps to name of every process whose background only is false
end tell

-- Join the list of application names into a single string with newlines
set AppleScript's text item delimiters to \",\"
set openAppsList to openApps as string

-- Output the list to the terminal
do shell script \"echo \" & quoted form of openAppsList
EOF
") ","))

(provide 'osawm)
;;; osawm.el ends here
