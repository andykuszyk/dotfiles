(defcustom osawm-titlebar-height
  68
  "The height of the title bar, which is taken into account when resizing
  windows")

(defcustom osawm-modeline-height
  20
  "The height of the modeline, which is taken into account when resizing windows")

(defun osawm--list-installed-applications ()
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

(defun osawm-launch-application (application)
  (interactive
   (list
    (completing-read
     "Application: "
     (osawm--list-installed-applications)
     nil
     t)))
  (message application)
  (shell-command (format "
osascript <<EOF
tell application \"%s\"
	activate
end tell
EOF" application))
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
    application
    (window-pixel-left)
    (+  (window-pixel-top) osawm-titlebar-height)
    (+ (window-pixel-left) (window-pixel-width))
    (- (+ (window-pixel-top) (window-pixel-height) osawm-titlebar-height)
  osawm-modeline-height)))
  (let* ((new-buffer-name (format "*osawm %s*" application))
	 (window-id (shell-command-to-string (format "
osascript <<EOF
set appName to \"%s\"

if appName is equal to \"Google Chrome\" then
	-- Special handling for Google Chrome
	tell application \"System Events\"
		if not (exists application process appName) then
			return \"The application is not running.\"
		end if
	end tell
	
	tell application \"Google Chrome\"
		if (count of windows) > 0 then
			set windowID to id of front window
			return windowID
		else
			return \"The application has no open windows.\"
		end if
	end tell
else
	-- General approach for other applications
	tell application \"System Events\"
		if exists (application process appName) then
			tell application process appName
				if (count of windows) > 0 then
					set windowID to id of window 1
					return windowID
				else
					return \"The application has no open windows.\"
				end if
			end tell
		else
			return \"The application is not running.\"
		end if
	end tell
end if
EOF" application)))
	 (temp-file-name (f-join (temporary-file-directory) window-id ".png")))
    (message (format "window id is %s" window-id))
    (if (or
	 (s-contains? "The application has no open windows." window-id)
	 (s-contains? "The application is not running." window-id))
	(error window-id))
    (shell-command
     (format "screencapture -l \"%s\" \"%s\"" window-id temp-file-name))
    (find-file temp-file-name)
    (display-line-numbers-mode -1)
    (rename-buffer new-buffer-name))
  )

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
