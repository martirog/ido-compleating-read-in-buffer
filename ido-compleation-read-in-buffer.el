; ido compleation read in buffer
; icrib

(defgroup icrib nil
  "Get ido-compleating-read into the buffer")

(defvar icrib-prewview-state nil)
(defvar icrib-prewview-window nil)
(defvar icrib-insert-text "icrib-insert: ")
(defvar icrib-use-vertical nil)

; this is copied from atomic-change-group in subr.el
; the change is that this always remove the change
(defmacro icrib-atomic-change (&rest body)
  `(let ((handle (prepare-change-group))
        (undo-outer-limit nil)
        (undo-limit most-positive-fixnum)
        (undo-strong-limit most-positive-fixnum)
        (sucsess nil))
    (unwind-protect
	(progn
	  ;; This is inside the unwind-protect because
	  ;; it enables undo if that was disabled; we need
	  ;; to make sure that it gets disabled again.
	  (activate-change-group handle)
	  (prog1 ,(macroexp-progn body)
                  (setq sucsess t)))
      (cancel-change-group handle)
      (unless sucsess
        (goto-char (nth 1 icrib-prewview-state))))))

(require 'text-property-search)
(defun icrib-insert-vertical-preview ()
  (let* ((pre-view (propertize (minibuffer-contents) 'field 'icrib-prewview))
         (point-offset (- (point) (length icrib-insert-text) 1))
         (point-line-break (string-match "\n" pre-view))
         (pre-view-first (substring pre-view 0 point-line-break))
         (pre-view-second (substring pre-view (+ point-line-break 1) nil))
         (pre-view-length (length pre-view)))
    (with-selected-window icrib-prewview-window ;use the correct window here
      (message "beginning of line %d" (line-beginning-position))
      (let* ((inhibit-field-text-motion t)
             (second-offset (- (nth 0 icrib-prewview-state) (line-beginning-position))))
        (if (eq (get-text-property (- (point) 1) 'field) 'icrib-prewview)
            (progn
              (save-excursion
                (delete-field)
                (while (text-property-search-forward 'field 'icrib-prewview t)
                  (delete-field)))))
        (unless (equal pre-view-length 0)
          (save-excursion
            (insert pre-view-first)
            (move-end-of-line 1)
            (insert (propertize "\n" 'field 'icrib-prewview))
            (dolist (line (split-string pre-view-second "\n"))
              (insert-char ?  second-offset t)
              (insert line)
              (insert-and-inherit "\n")))
          (goto-char (+ (nth 0 icrib-prewview-state) point-offset)))))))  ; insert


(defun icrib-insert-preview ()
  (let* ((pre-view (propertize (minibuffer-contents) 'field 'icrib-prewview))
         (pre-view-length (length pre-view))
         (point-offset (- (point) (length icrib-insert-text) 1)))
    (with-selected-window icrib-prewview-window ;use the correct window here
      (if (eq (get-text-property (+ (point) 1) 'field) 'icrib-prewview)
          (delete-field))
      (unless (equal pre-view-length 0)
        (insert pre-view)
        (goto-char (+ (nth 0 icrib-prewview-state) point-offset))))))  ; insert preview


(defun icrib-ido-in-buffer-compleation-read (init-string choises)
  "use ido-complete-read in buffer this currently only supports choises as input."
  (setq icrib-prewview-window (selected-window))
  (make-local-variable 'icrib-prewview-state)
  (let (start end text start-string)
    (if (use-region-p)
        (setq start (region-beginning)
                end (region-end)
                start-string (delete-and-extract-region (region-beginning) (region-end)))
      (if (string= (buffer-substring-no-properties (- (point) (length init-string)) (point)) init-string)
          (setq start (- (point) (length init-string))
                  end (point)
                  start-string init-string)
        (setq start (point)
              end (point)
              start-string nil)))
    (icrib-atomic-change
     (delete-region start end)
     (setq-local icrib-prewview-state `(,start ,end ,start-string))
     (setq text
           (minibuffer-with-setup-hook
               (lambda ()
                 (setq-local icrib-last-cmd nil)
                 (setq-local ido-confirm-unique-completion nil)
                 (if icrib-use-vertical
                     (progn
                       (add-hook 'post-command-hook #'icrib-insert-vertical-preview nil t)
                       (setq-local ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))))
                   (add-hook 'post-command-hook #'icrib-insert-preview nil t))
                  (setq-local ido-enable-flex-matching nil))
             (ido-completing-read icrib-insert-text choises nil nil start-string nil nil nil)))) ; need to add histrory here
    (delete-region start end)
    (insert text)))


; hevely influensed by hippie-expand
(defun icrib-search-all-buffers (str &optional mmod ignore)
  (let ((all-the-buffers (buffer-list))
        (regexp-str (regexp-opt `(,str) 'word))
        (cur-buf (current-buffer))
        (ignore-first nil)
        (ret '()))
    (dolist (buf all-the-buffers ret)
      (with-current-buffer buf
        (when (and (or (not mmod) (member (symbol-name major-mode) mmod))
                   (or (not ignore) (not (member (buffer-name) ignore))))
          (when (eq buf cur-buf)
            (setq ignore-first t))
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-min))
              (while (re-search-forward regexp-str nil t)
                (if (and (string= (thing-at-point 'symbol t) str)
                         ignore-first)
                    (setq ignore-first nil)
                  (add-to-list 'ret (thing-at-point 'symbol t)))))))))))


(defun icrib-buffer-and-tag-compleation (str &optional mmod ignore comp-list)
  (let ((comps '())
        (buf-comps (icrib-search-all-buffers str mmod ignore))
        (tag-comps (all-completions str (tags-completion-table))))
    (setq comps (append comp-list buf-comps tag-comps))
    (icrib-ido-in-buffer-compleation-read str comps)))


(provide 'icrib-buffer-and-tag-compleation)
