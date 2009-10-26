;;; vkontakte.el --- retrive info from vkontakte.ru

;; Copyright (C) 2009 mad

;; Author:  <owner.mad.epa@gmail.com>
;; Keywords: vkontakte
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; http://userapi.com/?act=doc

;;; TODO:

;; - notify of new message
;; - read full news

;;; Code:

(require 'cl)
(require 'url)
(require 'url-http)
(require 'json)

(defgroup vkontakte-faces nil
  "Faces for displaying vkontakte stuff"
  :group 'vkontakte)

(defface vkontakte-face-online
  '((t (:foreground "blue")))
  "Face for displaying online users"
  :group 'vkontakte-faces)

(defconst vkontakte-login-url "http://login.vk.com/?act=login"
  "URL used to login vkontakte.ru")

(defconst vkontakte-api-url "http://userapi.com/data?"
  "base URL for API vkontakte.ru")

(defvar vkontakte-mail nil
  "Mail to use for auth")

(defvar vkontakte-password nil
  "Password to use auth")

(defvar vkontakte-sid nil
  "SID to use vkontakte api")

(defvar vkontakte-id nil
  "user id")

(defvar *vkontakte-friend-list* nil)

(defun vkontakte-login (&optional force)
  "Used to retrive SID from cookie.

If you logged and FORCE set then relogin."
  (interactive "P")
  (when (or force (not vkontakte-sid))
    (if (and vkontakte-mail vkontakte-password)
        (let ((url-request-method "POST")
              (url-request-extra-headers
               `(("Content-Type" .
                  "application/x-www-form-urlencoded; charset=UTF-8")))
              (url-request-data (concat "email="
                                        (url-hexify-string vkontakte-mail)
                                        "&pass="
                                        (url-hexify-string vkontakte-password)
                                        "&expire=&vk=")))
          (url-retrieve vkontakte-login-url 'vkontakte-login-sentinel)))))

(defun vkontakte-login-sentinel (status)
  (let ((result-buffer (current-buffer)))
    (goto-char (point-min))
    (if (and (re-search-forward "HTTP/1.1 200 OK" nil t)
             ;; because vkonakte may redirecting
             (re-search-forward "id='s'" nil t))
        (progn
          (goto-char (point-min))
          (setq vkontakte-sid (buffer-substring
                               (re-search-forward "id='s' value='" nil t)
                               (- (re-search-forward "'" nil t) 1)))
          (goto-char (point-min))
          (setq vkontakte-id (buffer-substring
                              (re-search-forward "l=" nil t)
                              (- (re-search-forward ";" nil t) 1)))
          (message "vkontakte: login successfull"))
      ;; (progn
      ;;   (goto-char (point-min))
      ;;   (if (re-search-forward "captcha" nil t)
      ;;       (progn
      ;;         (goto-char (point-min))
      ;;         (re-search-forward "_sid\":\"\\([0-9]+\\)\"")
      ;;         (match-string 1)
      ;;         (message "vkontakte: captcha required"))
      (message "vkontakte: login failed"))
    (kill-buffer result-buffer)))

(defun vkontakte-private-inbox (&optional arg)
  "TODO: docstring"
  (interactive "P")
  (when arg
    (setq arg (number-to-string arg)))
  (if vkontakte-sid
      (let ((url-request-method "GET")
            (api-url (concat vkontakte-api-url
                             "act=inbox&from=0&to=" (or arg "10") "&"
                             "id=&sid=" vkontakte-sid)))
        (url-retrieve api-url 'vkontakte-private-inbox-sentinel))
    (message "vkontakte: please login to vkontakte")))

(defun vkontakte-private-inbox-sentinel (status)
  (let ((result-buffer (current-buffer)))
    (unwind-protect
        (progn
          (goto-char (point-min))
          (fixing-json-data)
          (delete-region (point-min) (search-forward "\n\n")))
      (let* ((result (buffer-substring (point-min) (point-max)))
             (entries (cdr (nth 1 (json-read-from-string result))))
             (counter 0))
        (switch-to-buffer "*vkontakte inbox*")
        (kill-region (point-min) (point-max))
        (dotimes (x (length entries))
          (let ((message (aref (cdr (cadddr (aref entries x))) 0))
                (name (aref (cdar (aref entries x)) 1))
                (time (seconds-to-time (string-to-number (cdar (cddddr (aref entries x)))))))
            (insert (format-time-string "%H:%M" time) " ")
            (insert (decode-coding-string name 'utf-8) "\n")
            (insert (decode-coding-string message 'utf-8) "\n\n"
                    "-------------------\n"))))
      (goto-char (point-min))
      (url-entity-decode)
      (kill-buffer result-buffer))))

(defun vkontakte-activity-friends (&optional arg)
  "Get last status your friends.

ARG set how many status entry retrieve (default 10).

\(http://userapi.com/?act=doc#activity\)"
  (interactive "P")
  (when arg
    (setq arg (number-to-string arg)))
  (if vkontakte-sid
      (let ((url-request-method "GET")
            (news-url (concat vkontakte-api-url
                              "act=updates_activity&from=0&to=" (or arg "10")
                              "&sid=" vkontakte-sid)))
        (url-retrieve news-url 'vkontakte-activity-friends-sentinel))
    (message "vkontakte: please login to vkontakte")))

(defun vkontakte-activity-friends-sentinel (status)
  (let ((result-buffer (current-buffer)))
    (unwind-protect
        (progn
          (goto-char (point-min))
          (delete-region (point-min) (search-forward "\n\n")))
      (let* ((result (buffer-substring (point-min) (point-max)))
             (entries (cdr (nth 1 (json-read-from-string result))))
             (lenentries (length entries))
             (counter 0))
        (switch-to-buffer "*vkontakte activity*")
        (kill-region (point-min) (point-max))
        (while (< counter lenentries)
          (let ((time (seconds-to-time (aref (aref entries counter) 4)))
                (name (aref (aref entries counter) 3))
                (status (aref (aref entries counter) 5)))
            (insert (format-time-string "%H:%M" time) " ")
            (insert (decode-coding-string name 'utf-8) "\n")
            (insert (decode-coding-string status 'utf-8) "\n\n"))
          (setq counter (+ 1 counter)))
        (goto-char (point-min))
        (url-entity-decode)))
    (kill-buffer result-buffer)))

(defun vkontakte-activity-set (&optional string)
  "Set your status"
  (interactive "sType your status: ")
  (if vkontakte-sid
      (let ((url-request-method "GET")
            (news-url (concat vkontakte-api-url
                              "act=set_activity&text=" (url-hexify-string string)
                              "&sid=" vkontakte-sid)))
        (url-retrieve news-url 'vkontakte-activity-set-sentinel))
    (message "vkontakte: please login to vkontakte")))

(defun vkontakte-activity-set-sentinel (status)
  (let ((result-buffer (current-buffer)))
    (delete-region (point-min) (search-forward "\n\n"))
    (if (re-search-forward "\"ok\":1" nil t)
	(message "vkontakte: status set successful")
      (message "vkontakte: status set failed"))
    (kill-buffer result-buffer)))

(defun vkontakte-friend-list (&optional arg)
  "TODO: docstring"
  (interactive "P")
  (if vkontakte-sid
      (let ((url-request-method "GET")
            (news-url (concat vkontakte-api-url
                              "act=friends&from=0&to=200&id" vkontakte-id
                              "&sid=" vkontakte-sid)))
        (url-retrieve news-url 'vkontakte-friend-list-sentinel (list arg)))
    (message "vkontakte: please login to vkontakte")))

(defun vkontakte-friend-list-sentinel (status arg)
  (let ((result-buffer (current-buffer)))
    (unwind-protect
        (progn
          (goto-char (point-min))
          (delete-region (point-min) (search-forward "\n\n")))
      (let* ((result (buffer-substring (point-min) (point-max)))
             (entries (json-read-from-string result))
             (lenentries (length entries))
             (counter 0))
        (when (not arg)
          (switch-to-buffer "*vkontakte friend-list*")
          (kill-region (point-min) (point-max))
          (dotimes (x (length entries))
            (insert (number-to-string (aref (aref entries x) 0)) " ")
            (insert (decode-coding-string (aref (aref entries x) 1) 'cp1251))
            (when (eq (aref (aref entries x) 3) 1)
              (move-beginning-of-line 1)
              (set-text-properties (point) (point-max) `(face vkontakte-face-online))
              (goto-char (point-max)))
            (insert "\n"))
          (goto-char (point-min))
          (url-entity-decode))
        (setq *vkontakte-friend-list* (mapcar
                                       (lambda (x)
                                         (list (decode-coding-string (aref x 1) 'cp1251)
                                               (aref x 0))) entries))))
    (kill-buffer result-buffer)))

(defun vkontakte-compose-message (name)
  (interactive (list (ido-completing-read "To: " *vkontakte-friend-list*)))
  (switch-to-buffer "*vkontakte compose message*")
  (delete-region (point-min) (point-max))
  (insert "To: " name "\n\n")
  (local-set-key "\C-c\C-c" '(lambda()
                               (interactive)
                               (vkontakte-compose-message-send)))
  (set (make-local-variable 'user-id) (cadr (assoc name *vkontakte-friend-list*))))

(defun vkontakte-compose-message-send ()
  (goto-char (point-min))
  (if vkontakte-sid
      (let* ((url-request-method "GET")
             (message (buffer-substring-no-properties (re-search-forward "\n\n" nil t)
                                                      (point-max)))
             (news-url (concat vkontakte-api-url
                               "act=add_message&id=" (number-to-string user-id)
                               "&message=" (url-hexify-string message)
                               "&ts=" (substring (number-to-string (time-to-seconds (current-time))) 0 -7)
                               "&sid=" vkontakte-sid)))
        (url-retrieve news-url 'vkontakte-compose-message-send-sentinel)
        (kill-buffer (current-buffer)))
    (message "vkontakte: please login to vkontakte")))

(defun vkontakte-compose-message-send-sentinel (status)
  (let ((result-buffer (current-buffer)))
    (unwind-protect
        (progn
          (goto-char (point-min))
          (delete-region (point-min) (search-forward "\n\n")))
      (let* ((result (buffer-substring (point-min) (point-max)))
             (entries (json-read-from-string result))
             (success (cdr (assoc 'ok entries))))
        (cond ((eq success 1)
               "success")
              ((eq success 0)
               "failure")
              ((eq success -1)
               "auth error")
              ((eq success -2)
               "flood control")
              ((eq success -3)
               "privacy settings")))
      (kill-buffer result-buffer))))

(defun url-entity-decode ()
  (save-excursion
    (while (re-search-forward "\\(&quot;\\)\\|\\(&lt;\\)\\|\\(&gt;\\)\\|\\(&amp;\\)\\|\\(<br>\\)" nil t)
      (when (match-string 1)
        (replace-match "\"" nil nil nil 1))
      (when (match-string 2)
        (replace-match "<" nil nil nil 2))
      (when (match-string 3)
        (replace-match ">" nil nil nil 3))
      (when (match-string 4)
        (replace-match "&" nil nil nil 4))
      (when (match-string 5)
        (replace-match "\n" nil nil nil 5)))))

;; Maybe fix json.el
(defun fixing-json-data ()
  (save-excursion
    (while (re-search-forward "\\(0\\|1\\|2\\|3\\|4\\|5\\):" nil t)
      (when (match-string 1)
        (replace-match "\"\\&\"" nil nil nil 1)))))

(provide 'vkontakte)
;;; vkontakte.el ends here
