;;; z-stick.el --- getting data from a Z-Stick Z-Wave device -*- lexical-binding: t -*-
;; Copyright (C) 2018 Lars Magne Ingebrigtsen

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>
;; Keywords: extensions, processes

;; z-stick.el is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; z-stick.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; https://gist.github.com/tenderlove/40ea79896bb66a48c95cf569c5e24240

;;; Code:

(require 'cl)
(require 'server)

(defvar zs-device "/dev/z-stick"
  "The device, which will usually be /dev/ttyUSB0 or /dev/ttyACM0.
Or something else, if you have udev rules like

KERNEL==\"ttyACM[0-9]*\", SUBSYSTEM==\"tty\", SUBSYSTEMS==\"usb\", ATTRS{idProduct}==\"0200\", ATTRS{idVendor}==\"0658\", MODE=\"0666\", SYMLINK+=\"z-stick\"
")

(defvar zs-buffer "*z-stick*")

(defun zs-start ()
  (setq server-use-tcp t)
  (with-current-buffer (get-buffer-create "*z-stick*")
    (erase-buffer)
    (set-buffer-multibyte nil)
    (switch-to-buffer (current-buffer))
    (let ((start (point-min))
	  (proc (make-serial-process :port zs-device
				     :speed 115200
				     :coding 'no-conversion
				     :buffer (current-buffer))))
      
      (set-process-filter
       proc
       (lambda (_proc string)
	 (when (buffer-live-p (get-buffer zs-buffer))
	   (with-current-buffer zs-buffer
	     (goto-char (point-max))
	     (insert string)
	     ;; We receive data in very small chunks from the Z-Stick,
	     ;; and we have to collect the output until we get a
	     ;; complete command until we start to parse.
	     (when (zs-complete-command-p start)
	       (goto-char start)
	       (let ((commands (zs-parse)))
		 (setq start (point))
		 (dolist (command commands)
		   (let ((callback (intern (format "zs-callback-%s"
						   (getf command :command))
					   obarray)))
		     (when (fboundp callback)
		       (funcall callback command))))))
	     ;; ACK the message we got from the Z-Stick.
	     (zs-send '(#x06) t)))))
      (set-process-sentinel
       proc
       (lambda (proc _string)
	 (unless (process-live-p proc)
	   (run-at-time 10 nil 'zs-reconnect)))))))

(defun zs-reconnect ()
  ;; If the Z-Stick device doesn't exist, then wait some more and
  ;; check again.
  (if (file-exists-p zs-device)
      (zs-start)
    (run-at-time 10 nil 'zs-reconnect)))

(defvar zs-last-counter (make-hash-table))

(defun zs-callback-application-command-handler (command)
  (let* ((slots '((2 node)
		  (3 status)
		  (5 class-id)
		  (4 unk1)
		  (6 counter)
		  (8 sub-node)))
	 (message
	  (loop for (index slot) in slots
		append (list (intern (format ":%s" slot) obarray)
			     (elt (getf command :data) (1- index)))))
	 (last (gethash (getf message :node) zs-last-counter 0)))
    (message "%s %s" message (getf command :data))
    (when (or (> (getf message :counter) last)
	      (> (- last (getf message :counter)) 100))
      (setf (gethash (getf message :node) zs-last-counter)
	    (getf message :counter))
      (message "%s Doing %s %s"
	       (format-time-string "%FT%T")
	       message (getf command :data))
      (when t
	(server-eval-at
	 (concat "tellstick-central-" tellstick-central-server)
	 `(tellstick-execute-input
	   ,(format "%03d%03d" (getf message :node) (getf message :sub-node)))))
      )))

(defun zs-complete-command-p (point)
  (save-excursion
    (goto-char point)
    (unless (eobp)
      (let ((type (cadr (assq (zs-read) zs-transaction-types))))
	(cond
	 ((not (eq type 'sof))
	  t)
	 ((eobp)
	  nil)
	 (t
	  (let ((length (zs-read)))
	    (>= (- (point-max) (point)) length))))))))

(defvar zs-transaction-types
  '((#x01 sof)				; Start of field
    (#x06 ack)				; Acknowledgement
    (#x15 nak)				; Error
    (#x18 can))) ; Whatever

(defvar zs-commands
  '((#x02 serial-api-get-init-data)
    (#x03 serial-api-appl-node-information)
    (#x04 application-command-handler)
    (#x05 get-controller-capabilities)
    (#x06 serial-api-set-timeouts)
    (#x07 serial-api-get-capabilities)
    (#x08 serial-api-soft-reset)

    (#x12 send-node-information)
    (#x13 send-data)
    (#x15 get-version)
    (#x17 r-f-power-level-set)
    (#x1c get-random)
    (#x20 memory-get-id)
    (#x21 memory-get-byte)
    (#x23 read-memory)

    (#x40 set-learn-node-state)		; not implemented
    (#x41 get-node-protocol-info) ; get protocol info (baud rate,
				  ; listening, etc.) for a given node
    (#x42 set-default) ; reset controller and node info to default
		       ; (original) values
    (#x43 new-controller)		; not implemented
    (#x44 replication-command-complete) ; replication send data
					; complete
    (#x45 replication-send-data)	; replication send data
    (#x46 assign-return-route) ; assign a return route from the
			       ; specified node toq the controller
    (#x47 delete-return-route) ; delete all return routes from the
			       ; specified node
    (#x48 request-node-neighbor-update) ; ask the specified node to
					; update its neighbors (then
					; read them from the
					; controller)
    (#x49 application-update) ; get a list of supported (and
			      ; controller) command classes
    (#x4a add-node-to-network) ; control the addnode (or
			       ; addcontroller) process...start, stop,
			       ; etc.
    (#x4b remove-node-from-network) ; control the removenode (or
				    ; removecontroller)
				    ; process...start, stop, etc.
    (#x4c create-new-primary) ; control the createnewprimary
			      ; process...start, stop, etc.
    (#x4d controller-change) ; control the transferprimary
			     ; process...start, stop, etc.
    (#x50 set-learn-mode) ; put a controller into learn mode for
			  ; replication/ receipt of configuration info
    (#x51 assign-suc-return-route)  ; assign a return route to the suc
    (#x52 enable-suc)	; make a controller a static update controller
    (#x53 request-network-update)	; network update for a suc(?)
    (#x54 set-suc-node-id) ; identify a static update controller node id
    (#x55 delete-suc-return-route)   ; remove return routes to the suc
    (#x56 get-suc-node-id) ; try to retrieve a static update
			   ; controller node id (zero if no suc
			   ; present)
    (#x5a request-node-neighbor-update-options) ; allow options for
						; request node
						; neighbor update
    (#x5e explore-request-inclusion)		; supports nwi
    (#x60 request-node-info) ; get info (supported command classes)
			     ; for the specified node
    (#x61 remove-failed-node-id)  ; mark a specified node id as failed
    (#x62 is-failed-node-id) ; check to see if a specified node has
			     ; failed
    (#x63 replace-failed-node) ; remove a failed node from the
			       ; controller's list (?)
    (#x80 get-routing-info) ; get a specified node's neighbor
			    ; information from the controller
    (#xa0 serial-api-slave-node-info) ; set application virtual slave
				      ; node information
    (#xa1 application-slave-command-handler) ; slave command handler
    (#xa2 send-slave-node-info)	 ; send a slave node information frame
    (#xa3 send-slave-data)	 ; send data from slave
    (#xa4 set-slave-learn-mode)	 ; enter slave learn mode
    (#xa5 get-virtual-nodes)	 ; return all virtual nodes
    (#xa6 is-virtual-node)	 ; virtual node test
    (#xd0 set-promiscuous-mode) ; set controller into promiscuous mode
				; to listen to all frames
    (#xd1 promiscuous-application-command-handler)))

(defun zs-parse ()
  (let ((elems nil))
    (while (zs-complete-command-p (point))
      (push
       (let ((type (cadr (assq (zs-read) zs-transaction-types))))
	 (if (eq type 'sof)
	     (zs-parse-field)
	   type))
       elems))
    (nreverse elems)))

(defun zs-read ()
  (prog1
      (following-char)
    (forward-char 1)))
  
(defun zs-parse-field ()
  (let ((length (zs-read))
	(resreq (zs-read)))
    (prog1
	(list :length length
	      :resreq (if (= resreq 0) 'request
			'response)
	      :command (cadr (assq (zs-read) zs-commands))
	      :data (buffer-substring (point) (+ (point) (- length 3)))
	      :checksum (char-after (+ (point) (- length 3))))
      (forward-char (- length 2)))))

(defun zs-send (bytes &optional no-checksum)
  (zs-send-string (zs-make-string bytes no-checksum)))

(defun zs-make-string (bytes &optional no-checksum)
  "Compute the checksum for BYTES and return a string to send to Z-Stick."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    ;; The checksum is simply all the bytes XOR'd together, but
    ;; starting with 255.
    (let ((checksum #xff))
      (insert (pop bytes))
      (dolist (i bytes)
	(insert i)
	(setq checksum (logxor checksum i)))
      (unless no-checksum
	(insert checksum))
      (buffer-string))))

(defun zs-send-string (string)
  (process-send-string (zs-process) string))

(defun zs-process ()
  (get-buffer-process (get-buffer zs-buffer)))

(provide 'z-stick)

;;; z-stick.el ends here
