;;;; cepl.glfw.lisp

(in-package #:cepl.glfw)

(declaim (optimize (debug 3)))
;;======================================================================
;; api v1

(defun init ()
  (print "initializing GLFW")
  (let ((init-code (%glfw:init)))
    (unless (= init-code %glfw:+true+)
      (error "Failed to initialize GLFW"))))

(defgeneric glfw-init (&rest init-flags)
  (:method (&rest init-flags)
    (declare (ignore init-flags))
    (init)))

(defun init-glfw-low-level (&rest glfw-init-flags)
  (declare (ignore glfw-init-flags))
  (init))

;;----------------------------------------------------------------------

(defun glfw-shutdown ()
  (print "destroying GLFW 1")
  (low-level-quit))

(defun low-level-quit ()
  (print "destroying GLFW 2")
  (%glfw:terminate))

;;----------------------------------------------------------------------

(let ((listeners nil))
  ;;
  (defun glfw-register-listener (func)
    (unless (find func listeners)
      (push func listeners)))
  ;;
  (defun glfw-step-v1 (surface)
    (declare (ignore surface))
     (loop :for listener :in listeners :do
            (funcall listener nil))
     (%glfw:poll-events)))

;;----------------------------------------------------------------------

(defun glfw-swap (handle)
  (%glfw:swap-buffers handle))

;;----------------------------------------------------------------------

(defun make-glfw-context (surface version double-buffer
                          alpha-size depth-size stencil-size buffer-size
                          red-size green-size blue-size)
  (declare (ignorable version double-buffer
		      alpha-size depth-size stencil-size buffer-size
		      red-size green-size blue-size))
  (print "what")
  (print (list version double-buffer
	       alpha-size depth-size stencil-size buffer-size
	       red-size green-size blue-size))
  surface)

(defvar *core-context* t)

(defun glfw-make-current (context surface)
  (let ((a
	 (not (cffi:null-pointer-p (claw:ptr context))))
	(b
	 (not (cffi:null-pointer-p (claw:ptr surface)))))
    (%glfw:make-context-current (cond (a context)
				      (b surface)
				      (t (error "both the context and suface are null pointers!"))))))

;;----------------------------------------------------------------------

(defun make-glfw-surface (width height title fullscreen
                          no-frame alpha-size depth-size stencil-size
                          red-size green-size blue-size buffer-size
                          double-buffer hidden resizable)
  (declare (ignore fullscreen buffer-size))
  (print 2342342)
  (labels
      ((create-window (;major minor
		       )
	 (flet ((hint (enum bool)		  
		  (%glfw:window-hint enum (if bool
					      %glfw:+true+
					      %glfw:+false+
					      ))))
	   ;;(hint %glfw:+doublebuffer+ double-buffer)
	   (hint %glfw:+resizable+ resizable)
	   ;;(hint %glfw:+visible+ (not hidden))
	   ;;(hint %glfw:+decorated+ (not no-frame))
	   ;;(hint %glfw:+red-bits+ red-size)
	   ;;(hint %glfw:+green-bits+ green-size)
	   ;;(hint %glfw:+blue-bits+ blue-size)
	   ;;(hint %glfw:+depth-bits+ depth-size)
	   ;;(hint %glfw:+stencil-bits+ stencil-size)
	   ;;(hint %glfw:+alpha-bits+ alpha-size)
	   #+nil
	   (hint %glfw:+opengl-profile+
		 ;;%glfw:+opengl-any-profile+

		 ;;#+nil
		 (if *core-context*
		     %glfw:+opengl-core-profile+
		     %glfw:+opengl-compat-profile+))
	   ;;(hint %glfw:+context-version-major+ major)
	   ;;(hint %glfw:+context-version-minor+ minor)
	   )
	   
	 (let ((window (%glfw:create-window width height title nil nil)))
	   (if (not (cffi:null-pointer-p (claw:ptr window)))
	       (progn
		 (%glfw:make-context-current window)
		 window)
	       (error "why no glfw window?"))))
       ;; (create-context-by-version (version)
       ;;   (destructuring-bind (&optional major minor)
       ;;       (cepl.context:split-float-version version)
       ;;     (create-window major minor)))

       #+nil
       (search-for-context ()
         (let ((context nil))
           (loop :for (major minor) :in `((4 5) (4 4) (4 3)
                                          (4 2) (4 1) (4 0)
                                          (3 3) (3 1))
              :until context
              :do (setf context (create-window major minor)))
           (assert context)
           context)))
    (create-window)
    ;(search-for-context)
    ))

(defun destroy-glfw-surface (surface)
  (%glfw:destroy-window surface))

(defun glfw-surface-size (win-handle)
  (cffi:with-foreign-objects ((w :int)
			      (h :int))
    (%glfw:get-window-size win-handle w h)
    (list (cffi:mem-ref w :int)
	  (cffi:mem-ref h :int))))

(defun glfw-set-surface-size (win-handle width height)
  (%glfw:set-window-size win-handle width height))

(defun glfw-surface-fullscreen-p (surface)
  (declare (ignore surface))
  nil)

(defun glfw-set-surface-fullscreen (surface state)
  (declare (ignore surface state))
  t)

(defun glfw-surface-title (surface)
  (declare (ignore surface))
  nil)

(defun glfw-set-surface-title (surface title)
  (%glfw:set-window-title surface title))

;;----------------------------------------------------------------------

(defclass glfw-api (cepl.host:api-1)
  (;;
   (supports-multiple-contexts-p :initform nil)
   ;;
   (supports-multiple-surfaces-p :initform t)
   ;;
   (init-function :initform #'glfw-init)
   ;;
   (shutdown-function :initform #'glfw-shutdown)
   ;;
   (make-surface-function :initform #'make-glfw-surface)
   ;;
   (destroy-surface-function :initform #'destroy-glfw-surface)
   ;;
   (make-context-function :initform #'make-glfw-context)
   ;;
   (step-function :initform #'glfw-step-v1)
   ;;
   (register-event-callback-function :initform #'glfw-register-listener)
   ;;
   (swap-function :initform #'glfw-swap)
   ;;
   (surface-size-function :initform #'glfw-surface-size)
   ;;
   (make-context-current-function :initform #'glfw-make-current)
   ;;
   (set-surface-size-function :initform #'glfw-set-surface-size)
   ;;
   (surface-fullscreen-p-function :initform #'glfw-surface-fullscreen-p)
   ;;
   (set-surface-fullscreen-function :initform #'glfw-set-surface-fullscreen)
   ;;
   (surface-title-function :initform #'glfw-surface-title)
   ;;
   (set-surface-title-function :initform #'glfw-set-surface-title)))

(register-host 'glfw-api)

;;----------------------------------------------------------------------

(defun (setf vsync) (boolean)
  ;;(warn "Sorry setting vsync is not supported")
  (%glfw:swap-interval (if boolean 1 0)) ;;1 is on
  boolean)
