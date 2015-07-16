# Clipper

[![Build Status](https://travis-ci.org/Rudolph-Miller/clipper.svg)](https://travis-ci.org/Rudolph-Miller/clipper)

Clipper is a file attachment library.

## Usage

```Lisp
(in-package :cl-user)
(defpackage clipper.sample
  (:use :cl
        :integral
        :clipper))
(in-package :clipper.sample)

(connect-toplevel :mysql :database-name "clipper_sample" :username "root")

(defclass picture ()
  ((id :col-type (:integer 11)
       :primary-key t
       :auto-increment t
       :not-null t
       :initarg :id)
   (image-file-name :col-type (:varchar 255)
                    :initarg :image-file-name)
   (image-content-type :col-type (:varchar 255)
                       :initarg :image-content-type)
   (image-file-size :col-type (:integer 11)
                    :initarg :image-file-size)
   (url :type string
        :initarg :url))
  (:metaclass <dao-table-class>)
  (:table-name "pictures"))

(setup-clipper :store-type :s3
               :aws-access-key (asdf::getenv "AWS_ACCESS_KEY")
               :aws-secret-key (asdf::getenv "AWS_SECRET_KEY")
               :s3-endpoint "s3-ap-northeast-1.amazonaws.com"
               :s3-bucket-name "clipper-sample"
               :clpper-class (find-class 'picture)
               :format ":ID/FILE-NAME.:EXTENSION")

(let ((object (create-dao 'picture)))
  (save-dao (attach-image object :url "http://www.lisperati.com/lisplogo_alien_256.png"))
  (image-url object))

=> "https://s3-ap-northeast-1.amazonaws.com/clipper-sample/1/lisplogo_alien_256.png"

(setup-clipper :store-type :local
               :image-directory #P"/home/cl-user/common-lisp/clipper/images/"
               :relative #P"/home/cl-user/common-lisp/clipper/"
               :prefix "http://localhost:3000/"
               :clpper-class (find-class 'picture)
               :format ":ID/FILE-NAME.:EXTENSION")

(let ((object (create-dao 'picture)))
  (save-dao (attach-image object :url "http://www.lisperati.com/lisplogo_alien_256.png"))
  (image-url object))

=> "http://localhsot:3000/images/2/lisplogo_alien_256.png"
```

## setup-clipper

```Lisp
(setup-clipper :store-type :local)
```

or

```Lisp
(setup-clipper :store-type :s3)
```

or you can create `:store-type` other than `:local` or `:s3`.

### :store-type :local

```Lisp
(setup-clipper :store-type :local
               :image-directory #P"/home/cl-user/common-lisp/clipper/images/"
               :relative #P"/home/cl-user/common-lisp/clipper/"
               :prefix "http://localhost:3000/"
               :clpper-class (find-class 'picture)
               :format ":ID/FILE-NAME.:EXTENSION")

(let ((object (create-dao 'picture)))
  (save-dao (attach-image object :url "http://www.lisperati.com/lisplogo_alien_256.png"))
  (image-url object))

=> "http://localhsot:3000/images/2/lisplogo_alien_256.png"
```

- `:image-directory`

```Lisp
(setup-clipper :store-type :local
               :image-directory #P"/home/cl-user/common-lisp/clipper/images/"
               :clpper-class (find-class 'picture)
               :format ":ID/FILE-NAME.:EXTENSION")

(let ((object (create-dao 'picture)))
  (save-dao (attach-image object :url "http://www.lisperati.com/lisplogo_alien_256.png"))
  (image-url object))

=> "/home/cl-user/common-lisp/clipper/images/2/lisplogo_alien_256.png"
```

- `:relative`

```Lisp
(setup-clipper :store-type :local
               :image-directory #P"/home/cl-user/common-lisp/clipper/images/"
               :relative #P"/home/cl-user/common-lisp/clipper/"
               :clpper-class (find-class 'picture)
               :format ":ID/FILE-NAME.:EXTENSION")

(let ((object (create-dao 'picture)))
  (save-dao (attach-image object :url "http://www.lisperati.com/lisplogo_alien_256.png"))
  (image-url object))

=> "images/2/lisplogo_alien_256.png"
```

- `:prefix`

```Lisp
(setup-clipper :store-type :local
               :image-directory #P"/home/cl-user/common-lisp/clipper/images/"
               :relative #P"/home/cl-user/common-lisp/clipper/"
               :prefix "http://localhost:3000/"
               :clpper-class (find-class 'picture)
               :format ":ID/FILE-NAME.:EXTENSION")

(let ((object (create-dao 'picture)))
  (save-dao (attach-image object :url "http://www.lisperati.com/lisplogo_alien_256.png"))
  (image-url object))

=> "http://localhost3000/images/2/lisplogo_alien_256.png"
```

### :store-type :s3

```Lisp
(setup-clipper :store-type :s3
               :aws-access-key (asdf::getenv "AWS_ACCESS_KEY")
               :aws-secret-key (asdf::getenv "AWS_SECRET_KEY")
               :s3-endpoint "s3-ap-northeast-1.amazonaws.com"
               :s3-bucket-name "clipper-sample"
               :clpper-class (find-class 'picture)
               :format "images:ID/FILE-NAME.:EXTENSION")

(let ((object (create-dao 'picture)))
  (save-dao (attach-image object :url "http://www.lisperati.com/lisplogo_alien_256.png"))
  (image-url object))

=> "https://s3-ap-northeast-1.amazonaws.com/clipper-sample/images/1/lisplogo_alien_256.png"
```

### :clipper-class

`:clipper-class` can take any class or struct which have slots for `id`, `image-file-name`, `image-content-type`, `image-file-size` and `url`, and each slot can be specified by `setup-clipper`.

```Lisp
(defclass picture ()
  ((id :col-type (:integer 11)
       :primary-key t
       :auto-increment t
       :not-null t
       :initarg :id)
   (image-file-name :col-type (:varchar 255)
                    :initarg :image-file-name)
   (image-content-type :col-type (:varchar 255)
                       :initarg :image-content-type)
   (image-file-size :col-type (:integer 11)
                    :initarg :image-file-size)
   (url :type string
        :initarg :url))
  (:metaclass <dao-table-class>)
  (:table-name "pictures"))

(setup-clipper :clpper-class (find-class 'picture)
               :id-slot 'id
               :url-slot 'url
               :image-file-name-slot 'image-file-name
               :image-content-type-slot 'image-content-type
               :image-file-size-slot 'image-file-size)
```

### :format

- `:format` can take string with `:KEYWORD` and `:KEYWORD` is declared in `*format-keys*`.
- Default declared `:KEYWORD` is `:ID`, `:URL`, `:FILE-NAME` and `:EXTENSION`.
- `*format-keys*` is a plist of `:KEYWORD` and `function` which will be called with `object`.
- `:FILE-NAME` will return `:image-file-name` without extension.

```Lisp
(defvar *format-keys*
  (list :ID #'clip-id
        :URL #'clip-url
        :FILE-NAME #'clip-image-file-name-without-extension
        :EXTENSION #'clip-extension))

(store-format (setup-clipper :format ":ID/:URL/:FILE-NAME.:EXTENSION"))

(make-instance 'picture :id 1 :url "sample-url" :image-file-name "smaple.png")

=> format will be "1/sample-url/sample.png"
```

## attach-image
- `attach-image` take `object` and keyword arguments(`:url`, `:image`, `:path-name` and `:file-name`).
- `attach-image` return `object` with `image-file-name`, `image-content-type`, `image-file-size` and `url`.
- You have to save returned `object` on yourself.

```Lisp
(let ((object (make-instance 'picture)))
  (attach-image object :url "http://www.lisperati.com/lisplogo_alien_256.png")
  (attach-image object :path-name "/home/lisp-user/image/lisplogo_alien_256.png")
  (attach-image object :image (drakma:http-request "http://www.lisperati.com/lisplogo_alien_256.png")
                       :file-name "lisplogo_alien_256.nng")

  ;; or
  (setf (picture-url object) "http://www.lisperati.com/lisplogo_alien_256.png")
  (attach-image object))
```

## attach-image with resize

If you add `:width` and `:height`, `attach-image` resize image with `opticl:fit-image-into`.

```Lisp
(setup-clipper :store-type :local
               :image-directory #P"/home/cl-user/common-lisp/clipper/images/"
               :relative #P"/home/cl-user/common-lisp/clipper/"
               :prefix "http://localhost:3000/"
               :clpper-class (find-class 'picture)
               :format ":ID/FILE-NAME.:EXTENSION"
               :width 200
               :height 200)
```

## See Also

- [Integral](https://github.com/fukamachi/integral) - Object relational mapper for Common Lisp
- [opticl](https://github.com/slyrus/opticl) -  A library for representing and processing images in Common Lisp

## Author

* Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)

## Copyright

Copyright (c) 2015 Rudolph-Miller (chopsticks.tk.ppfm@gmail.com)
