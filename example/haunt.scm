(use-modules (haunt asset)
             (haunt builder blog)
             (haunt builder atom)
             (haunt builder assets)
             (haunt reader)
             (haunt reader skribe)
             (haunt site))

(site #:title "Built with Guile"
      #:domain "example.com"
      #:default-metadata
      '((author . "Eva Luator")
        (email  . "eva@example.com"))
      #:readers (list skribe-reader sxml-reader html-reader)
      #:builders (list (blog)
                       (atom-feed)
                       (atom-feeds-by-tag)
                       (static-directory "images")))
