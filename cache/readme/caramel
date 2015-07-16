# Caramel

Enlive like css selector-based template system in Common Lisp

## Installation

    (ql:quickload :caramel)

## Usage

### html-resource

build dom from html file

    (html-resource #p"/path/to/your/html/file")

### select

search node by css selector

    (select "#id" dom-node)
    => Matching node list 

### Translator

Every translator returns a function 
which takes a node and return translated node or nodes.

#### set-attr

Set attributes to node

    (set-attr :foo "baz" :bar "wow")

#### remove-attr

Remove attributes from node

    (remove-attr :foo :bar)


#### add-class

Add css classes to node

    (add-class "cls-foo" "cls-bar")

#### remove-class

Remove css classes from node

    (remove-class "cls-foo" "cls-bar")


#### content

Set content of node

    (content "foo" a-node "foo")

#### html-content

Build html from specified string and set

    (html-content "<p>Foo</p>")


#### wrap

Wrap node with specified tag

    (wrap "p")

#### unwrap

Get content of node

    (unwrap)

#### do->

Cascade transform to node

    (do-> (content "foo") (set-attr :color "green") (add-class "cls-foo"))

#### before

Insert nodes before node
    
    (before "foo" a-node "baz")

#### after

Insert nodes after node

    (after "foo" a-node "baz")

#### substitute
    
Replace node with nodes

    (substitute "foo" a-node "baz")

#### move

Move matched node

    (move src-selector dst-selector)

#### clone-for

Clone nodes

    (clone-for x '(1 2 3) (content x))
    
    (clone-for x '(1 2 3)
     "p" (content x)
     "h1" (content "foo"))

## Snippet & Template 

### defsnippet

Define snippet from file.

    (defsnippet bar #p"/path/to/your/file" "div#baz" ()
      "p" (content "foo"))

    (bar)
    => node-list
      
### deftemplate

Define template from file.

    (deftemplate foo #p"/path/to/your/base/file" (&optional foo)
      "#bar" (do-> 
              (content "fuge") 
              (set-attr :color "green") 
              (add-class "cls-foo"))
      "p#para" (if foo
                 (content foo)
                 (content "defaul")))

## Scraping

### get-attr

Get attribute value of node

    (get-attr node name)

### get-attrs

Get attribute alist of node

    (get-attrs node)

### get-content

Get content of node

    (get-content text-node)
    => content string
    (get-content document-or-element)
    => children-list

#### example

        (defun -> (&rest fns)
         (lambda (init)
          (loop with citem = init
           for fn in fns
           do
           (setf citem (funcall fn citem))
           finally (return citem))))

        (defun google-search (word)
           (let* ((query (list (cons "q" word)))
                  (str (drakma:http-request "http://www.google.com/search"
                                      :parameters query))
                  (dom (html-resource str)))
                (loop 
                   for node in (select "h3.r" dom)
                   collect (get-attr (funcall (-> (unwrap) #'first) node) "href"))))

        (google-search "foo")
        =>
        ("/url?q=http://en.wikipedia.org/wiki/Foobar&sa=U&ei=Oas2UZrOFoyIkwXot4C4Cw&ved=0CBgQFjAA&usg=AFQjCNENNqcYY0yw8Y9RKmzildDpcRlcSg"
         "/url?q=http://www.foo.com/&sa=U&ei=Oas2UZrOFoyIkwXot4C4Cw&ved=0CCEQFjAB&usg=AFQjCNEi6s8gBpsT6sK5Em5Rq-zpL6v01w"
         "/url?q=http://www.urbandictionary.com/define.php%3Fterm%3Dfoo&sa=U&ei=Oas2UZrOFoyIkwXot4C4Cw&ved=0CCUQFjAC&usg=AFQjCNFC3xe17h6LLn86ZXUtY4CXfCcOwQ"
         "/url?q=http://catb.org/jargon/html/F/foo.html&sa=U&ei=Oas2UZrOFoyIkwXot4C4Cw&ved=0CCkQFjAD&usg=AFQjCNFmr2ssHlV9Sjrrq833Rz8TjsDSFQ"
         "/url?q=http://foofood.ca/&sa=U&ei=Oas2UZrOFoyIkwXot4C4Cw&ved=0CCwQFjAE&usg=AFQjCNFVHsem3EcurfHqsByEIR70wJ0vNA"
         "/url?q=http://www.foofighters.com/&sa=U&ei=Oas2UZrOFoyIkwXot4C4Cw&ved=0CC8QFjAF&usg=AFQjCNFgY5a73m8zvOltlo1SeHm3h0asUw"
         "/url?q=http://www.forgetfoo.com/&sa=U&ei=Oas2UZrOFoyIkwXot4C4Cw&ved=0CDMQFjAG&usg=AFQjCNFHZEG0pjLC-fwDNKPOv6MZu4Y4qQ"
         "/url?q=http://www.foo-apartment.com/&sa=U&ei=Oas2UZrOFoyIkwXot4C4Cw&ved=0CDgQFjAH&usg=AFQjCNHkIFr_2j-KtvcHPxWd-XMhLudjcQ"
         "/url?q=http://www.facebook.com/foofighters&sa=U&ei=Oas2UZrOFoyIkwXot4C4Cw&ved=0CDwQFjAI&usg=AFQjCNE-mJsSRMzQUdMOXjPToJljjmVeFg"
         "/url?q=http://www.ietf.org/rfc/rfc3092.txt&sa=U&ei=Oas2UZrOFoyIkwXot4C4Cw&ved=0CEAQFjAJ&usg=AFQjCNFYfQd6aQqdZy9M5W4lzgTkosaniA")


## Author

* Masato Sogame (poketo7878@gmail.com)

## Copyright

Copyright (c) 2013 Masato Sogame (poketo7878@gmail.com)

# License

        Licensed under the LLGPL License.


