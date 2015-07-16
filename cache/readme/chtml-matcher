# chtml-matcher

## A simple Lisp-based DSL for extracting information from web pages

chtml-matcher performs pattern-based unification over HTML via a set
of compiled nested closures.  It uses the closure-html library to
parse HTML to lhtml, a lisp form of HTML. A template list is passed to
(match-template template lhtml) and returns a bindings object
containing an alist of all the extracted information. 

The semantics are reasonably intuitive, but might require a little
playing around to get a good feel for how to solve most common
problems. The API is small and the package.lisp provides pointers to
where to look. The whole library is less than 1k lines of code so easy
enough to read through.

## Download and Dependencies

Clone it from github.  The [old
repository](http://common-lisp.net/project/chtml-matcher) on
common-lisp.net is deprecated.

chtml-matcher depends on my home-brew cl-stdutils, closure-html,
cl-ppcre, and f-underscore, although all but closure-html could be
removed if necessary.

## LXML Template Unfication

The DSL provides a light-weight way to extract fields from nested
HTML/XML structure represented in LHTML (as produced by closure-html).
A template is a declarative representation of substructure with
embedded variables that are bound when the substructure matches.

Substructure is loosely matched, such that if any given body element
doesn't match, the next child is considered until all the template
body elements have matched a lhtml element or the end of the elment
has been reached without a match.

Prepending < to a tag enables a depth-first search for that tag so you
can avoid specifying the parent path (similar to // in xpath)

Any matching template that consists of a variable reference results in
a binding set being created and returned if all elements of the
template node successfully match.

Additional reserved operators allow you more flexibility on 
managing what is matched and how bindings from subtrees are combined

all: match same template multiple times over the children of a given node
     and store them as a list attached to a fresh bindings list

merge: create a single binding out of each of the sub-bindings.  A 
     node body has an implicit merge

nth: find the nth instance that matches the full body of this operator

regex: matches if regex returns register values for a string (as a list)

fn: Run the referenced function symbol on the current parse state and
    return bindings, t or nil as appropriate.

## Example

I've recently been mining some posts from vBulletin sites. I go to the
last day's posts, get a list of all the new posts, then go to the
thread and grab the post body. The following two templates do 90% of
the work. Of course, I have to write code to convert the data I
extract to web page fetches, etc.

	(defparameter *vbulletin-search-template*
	  '(<tbody nil
	     (all ?records 
	       (tr nil
	         (td nil)
		   (td ((class "alt1"))
		     (div nil
		       (a ((href ?thread-uri))
		         ?thread-name)))
	         (td ((class "alt2") (title ?activity))
	           (div nil ?post-date
		     (span nil ?post-time)
		     (a ((href ?user-uri))
		       ?username)
		     (a ((href ?last-post-uri)))))))))
        =>
        '(:records ((:thread-name . "Thread name")
                    (:thread-uri  . "Thread URI")
                    (:post-date   . "Date String")
		    (:post-time   . "Time String")
                    (:username    . "Username")
                    (:user-uri    . "URI String")
                    (:last-post-uri . "URI String"))
                   ...)

This looks for a table body in the search results page, then gets
bindings for all matching <tr> elements and puts them within another
bindings object bound to :records as specified by 'all'. The pattern
pulls out all the user, thread, post and date information for all
results. You can match elements on strings, regular expressions and
arbitrary function calls as well.

I use subst to customize the following pattern to find a particular
post in a page. It replaces 'post_message_?' with a unique id for a
post then returns its thread number and the entire post body.

      (defparameter *vbulletin-post-template*
        `(<tbody nil 
	   (tr nil (<a ((name ?post-num))))
	   (tr nil)
	   (tr nil (?post-body <div ((id "post_message_?"))))))

I use Firefox FireBug to inspect the HTML tree, identify the best
unique enclosing context I can specify and then provide enough
structure to uniquely capture the data I want. This approach is highly
robust to many small HTML changes and should be reasonably fast.


