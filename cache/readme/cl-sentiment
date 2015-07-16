cl-sentiment
============

cl-sentiment is a Common Lisp library that uses the AFINN-111 word list
to perform sentiment analysis on arbitrary text.

Inspired by https://github.com/thisandagain/sentiment, it's
particularly suited to looking at Twitter tweets.

The AFINN word list is by Finn Arup Nielsen available from
http://www2.imm.dtu.dk/pubdb/views/publication_details.php?id=6010

You might also like to read his paper:

"A new ANEW: Evaluation of a word list for sentiment analysis in
microblogs", Proceedings of the ESWC2011 Workshop on 'Making Sense of
Microposts': Big things come in small packages 718 in CEUR Workshop
Proceedings : 93-98. 2011 May.  http://arxiv.org/abs/1103.2903

The AFINN files are copyright protected and distributed under the
terms of the Open Database License (ODbL) v1.0,
http://www.opendatacommons.org/licenses/odbl/1.0/

cl-sentiment is now available via Quicklisp.

Use initialize to initialize cl-sentiment with the default sentiment
word scores.

Call sentiment with some arbitrary text and it returns two values -
the sentiment score and the comparative sentiment score. Positive
values convey positive sentiment; negative values convey negative
sentiment.

Example
-------

	CL-USER> (ql:quickload "cl-sentiment")
	...
	CL-USER> (cl-sentiment:initialize)
	#<HASH-TABLE :TEST EQUAL :COUNT 2477 {1003A54463}>
	CL-USER> (cl-sentiment:sentiment "Cats are totally amazing!")
	4
	1
	CL-USER> 

Rob Blackwell

December 2012
