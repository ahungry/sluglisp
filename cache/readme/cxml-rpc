# CXML-RPC: An xml-rpc library for Common Lisp

This library implements xml-rpc client and server functionality using [Drakma](http://weitz.de/drakma), [CXML](http://common-lisp.net/project/cxml/) and [Hunchentoot](http://weitz.de/hunchentoot). 

## Installation


Get it via git:

	git clone http://github.com/antifuchs/cxml-rpc
	
Then adjust your ASDF to find cxml-rpc.asd. After that, 

	(asdf:oos 'asdf:load-op :cxml-rpc)

## Usage


### An XML-RPC Client

Acting as an XML-RPC client is pretty easy: Just call the functions on the endpoints you need. The function's first return value is the value returned by the server, and the second return value is its type.

	* (xrpc:call "http://betty.userland.com/RPC2" "examples.getStateName" '(:integer 41))
	"South Dakota"
	:STRING

##### Argument passing

Since the mapping of CL types to XML-RPC types is not easily DWIM-able, you need to pass args with explicit type hints. CXML-RPC maps these types (as keywords):

* `:boolean` - generalized boolean
* `:integer` - 4-octet integers
* `:double` - floating-point doubles
* `:string` - strings
* `:time` - universal times as CL integers
* `:base64` - strings, file streams or vectors containing (unsigned-byte 8) elements.
* `:struct` - alists
* `:array` - sequences

### An XML-RPC Server

You can run multiple XML RPC handlers in your hunchentoot instance: These are identified by a handler tag that you can choose. This tag is not exposed to the outside world, so you can run multiple sites (or endpoints) with a different set of xml-rpc methods in the same lisp image.

#### Step one: Teach hunchentoot about your handler.

	(push 
         (hunchentoot:create-prefix-dispatcher "/RPC2" 
                                               (cxml:cxml-rpc-method-handler 'some-tag))
         hunchentoot:*dispatch-table*)

This tells hunchentoot to dispatch xml-rpc calls on /RPC2 to dispatch xml-rpc methods. This handler will serve only methods that are defined with the group tag some-tag.
	
#### Step two: Define methods.

Define-xrpc-method is what you use to create a function that can be called from xml-rpc. Its arguments are:

1. The function name and handler group (following the example above, that should be some-tag),
2. a list of argument variables,
3. a type signature: First the return type, then the parameter types,
4. an optional docstring.

CXML-RPC allows introspection via the system.getCapabilities, system.listMethods, system.methodHelp, and system.methodSignature.

	(define-xrpc-method (add some-tag) (a b) (:integer :integer :integer)
	  "Add two numbers"
	  (+ a b))
	