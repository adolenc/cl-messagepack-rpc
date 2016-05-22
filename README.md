cl-messagepack-rpc
==================
[![Build Status](https://travis-ci.org/adolenc/cl-messagepack-rpc.svg?branch=master)](https://travis-ci.org/adolenc/cl-messagepack-rpc)
[![Coverage Status](https://coveralls.io/repos/github/adolenc/cl-messagepack-rpc/badge.svg?branch=master)](https://coveralls.io/github/adolenc/cl-messagepack-rpc?branch=master)
[![Quicklisp dist](http://quickdocs.org/badge/cl-messagepack-rpc.svg)](http://quickdocs.org/cl-messagepack-rpc/)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](./LICENSE)

A Common Lisp library implementing the [MessagePack-RPC](https://github.com/msgpack-rpc/msgpack-rpc) [specification](https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md), which uses [MessagePack](http://msgpack.org/) serialization format to achieve efficient [remote procedure calls (RPCs)](https://en.wikipedia.org/wiki/Remote_procedure_call).

Library uses [cl-messagepack](https://github.com/mbrezu/cl-messagepack) and [cl-async](https://github.com/orthecreedence/cl-async) under the hood. Currently only the client side functionality is fully supported, but some server-side features (such as registering callbacks for sessions) are also implemented. Library supports connecting to the server via TCP sockets, named pipes or via user-specified streams (e.g. standard input and output streams).

## Installing package
The simplest way to install the package and the dependencies is to use [quicklisp](https://www.quicklisp.org/). For now you will need to manually clone this repository into your `~/quicklisp/local-projects` folder:

    $ git clone https://github.com/adolenc/cl-messagepack-rpc ~/quicklisp/local-projects/cl-messagepack-rpc

After that, evaluating

    * (ql:quickload :cl-messagepack-rpc)

from your REPL should properly load all the dependencies and cl-messagepack-rpc itself.

In order to get cl-async working, you will also need `libuv1-dev` binaries, which you can install using your package manager.

## Usage
This library tries to follow the official specification as closely as possible. For a quick taste:

```common-lisp
(defparameter *client* (make-instance 'mrpc:client :file "/path/to/named/pipe"))
(mrpc:call *client* "echo" "Hello server!")
;=> "Hello server!"

(defparameter *future* (mrpc:call-async *client* "execute_some_long_task"))
;=> *future*

(mrpc:request *client* "add" 3 2) ; mrpc:request is an alias for mrpc:call
;=> 5

(mrpc:join *future*) ; calling join on future also returns its result (or throws an error)
;=> "Done with the execution of some long task!"

(mrpc:notify *client* "client_finished")
;=> T
```

## Exported symbols

#### \*extended-types\*
Export of [\*extended-types\* parameter from cl-mesasgepack](https://github.com/mbrezu/cl-messagepack#extended-types).

#### session
Class serving as a superclass to client. 

#### client (session)
Main class used to connect to an already running server. You can specify which means of transport the server uses via `make-instance` call:
```common-lisp
(defparameter *client*  (make-instance 'client :host "127.0.0.1" :port 1985)) ; to connect via TCP
(defparameter *client2* (make-instance 'client :file "/path/to/named/pipe")) ; to connect via named pipe
(defparameter *client3* (make-instance 'client)) ; to connect via standard input/output
(defparameter *client4* (make-instance 'client :input-stream *standard-input* :output-stream *output-stream*)) ; same as above, but can be used for any (binary) input and output streams
```

#### register-callback (session method callback)
Register a CALLBACK with name METHOD for some SESSION. Callback should be something callable:
```common-lisp
(register-callback *client* "add" #'(lambda (a b) (+ a b)))
```

#### remove-callback (session method)
Remove a previously registered callback with name METHOD from SESSION.
```common-lisp
(remove-callback *client* "add")
```

#### call-async (session method &rest params) => future
Use SESSION to call METHOD with PARAMS and immediately return control to the caller, returning a [future](#future) object.
```common-lisp
(call-async *client* "server_side_add" 1 2 3)
;=> #<EVENT-LOOP:FUTURE {100962B8F3}>
```

#### call (session method &rest params)
Invoke [CALL-ASYNC](#call-async-session-method-rest-params--future) on the passed arguments, and call [JOIN](#join-future) on the returned future.
```common-lisp
(call *client* "server_side_add" 1 2 3)
;=> 6
```

#### request (session method &rest params)
Alias for [CALL](#call-session-method-rest-params).

#### notify (session method &rest params)
Use SESSION to call METHOD with PARAMS, immediately returning control to the caller. This call completely ignores server responses.
```common-lisp
(notify *client* "do_something")
;=> T
```

#### future
Class used to hold responses from the server. You should not need to create future objects by hand.

#### join (future)
Block execution until FUTURE has a result from the server. Then either return a result, or throw an error, depending on how the server responded.
```common-lisp
(let ((future (call-async *client* "add" 3 2)))
  ; do something...
  (join future))
;=> 5

(let ((future (call-async *client* "add" 3 "some string")))
  ; do something...
  (join future))
;=> ERROR: unexpected types of arguments.
```

## Running unit tests
Because server-side support is not yet implemented, tests use a python based server. So in order to test the library, first start the python server:

    $ python t/server.py

and then evaluate

    * (asdf:test-system :cl-messagepack-rpc)

in the REPL to run the actual tests.

## Support
The library is developed and tested with `sbcl` under Debian GNU/Linux, but should work everywhere [cl-async](https://github.com/orthecreedence/cl-async) does.

## License
Copyright (c) 2016 Andrej Dolenc

Licensed under the MIT License.
