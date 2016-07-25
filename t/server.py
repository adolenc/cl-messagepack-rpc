# Taken and adapted from https://github.com/msgpack-rpc/msgpack-rpc-python/blob/master/test/test_msgpackrpc.py
from time import sleep
import signal, sys, threading
import msgpack
import msgpackrpc
from msgpackrpc import error

class TestServer(object):
    def __init__(self):
        self.register_ = []

    def hello(self):
        return "world"

    def sum(self, x, y):
        return x + y

    def nil(self):
        return None

    def push_to_register(self, n):
        self.register_.append(n)
        return self.register_

    def clear_register(self):
        self.register_ = []
        return True

    def get_ext_type(self, id, data):
        return msgpack.ExtType(id, str(data))

    def id(self, x):
        return x

    def raise_error(self):
        raise Exception('error')

    def long_exec(self):
        sleep(3)
        return 'finish!'

    def async_result(self):
        ar = msgpackrpc.server.AsyncResult()
        def do_async():
            sleep(2)
            ar.set_result("You are async!")
        threading.Thread(target=do_async).start()
        return ar

def kill_server(server):
    def handler(signal, frame):
        print "Stopping server..."
        server.stop()
        print "Server stopped."
        sys.exit(0)
    return handler

if __name__ == "__main__":
    try:
        server = msgpackrpc.Server(TestServer())
        server.listen(msgpackrpc.Address("localhost", 18800))
        signal.signal(signal.SIGINT, kill_server(server))
        print "Starting server on localhost:18800."
        server.start()
    except Exception as err:
        print "Could not start server on localhost:18800."
        sys.exit(1)
