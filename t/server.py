# Taken and adapted from https://github.com/msgpack-rpc/msgpack-rpc-python/blob/master/test/test_msgpackrpc.py
from time import sleep
import threading

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

if __name__ == "__main__":
    server = msgpackrpc.Server(TestServer())
    server.listen(msgpackrpc.Address("localhost", 18800))
    server.start()
