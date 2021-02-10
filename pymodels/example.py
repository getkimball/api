from concurrent import futures
import queue

from river import linear_model
import grpc

import features_proto_pb2 as pb2
import features_proto_pb2_grpc as pb2_grpc



class KimballIntegrationServer(pb2_grpc.KimballIntegrationServicer):

    def __init__(self, q):
        self.q = q

    def EventStream(self, request_it, context):
        print('event stream starting')
        for e in request_it:
            Goal = "goal" in e.name or 'paid' in e.name
            print(e, "is goal:", Goal)
            ed = event_to_learnable(e)
            print(ed)
            self.q.put((ed, Goal))
            print('q size', self.q.qsize())

def modeler(q):
    model = linear_model.LogisticRegression()
    print('Modeler started')
    def c():
        print('Modeler runtime started')
        while True:
            print('size', q.qsize())
            (obj, is_goal) = q.get()
            print("Got item", obj, is_goal)
            try:
                model.learn_one(obj, is_goal)
            except Exception as e:
                print('error', e)
            predict_obj = {'k:10': 1}
            print("predict:", predict_obj,  model.predict_proba_one(predict_obj))
            print(model.weights)
    return c

def event_to_learnable(e):
    return {"k:"+ e.key :1, 'n:'+e.namespace:1, 'e:'+e.name:1}

def main():
    model = linear_model.LogisticRegression()
    pool = futures.ThreadPoolExecutor(max_workers=10)
    server = grpc.server(pool)
    q = queue.Queue(5)
    pool.submit(modeler(q))
    KIS = KimballIntegrationServer(q)
    pb2_grpc.add_KimballIntegrationServicer_to_server(KIS , server)
    server.add_insecure_port('127.0.0.1:8079')
    server.start()
    server.wait_for_termination()


if __name__ == "__main__":
    main()
