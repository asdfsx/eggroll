# Generated by the gRPC Python protocol compiler plugin. DO NOT EDIT!
import grpc

import kv_pb2 as kv__pb2
import processor_pb2 as processor__pb2
import storage_basic_pb2 as storage__basic__pb2


class ProcessServiceStub(object):
  # missing associated documentation comment in .proto file
  pass

  def __init__(self, channel):
    """Constructor.

    Args:
      channel: A grpc.Channel.
    """
    self.map = channel.unary_unary(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/map',
        request_serializer=processor__pb2.UnaryProcess.SerializeToString,
        response_deserializer=storage__basic__pb2.StorageLocator.FromString,
        )
    self.mapValues = channel.unary_unary(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/mapValues',
        request_serializer=processor__pb2.UnaryProcess.SerializeToString,
        response_deserializer=storage__basic__pb2.StorageLocator.FromString,
        )
    self.join = channel.unary_unary(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/join',
        request_serializer=processor__pb2.BinaryProcess.SerializeToString,
        response_deserializer=storage__basic__pb2.StorageLocator.FromString,
        )
    self.reduce = channel.unary_stream(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/reduce',
        request_serializer=processor__pb2.UnaryProcess.SerializeToString,
        response_deserializer=kv__pb2.Operand.FromString,
        )
    self.mapPartitions = channel.unary_unary(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/mapPartitions',
        request_serializer=processor__pb2.UnaryProcess.SerializeToString,
        response_deserializer=storage__basic__pb2.StorageLocator.FromString,
        )
    self.mapPartitions2 = channel.unary_unary(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/mapPartitions2',
        request_serializer=processor__pb2.UnaryProcess.SerializeToString,
        response_deserializer=storage__basic__pb2.StorageLocator.FromString,
        )
    self.glom = channel.unary_unary(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/glom',
        request_serializer=processor__pb2.UnaryProcess.SerializeToString,
        response_deserializer=storage__basic__pb2.StorageLocator.FromString,
        )
    self.sample = channel.unary_unary(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/sample',
        request_serializer=processor__pb2.UnaryProcess.SerializeToString,
        response_deserializer=storage__basic__pb2.StorageLocator.FromString,
        )
    self.subtractByKey = channel.unary_unary(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/subtractByKey',
        request_serializer=processor__pb2.BinaryProcess.SerializeToString,
        response_deserializer=storage__basic__pb2.StorageLocator.FromString,
        )
    self.filter = channel.unary_unary(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/filter',
        request_serializer=processor__pb2.UnaryProcess.SerializeToString,
        response_deserializer=storage__basic__pb2.StorageLocator.FromString,
        )
    self.union = channel.unary_unary(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/union',
        request_serializer=processor__pb2.BinaryProcess.SerializeToString,
        response_deserializer=storage__basic__pb2.StorageLocator.FromString,
        )
    self.flatMap = channel.unary_unary(
        '/com.webank.ai.eggroll.api.computing.processor.ProcessService/flatMap',
        request_serializer=processor__pb2.UnaryProcess.SerializeToString,
        response_deserializer=storage__basic__pb2.StorageLocator.FromString,
        )


class ProcessServiceServicer(object):
  # missing associated documentation comment in .proto file
  pass

  def map(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')

  def mapValues(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')

  def join(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')

  def reduce(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')

  def mapPartitions(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')

  def mapPartitions2(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')

  def glom(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')

  def sample(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')

  def subtractByKey(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')

  def filter(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')

  def union(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')

  def flatMap(self, request, context):
    # missing associated documentation comment in .proto file
    pass
    context.set_code(grpc.StatusCode.UNIMPLEMENTED)
    context.set_details('Method not implemented!')
    raise NotImplementedError('Method not implemented!')


def add_ProcessServiceServicer_to_server(servicer, server):
  rpc_method_handlers = {
      'map': grpc.unary_unary_rpc_method_handler(
          servicer.map,
          request_deserializer=processor__pb2.UnaryProcess.FromString,
          response_serializer=storage__basic__pb2.StorageLocator.SerializeToString,
      ),
      'mapValues': grpc.unary_unary_rpc_method_handler(
          servicer.mapValues,
          request_deserializer=processor__pb2.UnaryProcess.FromString,
          response_serializer=storage__basic__pb2.StorageLocator.SerializeToString,
      ),
      'join': grpc.unary_unary_rpc_method_handler(
          servicer.join,
          request_deserializer=processor__pb2.BinaryProcess.FromString,
          response_serializer=storage__basic__pb2.StorageLocator.SerializeToString,
      ),
      'reduce': grpc.unary_stream_rpc_method_handler(
          servicer.reduce,
          request_deserializer=processor__pb2.UnaryProcess.FromString,
          response_serializer=kv__pb2.Operand.SerializeToString,
      ),
      'mapPartitions': grpc.unary_unary_rpc_method_handler(
          servicer.mapPartitions,
          request_deserializer=processor__pb2.UnaryProcess.FromString,
          response_serializer=storage__basic__pb2.StorageLocator.SerializeToString,
      ),
      'mapPartitions2': grpc.unary_unary_rpc_method_handler(
          servicer.mapPartitions2,
          request_deserializer=processor__pb2.UnaryProcess.FromString,
          response_serializer=storage__basic__pb2.StorageLocator.SerializeToString,
      ),
      'glom': grpc.unary_unary_rpc_method_handler(
          servicer.glom,
          request_deserializer=processor__pb2.UnaryProcess.FromString,
          response_serializer=storage__basic__pb2.StorageLocator.SerializeToString,
      ),
      'sample': grpc.unary_unary_rpc_method_handler(
          servicer.sample,
          request_deserializer=processor__pb2.UnaryProcess.FromString,
          response_serializer=storage__basic__pb2.StorageLocator.SerializeToString,
      ),
      'subtractByKey': grpc.unary_unary_rpc_method_handler(
          servicer.subtractByKey,
          request_deserializer=processor__pb2.BinaryProcess.FromString,
          response_serializer=storage__basic__pb2.StorageLocator.SerializeToString,
      ),
      'filter': grpc.unary_unary_rpc_method_handler(
          servicer.filter,
          request_deserializer=processor__pb2.UnaryProcess.FromString,
          response_serializer=storage__basic__pb2.StorageLocator.SerializeToString,
      ),
      'union': grpc.unary_unary_rpc_method_handler(
          servicer.union,
          request_deserializer=processor__pb2.BinaryProcess.FromString,
          response_serializer=storage__basic__pb2.StorageLocator.SerializeToString,
      ),
      'flatMap': grpc.unary_unary_rpc_method_handler(
          servicer.flatMap,
          request_deserializer=processor__pb2.UnaryProcess.FromString,
          response_serializer=storage__basic__pb2.StorageLocator.SerializeToString,
      ),
  }
  generic_handler = grpc.method_handlers_generic_handler(
      'com.webank.ai.eggroll.api.computing.processor.ProcessService', rpc_method_handlers)
  server.add_generic_rpc_handlers((generic_handler,))
