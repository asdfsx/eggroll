/*
 * Copyright (c) 2019 - now, Eggroll Authors. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 *
 */

package com.webank.eggroll.rollpair.component

import java.nio.ByteBuffer
import java.util
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong}
import java.util.concurrent.{CompletableFuture, CountDownLatch, TimeUnit}
import java.util.function.Supplier

import com.google.common.collect.Lists
import com.google.protobuf.{ByteString, UnsafeByteOperations}
import com.webank.eggroll.core.constant.{NetworkConstants, StringConstants}
import com.webank.eggroll.core.datastructure.{Broker, LinkedBlockingBroker}
import com.webank.eggroll.core.error.DistributedRuntimeException
import com.webank.eggroll.core.meta.MetaModelPbMessageSerdes._
import com.webank.eggroll.core.meta._
import com.webank.eggroll.core.transfer.{GrpcTransferClient, GrpcTransferService}
import com.webank.eggroll.core.util.{Logging, ThreadPoolUtils}
import com.webank.eggroll.rollpair.io.RocksdbSortedKvAdapter
import io.grpc.netty.shaded.io.netty.buffer.UnpooledUnsafeDirectByteBuf

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait Shuffler {
  def start(): Unit
  def isFinished(): Boolean
  def waitUntilFinished(timeout: Long, unit: TimeUnit): Boolean
  def getTotalPartitionedCount(): Long
  def hasError(): Boolean
  def getError(): DistributedRuntimeException
}

object Shuffler {
  val idPrefix = classOf[Shuffler].getSimpleName
  val idIndex = new AtomicInteger(0)
  def generateId(idPrefix: String = Shuffler.idPrefix,
                 idIndex: Int = Shuffler.idIndex.incrementAndGet()) = s"${idPrefix}-${idIndex}"
}


// todo: move calculation to threads
class DefaultShuffler(shuffleId: String,
                      map_results_broker: Broker[(Array[Byte], Array[Byte])],
                      outputStore: ErStore,
                      outputPartition: ErPartition,
                      partitionFunction: Array[Byte] => Int,
                      parallelSize: Int = 5) extends Shuffler with Logging {

  val partitionThreadPool = ThreadPoolUtils.newFixedThreadPool(parallelSize, s"${shuffleId}")
  val sendRecvThreadPool = ThreadPoolUtils.newFixedThreadPool(2, prefix = s"${shuffleId}-sendrecv")

  val outputPartitions = outputStore.partitions
  val outputPartitionsCount = outputPartitions.length

  val partitionedBrokers = for (i <- 0 until outputPartitionsCount) yield new LinkedBlockingBroker[(Array[Byte], Array[Byte])]()

  val shuffleFinishLatch = new CountDownLatch(1);
  val isPartitionFinished = new AtomicBoolean(false)
  val isSendFinished = new AtomicBoolean(false)
  val isRecvFinished = new AtomicBoolean(false)
  val totalPartitionedElementsCount = new AtomicLong(0L)
  val errors = new DistributedRuntimeException

  def start(): Unit = {
    GrpcTransferService.getOrCreateBroker(key = s"${shuffleId}-${outputPartition.id}", writeSignals = outputPartitionsCount)

    val partitionSubFutures = ArrayBuffer[CompletableFuture[Long]]()

    for (i <- 0 until parallelSize) {
      val cf: CompletableFuture[Long] =
        CompletableFuture
          .supplyAsync(
            new DefaultPartitioner(map_results_broker,
              partitionedBrokers.toList,
              partitionFunction),
            partitionThreadPool)
          .whenCompleteAsync((result, exception) => {
            if (exception == null) {
              totalPartitionedElementsCount.addAndGet(result)
            } else {
              logError(s"error in computing partition ${i}", exception)
              errors.append(exception)
            }
          }, partitionThreadPool)

      partitionSubFutures += cf
    }

    val partitionFuture = CompletableFuture.allOf(partitionSubFutures.toArray: _*)
      .whenCompleteAsync((result, exception) => {
        if (exception == null) {
          partitionedBrokers.foreach(b => b.signalWriteFinish())
          logInfo(s"finished partition ops for partition")
        } else {
          errors.append(exception)
        }
        isPartitionFinished.compareAndSet(false, true)
      })

    val sender: CompletableFuture[Long] =
      CompletableFuture.supplyAsync(new GrpcShuffleSender(shuffleId = shuffleId,
          brokers = partitionedBrokers.toArray,
          targetPartitions = outputPartitions), sendRecvThreadPool)
      .whenCompleteAsync((result, exception) => {
        if (exception == null) {
          logInfo(s"finished send. total sent: ${result}")
          isSendFinished.compareAndSet(false, true)
        } else {
          logError(s"error in send", exception)
          errors.append(exception)
        }
      }, sendRecvThreadPool)

    val receiver: CompletableFuture[Long] =
      CompletableFuture.supplyAsync(new GrpcShuffleReceiver(shuffleId = shuffleId,
          outputPartition = outputPartition,
          totalPartitionsCount = outputPartitionsCount), sendRecvThreadPool)
          .whenCompleteAsync((result, exception) => {
            if (exception == null) {
              isRecvFinished.compareAndSet(false, true)
            } else {
              logError(s"error in receive", exception)
              errors.append(exception)
            }
          }, sendRecvThreadPool)

    val finalFuture = CompletableFuture.allOf(partitionFuture, sender, receiver)
      .whenComplete((result, exception) => {
        if (exception != null) {
          errors.append(exception)
        }
        shuffleFinishLatch.countDown()
      })

  }

  override def isFinished(): Boolean = isSendFinished.get() && isSendFinished.get() && isRecvFinished.get()

  override def waitUntilFinished(timeout: Long, unit: TimeUnit): Boolean = shuffleFinishLatch.await(timeout, unit)

  override def getTotalPartitionedCount(): Long = totalPartitionedElementsCount.get()

  override def hasError(): Boolean = errors.check()

  override def getError(): DistributedRuntimeException = errors
}

class DefaultPartitioner(input: Broker[(Array[Byte], Array[Byte])],
                         output: List[Broker[(Array[Byte], Array[Byte])]],
                         partitionFunction: Array[Byte] => Int,
                         // todo: configurable
                         chunkSize: Int = 1000) extends Supplier[Long] {
  var partitionedElementsCount = 0L

  override def get(): Long = {
    val chunk = new util.ArrayList[(Array[Byte], Array[Byte])](chunkSize)
    while (!input.isClosable()) {
      chunk.clear()
      input.drainTo(chunk, chunkSize)

      chunk.forEach(t => {
        output(partitionFunction(t._1)).put(t)
        partitionedElementsCount += 1L
      })
    }

    partitionedElementsCount
  }
}

class GrpcShuffleSender(shuffleId: String,
                        brokers: Array[Broker[(Array[Byte], Array[Byte])]],
                        targetPartitions: Array[ErPartition],
                        // todo: make it configurable
                        chunkSize: Int = 100,
                        packetSize: Int = 1 << 20)
  extends Supplier[Long] {

  override def get(): Long = {
    val transferClients = new Array[GrpcTransferClient](brokers.size)

    val notFinishedBrokerIndex = mutable.Set[Int]()
    for (i <- brokers.indices) notFinishedBrokerIndex.add(i)

    val newlyFinishedIdx = mutable.ListBuffer[Int]()
    var totalSent = 0L

    while (notFinishedBrokerIndex.nonEmpty) {
      notFinishedBrokerIndex.foreach(idx => {
        val curBroker = brokers(idx)
        if (!transferClients.contains(idx)) {
          val newTransferClient = new GrpcTransferClient()
          newTransferClient.init(dataBroker = curBroker, tag = s"${shuffleId}-${idx}", processor = targetPartitions(idx).processor)
          transferClients.update(idx, newTransferClient)
        }

        val client = transferClients(idx)

        client.doSend()

        if (curBroker.isClosable()) {
          client.complete()
          newlyFinishedIdx += idx
        }
      })

      newlyFinishedIdx.foreach(idx => notFinishedBrokerIndex -= idx)
      newlyFinishedIdx.clear()
    }

    brokers.foreach(b => totalSent += b.total())

    totalSent
  }
}

/*// todo: consider change (Array[Byte], Array[Byte]) to ErPair
class ShuffleSender(shuffleId: String,
                    brokers: Array[Broker[(Array[Byte], Array[Byte])]],
                    targetPartitions: Array[ErPartition],
                    // todo: make it configurable
                    chunkSize: Int = 100)
  extends Supplier[Long] {

  override def get(): Long = {
    // todo: change to event-driven
    val notFinishedBrokerIndex = mutable.Set[Int]()
    for (i <- brokers.indices) {
      notFinishedBrokerIndex.add(i)
    }

    val transferClient = new GrpcTransferClient()
    val newlyFinishedIdx = mutable.ListBuffer[Int]()
    var totalSent = 0L

    while (notFinishedBrokerIndex.nonEmpty) {
      notFinishedBrokerIndex.foreach(idx => {
        val sendBuffer = new util.ArrayList[(Array[Byte], Array[Byte])](chunkSize)
        val broker = brokers(idx)
        var isBrokerClosable = false

        this.synchronized {
          if (broker.isWriteFinished() || broker.size() >= chunkSize) {
            broker.drainTo(sendBuffer, chunkSize)
            isBrokerClosable = broker.isClosable()
          }
        }

        if (!sendBuffer.isEmpty) {
          val pairs = mutable.ListBuffer[ErPair]()
          sendBuffer.forEach(t => {
            pairs += ErPair(key = t._1, value = t._2)
          })

          val pairBatch = ErPairBatch(pairs = pairs.toArray)

          var transferStatus = StringConstants.EMPTY
          if (isBrokerClosable) {
            transferStatus = StringConstants.TRANSFER_END
            newlyFinishedIdx += idx
          }

          transferClient.send(data = pairBatch.toProto.toByteArray,
            tag = s"${shuffleId}-${idx}",
            serverNode = targetPartitions(idx).processor,
            status = transferStatus)
          totalSent += pairs.length
        }
      })

      newlyFinishedIdx.foreach(idx => notFinishedBrokerIndex -= idx)
      newlyFinishedIdx.clear()
    }

    totalSent
  }
}*/

class GrpcShuffleReceiver(shuffleId: String,
                          outputPartition: ErPartition,
                          totalPartitionsCount: Int)
  extends Supplier[Long] with Logging {

  override def get(): Long = {
    var totalRecv = 0L
    val broker = GrpcTransferService.getOrCreateBroker(s"${shuffleId}-${outputPartition.id}")

    val path = EggPair.getDbPath(outputPartition)
    logInfo(s"outputPath: ${path}")
    val outputAdapter = new RocksdbSortedKvAdapter(path)

    while (!broker.isClosable()) {
      val transferBatch = broker.poll(10, TimeUnit.SECONDS)

      if (transferBatch != null) {
        val binData: ByteString = transferBatch.getData
        val totalSize = transferBatch.getHeader.getTotalSize
        if (totalSize != binData.size()) {
          throw new IllegalStateException("batch size != bin data size")
        }

        val byteBuffer: ByteBuffer = binData.asReadOnlyByteBuffer()
        val batchBuffer = ArrayBuffer[(Array[Byte], Array[Byte])]()
        batchBuffer.sizeHint(transferBatch.getBatchSize)

        val magicNumber = Array.fill[Byte](8)(0)
        val protocolVersion = Array.fill[Byte](4)(0)

        byteBuffer.get(magicNumber)
        byteBuffer.get(protocolVersion)

        if (!magicNumber.sameElements(NetworkConstants.TRANSFER_PROTOCOL_MAGIC_NUMBER)) {
          throw new IllegalArgumentException("transfer protocol magic number not match")
        }

        if (!protocolVersion.sameElements(NetworkConstants.TRANSFER_PROTOCOL_VERSION)) {
          throw new IllegalArgumentException("protocol not supported")
        }

        val headerSize = byteBuffer.getInt
        if (headerSize > 0) {
          // todo: process header > 0
        }

        val bodySize = byteBuffer.getInt()

        while (byteBuffer.hasRemaining) {
          val kLen = byteBuffer.getInt()
          val k = new Array[Byte](kLen)
          byteBuffer.get(k)

          val vLen = byteBuffer.getInt()
          val v = new Array[Byte](vLen)
          byteBuffer.get(v)

          batchBuffer.append((k, v))
        }

        outputAdapter.writeBatch(batchBuffer.iterator)
      }
    }

    outputAdapter.close()

    totalRecv
  }
}

/*class ShuffleReceiver(shuffleId: String,
                      outputPartition: ErPartition,
                      totalPartitionsCount: Int)
  extends Supplier[Long] with Logging {
  override def get(): Long = {
    var totalWrite = 0L
    val broker = GrpcTransferService.getOrCreateBroker(s"${shuffleId}-${outputPartition.id}")

    val path = EggPair.getDbPath(outputPartition)
    logInfo(s"outputPath: ${path}")
    val outputAdapter = new RocksdbSortedKvAdapter(path)

    while (!broker.isClosable()) {
      val transferBatch = broker.poll(1, TimeUnit.SECONDS)

      if (transferBatch != null) {
        // todo: add 'parseFromBytes' to ErPairBatch to decouple from pb
        val binData = transferBatch.getData
        val pbPairBatch = Meta.PairBatch.parseFrom(binData)

        val pairBatch = pbPairBatch.fromProto()

        val result = pairBatch.pairs.map(erPair => (erPair.key, erPair.value))
        logInfo(s"result: ${result}, length: ${result.length}")

        outputAdapter.writeBatch(pairBatch.pairs.map(erPair => (erPair.key, erPair.value)).iterator)

        totalWrite += pairBatch.pairs.length
      }
    }

    outputAdapter.close()

    totalWrite
  }
}*/