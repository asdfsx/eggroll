package com.webank.eggroll.rollframe.pytorch.script
import com.webank.eggroll.format.{ColumnFrame, FrameBatch, FrameSchema, FrameUtils}
import com.webank.eggroll.rollframe.pytorch.native.LibraryLoader
import com.webank.eggroll.rollframe.pytorch.{Torch, TorchTensor}
import junit.framework.TestCase
import org.junit.{Before, Test}

class ScriptTest {
  @Before
  def loadLibrary(): Unit = {
    LibraryLoader.load
  }
  @Test
  def runScriptMapTest(): Unit ={
    // make columnFrame
    val matrixRows = 10
    val matrixCols = 5
    val newMatrixCols = 3
    val fb = new FrameBatch(new FrameSchema(FrameSchema.oneFieldSchema), matrixCols * matrixRows)
    (0 until fb.rowCount).foreach(i=>fb.writeDouble(0,i,1))
    val tensor = new TorchTensor
    tensor.setAddress(fb.rootVectors(0).getDataBufferAddress)
    tensor.setSize(fb.rowCount)
    val parameters:Array[Double] = Array(matrixCols.toDouble) ++ Array(newMatrixCols.toDouble) ++ Array.fill[Double](matrixCols*newMatrixCols)(0.5);
    // run model
    val ptr = Torch.getTorchScript("D:\\program\\Java\\torch_run_lib\\torch_model_map.pt")
    val res = Torch.run(ptr,Array(tensor),parameters)
    println(res.size)
    println(res(0),res(1))
  }

  @Test
  def runScriptMergeTest(): Unit ={
    val matrixRows = 10
    val matrixCols = 5
    val newMatrixCols = 3
    val fb = new FrameBatch(new FrameSchema(FrameSchema.oneFieldSchema), matrixCols * matrixRows)
    (0 until fb.rowCount).foreach(i=>fb.writeDouble(0,i,1))
    val tensor = new TorchTensor
    tensor.setAddress(fb.rootVectors(0).getDataBufferAddress)
    tensor.setSize(fb.rowCount)

    val fb1 = FrameUtils.fork(fb)
    val tensor1 = new TorchTensor
    tensor1.setAddress(fb1.rootVectors(0).getDataBufferAddress)
    tensor1.setSize(fb1.rowCount)

    val parameters:Array[Double] = Array(0.0)
    val ptr = Torch.getTorchScript("D:\\program\\Java\\torch_run_lib\\torch_model_merge.pt")
    val res = Torch.run(ptr,Array(tensor,tensor1),parameters)
    println(res.length)
    println(res(0))
  }

}