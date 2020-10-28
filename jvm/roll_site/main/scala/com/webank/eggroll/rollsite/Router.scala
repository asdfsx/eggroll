package com.webank.eggroll.rollsite

import java.io.{File, FileInputStream, FileOutputStream, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.util

import com.webank.eggroll.core.meta.ErEndpoint
import com.webank.eggroll.core.util.Logging
import org.json.{JSONArray, JSONObject}
import org.yaml.snakeyaml.Yaml

import scala.io.Source

case class QueryResult(point: ErEndpoint, isSecure: Boolean, isPolling: Boolean)

object Router extends Logging{
  @volatile private var routerTable = new util.HashMap()
  @volatile private var defaultEnable: Boolean = true



  def initOrUpdateRouterTable(path: String): Unit = {

    val yaml = new Yaml
    val stream = new FileInputStream(path)
    routerTable = yaml.load(stream)
    routerTable = routerTable.get("route_table")
    logDebug("routeTable:" + routerTable)
    println("routeTable:" + routerTable)
  }

  def query(partyId: String, role: String = "default"): QueryResult = {
    if (routerTable == null) {
      throw new Exception("The routing table is not initialized!")
    }

    if (!routerTable.containsKey(partyId) && !routerTable.containsKey("default")) {
      throw new Exception(s"The routing table not have current party=${partyId} and default party.")
    }

    val curParty = if (routerTable.containsKey(partyId)) {partyId} else {
      if (defaultEnable) {
        "default"
      } else {
        throw new Exception(s"The routing table not have current party=${partyId} and disable default party.")
      }

    }
    val rt = routerTable.get(curParty).asInstanceOf[util.HashMap[String, String]]

    if (!rt.containsKey(role) && !rt.containsKey("default")) {
      throw new Exception(s"The routing table not have current role=${role}")
    }

    val curRole = if (rt.containsKey(role)) {role} else {
      if (defaultEnable) {
        "default"
      } else {
        throw new Exception(s"The routing table not have current role=${role} and disable default role.")
      }
    }
    //    val default: JSONObject = routerTable.get(curParty).asInstanceOf[JSONObject]
    //      .get(curRole).asInstanceOf[JSONArray]
    //      .get(0).asInstanceOf[JSONObject]
    val default: util.HashMap[String, String] = routerTable.get(curParty).asInstanceOf[util.HashMap[String, String]]
      .get(curRole).asInstanceOf[util.HashMap[String, String]]
    val host = default.get("ip").asInstanceOf[String]
    val port = default.get("port").asInstanceOf[Int]
    var isSecure = false
    if (default.containsKey("is_secure")) {
      if (default.get("is_secure").asInstanceOf[Boolean] || default.get("is_secure").toString == "1") {
        isSecure = true
      }
    }

    var isPolling = false
    if (default.containsKey("is_polling")) {
      if (default.get("is_polling").asInstanceOf[Boolean] || default.get("is_polling").toString == "1") {
        isPolling = true
      }
    }

    QueryResult(ErEndpoint(host, port), isSecure, isPolling)
  }

  private def jsonCheck(data: String): Boolean = {
    try {
      val js = new JSONObject(data)
      js.has("route_table")
    } catch {
      case _: Throwable =>
        logError("route table data check failed.")
        false
    }
  }
  //
  //  private def yamlCheck(data: String): Boolean = {
  //    try {
  //      val stream = new FileInputStream(fileName)
  //      val conf = yaml.load(stream)
  //    } catch {
  //      throw
  //    }
  //  }

  def update(jsonString: String, path: String): Unit = {
    try {
      if (jsonCheck(jsonString)) {
        val file = new File(path)
        if (!file.getParentFile.exists) file.getParentFile.mkdirs
        if (!file.exists) file.createNewFile
        val write = new OutputStreamWriter(new FileOutputStream(file), StandardCharsets.UTF_8)
        write.write(jsonString)
        write.flush()
        write.close()
      }
    } catch {
      case e: Throwable =>
        logError("route table update failed.")
        e.printStackTrace()
        throw e
    } finally {
      initOrUpdateRouterTable(path)
    }
  }

  def get(path: String): String = {
    try {
      val confFile = new File(path)
      val fileLength = confFile.length
      val fileContent = new Array[Byte](fileLength.intValue)
      val in = new FileInputStream(confFile)
      in.read(fileContent)
      new String(fileContent, StandardCharsets.UTF_8)
    } catch {
      case e: Throwable =>
        logError("route table get failed.")
        e.printStackTrace()
        throw e
    }
  }

  def main(args: Array[String]): Unit = {
    Router.initOrUpdateRouterTable("conf/route_table.yaml")
    var ret = Router.query("10001", "fate_flow").point
    println(ret.getHost, ret.getPort)

    ret = Router.query("10001").point
    println(ret.getHost, ret.getPort)

    ret = Router.query("10001", "acd").point
    println(ret.getHost, ret.getPort)

    ret = Router.query("10002").point
    println(ret.getHost, ret.getPort)


    val str = Router.get("conf/route_table.yaml")
    println(str)

    Router.update("{testing}", "conf/route_table.yaml")
    println(Router.get("conf/route_table.yaml"))

    Router.update(str, "conf/route_table.yaml")
  }
}
