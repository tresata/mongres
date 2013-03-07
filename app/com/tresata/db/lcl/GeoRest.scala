package com.tresata.db.lcl

import com.mongodb.casbah.Imports._
import com.mongodb.util.{JSON => JJSON}
import java.io.File
import java.io.FileWriter


abstract class QueryObject // marker 
case class Find(val coll : String, val options : Map[String,QueryParam]) extends QueryObject
case class More(val cursorId : Int, val options : Map[String,QueryParam]) extends QueryObject
case class Exporter(val coll : String, val options : Map[String,QueryParam]) extends QueryObject

abstract class QueryParam
case class Query(val mobj : DBObject) extends QueryParam
case class Fields(val fields : DBObject) extends QueryParam
case class Paging(val size : Int, val skip : Int) extends QueryParam
case class Hint(val hint : Either[String,DBObject]) extends QueryParam
case class BatchSize(val size : Int) extends QueryParam

object GeoRest {
  
  var cachedServer : Option[GeoRest] = None
  var defaultHost = "localhost"
  var defaultPort = 27017
  
  def apply(host : String = "localhost", port : Int = 27017) = {
    cachedServer match {
      case Some(g) => g
      case None => val gr = new GeoRest(host,port); cachedServer = Some(gr); gr
    }
  }
  
  def dbObjectFromJsonString( s : String) : DBObject = {
    val obj = JJSON.parse(s)
    val mobj : DBObject = obj.asInstanceOf[DBObject]
    mobj
  }
  
  def jsonStringFromDbObject(db : DBObject) = {
    JJSON.serialize(db)
  }
}


class GeoRest(val host : String = "localhost", val port : Int = 27017) {

  var cursors = Map.empty[Int,MongoCursor]
  
  var cursorNum : Int = 0
  
  var mongoConn = MongoConnection(host,port)
  
  var tmpDirectory : File = null;
  
  
  
  def doQuery(db : String, q : QueryObject) : (Int,List[DBObject]) = {
    
    //val mongoCollection : MongoCollection = mongoConn(db)(table)
    val mongoDb : MongoDB = mongoConn(db)
    var options : Map[String,QueryParam] = Map.empty
    		// get the cursor for the query
    val (cursonNum,cursor) = q match {
      case Find(coll,opt) => options = opt; getCursor(mongoDb(coll), options)
      case More(id,opt) => options = opt; getCursor(id,options)
    }
    
    		// now fetch data from cursor
    var bs : Option[QueryParam] = options.values.find(_ match {case BatchSize(s) => true; case _ => false})
    
    var cnt = -2
    
    val size = bs match {
      case Some(BatchSize(s)) => cnt=0;s
      case None => -1
    }
    
    var results : List[DBObject] = Nil

    cursor match {
      case Some(c) => {
        while (c.hasNext && cnt < size){
	      results = c.next :: results
	      if (cnt > -1) cnt = cnt + 1
	    }
      }
      case None =>
    }
    
    (cursorNum,results)
  }
  
  def getCursor (cursorNum : Int, options : Map[String,Any])={
    (cursorNum,cursors.get(cursorNum))
  }
  
  def getCursor ( collection : MongoCollection, options : Map[String,Any])={
    try {
	    var refObj : Option[DBObject] = None //options.get("ref").asInstanceOf    // map or mongodb obj
	    var fields : Option[DBObject] = None //options.get("fields").asInstanceOf // map or mongodb obj
	    var paging : Option[Paging] = None //options.get("paging").asInstanceOf // Paging class
	    
	    for (v <- options.values){
	      v match {
	        case Query(mobj) => refObj = Some(mobj)
	        case Fields(mobj) => fields = Some(mobj)
	        case p : Paging => paging = Some(p)
	        case _  =>
	      }
	    }
	    
	    
	    var c : MongoCursor = (refObj,fields) match {
	      case (None,None) => collection.find()
	      case (Some(r),None) => collection.find(r)
	      case (None, Some(f)) => collection.find(MongoDBObject.empty,f)
	      case (Some(r),Some(f)) => collection.find(r,f)
	    }
	    
	    	// now use options to implement modifiers to cursor
	    for (v <- options.values){
	      v match {
	        case Hint(h) => c = h match {
						          case Left(s) => c.hint(s)    // String hint
						          case Right(md) => c.hint(md) // Object hint e.g. { "f1":1,"f2":1}
								}
	        case Paging(l,s) => c = c.limit(l).skip(s)
	        case BatchSize(b) => c.batchSize(b)
	        case _ =>
	      }
	    }
	    cursorNum = cursorNum + 1
	    cursors = cursors + (cursorNum -> c)
	    (cursorNum -> Some(c))
    } catch {
      case _ => (-1,None)
    }   
    
  }
  
  def doExport(db:String,q : Exporter) = {
      val mongoDb : MongoDB = mongoConn(db)
      
      val (cursorNum,cursor) = q match {
        case Exporter(coll,qry) => getCursor(mongoDb(coll),qry)
      }
      
      	// Now create temporary file to store data
      val f = File.createTempFile("geo-export",".csv.mongrestmp")
      
      if (tmpDirectory == null)
        tmpDirectory = f.getParentFile()
      
      val fr = new FileWriter(f)
      var cnt = 0
      var headerStr : Option[String] = None
      cursor match {
      	  case Some(c) => {
				        while (c.hasNext){
				          val dbo = c.next
				          if (headerStr == None){
				            headerStr = Some(dbo.keys.mkString("|"))
				            fr.write(headerStr.get + "\n")
				          }
				          val line = dbo.values.mkString("|") + "\n"
					      fr.write(line)
					      if (cnt > -1) cnt = cnt + 1
					    }
				      }
	      case None =>
	   }
      fr.close
      
      (f.getName(),cnt)
  }
  
  def getExportFile(filename:String) = {
    if (tmpDirectory == null){
      tmpDirectory = File.createTempFile("deleteme",".tmp").getParentFile()
    }
    val f = new File(tmpDirectory,filename)
    
    if (f.exists())
      Some(f)
    else
      None
  }
  
}