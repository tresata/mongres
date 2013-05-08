package controllers

import java.io.File
import play.api._
import play.api.mvc._
import play.api.libs.json._
import models._

import com.mongodb.casbah.Imports._

import com.tresata.db.lcl._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index("Your new application is ready.",
        Quote("Citer les pensees des autres, c'est regretter de ne pas les avoir trouvees soi-meme.","Sacha Guitry")))
  }
  
  def hello = Action {
      Ok("Do not search for the truth...only cease to cherish opinions. \n " )
  }
  
  def testAjax(thingy : String) = Action { implicit request =>
 
    val jsonObject = Json.toJson(
    	Map(
    	  "users" -> Seq(
    		Json.toJson(
    		  Map(
    		   "name" -> Json.toJson("Bob"),
    		   "age"  -> Json.toJson(31),
    		   "email" -> Json.toJson("bob@gmail.com")
    		  )    
    		),
    		Json.toJson(
    		  Map(
    		   "name" -> Json.toJson("Silly"),
    		   "age"  -> Json.toJson(22),
    		   "email" -> JsNull
    		  )    
    		)
    	  )
    	)
    )
    Ok(jsonObject)
  }
  
  def testMongo = Action { implicit request =>
  	val mongoConn = MongoConnection()
  	
  	val tomDb = mongoConn("tom")
  	
  	val storeColl = tomDb("store")
  	
  	val s1 = MongoDBObject("STORE" -> "477")
  
  	val storeRec = storeColl.findOne(s1)
  	
  	
  	storeRec.map(js => {
  		val storeJson = Json.parse(js.toString)
  		Ok(storeJson)
  	  }).getOrElse{
  	  BadRequest("Error Retrieving Data")
  	}
  	
  	storeRec match {
  	  case Some(js) => Ok(Json.parse(js.toString))
  	  case None => BadRequest("Error retrieving Data")
  	}
  	
 // 	Ok("working")
  }
  
  def find(db:String,coll:String) = Action { implicit request => 
  	val qryObj = request.queryString
  	
  	var options = Map.empty[String,QueryParam]
  	
  	for ((k,v) <- qryObj){
  	  try {
	  	  (k,v.headOption) match {
	  		case ("criteria",Some(vh)) => options = options + ("query" -> Query(GeoRest.dbObjectFromJsonString(vh)))
	  		case ("fields",Some(vh))  => options = options + ("fields" -> Fields(GeoRest.dbObjectFromJsonString(vh)))
	  		case ("paging",Some(vh))  => {
	  		  var obj : Map[String,Int] = GeoRest.dbObjectFromJsonString(vh).asInstanceOf[Map[String,Int]]
	  		  try{
	  		    options = options + ("paging" -> Paging(obj("limit"),obj("skip")))
	  		  } catch {
	  		    case e => println("Exception in Application.find method while parsing 'paging'. Exception was:" + e.toString)
	  		    case _ => println("unknown exception in Application.find method while parsing 'paging'")
	  		  }
	  		}
	  		case ("hint",Some(vh)) => {
	  		  // NOTE: index names in url query should NOT be quoted. They are not JSON values. If fields are explicitly
	  		  //       listed for the index, they are JSON and therefore the field names SHOULD be quoted.
	  		  //       Quoting index names WILL cause an error and the query will fail.
	  		  options = options + 
			  		    (if (vh.contains("{") || vh.contains("["))
			  		      ("hint" -> Hint(Right(GeoRest.dbObjectFromJsonString(vh))))
			  		    else
			  		      ("hint" -> Hint(Left(vh)))
			  		     )
	  		  }
	  		case ("batch_size",Some(vh)) => options = options + ("batch_size" -> BatchSize(vh.toInt))
	  		case _ =>
	  	  }
  	  } catch {
  	    case e => println("Error processing find action on key: " + k + ", Error: " + e.toString)
  	    case _ => 
  	  }
  	  
  	    
  	}
  	try {
		val fobj = Find(coll,options)
		  	  
		val gs = GeoRest()
		  
		val (cursorNum,res) = gs.doQuery(db, fobj)
		var resStr = ""
		(cursorNum,res) match {
		  case (cn,Nil) =>  resStr = """{"ok":1,"id":-1,"results":{}}"""
		  case (cn,res) =>  resStr = "[" + res.tail.foldLeft(res.head.toString)(_ + "," + GeoRest.jsonStringFromDbObject(_)) + "]"
							resStr = """{"ok": 1, "id": """ + cursorNum +  """, "results": """ + resStr + "}"
		}
		val resStr2 = resStr.replaceAll("""\bNaN\b""","0")
		val storeJson = Json.parse(resStr2)
		
	    Ok(storeJson)
  	} catch {
  	  case e => BadRequest("Query Failed. Received: " + e.toString + "\n" )
  	}
  }
  
  def more(db:String,coll:String) = Action { implicit request => 
  	val qryObj = request.queryString
  	
  	var options = Map.empty[String,QueryParam]
  	var cNum = -1
  	
  	for ((k,v) <- qryObj){
  	  try {
	  	  (k,v.headOption) match {
	  	    case ("id",Some(vh)) => cNum = vh.toInt 
	  		case ("batch_size",Some(vh)) => options = options + ("batch_size" -> BatchSize(vh.toInt))
	  		case _ =>
	  	  }
  	  } catch {
  	    case e => println("Error processing more action on key: " + k + ", Error: " + e.toString)
  	    case _ => 
  	  }
  	}
  	try {
		val mobj : QueryObject = More(cNum,options)
		  	  
		val gs = GeoRest()
		  
		val (cursorNum,res) = gs.doQuery(db, mobj)
		 
		val resStr = (cursorNum,res) match {
		  case (cn,Nil) =>  """{"ok":1,"id":-1,"results":{}}"""
		  case (cn,hd :: Nil) => val tmp = "[" + GeoRest.jsonStringFromDbObject(res.head) + "]"
								 """{"ok": 1, "id": """ + cursorNum +  """, "results": """ + tmp + "}"
		  case (cn,res) =>  val tmp = "[" + res.tail.foldLeft(res.head.toString)(_ + "," + GeoRest.jsonStringFromDbObject(_)) + "]"
							"""{"ok": 1, "id": """ + cursorNum +  """, "results": """ + tmp + "}"
		}
		
		
//		var resStr = "[" + res.tail.foldLeft(res.head.toString)(_ + "," + GeoRest.jsonStringFromDbObject(_)) + "]"
//		println("ResStr----" + resStr)
//		resStr = """{"ok": 1, "id": """ + cursorNum +  """, "results": """ + resStr + "}"  
		val storeJson = Json.parse(resStr)
		
	    Ok(storeJson)
  	} catch {
  	  case e => BadRequest("Query Failed. Received: " + e.toString + "\n" )
  	}
  }
  
  def authenticate = Action(parse.urlFormEncoded) { implicit request => 
  	val body = request.body
  	
  	var user = ""
  	var pw = ""
  	
  	for ((k,v) <- body){
  	  val vh = v.headOption
  	  
  	  (k,vh) match {
  	    case ("user",Some(s)) => user = s
  	    case ("pw",Some(s)) => pw = s
  	    case _ =>
  	  }
  	}
  	
  	val res = """{"ok": 1,"user": """ + user + "}"
  	
  	Ok(res)
  }
  
  def connect = Action(parse.urlFormEncoded){ implicit request => 
  	val body = request.body
  	
  	var host = ""
  	var port : Int = -1
  	
  	for ((k,v) <- body){
  	  val vh = v.headOption
  	  
  	  (k,vh) match {
  	    case ("host",Some(s)) => host = s
  	    case ("port",Some(s)) => val pint = s.toInt ; port = pint
  	    case _ =>
  	  }
  	}
  	val res = """{"ok": 1,"host": """ + host + """, "port": """ + port.toString + "}"
  	
  	Ok(res)
  }
  
  /**
   * Method receives a list of id's and exports the records to a temporary csv file and 
   * returns the url to retrieve the file.
   */
  def constructExport(db:String,coll:String) = Action{ implicit request =>
  	val body = request.body
  	
  	val idList =  body.asJson match {
  	  case Some(js : JsObject) => (js \ "id-list").asOpt[List[String]].getOrElse(List())
  	  case _ => List()
  	}
  	
  	val idField = body.asJson match {
  	  case Some(js : JsObject) => (js \ "id-field").asOpt[String]
  			  						.getOrElse(BadRequest("id-field name required for export request"))
  	  case _ => BadRequest("id-field name required for export request")
  	} 
  	
  	val requestString = "{\"" + idField + "\": { \"$in\" : [" + idList.mkString(",") + "]}}"
  	
  	val gs = GeoRest()
  	
  	val requestObject = GeoRest.dbObjectFromJsonString(requestString)
  	val qobj = Query(requestObject) 
  	
  	var options = Map[String,QueryParam]("criteria" -> qobj)
  	val (exportResourcePath,cnt) = gs.doExport(db,Exporter(coll,options))
  	
  	val res = """{"resourceUrl":"""" + exportResourcePath + """","count":""" + cnt + "}"
  	
  	Ok(res)
  }
  
  def downloadExport(db : String, filename : String) = Action {
    val gs = GeoRest()
    
    gs.getExportFile(filename) match {
      case Some(file) => Ok.sendFile(file, false, (f : File) => { val fn = f.getName; if (fn.endsWith(".mongrestmp")) fn.dropRight(11) else fn }).as("text/csv") 
 //     case Some(file) => Ok.sendFile(file, true) 
      case None => BadRequest("Export File Not found") 
    }
  }
  
  
}