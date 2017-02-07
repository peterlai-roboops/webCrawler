package websiteMapGenerator

import scala.util.Try
import scala.collection.mutable.HashMap

object siteMapGenerator  {
var domain = ""
var visted : HashMap[String, Website] = HashMap()

  def main(args : Array[String]) : Unit = {
    if (args.length == 0)
      println ("No arguments \n try siteMapGenerator http://www.programcreek.com/")
    else if(!args(0).isEmpty){
      println("Generating Site map for " + args(0))
      domain =  args(0).split("//")(1).split("/")(0)
      organiser(new Website(), args(0))
    }
  }

  def organiser(websiteInstance : Website, url : String) :Unit ={
    val fAndThenG = replaceHrefinString _ andThen replaceIfendsWithSlash _
    websiteInstance.url = fAndThenG(url)

    if(!visted.contains(websiteInstance.url)) {
      if (websiteInstance.url.contains(domain)) {
        println("internal site found " + websiteInstance.url)
        websiteInstance.internal = true
        websiteInstance.visited = true
        downloadHtml(websiteInstance)
        visted += (websiteInstance.url -> websiteInstance)
        findLink(websiteInstance)
      }
      else {
        websiteInstance.external = true
        websiteInstance.visited = true
        println("external site found " + websiteInstance.url)
        visted += (websiteInstance.url -> websiteInstance)
      }
    }
  }


  def findLink(websiteInstance : Website) :Unit = {
    "<img\\s+[^>]*src=\"([^\"]*)=[^>]*>".r.findAllIn(websiteInstance.hTML.toString).foreach({ urls =>
      println("images = " + urls.replace("<img src=\"", ""))
    }) //This regex needs cleaning, it's spitting out too much junk at the moment.

    """href="([a-zA-Z0-9:/\.]*)?"""".r.findAllIn(websiteInstance.hTML.toString).foreach({ urls =>
        organiser(new Website(), urls)
    })
  }

  def replaceHrefinString(url : String) :String ={
    "href=".r.replaceFirstIn(url, "").replace("\"", "")
  }

  def replaceIfendsWithSlash(url : String) :String = {
    if (url.endsWith("/")) url.dropRight(1)
    else url
  }

  def downloadHtml(websiteInstance : Website) :Unit = {
    websiteInstance.hTML = Try(scala.io.Source.fromURL(websiteInstance.url).mkString) getOrElse({System.out.println("Cannot reach page " + websiteInstance.url); ""})
  }
}