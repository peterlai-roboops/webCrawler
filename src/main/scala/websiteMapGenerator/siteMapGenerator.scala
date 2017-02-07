package websiteMapGenerator

import scala.util.Try
import scala.collection.mutable.Set

object siteMapGenerator {
  var listofVistedUrl = Set[String]() //List of sites visited already, so the crawler doesn't get into a infinite loop

  def main(args : Array[String]) : Unit = {
    if (args.length == 0)
      println ("No arguments \n try siteMapGenerator http://www.programcreek.com/")
    else if(!args(0).isEmpty){
      println("Generating Site map for " + args(0))
      findLink(downloadHtml(args(0)), domain = args(0).split("//")(1).split("/")(0))
    }
  }

  def findLink(html : String, domain : String) :Unit = {
    """href="([a-zA-Z0-9:/\.]*)?"""".r.findAllIn(html.toString).foreach({ urls =>
      val fAndThenG = replaceHrefinString _ andThen endsWithSlash _ 
      val cleandUpUrl: String = fAndThenG(urls)

      if (!listofVistedUrl.contains(cleandUpUrl) && cleandUpUrl.contains(domain)) {
        println("Found internal URL " + cleandUpUrl)
        listofVistedUrl.add(cleandUpUrl)
        findLink(downloadHtml(cleandUpUrl), domain)
      }else
        println("Found external Links " + cleandUpUrl)
    })
  }

  def replaceHrefinString(text: String): String ={
    "href=".r.replaceFirstIn(text, "").replace("\"", "")
  }

  def endsWithSlash(url : String): String ={
    if (url.endsWith("/")) url.dropRight(1)
    else url
  }

  def downloadHtml(url : String): String = {
    Try(scala.io.Source.fromURL(url).mkString) getOrElse({System.out.println("Cannot reach page " + url); ""})
  }
}