package websiteMapGenerator

import scala.collection.mutable
import scala.util.Try


object siteMapGenerator {

  def main(args: Array[String]): Unit = {
    if (args.length == 0)
      println("No arguments \n try siteMapGenerator http://www.programcreek.com/")
    else if (!args(0).isEmpty) {
      println("Generating Site map for " + args(0))

      val websites = organiser(
        url = formatUrl(args(0)),
        domain = args(0).split("//")(1).split("/")(0),
        siteVisited = mutable.HashMap.empty
      )
//      websites.foreach(s => if (s.internal) println(s"internal site found ${s.url}") else println(s"external site found ${s.url}"))
    }
  }

  def organiser(url: String, domain: String, siteVisited: mutable.HashMap[String, Website]) :  mutable.HashMap[String, Website] = {
    if(!siteVisited.contains(url)) {
      val site: Website = {
        if (url.contains(domain)) {
          val html = downloadHtml(url)
          Website(url, Some(html), isinternal = true, visited = true)
        }
        else {
          println("external site found " + url)
          Website(url, None, isinternal = false, visited = true)
        }
      }

      if (site.isinternal && !siteVisited.contains(site.url)) { //Need this here as html might be empty, and thus a guard on the for will be too late as findLink2(site.hTML.get)
        println("internal site found " + url)
        for {
          nextUrl <- findLink(site.hTML.get)
        } organiser(formatUrl(nextUrl), domain, siteVisited += site.url -> site)
      }
    }
    siteVisited
  }

  def findLink(html: String): List[String] = {
    "<img\\s+[^>]*src=\"([^\"]*)=[^>]*>".r.findAllIn(html.toString).foreach({ urls =>
      println("images = " + urls.replace("<img src=\"", ""))
    }) //TODO This regex still needs tidying, might even before using a xml paraser
    """href="([a-zA-Z0-9:/\.]*)?"""".r.findAllIn(html.toString).toList
  }

  def formatUrl = replaceHrefinString _ andThen replaceIfendsWithSlash

  def replaceHrefinString(url: String): String = {
    "href=".r.replaceFirstIn(url, "").replace("\"", "")
  }

  def replaceIfendsWithSlash(url: String): String = {
    if (url.endsWith("/")) url.dropRight(1)
    else url
  }

  def downloadHtml(url: String): String = {
    Try {
      scala.io.Source.fromURL(url).mkString
    } getOrElse {
      System.out.println("Cannot reach page " + url)
      ""
    }
  }
}