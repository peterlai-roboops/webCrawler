package websiteMapGenerator

import scala.util.Try

object siteMapGenerator {

  def main(args: Array[String]): Unit = {
    if (args.length == 0)
      println("No arguments \n try siteMapGenerator http://www.allaboutscala.com/")
    else if (!args(0).isEmpty) {
      println("Generating Site map for " + args(0))

      val websites = organiser(
        sitesToVisit = List(formatUrl(args(0))),
        siteVisited = List.empty,
        domain = args(0).split("//")(1).split("/")(0)
      )
//      websites.foreach(s => if (s.internal) println(s"internal site found ${s.url}") else println(s"external site found ${s.url}"))
    }
  }

  def organiser(sitesToVisit: List[String], domain: String, siteVisited: List[String]):  List[String] = {
    if(!sitesToVisit.isEmpty) {
      val listOfFoundLinks = if (!siteVisited.contains(sitesToVisit.head) && sitesToVisit.head.contains(domain)) {
        println("Found Internal site " + sitesToVisit.head)
        findLink(downloadHtml(sitesToVisit.head))
      }
      else if (!siteVisited.contains(sitesToVisit.head) && !sitesToVisit.head.contains(domain)) {
        println("Found external site " + sitesToVisit.head)
        Nil
      } else {
        Nil
      }

      val newVisitedSites = sitesToVisit.head :: siteVisited
      val newSitesToVisit = sitesToVisit.drop(1) ++ listOfFoundLinks //Filter if they're already in the sitevisted?
      organiser(newSitesToVisit, domain, newVisitedSites)
    }
    /*
    TODO siteVisited doesn't contain whether its internal or not, might worth have a hashmap of sitevisted
    and internal|external I've left this as is because the various println shows whether its
    internal or external and it's good practice to return something in a functional program
     */
    siteVisited
  }

  def findLink(html: String): List[String] = {
    "<img\\s+[^>]*src=\"([^\"]*)=[^>]*>".r.findAllIn(html.toString).foreach({ urls =>
      println("images = " + urls.replace("<img src=\"", ""))
    }) //TODO This regex still needs tidying, I should use a xml paraser, but i just don't want to add a external dependency to this simple program
    """href="([a-zA-Z0-9:/\.]*)?"""".r.findAllIn(html.toString).map(formatUrl).filter(!_.isEmpty).toList
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