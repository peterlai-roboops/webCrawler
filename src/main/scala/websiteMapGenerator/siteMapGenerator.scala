package websiteMapGenerator

import scala.collection.mutable
import scala.util.Try


object siteMapGenerator {
  var visited : mutable.HashMap[String, Website] = mutable.HashMap()
  var domain = ""
  def main(args: Array[String]): Unit = {
    if (args.length == 0)
      println("No arguments \n try siteMapGenerator http://www.programcreek.com/")
    else if (!args(0).isEmpty) {
      println("Generating Site map for " + args(0))
      domain = args(0).split("//")(1).split("/")(0)
      organiser(formatUrl(args(0)), domain)
//      val websites: Stream[String] = organiser(formatUrl(args(0)), domain)
//      websites.foreach(s => if (s.internal) println(s"internal site found ${s.url}") else println(s"external site found ${s.url}"))
    }
  }

  def organiser(url: String, domain: String, acc: Stream[Website] = Stream.empty) = {
    if(!visited.contains(url)) {
      val site: Website = {
        if (url.contains(domain)) {
          println("internal site found " + url)
          val html = downloadHtml(url)
          Website(url, Some(html), external = false, internal = true, visited = true)
        }
        else {
          println("external site found " + url)
          Website(url, None, external = true, internal = false, visited = true)
        }
      }

      visited += (site.url -> site)
      findLink(site)
    }
//    val allSites = for {
//      nextUrl <- findLink(site).map(formatUrl).toStream
//      if !visited.contains(nextUrl)
//        nextSite <- findLink(site)
//    }yield{
//      visited += (nextUrl -> site)
//      nextSite
//    }

//    val allSites = for {
//      nextUrl <- findLink(site).map(formatUrl).toStream
//      if !(visited :+ url).contains(nextUrl)
//      nextSite <- organiser(nextUrl, domain, acc :+ site, visited :+ url)
//    } yield {
//      nextSite
//    }
//    allSites.distinct
  }

  def findLink(websiteInstance: Website): Unit = {
    "<img\\s+[^>]*src=\"([^\"]*)=[^>]*>".r.findAllIn(websiteInstance.hTML.toString).foreach({ urls =>
      println("images = " + urls.replace("<img src=\"", ""))
    })

    """href="([a-zA-Z0-9:/\.]*)?"""".r.findAllIn(websiteInstance.hTML.toString).foreach({ urls =>
      organiser(formatUrl(urls), domain)
    })
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
