package websiteMapGenerator

/**
  * Created by plai on 07/02/2017.
  */
case class Website(
                    url: String = "",
                    hTML: Option[String ],
                    external: Boolean,
                    internal: Boolean,
                    visited: Boolean
                  )
