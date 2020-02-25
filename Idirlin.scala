package NewsweekReader
import java.net.MalformedURLException
import java.net.ConnectException
import java.io.FileNotFoundException
import org.xml.sax.SAXParseException
import java.io.IOException
import scala.io.Source
import scala.xml._

/*
 * "You can't treat a car like a human being - a car needs love" ~ Walter Rohrl
 */

object ErrorHandler {
  /*
   * The heart of error handling mechanism.
   */
  def errout(e: Exception): String = {
    e match {
      case e: MalformedURLException => {
        "<h1 class=\"detailTitle\">Invalid article address.</h1>"
      }
      case e: ConnectException => {
        "<h1 class=\"detailTitle\">Unable to fetch the article.</h1>"
      }
      case e: FileNotFoundException => {
        "<h1 class=\"detailTitle\">Unable to find the file.</h1>"
      }
      case e: SAXParseException => {
        "<body><h1>Article contains invalid html.</h1></body>"
      }
      case e: IOException => {
        "<h1 class=\"detailTitle\">Unknown IO error</h1>"
      }
      case e: Exception => {
       "<h1 class=\"detailTitle\">Unknown error occoured</h1>"
      }
    }
  }
}

object Sanitizer {
  /*
   * Sanitizing regexes, used for stripping invalid html tags ...
   */
  val H1DT = "(<h1 class=\"detailTitle\"(.*?)>((.|\\s)*?)<\\/h1>)".r
  val ARTDET = "(<div class=\"articleDetail(?:.*?)>(?:[\\s\\S])*?<\\/div>[\\s]+<\\/div>)".r

  def sanitize(page: String): String = {
    /*
     * Strip non relevant html to avoid invalid xml exceptions ...
     */
    val title =H1DT.findFirstIn(page).mkString
    val body = ARTDET.findFirstIn(page).mkString
    return "<body>" + title + body + "</body>"
  }
}

class PageTree(val xml_tree: Elem){
  /*
   * main parsing object, take text only from only H1, H3 and P tags.
   */
  val body = xml_tree
  val sb = new StringBuilder()
  parsePage(body)

  def appendHeader(txt: String) = {
    sb.append(txt)
    sb.append("\n")
  }

  def appendParagraph(txt: String) = {
    sb.append("\t")
    appendHeader(txt)
  }

  def parsePage(node: Node): Any = {
    (node \ "_").foreach { tag =>
      tag match {
        case <h1>{text}</h1> => appendHeader(tag.text)
        case <h3>{text}</h3> => appendHeader(tag.text)
        case <p>{text}</p> => appendParagraph(tag.text)
        case _ => parsePage(tag)
      }
    }
  }

  def getPage(): String = {
    return sb.toString()
  }
}

class Fetcher(val address: String) {
  /*
   * Fetch the content from remote location, turn it to string or xml tree
   */
  val url = address
  val str = Sanitizer.sanitize(getPage())

  def getPage() = {
    try {
      Source.fromURL(url).mkString
    } catch {
      case e: Exception => ErrorHandler.errout(e)
    }
  }

  def getPageTree() = {
    try {
      scala.xml.XML.loadString(str)
    } catch {
      case e: Exception => scala.xml.XML.loadString(ErrorHandler.errout(e))
    }
  }
}

class Reader(val filepath: String) extends Fetcher(filepath) {
  /*
   * Fetch content from local location
   */
  override def getPage() = {
    try{
      Source.fromFile(url).getLines.mkString
    } catch {
      case e:Exception => ErrorHandler.errout(e)
    }
  }
}

class TxtAreaReader(val src: String) extends Fetcher(src){
  /*
   * If we get raw source from txt area, no need to load it from anywhere
   */
  override def getPage() = url
}

object Article {
  /*
   * get article XML tree.
   */
  def getArticle(url : String, local : Boolean = false): Elem = {
    val tree = local match {
      case false => new Fetcher(url).getPageTree()
      case true => new Reader(url).getPageTree()
    }
    return tree
  }
}
