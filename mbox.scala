import scala.jdk.CollectionConverters.*
import scala.util.Try
import org.apache.james.mime4j.mboxiterator.{CharBufferWrapper, MboxIterator}
import org.apache.james.mime4j.message.DefaultMessageBuilder
import java.util.Date
import java.nio.charset.Charset
import org.apache.james.mime4j.dom.Message
import org.apache.james.mime4j.dom.TextBody
import org.apache.james.mime4j.stream.MimeConfig
import org.apache.james.mime4j.samples.dom.MultipartMessage
import org.apache.james.mime4j.dom.Multipart
import org.apache.james.mime4j.dom.SingleBody
import java.io.InputStream

//https://github.com/apache/james-mime4j/blob/master/examples/src/main/java/org/apache/james/mime4j/samples/mbox/IterateOverMbox.java

extension (s: String) def toPath = java.nio.file.Path.of(s) 

val encIsoName = "ISO-8859-2"
val encUtfName = "UTF-8"
val EncIso = Charset.forName(encIsoName).newEncoder() 
val EncUtf = Charset.forName(encUtfName).newEncoder() 

def mboxIterator(s: String) = 
  MboxIterator.fromFile(s.toPath.toFile).charset(EncIso.charset()).build()

def parseSingleBody(sb: SingleBody, charset: String): String = 
  if charset.toLowerCase == "us-ascii" then "!!! STRANGE CHARACTERS DELETED"
  else 
    val data = String(sb.getInputStream().readAllBytes(), Charset.forName(charset))
    val parsed = 
      if data.trim.startsWith("<") 
      then 
        org.jsoup.Jsoup.parse(data).wholeText()
          .split("\n")
          .map(_.stripTrailing)
          .filter(line => line.trim.exists(ch => !ch.isWhitespace))
          .mkString("\n") 
      else data
    s"SingleBody $charset \n$parsed" 
    

case class Msg(underlying: Message):
  def from = underlying.getFrom()
  def to = underlying.getTo()
  def bcc = underlying.getBcc()
  def date = underlying.getDate()
  def subject = underlying.getSubject()
  def body: Seq[String] = 
    underlying.getBody() match
      case mp: Multipart => 
          mp.getBodyParts().asScala.map{ b => b.getBody() match 
            case sb: SingleBody => "BODYPART: " + parseSingleBody(sb, b.getCharset())
            case b => s"??? Unkown bodypart: $b"
          }.toSeq
      case sb: SingleBody => Seq("BODY: ", parseSingleBody(sb, underlying.getCharset()))
      case b => throw Exception(s"??? UNKOWN BODY: $b")

  override def toString: String = s"Subject: $subject\nDate: $date\nFrom: $from\nTo: $to"

object Msg:
  def parse(messageBytes: java.io.InputStream): Try[Msg] = 
    val b = DefaultMessageBuilder()
    b.setMimeEntityConfig(MimeConfig.PERMISSIVE)
    Try(Msg(b.parseMessage(messageBytes)))

@main def run(paths: String*) = 
  println("Welcome to mbox4s at https://github.com/bjornregnell/mbox4s")
  if paths.isEmpty then println ("missing argument: one or more paths")
  else
    for p <- paths do
      println(s"  input path: $p")
      println(s"    exists? ${p.toPath.toFile().exists()}")
      
      val mbit = mboxIterator(p)

      val it = mbit.iterator()
      
      var n = 0
      while it.hasNext() do
        val cbw: CharBufferWrapper = it.next()
        val bytes = cbw.asInputStream(EncIso.charset())
        val m = Msg.parse(bytes).get
        println("===\n"+ m + "\n---\n" + m.body.mkString("\n"))
        n += 1
      end while
      println(s"=== Total Number of messages = $n")
      