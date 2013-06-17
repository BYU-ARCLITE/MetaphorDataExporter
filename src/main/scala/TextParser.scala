import java.io.File
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

/**
 * Created with IntelliJ IDEA.
 * User: camman3d
 * Date: 6/11/13
 * Time: 10:29 AM
 * To change this template use File | Settings | File Templates.
 */

case class Infile(file: File) {

  val lines = io.Source.fromFile(file, "utf-8").getLines().toList.map(rstrip)
  var index = 0

  def rstrip(str: String): String = {
    val regex = "(.*?)\\s*$".r
    regex.findFirstMatchIn(str).get.group(1)
  }

  def current: String = lines(index)

  def next: String = {
    index += 1
    current
  }

  def peek: String = lines(index + 1)
}

object TextParser {

  val headPat = "Data [sS]ample\\s+#(\\d+)".r

  def parseBlock(infile: Infile): List[String] = {
    val lines = new ListBuffer[String]()
    if (!infile.peek.isEmpty) {
      lines.append(infile.next.trim)
      while (!infile.peek.isEmpty && !infile.peek.startsWith("\t"))
        lines.append(infile.next.trim)
    }
    lines.toList
  }

  def moveToHeader(infile: Infile) {
    while (!headPat.findFirstMatchIn(infile.peek).isDefined)
      infile.next
  }

  def parse(file: File): List[SampleItem] = {
    val infile = Infile(file)

    // Go to the first item
    var matchData: Option[Regex.Match] = None
    while (matchData.isEmpty)
      matchData = headPat.findFirstMatchIn(infile.next)
    var nextItem = SampleItem(matchData.get.group(1).toInt)
    val items = new ListBuffer[SampleItem]()

    try {
      // Skip the language
      infile.next
      var nextline = infile.next

      while (true) {
        matchData = headPat.findFirstMatchIn(nextline)
        if (matchData.isDefined) {
          if (nextItem.text.isEmpty)
            nextItem.error = "Missing Edited Text"
          else if (nextItem.text.length > 5)
            nextItem.error = "Too Many Sentences"
          else if (nextItem.text.length < 3)
            nextItem.error = "Too Few Sentences"
          else if (!nextItem.sentence.isEmpty) {
            if (!nextItem.text.contains(nextItem.sentence))
              nextItem.error = "LM Sentence does not occur in text"
            else if (nextItem.source.isEmpty || nextItem.sentence.indexOf(nextItem.source) == -1) {
              if (nextItem.target.isEmpty || nextItem.sentence.indexOf(nextItem.target) == -1)
                nextItem.error = "Neither Target or Source occurs in LM Sentence"
              else
                nextItem.error = "Source does not occur in LM Sentence"
            } else if (nextItem.target.isEmpty || nextItem.sentence.indexOf(nextItem.target) == -1)
              nextItem.error = "Target does not occur in LM Sentence"
          }
          items.append(nextItem)
          nextItem = SampleItem(matchData.get.group(1).toInt)
          infile.next
        } else if (nextline.isEmpty) { // Skip fields
        } else if (nextline == "\tGathered by")
            infile.next
        else if (nextline == "\tReviewed by")
            infile.next
        else if (nextline == "\tURL")
            infile.next
        else if (nextline == "\tLM")
            infile.next
        else if (nextline == "\tOriginal Text")
            parseBlock(infile)
        else if (nextline == "\tTranslated LM")
            parseBlock(infile)
        else if (nextline == "\tEditing Required")
            parseBlock(infile)
        // save relevant fields
        else if (nextline == "\tTarget Concept")
            nextItem.concept = infile.next.trim
        else if (nextline == "\tEdited Text")
          nextItem.text = parseBlock(infile)
        else if (nextline == "\tLM Sentence")
            nextItem.sentence = infile.next.trim
        else if (nextline == "\tLM Target")
            nextItem.target = infile.next.trim
        else if (nextline == "\tLM Source")
            nextItem.source = infile.next.trim
        else {
          nextline = nextline.trim
          nextline = if (nextline.length < 21) nextline else nextline.substring(0, 20)
          nextItem.error = s"Unrecognized Field $nextline"
          items.append(nextItem)

          moveToHeader(infile)
        }
        nextline = infile.next
      }
    } catch {
      case e: IndexOutOfBoundsException => {
        items.append(nextItem)
      }
    }
    items.toList
  }
}
