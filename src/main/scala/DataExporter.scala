import java.io.File
import java.text.SimpleDateFormat
import java.util.Calendar
import scala.xml.{Node, Elem, XML}

/**
 * Created with IntelliJ IDEA.
 * User: camman3d
 * Date: 6/11/13
 * Time: 9:48 AM
 * To change this template use File | Settings | File Templates.
 */
object DataExporter {
  def main(args: Array[String]) {

    val ansfile = args(0)

    val dom = XML.load(ansfile)
    val map: Map[Int, Int] = io.Source.fromFile(args(1)).getLines().map(line => (line.substring(0,3).toInt, line.substring(4,7).toInt)).toMap
    val key = TextParser.parse(new File(args(2))).map(item => (item.number, item)).toMap

    var out = StringTable(args(3))

    def getId(node: Node): Option[Int] = {
      val id = node.attribute("id").get(0).text
      if (id.isEmpty)
        None
      else
        Some(id.substring(2).toInt)
    }

    def getKeyId(node: Node): Int = {
      val idnum = getId(node)
      if (idnum.isDefined && map.contains(idnum.get))
        map(idnum.get)
      else
        -1
    }
    val datestamp = (new SimpleDateFormat("dd MMM, yyyy")).format(Calendar.getInstance().getTime)
    out += (s"Run Date: $datestamp","Answers: 0 = miss, 1 = perfect match, # = partial match")
    out += ("Item ID", "Performer ID","LM in TI", "LM Score", "LM Sentence", "Sentence Score", "Target", "Target Score", "Source", "Source Score")

    val pid = dom.attribute("teamId").get
    val results = (dom \ "Result").sortBy(getKeyId)
    for (ans <- results) {
      val keyid = getKeyId(ans)
      val idstr = s"#$keyid"
      if (keyid != -1) {
        val keyItem = key(keyid)
        if (!keyItem.error.isEmpty) {
          val err = keyItem.error
          out += (idstr, "Key", s"Error: $err", "-", "-", "-", "-", "-", "-", "-")
        } else {

          val keyFlag = !keyItem.sentence.isEmpty
          var keySnum = if (keyFlag) keyItem.text.indexOf(keyItem.sentence) + 1 else 999

          out += (if (keyFlag) (idstr,"Key","Yes","-",s"$keySnum: " + keyItem.sentence,"-",keyItem.target,"-",keyItem.source,"-")
          else (idstr,"Key","No","-","","-","","-","","-"))

          val flag = (ans \ "LmFlag")(0).text
          val LM = (if (flag == "-1") "Maybe" else if (flag == "0") "No" else "Yes")
          val snum = (ans \ "LmSentence")(0).text.toInt
          val sentence = if (snum <= keyItem.text.size) s"$snum: " + keyItem.text(snum-1) else ""
          val target =  (ans \ "LmTargetText")(0).text
          val source =  (ans \ "LmSourceText")(0).text
          out += (idstr,pid,
            LM, (if (flag == "-1") "#" else if (flag == "1") (if (keyFlag) "1" else "") else (if (keyFlag) "0" else "1")),
            sentence, (if (snum == keySnum) "1" else if (flag == "0" && keyFlag) "0" else ""),
            target, (if (target == keyItem.target) "1" else if (flag == "0" && keyFlag) "0" else ""),
            source, (if (source == keyItem.source) "1" else if (flag == "0" && keyFlag) "0" else ""))
        }
      }
    }
    out.save()

//    val file = new File("src/main/resources/Spanish pre-test 6.5.txt")
//    val results = TextParser.parse(file)
//    println(results)
  }
}
