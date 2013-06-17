/**
 * Created with IntelliJ IDEA.
 * User: camman3d
 * Date: 6/11/13
 * Time: 10:57 AM
 * To change this template use File | Settings | File Templates.
 */
case class SampleItem(number: Int) {
  var concept = ""
  var text: List[String] = Nil
  var sentence = ""
  var target = ""
  var source = ""
  var error = ""

  override def toString: String = {
    s"SampleItem($number, $concept, $text, $sentence, $target, $source, $error)"
  }
}
