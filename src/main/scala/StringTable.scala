import java.io.{OutputStreamWriter, FileOutputStream, File}
import java.util.Date
import org.apache.commons.lang3.StringEscapeUtils
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import scala.collection.mutable.ListBuffer

/**
 * A simple wrapper for holding and saving a table of data
 */
case class StringTable(filename: String) {

  var data: ListBuffer[List[Any]] = new ListBuffer()

  /**
   * Save the table to a CSV file
   */
  def save() {

//    // Create an output stream writer because we can specify which encoding
//    val output = new OutputStreamWriter(new FileOutputStream(new File(filename)), "utf-8")
//
//    // Format the data as a StringTable
//    val formattedData = data.map { row =>
//      row.map(s => StringEscapeUtils.escapeCsv(s.toString)).mkString(",")
//    }.mkString("\n")
//
//    // Write the data out
//    output.write(formattedData)
//    output.close()

    val workbook = new XSSFWorkbook()
    val sheet = workbook.createSheet("Data")
    data.zipWithIndex.foreach(data => {
      val rowData = data._1
      val index = data._2
      val row = sheet.createRow(index)

      rowData.zipWithIndex.foreach(data => {
        val cell = data._1
        val index = data._2
        row.createCell(index).setCellValue(cell.toString)
      })
    })
    workbook.write(new FileOutputStream(new File(filename)))
  }

  def addRow(row: List[Any]) {
    data.append(row)
  }

  def +=(row: List[Any]) {
    data.append(row)
  }

  def +=(row: Product) {
    data.append(row.productIterator.toList)
  }
}
