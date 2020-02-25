package NewsweekReader
import javafx.application.Application
import javafx.event.ActionEvent
import javafx.event.EventHandler
import javafx.scene.Scene
import javafx.scene.control.Button
import javafx.scene.control.TextField
import javafx.scene.control.TextArea
import javafx.scene.layout.FlowPane
import javafx.stage.Stage

/*
 * "Drifting is the art of stabilizing instability." ~ Walter Rohl
 */

object RuntimeData {
  /*
   * Runtime option settings
   */
  var local = false
  var gui = true
  var editable = false
  var source = false
  var sourceURL = ""
}

object Main {
  /*
   * Starting point
   */

  def parseCLA(args: Array[String]) = {
    /*
     * Parse Command Line Arguments:
     *  -l => read raw html from local source
     *  --nogui => run in text mode
     *  -e => set text area as editable, and use it as source of raw html
     *  -u url => use this as article address
     */
    var source = false
    args.foreach {arg =>
      arg match {
        case "-l" => RuntimeData.local = true
        case "--nogui" => RuntimeData.gui = false
        case "-e" => RuntimeData.editable = true
        case "-u" => source = true
        case _ => source match {
          case true => {
            RuntimeData.source = source
            RuntimeData.sourceURL = arg
            source = false
          }
          case _ =>
        }
      }
    }
  }

  def main(args: Array[String]) {
    parseCLA(args)
    RuntimeData.gui match {
      case true => Application.launch(classOf[ReaderGui], args: _*)
      case false => new ReaderText()
    }
  }
}

class ReaderText {
  /*
   * Command Line Interface
   */
  val article = Article.getArticle(RuntimeData.sourceURL, RuntimeData.local)
  println(new PageTree(article).getPage())
}

class ReaderGui extends Application {
  /*
   * Application gui.
   */
  override def start(primaryStage: Stage){
    primaryStage.setTitle("NewsweekReader")
    val address = new TextField()
    val submitBtn = new Button
    val articleArea = new TextArea()

    address.setText(RuntimeData.sourceURL)
    RuntimeData.editable match {
      case true => address.setEditable(false)
      case _ =>
    }

    articleArea.setWrapText(true)
    articleArea.setEditable(RuntimeData.editable)
    articleArea.setMinHeight(500)
    articleArea.setPrefWidth(400)

    submitBtn.setText("Get article")
    submitBtn.setPrefWidth(address.getMaxWidth())
    submitBtn.setOnAction(new EventHandler[ActionEvent](){
      @Override def handle(e: ActionEvent) {
        val article = RuntimeData.editable match {
          case false => {
            Article.getArticle(address.getText(), RuntimeData.local)
          }
          case true => new TxtAreaReader(articleArea.getText()).getPageTree()
        }
        articleArea.setText(new PageTree(article).getPage())
      }
    })

    val root = new FlowPane
    root.getChildren.add(address)
    root.getChildren.add(submitBtn)
    root.getChildren.add(articleArea)

    primaryStage.setScene(new Scene(root, 400,550))
    primaryStage.show
  }
}
