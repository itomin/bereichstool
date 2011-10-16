package beto.view

/****************************************************************************
 *
 *  Bibliotheken einbinden
 *
 ****************************************************************************/

import _root_.beto.event.ActionControl
import org.eclipse.jface.action._
import org.eclipse.swt.SWT
import org.eclipse.jface.resource.ImageDescriptor
import _root_.beto.log.Logger
import org.eclipse.swt.widgets._
import org.eclipse.jface.window.ApplicationWindow
import org.eclipse.swt.layout.{GridData, GridLayout}
import org.eclipse.swt.awt.SWT_AWT
import org.eclipse.jface.viewers.CheckboxTableViewer
import org.eclipse.swt.graphics.{Color, Point, Image, RGB}
import java.awt.{Frame, Toolkit, Dimension}

class BeToFrame(val device: Display) extends ApplicationWindow(null) with Logger {

  debug("GUI initialisiert")

  private var frame: Frame = _
  private var gwidth: Int = 0
  private var gheight: Int = 0


  private lazy val actionControl = new ActionControl(this)
  private lazy val history = actionControl.loadHistory
  private var graphEditor = new GraphEditor(actionControl)

  private lazy val darkMagenta = new RGB(139, 0, 139)
  private lazy val defaultColor = new Color(device, darkMagenta)


  private lazy val netzMenu = new MenuManager("Netz")
  private lazy val extMenu = new MenuManager("Extras")
  private lazy val historyMenu = new MenuManager("Zuletzt geöffnet")


  private lazy val fileDlg = {
    val dlg = new FileDialog(getShell, SWT.OPEN)
    dlg.setFilterNames(Array("Neues Netz (*.xml)", "Netz mit Bereichen (*.json)"))
    dlg.setFilterExtensions(Array("*.xml", "*.json"))
    dlg
  }

  private lazy val buttonUndo = new BeToAction(
    "",
    "src/main/resources/img/undo_32x32.png",
    actionControl.undo
  )

  private lazy val buttonRedo = new BeToAction(
    "",
    "src/main/resources/img/redo_32x32.png",
    actionControl.redo
  )

  private lazy val buttonBeResolve = new BeToAction(
    "",
    "src/main/resources/img/add.png",
    actionControl.resolveRange
  )

  private lazy val buttonBeGenerate = new BeToAction(
    "",
    "src/main/resources/img/delete.png",
    actionControl.newRange
  )

  private lazy val colorChooser = new BeToAction(
    "",
    "src/main/resources/img/colorchooser.png",
    actionControl.chooseColor
  )

  private lazy val buttonElementIn = new BeToAction(
    "",
    "src/main/resources/img/element_in_32.png",
    actionControl.moveElement
  )

  private lazy val buttonZoomIn = new BeToAction(
    "",
    "src/main/resources/img/Zoom-in-32.png",
    actionControl.moveElement
  )

  private lazy val buttonZoomOut = new BeToAction(
    "",
    "src/main/resources/img/Zoom-out-32.png",
    actionControl.moveElement
  )


  private lazy val newMenuItem = new BeToAction(
    "Neu",
    "",
    fileDlg.open match {
      case s: String => {
        val g = actionControl.newScenario(s)
        graphEditor.drawGraph(g)
        history.add(s)
      }
      case _ => // Vorgang abgebrochen
    }
  )

  private lazy val openMenuItem = new BeToAction(
    "Öffnen",
    "",
    actionControl.openGraph
  )

  private lazy val saveOpenItem = new BeToAction(
    "Speichern",
    "",
    actionControl.saveGraph
  )

  private lazy val exportMenuItem = new BeToAction(
    "Exportieren",
    "",
    actionControl.exportGraph
  )

  private lazy val optionsMenu = new BeToAction(
    "Optionen",
    "",
    println("not supported yet")
  )


  history.each{
    str => {
      val view = this
      val hiMenuItem = new Action(str) {
        override def run = {
          val g = actionControl.newScenario(getText)
          graphEditor.drawGraph(g)
          history.add(getText)
        }
      }
      historyMenu.add(hiMenuItem)
    }
  }


  addToolBar(SWT.FLAT | SWT.WRAP)
  addMenuBar
  addStatusLine

  /**
   * Elemente werden mittels Composite in einem Container gruppiert.
   * Auf die Gruppe wird anschließend ein GridLyout angewendet, da der Container
   * sich nicht um die Anordnung der Elemente kümmert.
   *
   * GridLayout positioniert alle Elemente tabellarisch von links nach
   * rechts. Das Layout für die einzelne Tabellenzelle wird über Griddata
   * beeinflusst.
   *
   */
  override def createContents(parent: Composite): Control = {

    /*--------------------   Allgemeines Fenster  -------------------------*/


    /*
    *  "Äußere" Composite teile die GUI in zwei Bereiche
    *  (rechts: Navigation, links: Netzdarstellung)
    */
    val container = new Composite(parent, SWT.NONE)
    val containerLayout = new GridLayout
    containerLayout.numColumns = 2


    /*-----------------------   Linkes Bedienfeld   ------------------------*/


    /*
    *  Die Navigationsschicht wird in 6 Spalten unterteilt
    */
    val compositeAttribute = new Composite(container, SWT.BORDER)
    val compositAttrLayout = new GridLayout
    compositAttrLayout.numColumns = 6



    /*
    * Breite des linken Bedienfeldes ist 1/4 der Gui breit
    */
    val gd_compositeAttribute = new GridData(GridData.FILL_VERTICAL)
    gd_compositeAttribute.widthHint = windowWidth / 4
    compositeAttribute.setLayoutData(gd_compositeAttribute)


    /*
    * Layout für die Zelle des Labels Bereiche setzen
    */

    val lblBereiche = new Label(compositeAttribute, SWT.NONE)
    lblBereiche.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, false, false, 3, 1))
    lblBereiche.setText("Bereiche:")

    val textSuchBereiche = new Text(compositeAttribute, SWT.BORDER);
    textSuchBereiche.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));

    /*
    * Layout für die Zelle der Bereichsliste setzen
    */
    val checkboxTableViewerBereiche = CheckboxTableViewer
      .newCheckList(compositeAttribute, SWT.BORDER | SWT.FULL_SELECTION)
    val table = checkboxTableViewerBereiche.getTable
    table.setLinesVisible(true);
    val gd_list = new GridData(SWT.FILL, SWT.FILL, true, false, 6, 1)
    gd_list.heightHint = 140
    table.setLayoutData(gd_list)
    checkboxTableViewerBereiche.setContentProvider(new BereicheContentProvider)
    checkboxTableViewerBereiche.setLabelProvider(new MyLabelProvider)
    checkboxTableViewerBereiche.setInput(Array("Szenario_XY", "Ursuppe_10",
      "Elten-Scheidt", "VEO-Paffrath",
      "Ursuppe", "Westraum",
      "VEO-Scheidt", "VO-Paffrath",
      "Elten-Werne", "VEO-Zons", "Ost"));


    /*
    * Leerzeile einfügen
    */
    val lblLeer = new Label(compositeAttribute, SWT.NONE)
    val gd_leer = new GridData(SWT.FILL, SWT.FILL, false, false, 6, 1)
    gd_leer.heightHint = 10
    lblLeer.setLayoutData(gd_leer)


    /*
    *  Layout für Zelle der aktuell gewählten Farbe setzen
    */
    val lblactColor = new Label(compositeAttribute, SWT.NONE)
    val gd_lblA = new GridData(SWT.FILL, SWT.FILL, false, false, 1, 1)
    gd_lblA.widthHint = 50
    lblactColor.setLayoutData(gd_lblA)
    lblactColor.setBackground(defaultColor)


    /*
    * Button für die Farbauswahl wird einfach ohne Layout eingefügt
    */
    val colorchooser = new Button(compositeAttribute, SWT.NONE)
    colorchooser.setImage(new Image(device, "src/main/resources/img/colorchooser_16x16.png"))


    /*
    *  Layout für die Zelle des Spinners setzen
    */
    val spinner = new Spinner(compositeAttribute, SWT.BORDER)
    val gd_Spinner = new GridData(SWT.FILL, SWT.CENTER, false, false, 3, 1)
    gd_Spinner.widthHint = 50
    spinner.setLayoutData(gd_Spinner)


    /*
    *  Layout für die Zelle des Labels cm setzen
    */
    val lblCm = new Label(compositeAttribute, SWT.NONE)
    lblCm.setText(" cm")
    lblCm.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 1, 1))

    /*
    * Leerzeile einfügen
    */
    val lblLeer_2 = new Label(compositeAttribute, SWT.NONE)
    val gd_leer_2 = new GridData(SWT.FILL, SWT.FILL, false, false, 6, 1)
    gd_leer_2.heightHint = 10
    lblLeer_2.setLayoutData(gd_leer_2)

    /*
    *  Layout für Zelle des Labels Punkte: setzen
    */
    val lblPunkte = new Label(compositeAttribute, SWT.NONE)
    lblPunkte.setText("Punkte:")
    lblPunkte.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, false, false, 3, 1))

    val textSuchPunkte = new Text(compositeAttribute, SWT.BORDER);
    val gd_text_SP = new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1)
    //gd_text_SP.widthHint = 80
    textSuchPunkte.setLayoutData(gd_text_SP);

    /*
    *  Layout für Zelle der Punktliste setzen
    */
    val checkboxTableViewerPunkte = CheckboxTableViewer
      .newCheckList(compositeAttribute, SWT.BORDER | SWT.FULL_SELECTION)
    val tablePunkte = checkboxTableViewerPunkte.getTable
    tablePunkte.setLinesVisible(true);
    val gd_list_punkte = new GridData(SWT.FILL, SWT.FILL, true, false, 6, 1)
    gd_list_punkte.heightHint = 140
    tablePunkte.setLayoutData(gd_list_punkte)
    checkboxTableViewerPunkte.setContentProvider(new BereicheContentProvider)
    checkboxTableViewerPunkte.setLabelProvider(new MyLabelProvider)
    checkboxTableViewerPunkte.setInput(Array("3265", "3266", "32363", "3610",
      "5136", "5141", "52467", "5257"));



    /*-----------------------   Rechtes Bedienfeld   ------------------------*/


    val swtAwtComponent = new Composite(container, SWT.BORDER | SWT.EMBEDDED)
    swtAwtComponent.setLayoutData(new GridData(GridData.FILL_BOTH))
    frame = SWT_AWT.new_Frame(swtAwtComponent)
    frame.add(graphEditor)

    compositeAttribute.setLayout(compositAttrLayout)
    container.setLayout(containerLayout)
    parent.setSize(screenWidth, screenHeight)

    gheight = swtAwtComponent.getSize.y
    gwidth = swtAwtComponent.getSize.x

    return container

  }


  /**
   * Erzeuge MenuManager
   */
  override def createMenuManager: MenuManager = {
    val menuManager = new MenuManager("menu")
    menuManager.add(netzMenu)
    netzMenu.add(newMenuItem)
    netzMenu.add(openMenuItem)
    netzMenu.add(historyMenu)
    netzMenu.add(saveOpenItem)
    netzMenu.add(exportMenuItem)
    menuManager.add(extMenu)
    extMenu.add(optionsMenu)
    menuManager
  }

  /**
   * Erzeuge Toolbar Manager
   */
  override def createToolBarManager(style: Int): ToolBarManager = {

    val toolBarManager = new ToolBarManager(style)

    buttonRedo.setToolTipText(ToolTipMsg.redo)
    buttonUndo.setToolTipText(ToolTipMsg.undo)
    buttonBeGenerate.setToolTipText(ToolTipMsg.create)
    buttonBeResolve.setToolTipText(ToolTipMsg.delete)
    buttonElementIn.setToolTipText(ToolTipMsg.add)

    toolBarManager.add(buttonUndo)
    toolBarManager.add(buttonRedo)
    toolBarManager.add(new Separator)
    toolBarManager.add(buttonZoomIn)
    toolBarManager.add(buttonZoomOut)
    toolBarManager.add(new Separator)
    toolBarManager.add(buttonBeGenerate)
    toolBarManager.add(buttonBeResolve)
    toolBarManager.add(buttonElementIn)
    toolBarManager.add(new Separator)
    toolBarManager
  }

  /**
   * Erzeuge Statusline Manager
   */
  override def createStatusLineManager: StatusLineManager = {
    new StatusLineManager
  }

  /**
   * Configure the shell.
   * @param newShell
   */
  override def configureShell(newShell: Shell) = {
    val img = new Image(newShell.getDisplay, "src/main/resources/img/logo.png")
    newShell.setImage(img)
    super.configureShell(newShell)
    newShell.setText("Bereichstool")
  }

  /**
   * Return the initial size of the window.
   */
  override def getInitialSize(): Point = {
    new Point(screenWidth, screenHeight)
  }

  override def close: Boolean = {
    actionControl.saveHistory(history)
    super.close
  }

  /**
   * Liefert die Breit des Bildschirms
   */
  def screenWidth: Int = dimension.width

  /**
   * Liefert die Höhe des Bildschirms
   */
  def screenHeight: Int = dimension.height

  /**
   *
   */
  def windowWidth: Int = getShell.getSize.x

  /**
   *
   */
  def windowHeight: Int = getShell.getSize.y

  /**
   *
   */
  def editorWidth: Int = gwidth - 20

  /**
   *
   */
  def editorHeight: Int = gheight - 80

  /**
   * Liefert die Bildschirmdimension
   */
  def dimension: Dimension = {
    Toolkit.getDefaultToolkit.getScreenSize
  }

  def status(msg: String) = {
    setStatus(msg)
    println(msg)
    getShell.update
  }

  def getGraphEditor = graphEditor

}
