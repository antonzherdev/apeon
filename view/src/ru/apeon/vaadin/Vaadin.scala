package ru.apeon.vaadin

import com.vaadin.ui._
import com.vaadin.terminal.gwt.client.ui.AlignmentInfo.Bits

/**
 * @author Anton Zherdev
 */

trait Vaadin {
  def vertical() : LayoutCreator = new LayoutCreator(new VerticalLayout)
  def horizontal() : LayoutCreator = new LayoutCreator(new HorizontalLayout)
  def grid(columns : Int, rows  :Int) : LayoutCreator = new LayoutCreator(new GridLayout(columns, rows))
  def form() : LayoutCreator = new LayoutCreator(new FormLayout)

  final class LayoutCreator(val layout : Layout){
    var last : Component = _

    def margin : LayoutCreator = {
      layout.setMargin(true)
      this
    }

    def margin(left: Boolean, top: Boolean, right: Boolean, bottom: Boolean): LayoutCreator = {
      layout.setMargin(top, right, bottom, left)
      this
    }


    def spacing : LayoutCreator = {
      layout match {
        case g : Layout.SpacingHandler => g.setSpacing(true)
      }
      this
    }

    def full : LayoutCreator = {
      layout.setSizeFull
      this
    }

    def add(createNext: => Component) : LayoutCreator = {
      val c : Component = createNext
      layout.addComponent(c)
      last = c
      this
    }

    def align(alignment : Alignment) : LayoutCreator = {
      layout.asInstanceOf[Layout.AlignmentHandler].setComponentAlignment(last, alignment)
      this
    }

    def align(horizontal : HorizontalAlignment, vertical : VerticalAlignment) : LayoutCreator = {
      align(new Alignment(horizontal.horizontalBits | vertical.verticalBits))
    }

    def addExpanded(createNext: => Component) : LayoutCreator = {
      val c : Component = createNext
      last = c
      layout match {
        case v : VerticalLayout => c.setHeight("100%")
        case h : HorizontalLayout => c.setWidth("100%")
        case _ => c.setSizeFull
      }

      layout.addComponent(c)
      layout match {
        case l : AbstractOrderedLayout => l.setExpandRatio(c, 1.0f)
        case l : GridLayout => {}
      }
      this
    }

    def expand(aspect : Float) : LayoutCreator = {
      layout.asInstanceOf[AbstractOrderedLayout].setExpandRatio(last, aspect)
      this
    }


    def rowsRatio(rows : List[Float]) : LayoutCreator = {
      var i : Int = 0
      rows.foreach{row : Float =>
        layout.asInstanceOf[GridLayout].setRowExpandRatio(i, row)
        i += 1
      }
      this
    }

    def columnsRatio(columns : List[Float]) : LayoutCreator = {
      var i : Int = 0
      columns.foreach{column : Float =>
        layout.asInstanceOf[GridLayout].setColumnExpandRatio(i, column)
        i += 1
      }
      this
    }

    def addStyleName(name : String) : LayoutCreator  = {
      layout.addStyleName(name)
      this
    }

    def setHeight(h : String) : LayoutCreator  = {
      layout.setHeight(h)
      this
    }

    def setWidth(h : String) : LayoutCreator  = {
      layout.setWidth(h)
      this
    }
  }

  def absolute = new AbsoluteCreator(new AbsoluteLayout)

  final class AbsoluteCreator(val layout : AbsoluteLayout) {
    def margin : AbsoluteCreator = {
      layout.setMargin(true)
      this
    }

    def full : AbsoluteCreator = {
      layout.setSizeFull
      this
    }

    def add(pos : String)(f : => Component) : AbsoluteCreator = {
      layout.addComponent(f, pos)
      this
    }

    def setHeight(h : String) : AbsoluteCreator  = {
      layout.setHeight(h)
      this
    }

    def setWidth(h : String) : AbsoluteCreator  = {
      layout.setWidth(h)
      this
    }
  }
  implicit def toLayout(creator : LayoutCreator) : Layout = creator.layout
  implicit def toLayout(creator : AbsoluteCreator) : Layout = creator.layout

  def horizontalSplit( createLeft: => Component) : Split = new Split(createLeft, SplitPanel.ORIENTATION_HORIZONTAL)
  def verticalSplit( createLeft: => Component) : Split = new Split(createLeft, SplitPanel.ORIENTATION_VERTICAL)

  final class Split(left : Component, orientation: Int) {
    def split(createRight: => Component) : SplitPanel = {
      var p : SplitPanel = new SplitPanel(orientation)
      p.addComponent(left)
      var r : Component = createRight
      r.setSizeFull
      p.addComponent(r)
      p
    }
  }



  def toolbar(createToolbar : => ComponentContainer) = new Toolbar(createToolbar)

  final class Toolbar(toolbar : ComponentContainer) {
    def body(createBody: => Component) : Layout = {
      vertical.full add{
        val panel = new Panel(toolbar)
        panel.setStyleName("m-toolbar")
        panel
      } addExpanded {
        createBody
      }
    }
  }
}

trait VerticalAlignment {
  def verticalBits : Int
}
trait HorizontalAlignment {
  def horizontalBits : Int
}
object Left extends HorizontalAlignment {
  def horizontalBits = Bits.ALIGNMENT_LEFT
}
object Center extends HorizontalAlignment with VerticalAlignment {
  def horizontalBits = Bits.ALIGNMENT_HORIZONTAL_CENTER

  def verticalBits = Bits.ALIGNMENT_VERTICAL_CENTER
}
object Right extends HorizontalAlignment {
  def horizontalBits = Bits.ALIGNMENT_RIGHT
}
object Top extends VerticalAlignment {
  def verticalBits = Bits.ALIGNMENT_TOP
}
object Bottom extends VerticalAlignment {
  def verticalBits = Bits.ALIGNMENT_BOTTOM
}