package ru.apeon.order

import com.vaadin.ui._
import com.vaadin.data.Property.{ValueChangeListener, ValueChangeEvent}
import com.vaadin.ui.Window.Notification
import ru.apeon.vaadin._
import ru.apeon.cloud.{DataMap, Cloud}
import java.io.Serializable
import com.vaadin.event.ItemClickEvent.ItemClickListener
import com.vaadin.event.ItemClickEvent
import collection.mutable.ListBuffer

/**
 * @author Anton Zherdev
 */

class Order extends MyApplication with Vaadin{
  val inventories : List[DataMap] = Cloud.query("InventoryList")
  val invList: MyTable = new MyTable
  initInvList

  val title : Label = new Label("")
  title.addStyleName("h1")

  val description : Label = new Label("", Label.CONTENT_XHTML)



  val toCartButton : MyButton = MyButton("В корзину") {e : MyClickEvent =>
    Cloud.action("ToCart", Map("inventory" -> invList.getId, "kol1" -> BigDecimal(kol.getValue.toString)))
    getMainWindow.showNotification("Товар добавлен в корзину", Notification.TYPE_HUMANIZED_MESSAGE)
  }

  val cartButton : MyButton = MyButton("Корзина") {e : MyClickEvent =>
    getMainWindow.addWindow(new CartWindow())
  }

  val kol : TextField = new TextField
  kol.setValue(1)
  kol.setColumns(5)

  val searchField : SearchField = SearchField{ e =>
    if(searchText != e.text) {
      searchText = e.text
      filterInv
    }
  }

  val price : Label = new Label
  val ost : Label = new Label

  def init = {
    setMainWindow(new Window("Формирование заяки",
      toolbar{
        absolute.setHeight("35px").add("top : 5px; right: 10px"){
          cartButton
        }.add("top : 5px; left : 10px") {
          searchField
        }
      } body {
        horizontalSplit {
          horizontalSplit {
            tree
          } split {
            invList
          }
        } split {
          vertical.margin add {
            title
          } add {
            horizontal.margin(false, true, false, true).spacing.add{
              price
            }.align(Right, Center).add{
              new Label("Остаток:")
            }.align(Right, Center).add{
              ost
            }.align(Left, Center).add{
              kol
            }.align(Left, Center).add{
              toCartButton
            }.align(Left, Center)
          } addExpanded {
            description
          }
        }
      }

      ))
  }

  val tree : Tree = new Tree
  val invTree : DataMap = Cloud.query("InventoryTree").head
  class TreeItem(val id : Int, val name : String, val data : DataMap, val parent : TreeItem) extends Serializable{
    if(parent != null) parent.child += this

    val child : ListBuffer[TreeItem] = new ListBuffer[TreeItem]

    override def toString = name
    override def hashCode = id

    def inventories : List[Int] = for(inv <- data.*("inventories")) yield inv.getId
    def allInventories : List[Int] = child.foldLeft(inventories){(ret, item) =>
      ret ::: item.allInventories
    }
  }

  val rootTreeItem = new TreeItem(invTree.int("id"), invTree.!("name"), invTree, null)

  addItems(rootTreeItem)
  private def addItems(treeItem : TreeItem) : Unit = {
    tree.addItem(treeItem)
    if(treeItem.parent != null) {
      tree.setParent(treeItem, treeItem.parent)
    } else {
      tree.expandItem(treeItem)
    }
    treeItem.data("child") match {
      case Nil => tree.setChildrenAllowed(treeItem, false)
      case l : List[_] => {
        tree.setChildrenAllowed(treeItem, true)
        l.foreach { case m : DataMap =>
          addItems(new TreeItem(m.int("id"), m.!("name"), m, treeItem))
        }
      }
    }
  }
  tree.setValue(rootTreeItem)
  tree.setSelectable(true)
  tree.setImmediate(true)

  tree.addListener(new ItemClickListener{
    def itemClick(event: ItemClickEvent) = {
      currentTreeItem = event.getItemId.asInstanceOf[TreeItem]
      filterInv
    }
  })


  var currentTreeItem = rootTreeItem
  var searchText : String = ""
  def filterInv = {
    var lst : List[DataMap] = inventories

    if(currentTreeItem != rootTreeItem) {
      val inv : List[Int] = currentTreeItem.allInventories
      lst = lst.filter{ m =>
        inv.contains(m.getId)
      }
    }

    if(!searchText.isEmpty) {
      val inv : List[Int] = Cloud.query("InventorySearch", Map("text" -> ("%" + searchText + "%"))).map{_.getId}
      lst = lst.filter{ m =>
        inv.contains(m.getId)
      }
    }


    invList.setContainerDataSource(loadData(lst))
    invList.setVisibleColumns(Array("label"))

    if(invList.getValue == null) {
      if(invList.getContainerDataSource.size > 0) {
        invList.setValue(invList.getMyContainerDataSource.getIdByIndex(0))
      }
    }
  }

  invList.setValue(invList.getMyContainerDataSource.getIdByIndex(0))

  private def initInvList = {
    val data: MyIndexedContainer = loadData(inventories)

    invList.setContainerDataSource(data)
    invList.setColumnHeader("label", "ТМЦ")
    invList.setVisibleColumns(Array("label"))
    invList.setSelectable(true)
    invList.setImmediate(true)
    invList.addListener(new ValueChangeListener {
      def valueChange(event: ValueChangeEvent): Unit = {
        kol.setValue(1)
        if(invList.getValue != null) {
          title.setValue(invList("name"))
          price.setValue(invList("price").toString + " руб.")
          ost.setValue(invList("kol1"))
          description.setValue(Cloud.find("Inventory", invList.getId, List("rem"))("rem"))
        }
        else {
          title.setValue("")
          description.setValue("")
        }
      }
    })

  }


  private def loadData(inv : List[DataMap]): MyIndexedContainer = {
    var ic: MyIndexedContainer = new MyIndexedContainer
    ic.addContainerProperty("id", classOf[Integer], "")
    ic.addContainerProperty("label", classOf[Label], "")
    ic.addContainerProperty("name", classOf[String], "")
    ic.addContainerProperty("price", classOf[BigDecimal], "")
    ic.addContainerProperty("kol1", classOf[BigDecimal], "")

    inv foreach { m =>
      var id: Object = ic.addItem
      ic.update(id, "id", m("id"))
      ic.update(id, "label", new Label(
        <div><div class="m-table-item-title"><span>{m("name")}</span></div>
          <div class="m-table-item-info"><span>{m.dec("price", 2)}руб. Остаток: {m.dec("kol1", 2)}</span></div></div>.toString,
        Label.CONTENT_XHTML))

      ic.update(id, "name", m("name"))
      ic.update(id, "price", m.dec("price", 2))
      ic.update(id, "kol1", m.dec("kol1", 2))
    }

    return ic
  }
}