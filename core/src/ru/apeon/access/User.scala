package ru.apeon.access

import org.springframework.security.core.context.SecurityContextHolder
import org.springframework.security.core.userdetails.UserDetails
import ru.apeon.core.sql.Sql

/**
 * @author Anton Zherdev
 */

object User {
  private var userByName : Map[String, User] = Map.empty

  def getCurrent = get(getCurrentUserName)

  private def appendUser(user : User) : User = {
    userByName = userByName + (user.name -> user)
    //    entitiesById = entitiesById + (model.id -> model)
    user
  }

  def get(name : String) = userByName.get(name) match {
    case Some(e) => e
    case None => appendUser(Mapper.retrieve(name))
  }

  def getCurrentUserName = SecurityContextHolder.getContext.getAuthentication.getPrincipal match {
    case ud : UserDetails => ud.getUsername
    case o : Object => o.toString
  }

  private object Mapper extends Sql {
    def retrieve(name : String) : User = transaction{
      selectOne("select x_user.id as id, x_user.id_person as id_person, person.manager as manager " +
              "from x_user " +
              "join person on person.id = x_user.id_person " +
              "where x_user.userName = :name", "name" -> name) match
      {
        case Some(row) => {
          new User(row("id"), name, row("id_person"), row("manager") == "Y")
        }
        case None => throw new AccessError("User " + name + " not found")
      }
    }
  }
}

class AccessError(var s : String) extends Exception(s)

class User(val id : Int, val name : String, val personId : Int, val isManager : Boolean)