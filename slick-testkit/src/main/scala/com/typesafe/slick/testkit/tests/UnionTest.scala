package com.typesafe.slick.testkit.tests

import org.junit.Assert._
import scala.slick.ast.Dump
import com.typesafe.slick.testkit.util.{TestkitTest, TestDB}

class UnionTest(val tdb: TestDB) extends TestkitTest {
  import tdb.profile.simple._

  object Managers extends Table[(Int, String, String)]("managers") {
    def id = column[Int]("id")
    def name = column[String]("name")
    def department = column[String]("department")
    def * = id ~ name ~ department
  }

  object Employees extends Table[(Int, String, Int)]("employees") {
    def id = column[Int]("id")
    def name = column[String]("name2")
    def manager = column[Int]("manager")
    def * = id ~ name ~ manager

    // A convenience method for selecting employees by department
    def departmentIs(dept: String) = manager in Managers.where(_.department is dept).map(_.id)
  }

  @deprecated("Testing deprecated method Query.orderBy", "0.10.0-M2")
  def test {
    (Managers.ddl ++ Employees.ddl).create

    Managers.insertAll(
      (1, "Peter", "HR"),
      (2, "Amy", "IT"),
      (3, "Steve", "IT")
    )

    Employees.insertAll(
      (4, "Jennifer", 1),
      (5, "Tom", 1),
      (6, "Leonard", 2),
      (7, "Ben", 2),
      (8, "Greg", 3)
    )

    val q1 = for(m <- Managers where { _.department is "IT" }) yield (m.id, m.name)
    println("Managers in IT: "+ q1.selectStatement)
    q1.foreach(o => println("  "+o))

    val q2 = for(e <- Employees where { _.departmentIs("IT") }) yield (e.id, e.name)
    println("Employees in IT: " + q2.selectStatement)
    q2.foreach(o => println("  "+o))

    val q3 = for(x @ (id, name) <- q1 union q2; _ <- Query.orderBy(name.asc)) yield x
    Dump(q3, "q3: ")
    println()
    println("Combined and sorted: " + q3.selectStatement)
    q3.foreach(o => println("  "+o))

    assertEquals(q3.list, List((2,"Amy"), (7,"Ben"), (8,"Greg"), (6,"Leonard"), (3,"Steve")))

    (Managers.ddl ++ Employees.ddl).drop
  }

  def testUnionWithoutProjection {
    Managers.ddl.create
    Managers.insertAll(
      (1, "Peter", "HR"),
      (2, "Amy", "IT"),
      (3, "Steve", "IT")
    )

    def f (s: String) = Managers where { _.name === s}
    val q = f("Peter") union f("Amy")
    Dump(q, "q: ")
    println(q.selectStatement)
    assertEquals(Set((1, "Peter", "HR"), (2, "Amy", "IT")), q.list.toSet)

    Managers.ddl.drop
  }
}
