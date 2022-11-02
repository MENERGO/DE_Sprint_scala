package org.example

import scala.collection.mutable.MutableList

object App {
  val taxes = 13

  def secondAndThirdA(): Unit = {
    val helloWorld = "Hello World!"
    val helloWorldSecondPart = "and goodbye python!"
    val helloWorldThirdPart = "Hello python again! Back off, Scala!!!"
    println(helloWorld)
    println(helloWorld.reverse)
    println(helloWorld.toLowerCase())
    println(helloWorld.slice(0, helloWorld.length() - 1))
    println(helloWorld + " " + helloWorldSecondPart)
    println(helloWorld + " " + helloWorldThirdPart)
  }

  def thirdB(annualIncome: Int, bonus: Float, compensation: Float): Float = {
    (annualIncome - compensation) / 100 * (100 - bonus) / 100 * (100 - taxes) / 12
  }

  def thirdC(annualIncome: Int, bonus: Float, compensation: Float, salariesList: MutableList[Float]): Float = {
    val new_employee = thirdB(annualIncome, bonus, compensation)
    salariesList += new_employee
    val averageSalaryList = salariesList.sum / salariesList.size
    val attributes: MutableList[Float] = MutableList()
    for (i <- 0 to salariesList.size - 1) {
      attributes += (salariesList(i) - averageSalaryList).abs / averageSalaryList * 100
    }
    println(attributes)
    (new_employee - averageSalaryList) / averageSalaryList * 100
  }

  def thirdD(employee_salary: Float, salariesList: MutableList[Float]): Unit = {
    var new_salary_new_employee = employee_salary - thirdC(2500, 30, 6, MutableList(100, 150, 200, 80, 120, 75))
    salariesList += new_salary_new_employee
    println(salariesList.reduceLeft(_ min _))
    println(salariesList.reduceLeft(_ max _))
  }

  def thirdE(employee_1: Int, employee_2: Int, salariesList: MutableList[Float]): MutableList[Float] = {
    salariesList += employee_1
    salariesList += employee_2
    println(salariesList.sorted)
    salariesList.sorted
  }

  def thirdF(employee_3: Int, salariesList: MutableList[Float]): MutableList[Float] = {
    var index_new_employee = 0
    for (i <- 0 to salariesList.size - 1) {
      if (salariesList(i) < employee_3) {
        index_new_employee = i
      }
    }
    val pair = salariesList.splitAt(index_new_employee)
    pair._1 ++ MutableList(employee_3.toFloat) ++ pair._2
  }

  def thirdH(salariesList: MutableList[Float]): MutableList[Float] = {
    val new_salary: MutableList[Float] = MutableList()
    for (i <- 0 to salariesList.size - 1) {
      new_salary += salariesList(i) * 1.07f
    }
    new_salary
  }


  def main(args: Array[String]) {
    secondAndThirdA()
    println(thirdB(2000, 25, 12))
    println(thirdC(2000, 25, 12, MutableList(100, 150, 200, 80, 120, 75)))
    thirdD(thirdB(2000, 25, 12), MutableList(100, 150, 200, 80, 120, 75))
    var for_f = thirdE(350, 90, MutableList(100, 150, 200, 80, 120, 75))
    var for_g = thirdF(130, for_f)
    println(for_g)
    println(thirdH(for_g))
  }

}
